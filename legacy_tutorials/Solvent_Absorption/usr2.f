!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR2                                                   C
!  Purpose: This routine is called from the outer iteration loop and   C
!           is user-definable.  The user may insert code in this       C
!           routine or call appropriate user defined subroutines. This C
!           can be used for updating quantities every iteration.  Use  C
!           this routine sparingly considering the large computational C
!           over head. This routine is not called from an              C
!           IJK loop, hence all indices are undefined.                 C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR2

! Modules
!---------------------------------------------------------------------//
      USE sendrecv, only: send_recv
      USE usr, only: omega_l
      USE usr, only: fwetarea_pack
      USE usr, only: solvent_absorption
      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
!---------------------------------------------------------------------//

      IF (SOLVENT_ABSORPTION) THEN
         CALL CALC_SURFACE_TENSION
         CALL CALC_WETAREA_FRACTION
      ENDIF

!      send_recv(omega_l, 2)
!      send_recv(fwetarea_pack, 2)

      RETURN
      END SUBROUTINE USR2

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!  Subroutine: CALC_SURFACE_TENSION                                    C
!                                                                      C
!  Purpose: Master routine to compute the surface tension of the       C
!  solvent on the packing material                                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CALC_SURFACE_TENSION

! Modules
!---------------------------------------------------------------------//
      use compar, only: ijkstart3, ijkend3
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use fldvar, only: T_s, X_s
      use functions, only: fluid_at
      use param1, only: undefined, zero
      use physprop, only: mw_s
      use usr, only: index_liq
      use usr, only: omega_l0, omega_l
      use usr, only: calc_mw_mix_l
      use usr, only: calc_omega_liq
      use toleranc, only: tmax
      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
! indices
      INTEGER :: IJK
! bounded phase temperatures (K)
      DOUBLE PRECISION :: xTl
! liquid phase mole fraction
      DOUBLE PRECISION :: y_lH2O
      DOUBLE PRECISION :: y_lRNH2
! liquid phase average molecular weight (g/gmol)
      DOUBLE PRECISION :: MW_MIX_L
! MFIX solids phases indices representing liquid and solid phase
      INTEGER :: iliquid

      INCLUDE 'species.inc'
!---------------------------------------------------------------------//

! if surface tension is undefined then calculate its value
      IF (omega_l0 /= UNDEFINED) RETURN

      iliquid=index_liq

      DO IJK=IJKSTART3,IJKEND3
         IF(FLUID_AT(IJK)) THEN
! initialize
            y_lH2O = zero
            y_lRNH2 = zero

! bounded liquid phase temperature
            xTl = min(TMAX,T_s(IJK,iliquid))

! Compute average molecular weight of liquid phase
            MW_MIX_L = calc_MW_mix_l(ijk, iliquid)

! compute the mole fraction of following liquid components
            y_lRNH2 = X_s(IJK,iliquid,lRNH2)*&
                     (MW_MIX_L/MW_s(iliquid,lRNH2))    ! (mol-RNH2/mol-Mix)

            y_lH2O = X_s(IJK,iliquid,lH2O)*&
                     (MW_MIX_L/MW_s(iliquid,lH2O))     ! (mol-H2O/mol-Mix)

! Also use this routine to calculate surface tension
            Omega_L(ijk)=calc_omega_liq(xTl, y_lrnh2, y_lh2O)
         ELSE
            Omega_l(ijk) = zero
         ENDIF
      ENDDO
      RETURN
      END SUBROUTINE CALC_SURFACE_TENSION



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!  Subroutine: CALC_WETAREA_FRACTION                                   C
!                                                                      C
!  Purpose: Master routine to compute the wetted area fraction of the  C
!  solvent on the packing material.  It is the ratio of the effective  C
!  contacting area between the gas and liquid phases to the specific   C
!  surface area.                                                       C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CALC_WETAREA_FRACTION

! Modules
!---------------------------------------------------------------------//
      use compar, only: myPe
      use compar, only: ijkstart3, ijkend3
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use exit, only: mfix_exit
      use fldvar, only: ro_s, ep_s, ro_g, ep_g, d_p
      use functions, only: fluid_at
      use funits, only: dmp_log, unit_log
      use machine, only: start_log, end_log
      use param1, only: zero, undefined
      use physprop, only: mu_g
      use usr, only: index_liq, index_sol
      use usr, only: wetarea_type, wetarea_type_enum
      use usr, only: undefined_wetarea_type
      use usr, only: onda_68, billet_95, lappalainen_08, constant_waf
      use usr, only: fwetarea_pack, wetareafrac0
      use usr, only: calc_fwa_onda, calc_fwa_lapp, calc_fwa_billet
      use usr, only: omega_l
      use visc_s, only: mu_s
      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
! error index
      INTEGER :: IER
! indices
      INTEGER :: IJK
! volume fraction of liquid and gas
      DOUBLE PRECISION :: ep_liq, ep_gas
! local variables for surface tension, viscosity and density of liquid
      DOUBLE PRECISION :: omega_liq, mu_liq, rho_liq
! local variables for viscosity and density of gas
      DOUBLE PRECISION :: mu_gas, rho_gas
! packing characteristics
      DOUBLE PRECISION :: ep_pack, dp_pack
! MFIX solids phases indices representing liquid and solid phase
      INTEGER :: iliquid, isolid
! local variable for fractional wetted area
      DOUBLE PRECISION :: fractional_wa
! Minimum phase volume fraction required for density calculation
      DOUBLE PRECISION, parameter :: v_Limiter = 1.0d-6
!---------------------------------------------------------------------//
      iliquid=index_liq
      isolid=index_sol

      SELECT CASE (WETAREA_TYPE_ENUM)

      CASE(ONDA_68)
         DO IJK=IJKSTART3,IJKEND3
            IF(FLUID_AT(IJK)) THEN

! Volume fraction of liquid and bed
               ep_liq = ep_s(ijk,iliquid)
               ep_pack = ep_s(ijk,isolid)

               IF (ep_pack >v_limiter .and. ep_liq >v_limiter) then

! liquid phase properties
                  omega_liq = Omega_L(ijk)
                  mu_liq = MU_s(IJK,iliquid)
                  rho_liq = RO_S(IJK,iliquid)
! Calculate wetting-area fraction (dimensionless)
                  fractional_wa = calc_fwa_onda(ijk, omega_liq, mu_liq,&
                   rho_liq, ep_liq, iliquid)
                  FWetArea_Pack(IJK) = fractional_wa
               ELSE
                  FWetArea_Pack(IJK) = zero
               ENDIF
            ELSE
               FWetArea_Pack(IJK) = zero
            ENDIF
         ENDDO

      CASE (LAPPALAINEN_08)
         DO IJK=IJKSTART3,IJKEND3
            IF(FLUID_AT(IJK)) THEN

! Volume fraction of liquid, bed and gas
               ep_liq = ep_s(ijk,iliquid)
               ep_pack = ep_s(ijk,isolid)

               IF (ep_pack >v_limiter .and. ep_liq >v_limiter) then
                  dp_pack = d_p(ijk,isolid)

! gas phase properties
                  ep_gas = ep_g(ijk)
                  rho_gas = RO_G(IJK)
                  mu_gas = MU_G(IJK)
! liquid phase properties
                  omega_liq = Omega_L(ijk)
                  mu_liq = MU_s(IJK,iliquid)
                  rho_liq = RO_S(IJK,iliquid)
! Calculate wetting-area fraction (dimensionless)
                  fractional_wa = calc_fwa_lapp(ijk, ep_gas, mu_gas, &
                     rho_gas, omega_liq, ep_liq, mu_liq, rho_liq, &
                     ep_pack, dp_pack, iliquid)
                  FWetArea_Pack(IJK) = fractional_wa
               ELSE
                  FWetArea_Pack(IJK) = zero
               ENDIF
            ELSE
               FWetArea_Pack(IJK) = zero
            ENDIF
         ENDDO

      CASE (BILLET_95)
         DO IJK=IJKSTART3,IJKEND3
            IF(FLUID_AT(IJK)) THEN

! Volume fraction of liquid
               ep_liq = ep_s(ijk,iliquid)
               ep_pack = ep_s(ijk,isolid)

               IF (ep_pack >v_limiter .and. ep_liq >v_limiter) then
! liquid phase properties
                  mu_liq = MU_s(IJK,iliquid)
                  rho_liq = RO_S(IJK,iliquid)
! Calculate wetting-area fraction (dimensionless)
                  fractional_wa = calc_fwa_billet(ijk, ep_liq, mu_liq,&
                  rho_liq, iliquid)
                  FWetArea_Pack(IJK) = fractional_wa
               ELSE
                  FWetArea_Pack(IJK) = zero
               ENDIF
            ELSE
               FWetArea_Pack(IJK) = zero
            ENDIF
         ENDDO

      CASE (CONSTANT_WAF, UNDEFINED_WETAREA_TYPE)
! apply only to areas of domain that see liquid and packing
         DO IJK=IJKSTART3,IJKEND3
            IF(FLUID_AT(IJK)) THEN

! Volume fraction of liquid & bed
               ep_liq = ep_s(ijk,iliquid)
               ep_pack = ep_s(ijk,isolid)

               IF (ep_pack >v_limiter .and. ep_liq >v_limiter) then
                  FWetArea_Pack(IJK) = wetareafrac0
               ELSE
                  FWetArea_Pack(IJK) = zero
               ENDIF
            ELSE
               FWetArea_Pack(IJK) = zero
            ENDIF
         ENDDO

      CASE DEFAULT
           CALL START_LOG
           IF(.NOT.DMP_LOG) call open_pe_log(ier)
           IF(DMP_LOG) WRITE (*, '(A,A)') &
              'Unknown WETAREA_TYPE: ', WETAREA_TYPE
           WRITE (UNIT_LOG, '(A,A)')&
              'Unknown WETAREA_TYPE: ', WETAREA_TYPE
           CALL END_LOG
           CALL mfix_exit(myPE)
      END SELECT

      RETURN
      END SUBROUTINE CALC_WETAREA_FRACTION



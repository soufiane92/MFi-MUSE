!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CALC_MU_g                                               C
!  Purpose: Calculate the effective viscosity for a turbulent flow,    C
!           which is the sum of molecular and eddy viscosities         C
!                                                                      C
!                                                                      C
!  Comments: This routine is called even if mu_g0 is defined           C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CALC_MU_G()

! Modules
!---------------------------------------------------------------------//
      use param1, only: undefined, zero
      use physprop, only: mu_g0
! invoke user defined quantity
      USE usr_prop, only: usr_mug, calc_usr_prop
      USE usr_prop, only: gas_viscosity

      use derived_types, only: TURBULENCE_MODEL_ENUM
      use derived_types, only: MIXING_LENGTH_ENUM
      use derived_types, only: K_EPSILON_ENUM

      IMPLICIT NONE

      IF (USR_MUg) THEN
         CALL CALC_USR_PROP(Gas_Viscosity,lm=0)
      ELSEIF (MU_G0 == UNDEFINED) THEN
! this is a necessary check as calc_mu_g is called at least once for
! initialization and for other possible reasons (turbulence, ishii, etc)
         CALL CALC_DEFAULT_MUg
      ENDIF

! adjust viscosity for tubulence
      SELECT CASE( TURBULENCE_MODEL_ENUM)
      CASE(K_EPSILON_ENUM)
         CALL CALC_K_EPSILON_MU
      CASE(MIXING_LENGTH_ENUM)
         CALL CALC_LSCALE_MU
      END SELECT

      CALL SET_EPMUG_VALUES

      RETURN
      END SUBROUTINE CALC_MU_G


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: SET_EPMUG_VALUES                                        C
!  Purpose: This routine sets the internal variables epmu_g and        C
!  eplambda_g that are used in the stress calculations. If the         C
!  keyword Ishii is invoked then these quantities represent the        C
!  viscosity and second viscosity multiplied by the volume fraction    C
!  otherwise they are simply viscosity/second viscosity (i.e. are      C
!  multipled by one).                                                  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_EPMUG_VALUES

! Modules
!---------------------------------------------------------------------//
      USE compar, only: ijkstart3, ijkend3
      USE fldvar, only: epg_ifac
      USE functions, only: fluid_at
      use mms, only: use_mms
      USE param1, only: zero
      USE visc_g, only: mu_gt, epmu_gt, lambda_gt, eplambda_gt
! Local variables
!---------------------------------------------------------------------//
! cell index
      INTEGER :: IJK
!---------------------------------------------------------------------//

!      EPMU_GT(:) = EPG_IFAC(:)*MU_gt(:)
!      EPLAMBDA_GT(:) = EPG_IFAC(:)*LAMBDA_gt(:)

! Assign and update ep_g*mu_gt and ep_g*lambda_gt. This is needed even
! for a constant viscosity case
      DO IJK = ijkstart3, ijkend3
! MMS: Force constant gas viscosity at all cells including ghost cells.
         IF (FLUID_AT(IJK) .OR. USE_MMS) THEN

! if ishii then multiply by void fraction otherwise multiply by 1
            EPMU_GT(IJK)= EPG_IFAC(IJK)*MU_GT(IJK)
            EPLAMBDA_GT(IJK) = EPG_IFAC(IJK)*LAMBDA_GT(IJK)
         ELSE
            EPMU_GT(IJK) = ZERO
            EPLAMBDA_GT(IJK) = ZERO
         ENDIF   ! end if (fluid_at(ijk) .or. use_mms)
      ENDDO   ! end do (ijk=ijkstart3,ijkend3)

      RETURN
      END SUBROUTINE SET_EPMUG_VALUES


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Compute a default value of gas viscosity where gas is      C
!  assumed to be air                                                   C
!  Author: W. Sams/M. Syamlal                         Date: 18-JUL-94  C
!                                                                      C
!  Literature/Document References:                                     C
!     Sutherland, W., The Viscosity of Gases and Molecular Force,      C
!        Phil. Mag. 5:507-531, 1893. Eq 4.                             C
!     Dry Air (Ideal Gas State), The NBS-NACA Tables of Thermal        C
!        Properties of Gases, U.S. Department of Commerce, National    C
!        Bureau of Standards, Table 2.39, Morey F. C., 1950.           C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CALC_DEFAULT_MUG

! Modules
!---------------------------------------------------------------------//
      use compar, only: ijkstart3, ijkend3
      use constant, only: to_si
      use fldvar, only: T_g
      use functions, only: fluid_at
      use param1, only: zero
      use physprop, only: mu_g
      use visc_g, only: mu_gt
      use visc_g, only: lambda_gt
      IMPLICIT NONE

! Local parameters
!---------------------------------------------------------------------//
      DOUBLE PRECISION, PARAMETER :: F2O3 = 2.D0/3.D0

! Local variables
!---------------------------------------------------------------------//
! Cell indices
      INTEGER :: IJK
!---------------------------------------------------------------------//

!!$omp parallel do private(ijk) schedule(dynamic,chunk_size)
      DO IJK = ijkstart3, ijkend3
         IF (FLUID_AT(IJK)) THEN

! Gas viscosity   (in Poise or Pa.s)
! Calculating gas viscosity using Sutherland's formula with
! Sutherland's constant (C) as 110.0 and reference viscosity of
! air at 273K: mu = 1.71*10-4 poise at T = 273K
            MU_G(IJK) = to_SI*1.7D-4 * &
               (T_G(IJK)/273.0D0)**1.5D0 * (383.D0/(T_G(IJK)+110.D0))

! assign values to quantities used throughout code
            MU_GT(IJK) = MU_G(IJK)
            LAMBDA_GT(IJK) = -F2O3*MU_GT(IJK)
         ELSE
            MU_G(IJK) = ZERO
            MU_GT(IJK) = ZERO
            LAMBDA_GT(IJK) = ZERO
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE CALC_DEFAULT_MUG



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: compute turbulent eddy viscosity                           C
!  Author: S. Benyahia                                Date: May-13-04  C
!                                                                      C
!                                                                      C
!  Literature/Document References:                                     C
!   Cao, J. and Ahmadi, G., 1995, Gas-particle two-phase turbulent     C
!      flow in a vertical duct. Int. J. Multiphase Flow, vol. 21,      C
!      No. 6, pp. 1203-1228.                                           C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CALC_K_EPSILON_MU

! Modules
!---------------------------------------------------------------------//
      use compar, only: ijkstart3, ijkend3
      use drag, only: f_gs
      use fldvar, only: k_turb_g, e_turb_g
      use fldvar, only: ro_g, ep_s, ro_s
      use functions, only: fluid_at
      use param1, only: zero, one, small_number
      use physprop, only: mu_g
      use run, only: kt_type_enum, ahmadi_1995
      use turb, only: tau_1, turb_c_mu
      use visc_g, only: mu_gt, lambda_gt
      use visc_g, only: mu_gmax
      use visc_s, only: ep_star_array
      IMPLICIT NONE

! Local parameters
!---------------------------------------------------------------------//
      DOUBLE PRECISION, PARAMETER :: F2O3 = 2.D0/3.D0

! Local variables
!---------------------------------------------------------------------//
! Solids phase index
      INTEGER :: M
! Constant in turbulent viscosity formulation
      DOUBLE PRECISION :: C_MU
! particle relaxation time
      DOUBLE PRECISION :: Tau_12_st
! cell index
      INTEGER :: IJK
!---------------------------------------------------------------------//

! initialize
      C_MU = turb_c_mu

      DO IJK = ijkstart3, ijkend3
         IF (FLUID_AT(IJK)) THEN

! Correction in Ahmadi paper (Cao and Ahmadi)
            IF(KT_TYPE_ENUM == AHMADI_1995 .AND.&
               F_GS(IJK,1) > SMALL_NUMBER) THEN
! solids phase index used throughout routine...
               M = 1 ! for solids phase
               Tau_12_st = Ep_s(IJK,M)*RO_S(IJK,M)/F_GS(IJK,1)
               C_MU = turb_C_MU/(ONE+ Tau_12_st/Tau_1(IJK) * &
                  (EP_s(IJK,M)/(ONE-EP_star_array(IJK)))**3)
            ENDIF

! I'm not very confident about this correction in Peirano paper,
! but it's made available here, uncomment to use it.
! sof@fluent.com --> 02/01/05
!            IF(KT_TYPE_ENUM==SIMONIN_1996 .AND.&
!               F_GS(IJK,1) > SMALL_NUMBER) THEN
! solids phase index used throughout routine...
!               M = 1 ! for solids phase
!               Tau_12_st = Ep_s(IJK,M)*RO_S(IJK,M)/F_GS(IJK,1)
!               X_21 = Ep_s(IJK,M)*RO_S(IJK,M)/(EP_g(IJK)*RO_g(IJK))
! new definition of C_mu (equation A.12, Peirano et al. (2002),
! Powder tech. 122,69-82)
!               IF( K_12(IJK)/(2.0D0*K_Turb_G(IJK)) < ONE) &
!                  C_MU = C_MU/(ONE+ 0.314D0*X_21*Tau_12_st / Tau_1(IJK) * &
!                         (ONE - K_12(IJK)/(2.0D0*K_Turb_G(IJK))) )
!            ENDIF

! Definition of the turbulent viscosity
            MU_GT(IJK) = MU_G(IJK) + RO_G(IJK)*C_MU*&
               K_Turb_G(IJK)**2 / (E_Turb_G(IJK) + SMALL_NUMBER)

            MU_GT(IJK) = MIN(MU_GMAX, MU_GT(IJK))
            LAMBDA_GT(IJK) = -F2O3*MU_GT(IJK)
         ELSE
            MU_GT(IJK) = ZERO
            LAMBDA_GT(IJK) = ZERO
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE CALC_K_EPSILON_MU


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: compute mixing length model                                C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CALC_LSCALE_MU

! Modules
!---------------------------------------------------------------------//
      use compar, only: ijkstart3, ijkend3
      use fldvar, only: ro_g
      use functions, only: fluid_at
      use param1, only: zero
      use physprop, only: mu_g
      use turb, only: l_scale
      use visc_g, only: mu_gt, lambda_gt
      use visc_g, only: mu_gmax
      IMPLICIT NONE

! Local parameters
!---------------------------------------------------------------------//
      DOUBLE PRECISION, PARAMETER :: F2O3 = 2.D0/3.D0

! Local variables
!---------------------------------------------------------------------//
      DOUBLE PRECISION :: D_g(3,3)
! Gas velocity gradient
      DOUBLE PRECISION :: DelV_g(3,3)
! Second invariant of the deviator of D_g
      DOUBLE PRECISION :: I2_devD_g
! cell index
      INTEGER :: IJK
!---------------------------------------------------------------------//

      DO IJK = ijkstart3, ijkend3
         IF (FLUID_AT(IJK)) THEN
! Calculate the rate of strain tensor D_g
            CALL CALC_DERIV_VEL_GAS(ijk, DelV_G, D_G)

! Calculate the second invariant of the deviator of D_g
            I2_DEVD_G = ((D_G(1,1)-D_G(2,2))**2+(D_G(2,2)-D_G(3,3))**2+&
                         (D_G(3,3)-D_G(1,1))**2)/6.D0 + &
                        D_G(1,2)**2 + D_G(2,3)**2 + D_G(3,1)**2

            MU_GT(IJK) =  MU_G(IJK)+2.0*L_SCALE(IJK)*L_SCALE(IJK)*&
                                     RO_G(IJK)*SQRT(I2_DEVD_G)

            MU_GT(IJK) = MIN(MU_GMAX, MU_GT(IJK))
            LAMBDA_GT(IJK) = -F2O3*MU_GT(IJK)
         ELSE
            MU_GT(IJK) = ZERO
            LAMBDA_GT(IJK) = ZERO
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE CALC_LSCALE_MU

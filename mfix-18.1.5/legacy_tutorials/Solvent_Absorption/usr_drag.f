!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: USR_DRAG                                               !
!                                                                      !
!  Purpose: Provide a hook for user defined drag law implementation.   !
!                                                                      !
!  This routine is called from inside fluid (TFM) and particle (DES)   !
!  loops. The fluid cell index (IJK) and phase (TFM) or particle index !
!  (DES) is passed.                                                    !
!                                                                      !
!  ***************************   WARNING   **************************  !
!  *----------------------------------------------------------------*  !
!  * The dummy arguments changed in the 2015-1 MFIX Release.        *  !
!  *                                                                *  !
!  *   1) Phase index (M) is now particle index (NP) for DES. This  *  !
!  *      is reflected in the name change M --> M_NP.               *  !
!  *                                                                *  !
!  *   2) The fluid velocity was added as a dummy argument. This    *  !
!  *      provides access to the interpolated gas velocity for      *  !
!  *      coupled DES simulations.                                  *  !
!  *                                                                *  !
!  ******************************************************************  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE DRAG_USR(IJK, M_NP, lDgA, EPg, Mug, ROg, VREL, DPM, &
         ROs, lUg, lVg, lWg)

!---------------------------------------------------------------------//
      use compar, only: myPe
      use error_manager
      use exit, only: mfix_exit
      use fldvar, only: d_p, ep_s, rop_g
      use fldvar, only: u_g, v_g, w_g
      use fldvar, only: u_s, v_s, w_s
      use fun_avg, only: avg_x_e, avg_y_n, avg_z_t
      use functions, only: im_of, km_of, jm_of
      use funits, only: dmp_log, unit_log
      use indices, only: i_of
      use machine, only: start_log, end_log
      use param1, only: zero, small_number, undefined, one
      use physprop, only: mu_g0
      use usr, only: index_liq, index_sol, lam_mu_g0
      use usr, only: usr_drag_type, usr_drag_type_enum
      use usr, only: attou_99, attou_99_mod, solomenko_15
      use usr, only: lappalainen_09, lappalainen_09_mod
      use usr, only: attou_gls_momexchange
      use usr, only: lappalainen_gls_momexchange
      use usr, only: lappalainen_mod_gls_momexchange
      use usr, only: fwetarea_pack, apply_waf
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Index of fluid cell:
      INTEGER, INTENT(IN) :: IJK
! TFM SOLIDS --> Index of phase (M)
! DES SOLIDS --> Index of particle (NP); M = PIJK(NP,5)
      INTEGER, INTENT(IN) :: M_NP

! drag coefficient
      DOUBLE PRECISION, INTENT(OUT) :: lDgA
! gas volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPg
! gas laminar viscosity
      DOUBLE PRECISION, INTENT(IN) :: Mug
! gas density
      DOUBLE PRECISION, INTENT(IN) :: ROg
! Magnitude of gas-solids relative velocity
      DOUBLE PRECISION, INTENT(IN) :: VREL
! particle diameter of solids phase M or
! average particle diameter if PCF
      DOUBLE PRECISION, INTENT(IN) :: DPM
! particle density of solids phase M
      DOUBLE PRECISION, INTENT(IN) :: ROs
! fluid velocity components:
! o TFM: Averaged from faces to cell center
! o DES: Interpolated to the particle's position
      DOUBLE PRECISION, INTENT(IN) :: lUg, lVg, lWg

! Local variables
!---------------------------------------------------------------------//
! error index
      INTEGER :: IER
! solids phase index associated with the liquid and solids phase
      INTEGER :: iliquid, isolid
! local alias for viscosity
      DOUBLE PRECISION :: lmug
! diameter of solids phase and volume fraction of solids phase
      DOUBLE PRECISION :: DP_bed, EP_bed
! porosity of solids phase
      DOUBLE PRECISION :: porosity
! volume fraction of phase M
      DOUBLE PRECISION :: EPsM
! bulk density of gas phase
      DOUBLE PRECISION :: ROPg
! wetting factor to apply
      DOUBLE PRECISION :: wetfac

! variation on local relative velocity needed by certain drag models
      DOUBLE PRECISION :: lvrel, velm
! cell center velocities
      DOUBLE PRECISION :: ugc, vgc, wgc, uscm, vscm, wscm
! indices
      INTEGER :: I, IMJK, IJMK, IJKM
!---------------------------------------------------------------------//
! local alias for gas viscosity
      lMug = Mug
! constant 'physical' viscosity applicable only when specifying viscosity
      IF (LAM_MU_G0 /= UNDEFINED) THEN
         lMug = LAM_MU_G0
      ENDIF
! local alias for gas bulk density
      ROPg = ROP_G(IJK)

      isolid = index_sol
      iliquid = index_liq

! 'packing' characteristics
      Dp_bed = d_p(ijk,isolid)
      EP_bed = ep_s(ijk,isolid)
      EPsM = ep_s(ijk,M_NP)
      porosity = 1.0 - ep_bed

! apply fractional wetted area to interaction terms
      IF (APPLY_WAF) THEN
         IF (M_NP == isolid) THEN
            wetfac = ONE-fwetarea_pack(ijk)
         ELSE
            wetfac = fwetarea_pack(ijk)
         ENDIF
      ELSE
         wetfac = one
      ENDIF

      SELECT CASE (USR_DRAG_TYPE_ENUM)
         CASE(ATTOU_99_MOD, ATTOU_99, SOLOMENKO_15)
            CALL ATTOU_GLS_MOMEXCHANGE(IJK, ldgA, M_NP, EPg, &
                   lMug, ROg, VREL, DP_bed, EP_bed, wetfac)
! must divide by ep_s(m) here because calling routine multiplies by it.
! this division creates numerical issues so add logic condition
            IF (EPsM > SMALL_NUMBER) THEN
               lDgA = lDgA/EPsM
            ELSE
               lDgA = ZERO
            ENDIF

         CASE(LAPPALAINEN_09_MOD)
! This model has unique relative velocity definition and so here the
! magnitude of relative velocity is redefined accordingly
! Calculate velocity components at i, j, k
            I = I_OF(IJK)
            IMJK = IM_OF(IJK)
            IJMK = JM_OF(IJK)
            IJKM = KM_OF(IJK)

            UGC = AVG_X_E(U_G(IMJK),U_G(IJK),I)
            VGC = AVG_Y_N(V_G(IJMK),V_G(IJK))
            WGC = AVG_Z_T(W_G(IJKM),W_G(IJK))

            IF (M_NP==index_sol) THEN
! magnitude of gas-solids relative velocity
               lVREL = SQRT(UGC**2 + VGC**2 + WGC**2)
               lVREL = porosity/EPg*lVREL

            ELSEIF(M_NP==index_liq) THEN
               USCM = AVG_X_E(U_S(IMJK,M_NP),U_S(IJK,M_NP),I)
               VSCM = AVG_Y_N(V_S(IJMK,M_NP),V_S(IJK,M_NP))
               WSCM = AVG_Z_T(W_S(IJKM,M_NP),W_S(IJK,M_NP))
               VELM = SQRT(USCM**2 + VSCM**2 + WSCM**2)
! magnitude of gas-solids relative velocity
               lVREL = SQRT((UGC-USCM)**2 + (VGC-VSCM)**2 + (WGC-WSCM)**2)
               lVREL = porosity/EPg*lVREL + EPsM/EPg*VELM
            ENDIF

            CALL LAPPALAINEN_MOD_GLS_MOMEXCHANGE(IJK, lDgA, M_NP, EPg, &
                   lMug, ROg, lVREL, DP_bed, EP_bed, wetfac)
! must divide by ep_s(m) here because calling routine multiplies by it.
! this division creates numerical issues so add logic condition
            IF (EPsM > SMALL_NUMBER) THEN
               lDgA = lDgA/EPsM
            ELSE
               lDgA = ZERO
            ENDIF

         CASE(LAPPALAINEN_09)
! This model has unique relative velocity definition and so here the
! magnitude of relative velocity is redefined accordingly
! Calculate velocity components at i, j, k
            I = I_OF(IJK)
            IMJK = IM_OF(IJK)
            IJMK = JM_OF(IJK)
            IJKM = KM_OF(IJK)

            UGC = AVG_X_E(U_G(IMJK),U_G(IJK),I)
            VGC = AVG_Y_N(V_G(IJMK),V_G(IJK))
            WGC = AVG_Z_T(W_G(IJKM),W_G(IJK))

            IF (M_NP==index_sol) THEN
! magnitude of gas-solids relative velocity
               lVREL = SQRT(UGC**2 + VGC**2 + WGC**2)
               lVREL = porosity/EPg*lVREL

            ELSEIF(M_NP==index_liq) THEN
               USCM = AVG_X_E(U_S(IMJK,M_NP),U_S(IJK,M_NP),I)
               VSCM = AVG_Y_N(V_S(IJMK,M_NP),V_S(IJK,M_NP))
               WSCM = AVG_Z_T(W_S(IJKM,M_NP),W_S(IJK,M_NP))
               VELM = SQRT(USCM**2 + VSCM**2 + WSCM**2)
! magnitude of gas-solids relative velocity
               lVREL = SQRT((UGC-USCM)**2 + (VGC-VSCM)**2 + (WGC-WSCM)**2)
               lVREL = porosity/EPg*lVREL + EPsM/EPg*VELM
            ENDIF

            CALL LAPPALAINEN_GLS_MOMEXCHANGE(IJK, lDgA, M_NP, EPg, &
                   lMug, ROg, lVREL, DP_bed, EP_bed, wetfac)
! must divide by ep_s(m) here because calling routine multiplies by it.
! this division creates numerical issues so add logic condition
            IF (EPsM > SMALL_NUMBER) THEN
               lDgA = lDgA/EPsM
            ELSE
               lDgA = ZERO
            ENDIF

         CASE DEFAULT
            CALL START_LOG
            IF(.NOT.DMP_LOG) call open_pe_log(ier)
            IF(DMP_LOG) WRITE (*, '(A,A)') &
               'Unknown USR_DRAG_TYPE: ', USR_DRAG_TYPE
            WRITE (UNIT_LOG, '(A,A)')&
               'Unknown USR_DRAG_TYPE: ', USR_DRAG_TYPE
            CALL END_LOG
            CALL mfix_exit(myPE)

      END SELECT

      
      END SUBROUTINE DRAG_USR


MODULE RESET_NEW_MOD
   use calc_coeff_mod
CONTAINS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: RESET_NEW                                               C
!  Purpose: Reset the new variables with the stored previous           C
!  time-step values of field variables.                                C
!    *****Remember to modify update_old also                           C
!                                                                      C
!  Author: M. Syamlal                                 Date: FEB-6-97   C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE RESET_NEW(MFIX_DAT)

! Modules
!---------------------------------------------------------------------//
      USE fldvar
      USE physprop, only: mmax, nmax
      USE run, only: energy_eq, call_dqmom, granular_energy, species_eq
      USE scalars, only: nscalar
      USE trace, only: trd_s_c, trd_s_co
      use turb, only: k_epsilon
      IMPLICIT NONE

! Path to input file
      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! Local Variables
!---------------------------------------------------------------------//
! Indices
      INTEGER :: M
! error index
      INTEGER :: IER
!---------------------------------------------------------------------//

      EP_G(:) = EP_GO(:)
      P_G(:) = P_GO(:)
      P_STAR(:) = P_STARO(:)
      RO_G(:) = RO_GO(:)
      ROP_G(:) = ROP_GO(:)
      U_G(:) = U_GO(:)
      V_G(:) = V_GO(:)
      W_G(:) = W_GO(:)
      IF (ENERGY_EQ) T_G(:) = T_GO(:)
      IF (SPECIES_EQ(0)) THEN
        IF (NMAX(0) > 0) THEN
          X_G(:,:NMAX(0)) = X_GO(:,:NMAX(0))
        ENDIF
      ENDIF

      IF (NScalar > 0) THEN
        Scalar(:,:NScalar) = ScalarO(:,:NScalar)
      ENDIF

      IF (K_Epsilon) THEN
        K_Turb_G(:) = K_Turb_GO(:)
        E_Turb_G(:) = E_Turb_GO(:)
      ENDIF

      DO M = 1, MMAX
        ROP_S(:,M) = ROP_SO(:,M)
        If (Call_DQMOM) D_P(:,M)=D_Po(:,M)
!       If (NScalar>0) ome(:,M)=ome_o(:,M)
        IF (ENERGY_EQ) T_S(:,M) = T_SO(:,M)
        IF (GRANULAR_ENERGY) THEN
          THETA_M(:,M) = THETA_MO(:,M)
          TRD_S_C(:,M) = TRD_S_CO(:,M)
        ENDIF
        U_S(:,M) = U_SO(:,M)
        V_S(:,M) = V_SO(:,M)
        W_S(:,M) = W_SO(:,M)
        IF (SPECIES_EQ(M)) THEN
          IF (NMAX(M) > 0) THEN
            X_S(:,M,:NMAX(M)) = X_SO(:,M,:NMAX(M))
          ENDIF
        ENDIF
! species_eq do not have to be solved to involve varying density
! (could be user defined function)
        RO_S(:,M) = RO_SO(:,M)
      ENDDO

! Recalculate all coefficients
      CALL CALC_COEFF_ALL(MFIX_DAT, 0, IER)

      RETURN
      END SUBROUTINE RESET_NEW
END MODULE RESET_NEW_MOD

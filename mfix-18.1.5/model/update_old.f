!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: UPDATE_OLD                                              C
!  Purpose: Update the stored previous-time-step values of certain     C
!           field variables                                            C
!    *****Remember to modify reset_new also                            C
!                                                                      C
!  Author: M. Syamlal                                 Date: 21-JAN-92  C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE UPDATE_OLD

! Modules
!---------------------------------------------------------------------//
      USE fldvar
      USE geometry
      USE indices
      USE param
      USE param1
      USE parallel
      USE physprop
      USE run
      USE scalars
      USE trace
      USE visc_s
      use turb, only: k_epsilon
      IMPLICIT NONE

! Local Variables
!---------------------------------------------------------------------//
! Indices
      INTEGER ::  M
!---------------------------------------------------------------------//

      EP_GO(:) = EP_G(:)
      P_GO(:) = P_G(:)
      P_STARO(:) = P_STAR(:)
      RO_GO(:) = RO_G(:)
      ROP_GO(:) = ROP_G(:)
      U_GO(:) = U_G(:)
      V_GO(:) = V_G(:)
      W_GO(:) = W_G(:)
      IF (ENERGY_EQ) T_GO(:) = T_G(:)
      IF (SPECIES_EQ(0)) THEN
        IF (NMAX(0) > 0) THEN
           X_GO(:,:NMAX(0)) = X_G(:,:NMAX(0))
        ENDIF
      ENDIF

      IF (NScalar > 0) THEN
        ScalarO(:,:NScalar) = Scalar(:,:NScalar)
      ENDIF

      IF (K_Epsilon) THEN
        K_Turb_GO(:) = K_Turb_G(:)
        E_Turb_GO(:) = E_Turb_G(:)
      ENDIF

!!$omp parallel do private(M,IJK,N)
      DO M = 1, MMAX
        ROP_SO(:,M) = ROP_S(:,M)
        IF(Call_DQMOM) D_Po(:,M)=D_P(:,M)
!        IF (NScalar>0) ome_o(:,M)=ome(:,M)
        IF (ENERGY_EQ) T_SO(:,M) = T_S(:,M)
        IF (GRANULAR_ENERGY) THEN
          THETA_MO(:,M) = THETA_M(:,M)
          TRD_S_CO(:,M) = TRD_S_C(:,M)
        ENDIF
        U_SO(:,M) = U_S(:,M)
        V_SO(:,M) = V_S(:,M)
        W_SO(:,M) = W_S(:,M)
        IF (SPECIES_EQ(M)) THEN
          IF (NMAX(M) > 0) THEN
            X_SO(:,M,:NMAX(M)) = X_S(:,M,:NMAX(M))
          ENDIF
        ENDIF
! species_eq do not have to be solved to involve varying density
! (could be user defined function)
        RO_SO(:,M) = RO_S(:,M)
      ENDDO

      RETURN
      END SUBROUTINE UPDATE_OLD

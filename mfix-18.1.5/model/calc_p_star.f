! TODO:
!   p_star calculation should be based on the sum of volume fractions of
!   close-packed solids.

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: CALC_P_star                                             C
!  Purpose: Calculate P_star in cells where solids continuity is       C
!     solved                                                           C
!                                                                      C
!  Author: M. Syamlal                                 Date: 21-AUG-96  C
!  Reviewer:                                          Date:            C
!                                                                      C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified: P_STAR,                                         C
!     if yu_standish or fedors_landel: ep_star_array,                  C
!                                      ep_g_blend_start,               C
!                                      ep_g_blend_end                  C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE CALC_P_STAR(EP_G, P_STAR)

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE param
      USE param1
      USE parallel
      USE geometry
      USE indices
      USE physprop
      USE constant
      USE pgcor
      USE pscor
      USE ur_facs
      USE residual
      USE compar
      USE run
      USE visc_s
      USE solids_pressure
      USE functions
      IMPLICIT NONE
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
! Gas volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EP_g(DIMENSION_3)
! Solids pressure
      DOUBLE PRECISION, INTENT(INOUT) :: P_star(DIMENSION_3)
!-----------------------------------------------
! Local variables
!-----------------------------------------------
!!   HPF$ align P_star(:) with TT(:)
!!   HPF$ align EP_g(:) with TT(:)

! Indices
      INTEGER :: IJK
! Blend factor
      DOUBLE PRECISION :: blend
!-----------------------------------------------
! External functions
!-----------------------------------------------
      DOUBLE PRECISION :: CALC_EP_STAR
!-----------------------------------------------

!!$omp parallel do private(ijk)
!!   HPF$ independent

      DO IJK = ijkstart3, ijkend3
         IF (FLUID_AT(IJK)) THEN


            IF (YU_STANDISH .OR. FEDORS_LANDEL) THEN
! if Yu_Standish or Fedors_Landel correlations are used, then
! ep_star_array is modified. this is the only time ep_star_array is
! modified (see set_constprop).  (sof Nov-16-2005)
               EP_star_array(ijk) = calc_ep_star(ijk)

! now the values of ep_g_blend_start and ep_g_blend_end need to be
! reassigned based on the new values of ep_star_array
               IF(BLENDING_STRESS.AND.TANH_BLEND) THEN
                  ep_g_blend_start(ijk) = ep_star_array(ijk) * 0.99d0
                  ep_g_blend_end(ijk)   = ep_star_array(ijk) * 1.01d0
               ELSEIF(BLENDING_STRESS.AND.SIGM_BLEND) THEN
                  ep_g_blend_start(ijk) = EP_star_array(ijk) * 0.97d0
                  ep_g_blend_end(ijk) = EP_star_array(ijk) * 1.01d0
               ELSE
                  ep_g_blend_start(ijk) = ep_star_array(ijk)
                  ep_g_blend_end(ijk)   = ep_star_array(ijk)
               ENDIF
            ENDIF

            IF (EP_G(IJK) < EP_g_blend_end(ijk)) THEN
               P_STAR(IJK) = NEG_H(EP_G(IJK),EP_g_blend_end(ijk))
               IF(BLENDING_STRESS) THEN
                  blend =  blend_function(IJK)
                  P_STAR (IJK) = (1.0d0-blend) * P_STAR (IJK)
               ENDIF
            ELSE
               P_STAR(IJK) = ZERO
            ENDIF
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE CALC_P_STAR


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  FUNCTION: CALC_ep_star                                              C
!  Purpose: calculate the local value of maximum packing               C
!                                                                      C
!  Author: D. Gera and M. Syamlal                     Date: 31-DEC-02  C
!  Reviewer:                                          Date:            C
!  Modified: S. Benyahia                              Date: 02-May-05  C
!                                                                      C
!  Literature/Document References:                                     C
!    A.B. Yu and N. Standish. Powder Tech, 52 (1987) 233-241           C
!    R.F. Fedors and R.F. Landel. Powder Tech, 23 (1979) 225-231       C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      DOUBLE PRECISION FUNCTION CALC_ep_star(IJK)

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE param1, only: zero, one
      USE fldvar, only: d_p, ep_s
      USE physprop, only: smax
      use constant, only: ep_s_max, m_max
      USE toleranc, only: dil_ep_s
      USE run, only: call_dqmom, fedors_landel, yu_standish
      IMPLICIT NONE
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
! IJK index
      INTEGER, INTENT(IN) :: IJK
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! Indices
      INTEGER :: I, J

! start sof modifications (02-May-05)
! maximum packing for the mixture
       DOUBLE PRECISION :: P_IT(sMAX)
! true maximum packing for the mixture
       DOUBLE PRECISION :: EPs_max_local
! maximum packing fraction for indicated binary mixture
       DOUBLE PRECISION :: P_IJ(sMAX, sMAX)
! particle diameter ratio (small/large)
       DOUBLE PRECISION :: R_IJ(sMAX, sMAX)
! maximum fractional solids volume corresponding to P_IJ
       DOUBLE PRECISION :: X_IJ(sMAX, sMAX)
! fractional solids volume fraction (i.e. solids volume fraction of
! phase i normalized by total solids volume). this is Xi in eq. 22
! of Yu-Standish
       DOUBLE PRECISION :: X_I(sMAX)
! local aliases for particle diameter, solids volume fraction and the
! maximum solids volume fraction wherein the data has been sorted in
! order of coarest to finest solids phase
       DOUBLE PRECISION :: DP_sort(sMAX), EPs_sort(sMAX), &
                           EPs_max_sort(sMAX)
       DOUBLE PRECISION :: old_value
! local total solids volume fraction
       DOUBLE PRECISION :: sum_eps
! variable for intermediate calculation
       DOUBLE PRECISION :: sum_local
!-----------------------------------------------

      IF (CALL_DQMOM) THEN
! sort particles to start from coarsest to finest particles
! assigning values to local aliases
         sum_eps = zero
         DO I = 1, SMAX
            DP_sort(I) = D_P(IJK,I)
            EPs_sort(I) = EP_s(IJK,I)
            EPs_max_sort(I) = ep_s_max(I)
            sum_eps = sum_eps + ep_s(ijk,i)
         ENDDO

! sorting particles from coarse to fine
         DO I = 1, SMAX-1
            DO J = I+1, SMAX
! check if phase J is larger than phase I
               IF(DP_sort(I) < DP_sort(J)) THEN
! temporarily store phase i diameter
                  old_value = DP_sort(I)
! overwrite phase i diameter with smaller phase j diameter
                  DP_sort(I) = DP_sort(J)
! overwrite phase j diameter with stoired phase i diameter
                  DP_sort(J) = old_value

                  old_value = EPs_sort(I)
                  EPs_sort(I) = EPs_sort(J)
                  EPs_sort(J) = old_value

                  old_value = EPs_max_sort(I)
                  EPs_max_sort(I) = EPs_max_sort(J)
                  EPs_max_sort(J) = old_value
               ENDIF
            ENDDO
         ENDDO

      ELSE  ! not dqmom

! m_max stores the sorted indices so now we assign values to local
! aliases based on sorted index (largest to smallest)
         sum_eps = zero
         DO I = 1, SMAX
            DP_sort(I) = D_P(IJK,M_MAX(I))
            EPs_sort(I) = EP_s(IJK,M_MAX(I))
            EPs_max_sort(I) = ep_s_max(M_MAX(I))
            sum_eps = sum_eps + ep_s(ijk,i)
         ENDDO
      ENDIF   ! end if/else (call_dqmom)

! Define quantities needed later
! initialize
      R_IJ(:,:) = ONE
      DO I = 1, SMAX-1
         DO J = I+1, SMAX
! equation 25 in Yu-Standish; particle diameter ratio (matrix)
! the ratio is defined such that we always divide the smaller particle
! diameter by the larger. here we use the fact that the particles have
! been sorted from largest to smallest diameter
! note r_ij=1 for i=j and r_ij=r_ji
            R_IJ(I,J) = DP_sort(J)/DP_sort(I)
            R_IJ(J,I) = R_IJ(I,J)
         ENDDO   ! end do (j=1,smax)
      ENDDO   ! end do (i=1,smax)

! equation 20 in Yu-Standish; the fractional solids volume (solids
! volume fraction normalized by total solids volume)
! note calculations will still proceed appropriately if defined zero
      X_I(:) = ZERO
      IF (SUM_EPS > DIL_EP_S) THEN
         DO I=1,SMAX
            X_I(I) = EPs_sort(I)/sum_eps
         ENDDO
      ENDIF

! Begin YU_STANDISH section
! ---------------------------------------------------------------->>>
      IF(YU_STANDISH) THEN
! initialize quantities
         X_IJ(:,:) = ONE
         DO I = 1, SMAX
            DO J = 1, SMAX
               IF(R_IJ(I,J) .LE. 0.741d0) THEN
! equation 23; maximum packing fraction for binary mixture comprised
! of phase i and j
! note pij=pji and pii=pi
                  P_IJ(I,J) = EPs_max_sort(I) + EPs_max_sort(I)*&
                     (ONE-EPs_max_sort(I)) * (ONE-2.35d0*R_IJ(I,J)+&
                     1.35d0*R_IJ(I,J)*R_IJ(I,J) )
               ELSE
                  P_IJ(I,J) = EPs_max_sort(I)
               ENDIF

! equation 24; fractional solids volume for binary mixture comprised
! of phase i and j
! note xij!=xji and xii=xjj=1
               IF(J .LT. I) THEN
                  X_IJ(I,J) = (ONE - R_IJ(I,J)*R_IJ(I,J))/&
                              (2.0d0 - EPs_max_sort(I))
               ELSE
                  X_IJ(I,J) = ONE - (ONE - R_IJ(I,J)*R_IJ(I,J))/&
                                    (2.0d0 - EPs_max_sort(I))
               ENDIF
            ENDDO   ! end do (j=1,smax)
         ENDDO   ! end do (i=1,smax)

         eps_max_local = one
         DO I = 1, SMAX
            SUM_LOCAL = ZERO
            DO J = 1,SMAX
               IF (I == J) CYCLE  ! do nothing
! equation 22 denominator
! note if x_i = 0 then sum_local will remain unchanged. also in cases
! where r_ij=1 and i!=j (i.e., different phases but equal size)
! x_ij will evaluate to zero when j < i.
               IF (X_IJ(I,J) > ZERO) SUM_LOCAL = SUM_LOCAL + &
                  (ONE - EPs_max_sort(I)/P_IJ(I,J))*X_I(J)/X_IJ(I,J)
            ENDDO

! equation 22
            IF (SUM_LOCAL < ONE) THEN
               P_IT(I) = EPs_max_sort(I)/(ONE - SUM_LOCAL)
            ELSE
               P_IT(I) = eps_max_sort(i)  ! limiter
            ENDIF

! packing fraction of the mixture is the smallest of P_IT:
            EPs_max_local = MIN(P_IT(I), eps_max_local)
         ENDDO   ! end do (i=1,smax)

         CALC_EP_star = ONE - EPs_max_local
! end YU_STANDISH section
! ----------------------------------------------------------------<<<

! Begin FEDORS_LANDEL empirical correlation (for binary mixtures)
! ---------------------------------------------------------------->>>
      ELSEIF(FEDORS_LANDEL) THEN

! equation 9
         IF(X_I(1) .LE. (EPs_max_sort(1)/ &
           (EPs_max_sort(1)+ (ONE-EPs_max_sort(1))*EPs_max_sort(2))) ) THEN

            EPS_max_local = (EPs_max_sort(1)-EPs_max_sort(2) + &
               (ONE-sqrt(R_IJ(2,1)))*(ONE-EPs_max_sort(1))*&
               EPs_max_sort(2)) * &
               (EPs_max_sort(1) + (ONE-EPs_max_sort(1))*&
               EPs_max_sort(2))*X_I(1)/EPs_max_sort(1) + &
               EPs_max_sort(2)
! equation 10
         ELSE
            EPs_max_local = (ONE-sqrt(R_IJ(2,1)))*(EPs_max_sort(1) + &
               (ONE-EPs_max_sort(1))*EPs_max_sort(2))*X_I(2) + &
               EPs_max_sort(1)
         ENDIF
! this is gas volume fraction at packing
         CALC_EP_star = ONE - EPs_max_local
      ENDIF ! for Yu_Standish and Fedors_Landel correlations
! end FEDORS_LANDEL correlation
! ----------------------------------------------------------------<<<

      RETURN
      END FUNCTION CALC_ep_star

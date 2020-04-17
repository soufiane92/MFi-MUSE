!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: USR_RATES                                              !
!                                                                      !
!  Purpose:                                                            !
!                                                                      !
!  Author: J.Musser                                   Date: 10-Oct-12  !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_RATES(IJK, RATES)

      USE compar
      USE constant
      USE energy
      USE fldvar
      USE fun_avg
      USE functions
      USE funits
      USE geometry
      USE indices
      USE parallel
      USE param
      USE param1
      USE physprop
      USE run
      USE rxns
      USE sendrecv
      USE toleranc
      USE usr

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: IJK

      DOUBLE PRECISION, DIMENSION(NO_OF_RXNS), INTENT(OUT) :: RATES

!-----------------------------------------------
      INCLUDE 'species.inc'

! Reaction specific variables:
!`````````````````````````````````````````````````````````````````````//


! PhseCng:  Solid --> Gas         (mol/cm^3.s)
!---------------------------------------------------------------------//
! Note: The rate is artificial


      IF(ROP_s(IJK, 1)> ZERO) THEN
         RATES(Phase_Change) = C(1) * ROP_s(IJK, 1) / MW_s(1,Solid)
      ELSE
         RATES(Phase_Change) = ZERO
      ENDIF

      RETURN

      END SUBROUTINE USR_RATES

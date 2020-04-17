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
      DOUBLE PRECISION c_O3   ! Ozone concentration mol/cm^3

! PhseCng:  O3 --> 1.5O2         (mol/cm^3.s)
!---------------------------------------------------------------------//
      c_O3 = (RO_g(IJK)*X_g(IJK,O3)/MW_g(O3))

      RATES(Ozone_Decomp) = (ONE - EP_g(IJK)) * C(1) * c_O3

      RETURN

      END SUBROUTINE USR_RATES

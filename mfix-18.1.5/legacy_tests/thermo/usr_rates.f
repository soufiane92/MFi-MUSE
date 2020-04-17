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
      INTEGER :: L

! This case is designed so that there is one fluid cell for each
! reaction. The first fluid cell start with and IJK value of 50.
! Therefore, subtract of 49 to have a one-to-one map between the
! rates array and fluid cells.
!
! This is overly complicated but done so that the table generated in
! usr3 for this test case can be populated without directly modifying
! any real source code.
      L = IJK - 49
      RATES(L) = ONE

      RETURN
      END SUBROUTINE USR_RATES

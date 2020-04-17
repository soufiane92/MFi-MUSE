!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR3                                                   C
!  Author: Avinash Vaidheeswaran                      Date: July, 2016 C
!  Reviewer: J.Musser                                                  C
!                                                                      C
!  Purpose: Calculates the exact solution to the species transport     C
!  problem and compares with the MFIX solution.                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR3

      use fldvar, only: X_g
      use physprop, only: MW_g
! Gas phase species database names.
      use rxns, only: SPECIES_g
      Use geometry, only: imin1, jmin1, kmin1
      Use geometry, only: imax1, jmax1, kmax1

      use functions, only: funijk

      Use param1, only: zero, small_number, half

      IMPLICIT NONE

! looping indices
      integer :: ijk, j, lc1, lc2
! Exact and numerical solutions
      double precision  :: Xg(3), Xg_MFIX(3)
! Absolute and absolute relative errors
      double precision :: absErr, L1(3), L2(3), LI(3), OoLC
! file unit for output data
      integer, parameter :: fUNIT = 2030

! Exact solution
      Xg(1) = MW_G(1)/(MW_G(1) + MW_G(2) + MW_G(3))
      Xg(2) = MW_G(2)/(MW_G(1) + MW_G(2) + MW_G(3))
      Xg(3) = MW_G(3)/(MW_G(1) + MW_G(2) + MW_G(3))

      Xg_MFIX = 0.0d0

      L1 = 0.0d0
      L2 = 0.0d0
      LI = 0.0d0

      lc1 = 0
      do j = jmin1, jmax1

         ijk = funijk(imax1,j,1)

         do lc2=1, 3

            Xg_MFIX(lc2) = Xg_MFIX(lc2) + X_G(ijk,lc2)

            absErr = abs(Xg(lc2) - X_G(ijk,lc2))

            L1(lc2) = L1(lc2) + absErr
            L2(lc2) = L2(lc2) + absErr**2
            LI(lc2) = max(LI(lc2),absErr)
         enddo
         lc1 = lc1 + 1

      end do

! Average values
      OoLC = 1.0d0/dble(lc1)
      Xg_MFIX = Xg_MFIX*OoLC
      L1 = L1*OoLC
      L2 = sqrt(L2*OoLC)

      do lc1=1,3
         OPEN(UNIT=fUNIT, FILE='POST_SPECIES_'//trim(SPECIES_G(lc1))//&
            &'.dat', position="append",status='old')

         write(fUnit, 1200) lc1, Xg(lc1), Xg_MFIX(lc1), L2(lc1)
         close(fUnit)
      enddo

 1200 FORMAT(I3,2(3x,f11.6),3xes13.6)

      RETURN
      END SUBROUTINE USR3

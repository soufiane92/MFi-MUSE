!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR3                                                   C
!  Author: Avinash Vaidheeswaran                      Date: July, 2016 C
!  Reviewer: J.Musser                                                  C
!                                                                      C
!  Purpose: To extract the turbulence kinetic energy at the wall       C
!  adjacent node along the axis (y direction)                          C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR3

      use fldvar, only: V_g, k_turb_g, e_turb_g
      Use geometry, only: dx, dy
      Use geometry, only: WIDTH => XLENGTH

      Use physprop, only: Mu_g0, Ro_g0

      Use geometry, only: imax
      Use geometry, only: imin1, jmin1, kmin1
      Use geometry, only: imax1, jmax1, kmax1

      use functions, only: funijk

      Use param1, only: zero, small_number, half

      IMPLICIT NONE

! looping indices
      integer :: i, ijk_XXX, ijk_100
! Calculated height of cell center
      double precision  :: xt,  xt_plus
! Friction velocity
      double precision, parameter :: Vstar = 0.2682
      double precision  :: Nu_g0
! Velocity at 0.75 and 1.00 of pipe length
      double precision  :: Vg_XXX, Vg_100
! file unit for output data
      integer, parameter :: fUNIT = 2030

! Open file for output.
      OPEN(UNIT=fUNIT, FILE='POST_VEL.dat', &
         POSITION="APPEND",STATUS='OLD')

      Nu_g0 = Mu_g0/RO_g0

      xt = -half*dx(imax1)
      do i = imax1, imin1, -1

         ijk_XXX = funijk(i,int(0.90*jmax1),kmin1)
         ijk_100 = funijk(i,jmax1,kmin1)

         xt = xt + dx(i)
         xt_plus = xt*Vstar/Nu_g0

         Vg_XXX = V_g(ijk_XXX)
         Vg_100 = V_g(ijk_100)

         write(fUnit,1200) xt, xt_plus, Vg_XXX, Vg_XXX/Vstar, &
            Vg_100, Vg_100/Vstar

      end do

      write(fUnit,"(5/)")
      close(fUnit)

 1200 FORMAT(6(3x,es13.6))

      RETURN
      END SUBROUTINE USR3

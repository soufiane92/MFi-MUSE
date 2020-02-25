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

      use fldvar, only: V_g,k_turb_g,e_turb_g
      Use geometry, only: dx, dy

      !Use bc, only: Uw => BC_Uw_g
      Use bc, only: delP_y
      Use physprop, only: Mu_g0, Ro_g0
      !Use bc, only: PS_X_g
      Use geometry, only: imax

      Use geometry, only: imin1, jmin1, kmin1
      Use geometry, only: imax1, jmax1, kmax1

      use functions, only: funijk

      Use param1, only: zero, small_number, half

      IMPLICIT NONE

! looping indices
      integer :: ijk, i, j, k
! temporary variable
      double precision  :: TMPdp
! Calculated height of cell center
      double precision  :: xt, yt, xt_plus
! Exact and numerical solutions
      double precision  :: Vg_MFIX, kg_MFIX, epsg_MFIX
      double precision  :: kg_BC, Vstar, Vgplus_MFIX, nu_g0
! Absolute and absolute relative errors
      double precision :: absErr, relErr
! file unit for output data
      integer, parameter :: fUNIT = 2030

! Open file for output.
      OPEN(UNIT=fUNIT, FILE='POST_VEL.dat', &
         POSITION="APPEND",STATUS='OLD')

      k = kmin1
      j = jmin1 + (jmax1-jmin1)/2
      i = imin1

      xt = -half*dx(imin1)

      Vstar = sqrt(delP_Y)
      Nu_g0 = mu_g0/Ro_g0

      do i = imin1, imax/2 + 1

! Get the MFIX solution
         ijk = funijk(i,j,k)

         xt = xt + dx(i)

         xt_plus = xt*Vstar/Nu_g0
         Vgplus_MFIX = V_g(ijk)/Vstar

         write(fUnit, 1200) xt, V_g(ijk), xt_plus, Vgplus_MFIX

      end do

      write(fUnit,"(5/)")
      close(fUnit)

 1200 FORMAT(4(3x,es13.6))

      RETURN
      END SUBROUTINE USR3

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR3                                                   C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Purpose: This routine is called after the time loop ends and is     C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.              C
!           This routine is not called from an IJK loop, hence         C
!           all indices are undefined.                                 C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR3

      use fldvar, only: T_g
      use geometry, only: dx

      use geometry, only: imin1, imax1, imax
      use geometry, only: jmin1, jmax1, jmax
      use geometry, only: kmin1, kmax1

      use functions, only: funijk

      IMPLICIT NONE

! looping indices
      integer :: ijk, i, j, k

! Calculated height of cell
      double precision  :: xt, yt
! Exact and Numerical solutions
      double precision :: lTg, Tg_MFIX
! Absolute and absolute relative errors
      double precision :: absErr, relErr
! File unit for output data
      integer, parameter :: fUnit= 2030
! Norm Errors
      double precision :: L1, L2, LI

      open(unit=fUnit, file='POST_TG.dat', &
         position='append', status='old')

      L1 = 0.0d0
      L2 = 0.0d0
      LI = 0.0d0

! Calculate the initial height
      xt = -0.5d0*dx(imin1)

! Calculate the U velocity solution at center of domain
      k = kmin1 + (kmax1-kmin1)/2
      j = jmin1 + (jmax1-jmin1)/2
      do i = imin1, imax1
         xt = xt + dx(i)

! Calculate the exact solution
         lTg = Tg(xt)
         Tg_MFIX = T_G(funijk(i,j,k))

         absErr = abs(lTg - Tg_MFIX)
         relErr = abs(absErr/max(abs(lTg), 1.0d-15))

         L1 = L1 + absErr
         L2 = L2 + absErr*absErr
         LI = max(LI, absErr)

         write(fUnit,1100) xt, lTg, Tg_MFIX, absErr, relErr
      end do
      write(fUnit,"(5/)")
      close(fUnit)

      open(unit=fUnit, file='POST_TG_NORMS.dat', &
         position='append', status='old')

      L1 = L1/dble(imax1-imin1+1)
      L2 = sqrt(L2/dble(imax1-imin1+1))

      write(fUnit,1200) imax, L1, L2, LI
      close(fUnit)

      RETURN
 1100 Format(5(3x,es13.6))
 1200 Format(3x,I3,3(3x,es13.6))

      contains

!----------------------------------------------------------------------!
! Function: Calculate the exact solution for pressure.                 !
!----------------------------------------------------------------------!
      double precision function Tg(x)

      use bc, only: BC_Tw_g
      use geometry, only: xLength

      double precision, intent(in) :: x
      double precision :: T1, T2, dTdx

      T1 = BC_Tw_g(1)
      T2 = BC_Tw_g(2)

      dTdx = (T2-T1)/xLength

      Tg = T1 + dTdx*x

      end function Tg

      end subroutine usr3

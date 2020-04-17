!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR0                                                   C
!  Purpose: This routine is called before the time loop starts and is  C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.  This        C
!           can be used for setting constants and checking errors in   C
!           data.  This routine is not called from an IJK loop, hence  C
!           all indices are undefined.                                 C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR0

      use geometry, only: dx, dy
      use fldvar, only  : p_g, u_g, v_g
      use functions
      use compar
      use usr
      IMPLICIT NONE
!-----------------------------------------------

! looping indices
      integer :: i, j, k, ijk
! Cell centroid
      double precision :: lXc, lYc

      do k=kstart2, kend2
         lXc = -0.5d0*dx(istart2)
         do i=istart2, iend2
            lXc = lXc + dx(i)
            lYc = -0.5d0*dy(jstart2)
            do j=jstart2, jend2
               lYc = lYc + dy(j)

               ijk = funijk(i,j,k)

               P_g(ijk) = Gresho_Pg(lXc, lYc)
               U_g(ijk) = Gresho_Ug(lXc + 0.5d0*dx(i), lYc)
               V_g(ijk) = Gresho_Vg(lXc, lYc + 0.5d0*dy(j))

            enddo
         enddo
      enddo

      RETURN
      END SUBROUTINE USR0

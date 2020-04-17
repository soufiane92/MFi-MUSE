!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR3                                                   C
!  Author:                                            Date: dd-mmm-yy  C
!                                                                      C
!  Purpose: This routine is called after the time loop ends and is     C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.              C
!           This routine is not called from an IJK loop, hence         C
!           all indices are undefined.                                 C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR3

      use geometry, only: dx, dy
      use run, only: discretize
      use fldvar, only  : p_g, u_g, v_g
      use functions
      use compar
      use usr

      implicit none

! Names of the discretization schemes
      CHARACTER(LEN=12), DIMENSION(0:9) :: DISCR_NAME = (/           &
         'FOUP        ','FOUP wDWF   ','Superbee    ','SMART       ',&
         'ULTRA-QUICK ','QUICKEST    ','MUSCL       ','Van Leer    ',&
         'Minmod      ','Central     '/)
! File unit for output data
      integer, parameter :: fUnit= 2030

! looping indices
      integer :: i, j, k, ijk, lc1
! Cell centroid
      double precision :: lXc, lYc, Ug, Vg, Pg
      double precision :: Pg_L1, Pg_L2, Pg_LI
      double precision :: Ug_L1, Ug_L2, Ug_LI
      double precision :: Vg_L1, Vg_L2, Vg_LI
      double precision :: TKE, TKE_MFIX, absErr

      Pg_L1 = 0.0d0;   Pg_L2 = 0.0d0;   Pg_LI = 0.0d0
      Ug_L1 = 0.0d0;   Ug_L2 = 0.0d0;   Ug_LI = 0.0d0
      Vg_L1 = 0.0d0;   Vg_L2 = 0.0d0;   Vg_LI = 0.0d0

      TKE = 0.0d0;   TKE_MFIX = 0.0d0

      lc1 = 0
      do k=kstart2, kend2
         lXc = -0.5d0*dx(istart2)
         do i=istart2, iend2
            lXc = lXc + dx(i)
            lYc = -0.5d0*dy(jstart2)
            do j=jstart2, jend2
               lYc = lYc + dy(j)

               ijk = funijk(i,j,k)

               Ug = Gresho_Ug(lXc + 0.5d0*dx(i), lYc)
               absErr = abs(Ug - U_g(ijk))
               Ug_L1 = Ug_L1 + absErr
               Ug_L2 = Ug_L2 + absErr**2
               Ug_LI = max(Ug_LI, absErr)

               Vg = Gresho_Vg(lXc, lYc + 0.5d0*dy(j))
               absErr = abs(Vg - V_g(ijk))
               Vg_L1 = Vg_L1 + absErr
               Vg_L2 = Vg_L2 + absErr**2
               Vg_LI = max(Vg_LI, absErr)

               Pg = Gresho_Pg(lXc, lYc)
               absErr = abs(Pg - P_g(ijk))
               Pg_L1 = Pg_L1 + absErr
               Pg_L2 = Pg_L2 + absErr**2
               Pg_LI = max(Pg_LI, absErr)

               TKE = TKE + 0.5d0*(Ug**2 + Vg**2)
               TKE_MFIX = TKE_MFIX + 0.5d0*(U_g(ijk)**2 + V_g(ijk)**2)

               lc1 = lc1 + 1
            enddo
         enddo
      enddo

! Open file for output
      open(unit=fUnit, file='POST_TKE.dat', &
         position='append', status='old')

      write(funit,1000) discr_name(discretize(1)), &
         TKE_MFIX, abs(TKE - TKE_MFIX), abs(TKE - TKE_MFIX)/TKE
      close(funit)

      open(unit=fUnit, file='POST_PG.dat', &
         position='append', status='old')
      write(funit,1000) discr_name(discretize(1)), &
         Pg_L1/dble(lc1), sqrt(Pg_L2/dble(lc1)), Pg_LI
      close(funit)

      open(unit=fUnit, file='POST_UG.dat', &
         position='append', status='old')
      write(funit,1000) discr_name(discretize(1)), &
         Ug_L1/dble(lc1), sqrt(Ug_L2/dble(lc1)), Ug_LI
      close(funit)

      open(unit=fUnit, file='POST_VG.dat', &
         position='append', status='old')
      write(funit,1000) discr_name(discretize(1)), &
         Vg_L1/dble(lc1), sqrt(Vg_L2/dble(lc1)), Vg_LI
      close(funit)

1000  Format(A12, 3(2x,f13.6))

      RETURN

      END SUBROUTINE USR3

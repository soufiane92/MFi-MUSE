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

      use fldvar, only: U_G, V_G
      use physprop, only: MU_G0

      IMPLICIT NONE

! U-Velocities from Ghia et al. (1982), Table I at X=0.5
      double precision, parameter :: U_Re00100(16) = (/&
          0.84123, 0.78871, 0.73722, 0.68717, &
          0.23151, 0.00332,-0.13641,-0.20581, &
         -0.21090,-0.15662,-0.10150,-0.06434, &
         -0.04775,-0.04192,-0.03717, 0.00000/)

      double precision, parameter :: U_Re00400(16) = (/&
          0.75837, 0.68439, 0.61756, 0.55892, &
          0.29093, 0.16256, 0.02135,-0.11477, &
         -0.17119,-0.32726,-0.24299,-0.14612, &
         -0.10338,-0.09266,-0.08186, 0.00000/)

      double precision, parameter :: U_Re01000(16) = (/&
          0.65928, 0.57492, 0.51117, 0.46604, &
          0.33304, 0.18719, 0.05702,-0.06080, &
         -0.10648,-0.27805,-0.38289,-0.29730, &
         -0.22220,-0.20196,-0.18109, 0.00000/)

      double precision, parameter :: U_Re03200(16) = (/&
          0.53236, 0.48296, 0.46547, 0.46101, &
          0.34682, 0.19791, 0.07156,-0.04272, &
         -0.86636,-0.24427,-0.34323,-0.41933, &
         -0.37827,-0.35344,-0.32407, 0.00000/)

! V-Velocities from Ghia et al. (1982), Table II at Y=0.5
      double precision, parameter :: V_Re00100(16) = (/&
         -0.05906,-0.07391,-0.08864,-0.10313, &
         -0.16914,-0.22445,-0.24533, 0.05454, &
          0.17527, 0.17507, 0.16077, 0.12317, &
          0.10890, 0.10091, 0.09233, 0.00000/)

      double precision, parameter :: V_Re00400(16) = (/&
         -0.12146,-0.15663,-0.19254,-0.22847, &
         -0.23827,-0.44993,-0.38598, 0.05186, &
          0.30174, 0.30203, 0.28124, 0.22965, &
          0.20920, 0.19713, 0.18360, 0.00000/)

      double precision, parameter :: V_Re01000(16) = (/&
         -0.21388,-0.27669,-0.33714,-0.39188, &
         -0.51550,-0.42665,-0.31966, 0.02526, &
          0.32235, 0.33075, 0.37095, 0.32627, &
          0.30353, 0.29012, 0.27485, 0.00000/)

      double precision, parameter :: V_Re03200(16) = (/&
         -0.39017,-0.47425,-0.52357,-0.54053, &
         -0.44307,-0.37401,-0.31184,-0.00999, &
          0.28188, 0.29030, 0.37119, 0.42768, &
          0.41906, 0.40917, 0.38560, 0.00000/)


      integer :: N_Re


      N_Re = int(1.0/Mu_g0)

      select case(N_Re)
      case (  100)
         call MFIX_GHIA_U(U_g, U_Re00100, 'UG_Re00100')
         call MFIX_GHIA_V(V_g, V_Re00100, 'VG_Re00100')
      case (  400)
         call MFIX_GHIA_U(U_g, U_Re00400, 'UG_Re00400')
         call MFIX_GHIA_V(V_g, V_Re00400, 'VG_Re00400')
      case( 1000)
         call MFIX_GHIA_U(U_g, U_Re01000, 'UG_Re01000')
         call MFIX_GHIA_V(V_g, V_Re01000, 'VG_Re01000')
      case( 3200)
         call MFIX_GHIA_U(U_g, U_Re03200, 'UG_Re03200')
         call MFIX_GHIA_V(V_g, V_Re03200, 'VG_Re03200')
      case default
         write(*,*) 'Unknown run settings.'
      end select

      RETURN
      contains

!----------------------------------------------------------------------!
! Subroutine: Write the MFIX and Ghia solutions.                       !
!----------------------------------------------------------------------!
      SUBROUTINE MFIX_GHIA_V(pVEL, pGhia, pVar)

      use geometry, only: imin1, jmin1, kmin1
      use geometry, only: imax1, jmax1, kmax1
      use geometry, only: dx, dy
      use functions, only: IS_ON_MYPE_OWNS
      use functions, only: FUNIJK
      use mpi_utility, only: GLOBAL_ALL_SUM
      use compar, only: myPE, PE_IO
      use param, only: DIMENSION_I

      double precision, parameter :: X(16) = (/&
          0.96880, 0.96090, 0.95310, 0.94530, &
          0.90630, 0.85940, 0.80470, 0.50000, &
          0.23440, 0.22660, 0.15630, 0.09380, &
          0.07810, 0.07030, 0.06250, 0.00000/)

      double precision, intent(in) :: pVel(:)
      double precision, intent(in) :: pGhia(:)
      character(len=*), intent(in) :: pVar

      integer :: lc1, i, j, k

      double precision :: Vg_MFIX(DIMENSION_I)
      double precision :: Xe, Yn
      integer, parameter :: fUnit = 2030

      Vg_MFIX = 0.0d0

      Yn = 0.0d0
      do j = jmin1, jmax1
         Yn = Yn + dy(j)
         if(Yn == 0.5d0) exit
      enddo

! Calculate the U velocity solution at center of domain
      k = kmin1 + (kmax1-kmin1)/2
      Xe = 0.0d0
      do i = imin1, imax1
         Xe = Xe + dx(i)
         if(is_On_myPE_OWNS(i,j,k)) &
            Vg_MFIX(i) = V_G(funijk(i,j,k))
      enddo

      call global_all_sum(Vg_MFIX)
      if(myPE /= PE_IO) RETURN

! Open file for output
      open(unit=fUnit, file='POST_'//pVar//'.dat',&
         status='unknown')

! Write data.
      write(fUnit,1000) 'Ghia'
      do lc1=15, 1, -1
         write(fUnit,1050) X(lc1), pGhia(lc1)
      enddo

! Calculate the U velocity solution at center of domain
      write(fUnit,1000) 'MFIX'
      Xe = 0.0d0
      do i = imin1, imax1
         Xe = Xe + dx(i)
         write(fUnit,1100) Xe, Vg_MFIX(i)
      enddo

      close(fUnit)

 1000 Format(2/4x,'Position',8x,A)
 1050 Format(2(2x,f8.5))
 1100 Format(2(2x,es13.6))

      END SUBROUTINE MFIX_GHIA_V

!----------------------------------------------------------------------!
! Subroutine: Write the MFIX and Ghia solutions.                       !
!----------------------------------------------------------------------!
      SUBROUTINE MFIX_GHIA_U(pVEL, pGhia, pVar)

      use geometry, only: imin1, jmin1, kmin1
      use geometry, only: imax1, jmax1, kmax1
      use geometry, only: dx, dy
      use functions, only: IS_ON_MYPE_OWNS
      use functions, only: FUNIJK
      use mpi_utility, only: GLOBAL_ALL_SUM
      use compar, only: myPE, PE_IO
      use param, only: DIMENSION_J

      double precision, parameter :: Y(16) = (/&
          0.97660, 0.96880, 0.96090, 0.95310, &
          0.85160, 0.73440, 0.61720, 0.50000, &
          0.45310, 0.28130, 0.17190, 0.10160, &
          0.07030, 0.06250, 0.05470, 0.00000/)

      double precision, intent(in) :: pVel(:)
      double precision, intent(in) :: pGhia(:)
      character(len=*), intent(in) :: pVar

      integer :: lc1, i, j, k

      double precision :: Ug_MFIX(DIMENSION_J)
      double precision :: Xe, Yn
      integer, parameter :: fUnit = 2030

      Ug_MFIX = 0.0d0

      Xe = 0.0d0
      do i = imin1, imax1
         Xe = Xe + dx(i)
         if(Xe == 0.5d0) exit
      enddo

! Calculate the U velocity solution at center of domain
      k = kmin1 + (kmax1-kmin1)/2
      do j = jmin1, jmax1
         if(is_On_myPE_OWNS(i,j,k)) &
            Ug_MFIX(j) = U_G(funijk(i,j,k))
      enddo

      call global_all_sum(Ug_MFIX)
      if(myPE /= PE_IO) RETURN

! Open file for output
      open(unit=fUnit, file='POST_'//pVar//'.dat',&
         status='unknown')

! Write data.
      write(fUnit,1000) 'Ghia'
      do lc1=15,1,-1
         write(fUnit,1050) Y(lc1), pGhia(lc1)
      enddo

! Calculate the U velocity solution at center of domain
      write(fUnit,1000) 'MFIX'
      Yn = 0.0d0
      do j = jmin1, jmax1
         Yn = Yn + dy(j)
         write(fUnit,1100) Yn, Ug_MFIX(j)
      enddo

      close(fUnit)

 1000 Format(2/4x,'Position',8x,A)
 1050 Format(2(2x,f8.5))
 1100 Format(2(2x,es13.6))

      END SUBROUTINE MFIX_GHIA_U

      END SUBROUTINE USR3

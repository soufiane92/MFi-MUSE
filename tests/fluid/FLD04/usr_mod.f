      MODULE usr

      use param
      use param1

      contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: gresho_ic                                              !
!                                                                      !
!  Author: Aniruddha Choudhary                        Date: Jun 2015   !
!  Purpose: Function to return gresho problem inital conditions.       !
!                                                                      !
!  Reference:                                                          !
!                                                                      !
!   Liska, R. & Wendroff, B. (2003). Comparison of Several Difference  !
!   Schemes on 1D and 2D Test Problems for the Euler Equations. SIAM   !
!   J. Sci. Comput., 25, 995--1017. doi: 10.1137/s1064827502402120     !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      DOUBLE PRECISION FUNCTION Gresho_Pg(pX, pY)

! Location
      double precision, intent(in)  :: pX, pY
! Radius at (pX, pY)
      double precision  :: lRad

      lRad = sqrt((pX-0.5d0)**2 + (pY-0.5d0)**2)

! Reference: Liska and Wendroff (2003)
      if((lRad >= 0.0d0).and.(lRad < 0.2d0)) then
         Gresho_Pg = 5.0d0 + 25.0d0/2.0d0*lRad**2
      elseif((lRad >= 0.2d0).and.(lRad < 0.4d0)) then
         Gresho_Pg = 9.0d0 - 4.0d0*log(0.2d0) + 25.0d0/2.0d0*lRad**2 &
              - 20.0d0*lRad + 4.0d0*log(lRad)
      elseif((lRad >= 0.4d0)) then
          Gresho_Pg = 3.0d0 + 4.0d0*log(2.0d0)
      else
        write(*,*) "Check IC setup in usr0.f"
        ERROR STOP
      end if

      end function Gresho_Pg
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: Gresho_Ug                                              !
!                                                                      !
!  Author: Aniruddha Choudhary                        Date: Jun 2015   !
!  Purpose: Function to return gresho problem inital conditions for Ug !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      DOUBLE PRECISION FUNCTION Gresho_Ug(pX, pY)
      IMPLICIT NONE

! Location
      double precision, intent(in)  :: pX, pY
! Radius at (pX, pY)
      double precision  :: lRad

      lRad = sqrt((pX-0.5d0)**2 + (pY-0.5d0)**2)

      if(lRad == 0.0d0) then
         Gresho_Ug = 0.0d0
      else
         Gresho_Ug = -Uphi(lRad)*((pY-0.5d0)/lRad)
      endif

      RETURN
      END FUNCTION Gresho_Ug

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: Gresho_Vg                                              !
!                                                                      !
!  Author: Aniruddha Choudhary                        Date: Jun 2015   !
!  Purpose: Function to return gresho problem inital conditions for Vg !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      double precision function Gresho_Vg(pX, pY)
      implicit none

! Location
      double precision, intent(in)  :: pX, pY
! Radius at (pX, pY)
      double precision  :: lRad

      lRad = sqrt((pX-0.5d0)**2 + (pY-0.5d0)**2)

      if(lRad == 0.0d0) then
         Gresho_Vg = 0.0d0
      else
         Gresho_Vg =  Uphi(lRad)*((pX-0.5d0)/lRad)
      endif

      RETURN
      END FUNCTION Gresho_Vg

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: gresho_ic                                              !
!                                                                      !
!  Author: Aniruddha Choudhary                        Date: Jun 2015   !
!  Purpose: Function to return gresho problem inital conditions.       !
!                                                                      !
!  Reference:                                                          !
!                                                                      !
!   Liska, R. & Wendroff, B. (2003). Comparison of Several Difference  !
!   Schemes on 1D and 2D Test Problems for the Euler Equations. SIAM   !
!   J. Sci. Comput., 25, 995--1017. doi: 10.1137/s1064827502402120     !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      DOUBLE PRECISION FUNCTION Uphi(pRad)

! Radial distance from center of vortex
      double precision, intent(in) :: pRad

! Reference: Liska and Wendroff (2003)
      if((pRad >= 0.0d0).and.(pRad < 0.2d0)) then
        Uphi = 5.0d0*pRad
      elseif((pRad >= 0.2d0).and.(pRad < 0.4d0)) then
        Uphi = 2.0d0 - 5.0d0*pRad
      elseif((pRad >= 0.4d0)) then
        Uphi = 0.0d0
      else
        write(*,*) "Check IC setup in usr0.f"
        ERROR STOP
      end if

      end function Uphi

      END MODULE usr

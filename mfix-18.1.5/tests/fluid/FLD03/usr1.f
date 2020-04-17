!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR1                                                   C
!  Purpose: This routine is called from the time loop and is           C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.  This        C
!           can be used for setting or checking errors in quantities   C
!           that vary with time.  This routine is not called from an   C
!           IJK loop, hence all indices are undefined.                 C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR1

      use usr
      use run, only: NSTEP, TIME, TSTOP
      use fldvar, only: U_GO, U_G
      use fldvar, only: V_GO, V_G
      use compar, only: myPE, PE_IO

      IMPLICIT NONE

      double precision :: uL1, uL2, uLI
      double precision :: vL1, vL2, vLI

      if(NSTEP > 1) then

         call CALC_NORMS(uL1, uL2, uLI, U_G, U_GO)
         call CALC_NORMS(vL1, vL2, vLI, V_G, V_GO)

         if(max(uL2,vL2) < 1.0e-8) TSTOP = TIME
         if(myPE == 0) write(*,"(I8,2(2x,es13.4))") NSTEP-1, uL2, vL2

      endif

      RETURN

   contains

!----------------------------------------------------------------------!
! Function: Calculate the exact solution for pressure.                 !
!----------------------------------------------------------------------!
      subroutine calc_norms(pL1, pL2, pLI, pVAR, pVARO)

      use compar
      use functions
      use mpi_utility
      use param, only: dimension_3

      double precision, intent(out) :: pL1, pL2, pLI

      double precision, intent(in) :: pVAR(DIMENSION_3)
      double precision, intent(in) :: pVARO(DIMENSION_3)

      double precision :: absErr, lNorms(4)
      integer :: ijk, i, j, k

      lNorms = 0.0d0

      do k=kstart2, kend2
         do i=istart2, iend2
            do j=jstart2, jend2
               ijk = funijk(i,j,k)
               absErr = abs(pVAR(IJK) -pVARO(IJK))
               lNorms(1) = lNorms(1) + absErr
               lNorms(2) = lNorms(2) + absErr*absErr
               lNorms(3) = max(lNorms(3), absErr)
               lNorms(4) = lNorms(4) + 1.0d0
            enddo
         enddo
      enddo

      call global_all_sum(lNorms)

      if(lNorms(4) > 0.0d0) then
         pL1 = lNorms(1)/lNorms(4)
         pL2 = sqrt(lNorms(2)/lNorms(4))
         pLI = lNorms(3)
      else
         pL1 = -1.0
         pL2 = -1.0
         pLI = -1.0
      endif

      end subroutine calc_norms

      END SUBROUTINE USR1

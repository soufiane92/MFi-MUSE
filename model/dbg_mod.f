!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module: DBG                                                         !
!  Author: J.Musser                                   Date: 11-Nov-13  !
!                                                                      !
!  Purpose: Contains routines used for extracting the Am matrix, Bm    !
!  vector in .csv format.  Additionally, routines for extracting array !
!  subsets are included.                                               !
!                                                                      !
!  Comments:                                                           !
!                                                                      !
!  > initExtract must be invoked prior to invoking any other routine   !
!       in this file. The initialization routine needs to set several  !
!       values required by other functions.                            !
!                                                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      MODULE dbg

      use exit, only: mfix_exit
      use mpi_utility
      use param1, only: UNDEFINED_C
      use param1, only: UNDEFINED_I

      IMPLICIT NONE

      PRIVATE

! Subroutine Access:
!---------------------------------------------------------------------//
      PUBLIC ::  initExtract, matrixExtract, arrayExtract, debug_write_layout, write_parallel_info

! Interface
!---------------------------------------------------------------------//
      interface arrayExtract
         module procedure arrayExtract_int
         module procedure arrayExtract_dbl
         module procedure arrayExtract_log
      end interface

! Module variables (private):
!---------------------------------------------------------------------//

! Coeffs for the space filling curve.
      INTEGER ::  Am_c0, Am_c1, Am_c2

! The extent to extract the Am/Bm matrix/vector.
      INTEGER :: iUB, iLB
      INTEGER :: jUB, jLB
      INTEGER :: kUB, kLB

! Logical flag to inflate A_M matrix to from sparce to full NxN.
      LOGICAL, parameter :: inflateAM = .FALSE.
! Debug Mode:
      LOGICAL, parameter :: dbgMode = .FALSE.

! Logical to verify that the initialization routine was called.
      LOGICAL :: initNotCalled = .TRUE.

! Data storage buffer size.
      INTEGER :: dbgDIMN

! Buffers for I/J/K indices
      INTEGER, allocatable :: i_ofBuff(:)
      INTEGER, allocatable :: j_ofBuff(:)
      INTEGER, allocatable :: k_ofBuff(:)
      INTEGER, allocatable :: ijk_Buff(:)

! Buffers for various data types.
! Buffer for generic integer array.
      INTEGER, allocatable :: outBuff_i(:)
! Buffer for generic double precision array.
      DOUBLE PRECISION, allocatable :: outBuff_dp(:)
! Buffer for Am matrix.
      DOUBLE PRECISION, allocatable :: outAm(:)

! Flag stating that the dgb routines are in-use
      LOGICAL :: dbgLock

! File Unit
      INTEGER, parameter :: dbgUnit = 9659

! File structure information: (optional)
! Pass count if dumping at multiple locations or during iterations.
      INTEGER :: fPass
! Flag to append to previous output.
      LOGICAL :: fApnd
! Flag to write IJK values with output.
      LOGICAL :: fwIJK, pwIJK

      contains

!``````````````````````````````````````````````````````````````````````!
!                                                                      !
!                                                                      !
!``````````````````````````````````````````````````````````````````````!
      INTEGER FUNCTION  dbg_funijk(xxi, xxj, xxk)
      INTEGER, intent(in) :: xxi, xxj, xxk

      dbg_funijk = xxj + Am_c0 + xxi*Am_c1 + xxk*Am_c2

      END FUNCTION dbg_funijk

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE initExtract(iLow, iHgh, jLow, jHgh, kLow, kHgh)

      use compar
      use funits, only: DMP_LOG
      use geometry, only: iMin3, iMax3
      use geometry, only: jMin3, jMax3
      use geometry, only: kMin3, kMax3, do_K

      INTEGER, optional, intent(in) :: iLow, iHgh
      INTEGER, optional, intent(in) :: jLow, jHgh
      INTEGER, optional, intent(in) :: kLow, kHgh

      INTEGER :: PROC

! Lock the dbg routines.
      dbgLock = .TRUE.

      iLB = merge(iLow, iMin3, present(iLow))
      iUB = merge(iHgh, iMax3, present(iHgh))

      jLB = merge(jLow, jMin3, present(jLow))
      jUB = merge(jHgh, jMax3, present(jHgh))

      kLB = merge(kLow, kMin3, present(kLow))
      kUB = merge(kHgh, kMax3, present(kHgh))

! Some basic logical checks:
      IF(iLB > iUB .OR. iLB<iMin3 .OR. iUB>iMax3) THEN
         IF(DMP_LOG) WRITE(*,1000)'I',iLB,iMin3,iUB,iMax3
         CALL MFIX_EXIT(myPE)
      ELSEIF(jLB > jUB .OR. jLB<jMin3 .OR. jUB>jMax3) THEN
         IF(DMP_LOG) WRITE(*,1000)'J',jLB,jMin3,jUB,jMax3
         CALL MFIX_EXIT(myPE)
      ELSEIF(kLB > kUB .OR. kLB<kMin3 .OR. kUB>kMax3) THEN
         IF(DMP_LOG) WRITE(*,1000)'K',kLB,kMin3,kUB,kMax3
         CALL MFIX_EXIT(myPE)
      ENDIF


! Calculate the coeffs for the space filling curve.
      Am_c0 = 1 - jLB
      Am_c1 = (jUB-jLB+1)
      Am_c2 = (jUB-jLB+1)*(iUB-iLB+1)
      Am_c0 =  Am_c0  - Am_c1*iLB - Am_c2*kLB

      dbgDIMN = (1+iUB-iLB) * (1+jUB-jLB) * (1+kUB-kLB)

! Allocate the output vector for Am
      if(inflateAM) then
         Allocate( outAm( dbgDIMN) )
      else
         if(do_K) then
            Allocate( outAm(-3:3) )
         else
            Allocate( outAm(-2:2) )
         endif
      endif

! Report some basic data to the screen.
      IF(DMP_LOG) THEN
         WRITE(*,"(4/,3x,'Matrix Map:')")
         write(*,"(/,5X,'Domain Limits >')")
         write(*,"(7X,'I: ',I4,2x,I4)") iMin3, iMax3
         write(*,"(7X,'J: ',I4,2x,I4)") jMin3, jMax3
         if(do_K) write(*,"(7X,'K: ',I4,2x,I4)") kMin3, kMax3

         write(*,"(/5x,'Local IJK Coeffs >')")
         write(*,"(7x,'C0: ',I9)") C0
         write(*,"(7x,'C1: ',I9)") C1
         write(*,"(7x,'C2: ',I9)") C2

         write(*,"(/5X,'Extraction Region: >')")
         write(*,"(7X,'I: ',I4,2x,I4)") iLB, iUB
         write(*,"(7X,'J: ',I4,2x,I4)") jLB, jUB
         if(do_K) write(*,"(7X,'K: ',I4,2x,I4)") kLB, kUB

         write(*,"(/5x,'dbgIJK Coeffs >')")
         write(*,"(7x,'Am_C0: ',I9)") Am_C0
         write(*,"(7x,'Am_C1: ',I9)") Am_C1
         write(*,"(7x,'Am_C2: ',I9)") Am_C2

         if(inflateAM) then
            write(*,"(/5x,'A_M is going to be inflated.')")
         else
            write(*,"(/5x,'A_M is NOT going to be inflated.')")
         endif
      ENDIF

! Set the flag indicating that the initialization routine was called.
      initNotCalled = .FALSE.

      do proc = 0, numPEs-1
         if(myPE == proc) then
            write(*,"(/3x,'Proc: ',I3)")proc
            write(*,"(5x,'I start/end 1:',2(2x,I3))") istart1, iend1
            write(*,"(5x,'J start/end 1:',2(2x,I3))") jstart1, jend1
            if(do_K)write(*,"(5x,'K start/end 1:',2(2x,I3))") kstart1, kend1
         endif
#ifdef MPI
         CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
#endif
      enddo

! Unlock dbg routines.
      dbgLock = .FALSE.

      RETURN

 1000 FORMAT(/1X,70('*')/' From: initExtract',/' Error 1000:',         &
         ' Invalid parameters.  Axis: ',A1,/8x,'LB =',I4,3x,'Min2 =',  &
         I12,/8x,'UB =',I4,3x,'Max2 =',I12,/1x,70('*'),2/)

      END SUBROUTINE initExtract

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE matrixExtract(A_m, B_m, M, VAR, PASS)

      use compar
      use fldvar
!      use geometry
!      use indices
!      use run
!      use sendrecv
      use functions
      use param, only: dimension_3, dimension_m
      use param1, only: zero

      IMPLICIT NONE

! Septadiagonal matrix A_m
      DOUBLE PRECISION, intent(in) :: A_m(DIMENSION_3, -3:3, 0:DIMENSION_M)
! Vector b_m
      DOUBLE PRECISION, intent(in) :: B_m(DIMENSION_3, 0:DIMENSION_M)

      INTEGER, intent(in) :: M

      CHARACTER(len=*), intent(in), optional :: VAR

      INTEGER, intent(in), optional :: PASS

! Septadiagonal matrix A_m
      DOUBLE PRECISION :: lA_m(-3:3)
! Vector b_m
      DOUBLE PRECISION :: lB_m
! Neighbor info for debugging.
      INTEGER :: NBGHS(-3:3)

      LOGICAL :: lexist
      CHARACTER(len=64)  :: AmFName
      CHARACTER(len=64)  :: BmFName

      INTEGER, parameter :: AmUnit = 9657
      INTEGER, parameter :: BmUnit = 9658

      INTEGER :: IJK, I, J, K, OWNER

! If the initialization routine was not called, flag the error and exit.
      IF(initNotCalled)THEN
         IF(DMP_LOG) THEN
            WRITE(*,"(3/,' Fatal Error in matrixExtract!')")
            WRITE(*,"(' The initialization routine was never called.')")
            WRITE(*,"(' USR0 should contain a call to initExtract.')")
            WRITE(*,"(' Forcing a hard stop.')")
         ENDIF
         CALL MFIX_EXIT(myPE)
      ENDIF

      AmFName=''
      BmFName=''
      IF(present(VAR) .AND. present(PASS)) THEN
         write(AmFName,"('Am_',A,'_',I6.6,'.csv')")trim(adjustl(VAR)),PASS
         write(BmFName,"('Bm_',A,'_',I6.6,'.csv')")trim(adjustl(VAR)),PASS
      ELSEIF(present(VAR)) THEN
         write(AmFName,"('Am_',A,'.csv')")trim(adjustl(VAR))
         write(BmFName,"('Bm_',A,'.csv')")trim(adjustl(VAR))
      ELSEIF(present(PASS)) THEN
         write(AmFName,"('Am_',I6.6,'.csv')") PASS
         write(BmFName,"('Bm_',I6.6,'.csv')") PASS
      ELSE
         AmFName='Am.csv'
         BmFName='Bm.csv'
      ENDIF

! Rank 0 opens the file.
      IF(myPE == PE_IO) THEN
         inquire(file=trim(AmFName),exist=lexist)
         IF(lexist) THEN
            open(AmUnit,file=trim(AmFName), status='replace')
         ELSE
            open(AmUnit,file=trim(AmFName), status='new')
         ENDIF

         inquire(file=trim(BmFName),exist=lexist)
         IF(lexist) THEN
            open(BmUnit,file=trim(BmFName), status='replace')
         ELSE
            open(BmUnit,file=trim(BmFName), status='new')
         ENDIF
      ENDIF

! Everyone waits for the file to open.
#ifdef MPI
      CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
#endif

      do K = kLB, kUB
      do I = iLB, iUB
      do J = jLB, jUB

! All ranks set lAm and lBm to zero.
         OWNER = 0
         NBGHS = 0
         lB_m = ZERO
         lA_m(-3:3) = ZERO

! Only the rank that owns IJK populates lAm and lBm.
         if(IS_ON_myPE_owns(I,J,K)) then
            IJK = funIJK(I,J,K)
            lA_m(-3:3) = A_M(IJK,-3:3,M)
            lB_m = B_M(IJK,M)
! Incorporate neighbor information for debugging.
            if(dbgMode) then
               OWNER = myPE
               if(do_K) Nbghs(-3) = Bottom_of(IJK)
               NBGHS(-2) = South_of(IJK)
               NBGHS(-1) = West_of(IJK)
               NBGHS( 0) = IJK
               NBGHS( 1) = East_of(IJK)
               NBGHS( 2) = North_of(IJK)
               if(do_K) Nbghs( 3) = Top_of(IJK)
            endif
         endif

! Global sum updates lAm and lBm for all ranks.
         CALL global_all_sum(lA_m)
         CALL global_all_sum(lB_m)
         if(dbgMode) then
            CALL global_all_sum(OWNER)
            CALL global_all_sum(NBGHS)
         endif

         if(myPE==PE_IO) then
            CALL Am_to_Aout(I, J, K, NBGHS, OWNER, lA_m)
            CALL write_AoutBm(lB_m)
         endif

! Everyone waits for the owner to complete this cycle.
#ifdef MPI
         CALL MPI_Barrier(MPI_COMM_WORLD, mpierr)
#endif

      enddo
      enddo
      enddo

! Rank 0 closes the file.
      IF(myPE == PE_IO) then
         close(AmUnit)
         close(BmUnit)
      ENDIF

      return

      contains

!``````````````````````````````````````````````````````````````````````!
!                                                                      !
!                                                                      !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE Am_to_Aout(I, J, K, lNBGHS, lOWNER, lA_m)

      INTEGER, intent(in) :: I, J, K
      INTEGER, intent(in) :: lNBGHS(-3:3), lOWNER
      DOUBLE PRECISION, intent(in) :: lA_m(-3:3)

! Local process IJK for neighbor cell mapped to global Am output matrix.
      INTEGER :: nIJK
! Increments from I,J,K for neighbor calculations.
      INTEGER :: ii, jj, kk

! Bounds for extracted section or walls.
      INTEGER :: sMin, wMin, bMin
      INTEGER :: eMax, nMax, tMax

! Initialize:
      outAm = 0.0d0

! Set up bounds check values.
      wMin = iLB;   eMax = (1+iUB-iLB)
      sMin = jLB;   nMax = (1+jUB-jLB)
      bMin = kLB;   tMax = (1+kUB-kLB)

! Print mapping info.
      if(dbgMode) write(*,9003) lNBGHS(0), dbg_funijk(i,j,k),           &
         i, j, k, lOWNER

! Bottom Neighboring Fluid Cell
!---------------------------------------------------------------------//
      if(do_K) then
         if(k > kLB) then
            ii = i;   jj = j;   kk = k-1
            nIJK = merge(dbg_funijk(ii,jj,kk), -3, inflateAM)
            outAm(nIJK) = lA_m(-3)

            if(dbgMode) write(*,9000)'Bottom of ', lNBGHS(-3),         &
               nIJK, ii, jj, kk, lA_m(-3)
         else
            if(dbgMode) write(*,9001)'Bottom of ', lA_m(-3)
         endif
      endif

! South Neighboring Fluid Cell
!---------------------------------------------------------------------//
      if(j > jLB) then

         ii = i;   jj = j-1;   kk = k
         nIJK = merge(dbg_funijk(ii,jj,kk), -2, inflateAM)
         outAm(nIJK) = lA_m(-2)

         if(dbgMode) write(*,9000)'South of  ', lNBGHS(-2),            &
            nIJK, ii, jj, kk, lA_m(-2)
      else
         if(dbgMode) write(*,9001)'South of  ', lA_m(-2)
      endif

! West Neighboring Fluid Cell
!---------------------------------------------------------------------//
      if(i > iLB) then

         ii = i-1; jj = j;   kk = k
         nIJK = merge(dbg_funijk(ii,jj,kk), -1, inflateAM)
         outAm(nIJK) = lA_m(-1)

         if(dbgMode) write(*,9000)'West of   ', lNBGHS(-1),            &
            nIJK, ii, jj, kk, lA_m(-1)
      else
         if(dbgMode) write(*,9001)'West of   ', lA_m(-1)
      endif

! Center Coefficient
!---------------------------------------------------------------------//

      ii = i;   jj = j;   kk = k
      nIJK = merge(dbg_funijk(ii,jj,kk), 0, inflateAM)
      outAm(nIJK) = lA_m(0)

      if(dbgMode) write(*,9000)'Cntr Coef ', lNBGHS(0),                &
         nIJK, ii, jj, kk, lA_m(0)

! East Neighboring Fluid Cell
!---------------------------------------------------------------------//
      if(i < iUB) then

         ii = i+1; jj = j;   kk = k
         nIJK = merge(dbg_funijk(ii,jj,kk), 1, inflateAM)
         outAm(nIJK) = lA_m( 1)

         if(dbgMode) write(*,9000)'East of   ', lNBGHS( 1),            &
            nIJK, ii, jj, kk, lA_m( 1)
      else
         if(dbgMode) write(*,9001)'East of   ', lA_m( 1)
      endif

! North Neighboring Fluid Cell
!---------------------------------------------------------------------//
      if(j < jUB) then

         ii = i; jj = j+1;   kk = k
         nIJK = merge(dbg_funijk(ii,jj,kk), 2, inflateAM)
         outAm(nIJK) = lA_m( 2)

         if(dbgMode) write(*,9000)'North of  ', lNBGHS(2),             &
            nIJK, ii, jj, kk, lA_m( 2)
      else
         if(dbgMode) write(*,9001)'North of  ', lA_m( 2)
      endif

! Top Neighboring Fluid Cell
!---------------------------------------------------------------------//
      if(do_K) then
         if(k < kUB) then

            ii = i; jj = j;   kk = k+1
            nIJK = merge(dbg_funijk(ii,jj,kk), 3, inflateAM)
            outAm(nIJK) = lA_m( 3)

            if(dbgMode) write(*,9000)'Top of    ', lNBGHS( 3),         &
               nIJK, ii, jj, kk, lA_m( 3)
         else
            if(dbgMode) write(*,9001)'Top of    ', lA_m( 3)
         endif
      endif

      return

 9000 Format(5x,A,':: ',I4,' --> ',I4,3x,'(',I3,',',I3,',',I3,         &
         ') = ',F12.4)

 9001 Format(5x,A,':: ............ OoR ............ = ',F12.4)


 9003 Format(//3x,'Mapping: ',I4,' --> ',I4,3x,'(',I3,',',I3,',',I3,   &
         ')',4x,'Rank: ',I5)

      END SUBROUTINE Am_to_Aout


!``````````````````````````````````````````````````````````````````````!
!                                                                      !
!                                                                      !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE write_AoutBm(lB_m)

      DOUBLE PRECISION, intent(in) :: lB_m

      INTEGER :: lStart
      INTEGER :: lEnd
      INTEGER :: IJK

      lStart = lbound(outAm,1)
      lEnd = ubound(outAm,1)

      if(dbgMode) then
         do IJK = lStart, lEnd-1
            write(AmUnit,"(F12.4,',')",advance='no')outAm(IJK)
         enddo
         write(AmUnit,"(F12.4)")outAm(lEnd)
         write(BmUnit,"(F12.4)")lB_m

      else
         do IJK = lStart, lEnd-1
            write(AmUnit,"(e14.6,',')",advance='no')outAm(IJK)
         enddo
         write(AmUnit,"(e14.6)")outAm(lEnd)
         write(BmUnit,"(e14.6)")lB_m
      endif

      END SUBROUTINE write_AoutBm

      END SUBROUTINE matrixExtract

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE dbg_write(lMsg, Flush)

      use compar
!      use sendrecv

      implicit none

      CHARACTER(len=*), intent(in) :: lMsg

      LOGICAL, optional, intent(in) :: FLUSH
      LOGICAL :: lFlush

      if(dbgMode)then
         lFlush = merge(FLUSH, .FALSE., present(FLUSH))

         if(myPE == PE_IO) then
            if(lFlush) write(*,"(' ')")
            write(*,"(3x,A)") trim(lMsg)
         endif
#ifdef MPI
            CALL MPI_Barrier(MPI_COMM_WORLD,mpierr)
#endif
      endif

      RETURN
      END SUBROUTINE DBG_WRITE

!----------------------------------------------------------------------!
!                                                                        !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE arrayExtract_init(vName)

      use compar, only: myPE
      use funits, only: DMP_LOG

      implicit none

! Variable named used to create file name.
      CHARACTER(len=*), intent(in) :: vName

! Variables for opening files.
      LOGICAL :: lexist
      CHARACTER(len=64)  :: VarFName
! Integer Error Flag.
      INTEGER :: iErr

! If the initialization routine was not called, flag the error and exit.
      IF(initNotCalled)THEN
         IF(DMP_LOG) WRITE(*,1000)
         CALL MFIX_EXIT(myPE)
      ENDIF

! Verify that the files are unlocked.
      IF(dbgLock)THEN
         IF(DMP_LOG) WRITE(*,1001)
         CALL MFIX_EXIT(myPE)
      ELSE
         dbgLock = .TRUE.
      ENDIF

! Allocate indice arrays.
      IF(allocated(i_ofBuff))THEN
         IF(DMP_LOG) WRITE(*,1002) 'i_ofBuff'
         CALL MFIX_EXIT(myPE)
      ELSE
         allocate( i_ofBuff(dbgDIMN) )
         i_ofBuff = 0
      ENDIF

      IF(allocated(j_ofBuff))THEN
         IF(DMP_LOG) WRITE(*,1002) 'j_ofBuff'
         CALL MFIX_EXIT(myPE)
      ELSE
         allocate( j_ofBuff(dbgDIMN) )
         j_ofBuff = 0
      ENDIF

      IF(allocated(k_ofBuff))THEN
         IF(DMP_LOG) WRITE(*,1002) 'k_ofBuff'
         CALL MFIX_EXIT(myPE)
      ELSE
         allocate( k_ofBuff(dbgDIMN) )
         k_ofBuff = 0
      ENDIF

      IF(allocated(ijk_Buff))THEN
         IF(DMP_LOG) WRITE(*,1002) 'ijk_Buff'
         CALL MFIX_EXIT(myPE)
      ELSE
         allocate( ijk_Buff(dbgDIMN) )
         ijk_Buff = 0
      ENDIF

! Construct the file name.
      VarFName=''
      IF(fApnd) THEN
         write(VarFName,"(A,'.csv')")                                  &
            trim(adjustl(vName))
      ELSEIF(fPass /= UNDEFINED_I) THEN
         write(VarFName,"(A,'_',I6.6,'.csv')")                         &
            trim(adjustl(vName)), fPass
      ELSE
         write(VarFName,"(A,'.csv')")                                  &
            trim(adjustl(vName))
      ENDIF

! Open the file  -- Rank 0 ONLY.
      inquire(file=trim(VarFName), exist=lExist)
      IF(myPE == PE_IO) THEN
         IF(lExist) THEN
            IF(fApnd) THEN
               open(dbgUnit,file=trim(VarFName),                       &
                  status='old', position='append', iostat=iErr)
            ELSE
               open(dbgUnit,file=trim(VarFName),                       &
                  status='replace', iostat=iErr)
            ENDIF
         ELSE
            open(dbgUnit,file=trim(VarFName),                          &
               status='new', iostat=iErr)
         ENDIF
      ENDIF
      CALL BCAST(iErr, PE_IO)
      IF(iErr /= 0)THEN
         IF(myPE == PE_IO) write(*,1003) trim(VarFName)
         CALL MFIX_EXIT(myPE)
      ENDIF

! Set printIJK Flag. Only print the IJK values if this is the first time
! for an append file.
      pwIJK = merge(.FALSE., fwIJK, fwIJK.AND.lExist)

      RETURN

 1000 FORMAT(//1X,70('*')/' From: dbg_mod -> arrayExtract_init',/,     &
         ' Error 1000: The initialization routine was never called.',  &
         ' Include the',/' following in USR0: CALL initExtract.',2/,   &
         ' These arguments are used to specify a domain subset to',    &
         ' extract. If',/' not defined, the entire domain (MIN3/MAX3)',&
         ' is extracted.',2/,' Optional arguments:',/,                 &
         3x,'iLow - lower I index;  iHgh - Upper I index  (X-axis)',/, &
         3x,'jLow - lower J index;  jHgh - Upper J index  (Y-axis)',/, &
         3x,'kLow - lower K index;  kHgh - Upper K index  (Z-axis)',/  &
         1x,70('*'),2/)

 1001 FORMAT(//1X,70('*')/' From: dbg_mod -> arrayExtract_init',/,     &
         ' Error 1001: dgbLock is set. Something must have failed.',/  &
         1x,70('*'),2/)

 1002 FORMAT(//1X,70('*')/' From: dbg_mod -> arrayExtract_init',/,     &
         ' Error 1002: Buffer already allocated: ',A,/1x,70('*'),2/)

 1003 FORMAT(//1X,70('*')/' From: dbg_mod -> arrayExtract_init',/,     &
         ' Error 1002: Error opening file: ',A,/1x,70('*'),2/)

      END SUBROUTINE arrayExtract_init

!----------------------------------------------------------------------!
!                                                                        !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE arrayExtract_finl

      implicit none

      IF(allocated(i_ofBuff)) deallocate(i_ofBuff)
      IF(allocated(j_ofBuff)) deallocate(j_ofBuff)
      IF(allocated(k_ofBuff)) deallocate(k_ofBuff)
      IF(allocated(ijk_Buff)) deallocate(ijk_Buff)

      IF(myPE == PE_IO) close(dbgUnit)

      dbgLock = .FALSE.

      RETURN
      END SUBROUTINE arrayExtract_finl

!----------------------------------------------------------------------!
!                                                                        !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE arrayExtract_prnt(dType)

      implicit none

! Data type
      CHARACTER(len=3), intent(in) :: dType

! Loop counter
      INTEGER :: IJK

! Rank 0 writes and closes the file.
      IF(myPE /= PE_IO) RETURN

      IF(fApnd) THEN
! Print header info.
         IF(pwIJK) THEN
            CALL WRITE_INDEX(i_ofBuff)
            CALL WRITE_INDEX(j_ofBuff)
            if(do_K) CALL WRITE_INDEX(k_ofBuff)
            CALL WRITE_INDEX(ijk_Buff)
         ENDIF

! Store the pass count.
         IF(fPass /= UNDEFINED_I)                                      &
            WRITE(dbgUnit,"(I14,',')",advance='no') fPass
! Output the data.
         SELECT CASE(dType)
            CASE('INT'); CALL WRITE_INT
            CASE('DBL'); CALL WRITE_DBL
            CASE('LOG'); CALL WRITE_LOG
         END SELECT

      ELSE

         IF(fPass /= UNDEFINED_I)                                      &
            WRITE(dbgUnit,"(2x,'Pass: ',I8)") fPass

         DO IJK=1, dbgDIMN
! Output the IJK info.
            IF(pwIJK)THEN
               WRITE(dbgUnit,"(4(I14,','))",advance='no') IJK,         &
                  ijk_Buff(IJK), i_ofBuff(IJK), j_ofBuff(IJK)
               if(do_K)WRITE(dbgUnit,"(I14,',')",advance='no')         &
                  k_ofBuff(IJK)
            ENDIF
! Write the formatted output.
            SELECT CASE(dType)
               CASE('INT'); CALL WRITE_INT(IJK)
               CASE('DBL'); CALL WRITE_DBL(IJK)
               CASE('LOG'); CALL WRITE_LOG(IJK)
            END SELECT
         ENDDO
      ENDIF

      RETURN
      END SUBROUTINE arrayExtract_prnt

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WRITE_INDEX(intArray)

      implicit none

      INTEGER, intent(in) :: intArray(dbgDIMN)
! Looped IJK
      INTEGER :: IJK

! Loop through array entries writing them in one long continuous line.
      IF(fPass /= UNDEFINED_I) WRITE(dbgUnit,"(14X,',')",advance='no')
      DO IJK = 1, dbgDIMN-1
         WRITE(dbgUnit,"(I14,',')",advance='no')intArray(IJK)
      ENDDO
      WRITE(dbgUnit,"(I14)")intArray(dbgDIMN)

      RETURN
      END SUBROUTINE WRITE_INDEX

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WRITE_INT(pIJK)

      implicit none

! Dummy arguments.
      INTEGER, optional, intent(in) :: pIJK
! Looped IJK
      INTEGER :: IJK

      IF(present(pIJK)) THEN
! Write the entry and return.
         WRITE(dbgUnit,"(I14)") outBuff_i(pIJK)

      ELSE
! Loop through array entries writing them in one long continuous line.
         DO IJK = 1, dbgDIMN-1
            WRITE(dbgUnit,"(I14,',')",advance='no')outBuff_i(IJK)
         ENDDO
         WRITE(dbgUnit,"(I14)")outBuff_i(dbgDIMN)
      ENDIF

      RETURN
      END SUBROUTINE WRITE_INT

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WRITE_DBL(pIJK)

      implicit none

! Dummy arguments.
      INTEGER, optional, intent(in) :: pIJK
! Looped IJK
      INTEGER :: IJK

      IF(present(pIJK)) THEN
! Write the entry and return.
         WRITE(dbgUnit,"(E14.6)") outBuff_dp(pIJK)
      ELSE
! Loop through array entries writing them in one long continuous line.
         DO IJK = 1, dbgDIMN-1
            WRITE(dbgUnit,"(E14.6,',')",advance='no')outBuff_dp(IJK)
         ENDDO
         WRITE(dbgUnit,"(E14.6)")outBuff_dp(dbgDIMN)
      ENDIF

      RETURN
      END SUBROUTINE WRITE_DBL

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WRITE_LOG(pIJK)

      implicit none

! Dummy arguments.
      INTEGER, optional, intent(in) :: pIJK
! Looped IJK
      INTEGER :: IJK
      LOGICAL :: INTtoLOG

      IF(present(pIJK)) THEN
! Write the entry and return.
         INTtoLOG= (outBuff_i(pIJK) .eq. 1)
         WRITE(dbgUnit,"(L14)") INTtoLOG

      ELSE
! Loop through array entries writing them in one long continuous line.
         DO IJK = 1, dbgDIMN-1
            INTtoLOG=(outBuff_i(IJK) .eq. 1)
            WRITE(dbgUnit,"(L14,',')",advance='no') INTtoLOG
         ENDDO
         INTtoLOG = (outBuff_i(dbgDIMN) .eq. 1)
         WRITE(dbgUnit,"(L14)") INTtoLOG

      ENDIF

      RETURN
      END SUBROUTINE WRITE_LOG

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE arrayExtract_int(Array, VAR, PASS, APND, withIJK)

      use compar
      use fldvar
      use geometry
      use indices
      use mflux
      use parallel
      use param
      use param1
      use physprop
      use run
      use sendrecv
      USE mpi_utility
      USE functions

      implicit none

! Array to be extracted.
      INTEGER, intent(in) :: Array(DIMENSION_3)
! Variable named used to create file name.
      CHARACTER(len=*), intent(in) :: VAR
! Pass count if dumping at multiple locations or during iterations.
      INTEGER, intent(in), optional :: PASS
! Flag to append to previous output.
      LOGICAL, intent(in), optional :: APND
! Flag to write IJK values with output.
      LOGICAL, intent(in), optional :: withIJK
! Loop counters:
      INTEGER :: I, J, K, IJK, dbgIJK
! Debugging message.
      CHARACTER(len=64) :: MSG

      MSG='Entered arrayExtract_int'
      if(dbgMode) CALL DBG_WRITE(trim(MSG),FLUSH=.TRUE.)

! Set local flags based on optional arguments.
      fPass = merge(PASS, UNDEFINED_I, present(PASS))
      fApnd = merge(APND, .FALSE., present(APND))
      fwIJK = merge(withIJK, .FALSE., present(withIJK))

      MSG='  > Calling arrayExtract_INIT'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))
      CALL arrayExtract_init(VAR)

      MSG='  > Allocating outBuff_i'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))
      allocate( outBuff_i(dbgDIMN) ); outBuff_i = 0

      MSG='  > Extracting array data.'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))

      do K = kLB, kUB
      do I = iLB, iUB
      do J = jLB, jUB
         if(IS_ON_myPE_owns(I,J,K)) then
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
            IJK = funIJK(I,J,K)
            dbgIJK = dbg_funijk(I,J,K)
! Only the rank that owns IJK populates lAm and lBm.
            outBuff_i(dbgIJK) = Array(IJK)
! Store indicies.
            i_ofBuff(dbgIJK) = I
            j_ofBuff(dbgIJK) = J
            k_ofBuff(dbgIJK) = K
            ijk_Buff(dbgIJK) = IJK
         endif
      enddo
      enddo
      enddo

      MSG='  > Collecting array data.'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))
! Global sum updates lAm and lBm for all ranks.
      CALL global_all_sum(outBuff_i)
      if(pwIJK) then
         CALL global_all_sum(i_ofBuff)
         CALL global_all_sum(j_ofBuff)
         CALL global_all_sum(ijk_Buff)
         if(do_K)CALL global_all_sum(k_ofBuff)
      endif

! Write the data.
      MSG='  > Calling arrayExtract_prnt.'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))
      CALL arrayExtract_prnt('INT')

! Clean up the used memory.
      MSG='  > Calling arrayExtract_finl.'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))

      if(allocated(outBuff_i)) deallocate(outBuff_i)
      CALL arrayExtract_finl

      MSG='Leaving arrayExtract_int.'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))

      RETURN
      END SUBROUTINE arrayExtract_int

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE arrayExtract_dbl(Array, VAR, PASS, APND, withIJK)

      use compar
      use fldvar
      use geometry
      use indices
      use mflux
      use parallel
      use param
      use param1
      use physprop
      use run
      use sendrecv
      USE mpi_utility
      USE functions

      implicit none

! Array to be extracted.
      DOUBLE PRECISION, intent(in) :: Array(DIMENSION_3)
! Variable named used to create file name.
      CHARACTER(len=*), intent(in) :: VAR
! Pass count if dumping at multiple locations or during iterations.
      INTEGER, intent(in), optional :: PASS
! Flag to append to previous output.
      LOGICAL, intent(in), optional :: APND
! Flag to write IJK values with output.
      LOGICAL, intent(in), optional :: withIJK

! Loop indices:
      INTEGER :: I, J, K, IJK, dbgIJK
! Debugging message.
      CHARACTER(len=64) :: MSG

      MSG='Entered arrayExtract_dbl'
      CALL DBG_WRITE(trim(MSG), flush=.TRUE.)

! Set local flags based on optional arguments.
      fPass = merge(PASS, UNDEFINED_I, present(PASS))
      fApnd = merge(APND, .FALSE., present(APND))
      fwIJK = merge(withIJK, .FALSE., present(withIJK))

      MSG='  > Calling arrayExtract_INIT'
      CALL DBG_WRITE(trim(MSG))
      CALL arrayExtract_init(VAR)

! Allocate the global storage array.
      allocate( outBuff_dp(dbgDIMN) ); outBuff_dp = 0

      MSG='  > Extracting array data.'
      CALL DBG_WRITE(trim(MSG))

! Extract the data... one cell at a time.
      do K = kLB, kUB
      do I = iLB, iUB
      do J = jLB, jUB
         if(IS_ON_myPE_owns(I,J,K)) then
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
            IJK = funIJK(I,J,K)
            dbgIJK = dbg_funijk(I,J,K)
! Only the rank that owns IJK populates lAm and lBm.
            outBuff_dp(dbgIJK) = Array(IJK)
! Store indicies.
            i_ofBuff(dbgIJK) = I
            j_ofBuff(dbgIJK) = J
            k_ofBuff(dbgIJK) = K
            ijk_Buff(dbgIJK) = IJK
         endif
      enddo
      enddo
      enddo

! Global sum updates lAm and lBm for all ranks.
      MSG='  > Collecting array data.'
      CALL DBG_WRITE(trim(MSG))

      CALL global_all_sum(outBuff_dp)
      if(pwIJK) then
         CALL global_all_sum(i_ofBuff)
         CALL global_all_sum(j_ofBuff)
         CALL global_all_sum(ijk_Buff)
         if(do_K)CALL global_all_sum(k_ofBuff)
      endif

! Write the data.
      MSG='  > Calling arrayExtract_prnt.'
      CALL DBG_WRITE(trim(MSG))
      CALL arrayExtract_prnt('DBL')

! Clean up the used memory.
      MSG='  > Calling arrayExtract_finl.'
      CALL DBG_WRITE(trim(MSG))
      if(allocated(outBuff_dp)) deallocate(outBuff_dp)
      CALL arrayExtract_finl

      MSG='Leaving arrayExtract_dbl.'
      CALL DBG_WRITE(trim(MSG))

      RETURN
      END SUBROUTINE arrayExtract_dbl

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE arrayExtract_log(Array, VAR, PASS, APND, withIJK)

      use compar
      use fldvar
      use geometry
      use indices
      use mflux
      use parallel
      use param
      use param1
      use physprop
      use run
      use sendrecv
      USE mpi_utility
      USE functions

      implicit none

! Array to be extracted.
      LOGICAL, intent(in) :: Array(DIMENSION_3)
! Variable named used to create file name.
      CHARACTER(len=*), intent(in) :: VAR
! Pass count if dumping at multiple locations or during iterations.
      INTEGER, intent(in), optional :: PASS
! Flag to append to previous output.
      LOGICAL, intent(in), optional :: APND
! Flag to write IJK values with output.
      LOGICAL, intent(in), optional :: withIJK
! Loop counters:
      INTEGER :: I, J, K, IJK, dbgIJK
! Debugging message.
      CHARACTER(len=64) :: MSG

      MSG='Entered arrayExtract_log'
      if(dbgMode) CALL DBG_WRITE(trim(MSG),FLUSH=.TRUE.)

! Set local flags based on optional arguments.
      fPass = merge(PASS, UNDEFINED_I, present(PASS))
      fApnd = merge(APND, .FALSE., present(APND))
      fwIJK = merge(withIJK, .FALSE., present(withIJK))

      MSG='  > Calling arrayExtract_INIT'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))
      CALL arrayExtract_init(VAR)

      MSG='  > Allocating outBuff_i'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))
      allocate( outBuff_i(dbgDIMN) ); outBuff_i = 0

      MSG='  > Extracting array data.'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))

      do K = kLB, kUB
      do I = iLB, iUB
      do J = jLB, jUB
         if(IS_ON_myPE_owns(I,J,K)) then
         IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
            IJK = funIJK(I,J,K)
            dbgIJK = dbg_funijk(I,J,K)
! Only the rank that owns IJK populates lAm and lBm.
! Convert the logical to interger.
            outBuff_i(dbgIJK) = merge(1,0,Array(IJK))
! Store indicies.
            i_ofBuff(dbgIJK) = I
            j_ofBuff(dbgIJK) = J
            k_ofBuff(dbgIJK) = K
            ijk_Buff(dbgIJK) = IJK
         endif
      enddo
      enddo
      enddo

      MSG='  > Collecting array data.'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))
! Global sum updates lAm and lBm for all ranks.
      CALL global_all_sum(outBuff_i)
      if(pwIJK) then
         CALL global_all_sum(i_ofBuff)
         CALL global_all_sum(j_ofBuff)
         CALL global_all_sum(ijk_Buff)
         if(do_K)CALL global_all_sum(k_ofBuff)
      endif

! Write the data.
      MSG='  > Calling arrayExtract_prnt.'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))
      CALL arrayExtract_prnt('LOG')

! Clean up the used memory.
      MSG='  > Calling arrayExtract_finl.'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))

      if(allocated(outBuff_i)) deallocate(outBuff_i)
      CALL arrayExtract_finl

      MSG='Leaving arrayExtract_log.'
      if(dbgMode) CALL DBG_WRITE(trim(MSG))

      RETURN
      END SUBROUTINE arrayExtract_log

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: debug_write()                                          C
!  Purpose: Write out full geometry index setup information for the
!  case
!                                                                      C
!  Author: Aytekin Gel                                Date: 19-SEP-03  C
!  Reviewer:                                          Date:            C
!                                                                      C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE debug_write_layout()

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE cdist
      USE compar
      USE functions
      USE funits
      USE geometry
      USE indices
      USE leqsol
      USE mpi_utility
      USE parallel
      USE param
      USE param1
      USE run
      USE sendrecv
      USE time_cpu
      IMPLICIT NONE
!-----------------------------------------------
! Local Variables
!-----------------------------------------------
! phase index
      INTEGER :: M
! indices
      INTEGER :: i, j, k, ijk, ijk_GL, ijk_PROC, ijk_IO
!
      integer :: indxA, indxA_gl, indxB, indxB_gl, indxC, indxC_gl
      integer :: indxD, indxD_gl, indxE, indxE_gl, indxF, indxF_gl
      integer :: indxG, indxG_gl, indxH, indxH_gl
!
      logical :: amgdbg = .TRUE.

      character(LEN=80) :: fname

!DISTIO
!      fname = "layout_xxxx.txt"
!      write (fname(8:11),'(i4.4)') myPE
      fname = "layout_xxxxx.txt"
      write (fname(8:12),'(i5.5)') myPE
      open (unit=11,file=fname,status='unknown')

      write (11,*) ' ********************************************'
      write (11,*) ' ********************************************'
      write (11,*) ' ********************************************'
      write (11,*) ' ********************************************'
      write (11,*) ' '
      write (11,*) ' '
      write (11,*) ' myPE =           ' , myPE
      write (11,*) ' '
      write (11,*) ' '


      IF (AMGDBG .OR. bDist_IO) THEN
         write(11,"('BLK1: Running from istart3,iend3 .AND. jstart3, jend3 .AND. kstart3, kend3')")
         write(11,"(' (   i ,    j,     k) =>    ijk      ijk_GL     ijk_PROC    ijk_IO')")
         write(11,"(' ====================      =====     =======    ========    ======')")
         DO k = kstart3, kend3
            DO i = istart3,iend3
               DO j = jstart3, jend3
                  ijk = FUNIJK(i,j,k)
                  ijk_GL = FUNIJK_GL(i,j,k)
                  ijk_PROC = FUNIJK_PROC(i,j,k,myPE)
                  ijk_IO = FUNIJK_IO(i,j,k)
                  write(11,"(' (',I4,' , ',I4,' , ',I4,') => ',4(I8,' , '))") &
                       i,j,k,ijk,ijk_GL,ijk_PROC,ijk_IO
               ENDDO
            ENDDO
         ENDDO

         write(11,"(/,/,'BLK2: Print out Bottom, South, West, East, North, Top neighbors')")
         write(11,"(' (   i ,    j,     k) =>    ijk    ijk_GL    B_of    S_of    W_of    E_of    N_of    T_of')")
         write(11,"(' ====================      =====   =======  ======  ======  ======  ======  ======  ======')")
         DO k = kstart3, kend3
            DO i = istart3,iend3
               DO j = jstart3, jend3
                  ijk = FUNIJK(i,j,k)
                  ijk_GL = FUNIJK_GL(i,j,k)
                  write(11,"(' (',I4,' , ',I4,' , ',I4,') => ',2(I7,' , '),6(I7,2X))") &
                       i,j,k,ijk,ijk_GL,bottom_of(ijk),south_of(ijk),west_of(ijk),&
                       east_of(ijk),north_of(ijk),top_of(ijk)
               ENDDO
            ENDDO
         ENDDO

         write(11,"(/,/,'BLK3: Print out km, jm, im, ip, jp, kp neighbors')")
         write(11,"(' (   i ,    j,     k) =>    ijk    ijk_GL    km_of   jm_of   im_of   ip_of   jp_of   kp_of')")
         write(11,"(' ====================      =====   =======  ======  ======  ======  ======  ======  ======')")
         DO k = kstart3, kend3
            DO i = istart3,iend3
               DO j = jstart3, jend3
                  ijk = FUNIJK(i,j,k)
                  ijk_GL = FUNIJK_GL(i,j,k)
                  write(11,"(' (',I4,' , ',I4,' , ',I4,') => ',2(I7,' , '),6(I7,2X))") &
                       i,j,k,ijk,ijk_GL,km_of(ijk),jm_of(ijk),im_of(ijk),&
                       ip_of(ijk),jp_of(ijk),kp_of(ijk)
               ENDDO
            ENDDO
         ENDDO

         write(11,"(/,'BLK4a: Active Fluid Cells:FLUID_AT(ijk)=.T.',/,&
              &           ' (   i ,    j,     k) =>    ijk  [   x ,     ,     z]')")
         write(11,"(' ====================      =====  ====================')")
         DO ijk = ijkstart3, ijkend3
            I = I_OF(IJK)
            J = J_OF(IJK)
            K = K_OF(IJK)

            !         IF (FLOW_AT_E(IJK)) THEN
            IF (FLUID_AT(IJK)) THEN
               !          write(11,"(' (',I4,' , ',I4,' , ',I4,') => ',I8)") I,J,K,ijk
               write(11,"(' (',I4,' , ',I4,' , ',I4,') => ',I8,' [',E12.5,',',E12.5,' ]')") I,J,K,ijk,X(i),Z(k)
            ENDIF
         ENDDO

         write(11,"(/,'BLK4b: Cells that are (.NOT.WALL_AT(IJK)) = .T.',/,&
              &           ' (   i ,    j,     k) =>    ijk  [   x ,     ,     z]')")
         write(11,"(' ====================      =====  ====================')")
         DO ijk = ijkstart3, ijkend3
            I = I_OF(IJK)
            J = J_OF(IJK)
            K = K_OF(IJK)

            IF (.NOT.WALL_AT(IJK)) THEN
               !          write(11,"(' (',I4,' , ',I4,' , ',I4,') => ',I8)") I,J,K,ijk
               write(11,"(' (',I4,' , ',I4,' , ',I4,') => ',I8,' [',E12.5,',',E12.5,' ]')") I,J,K,ijk,X(i),Z(k)
            ENDIF
         ENDDO

         DO k = kstart3, kend3
            DO i = istart3,iend3
               DO j = jstart3, jend3
                  ijk = FUNIJK(i,j,k)
                  ijk_GL = FUNIJK_GL(i,j,k)

                  if (i == istart2 .AND. j == jstart2) then
                     indxA = ijk
                     indxA_gl = ijk_GL
                  endif
                  if (i == istart1 .AND. j == jstart1) then
                     indxE = ijk
                     indxE_gl = ijk_GL
                  endif
                  if (i == istart2 .AND. j == jend2) then
                     indxB = ijk
                     indxB_gl = ijk_GL
                  endif
                  if (i == istart1 .AND. j == jend1) then
                     indxF = ijk
                     indxF_gl = ijk_GL
                  endif
                  if (i == iend1 .AND. j == jstart1) then
                     indxH = ijk
                     indxH_gl = ijk_GL
                  endif
                  if (i == iend2 .AND. j == jstart2) then
                     indxD = ijk
                     indxD_gl = ijk_GL
                  endif
                  if (i == iend1 .AND. j == jend1) then
                     indxG = ijk
                     indxG_gl = ijk_GL
                  endif
                  if (i == iend2 .AND. j == jend2) then
                     indxC = ijk
                     indxC_gl = ijk_GL
                  endif
               ENDDO
            ENDDO
            write(11,"('BLK5:')")
            write(11,"(57('='))")
            write(11,"('k= ',I5,/,57('='))") k
            write(11,"('B= ',I5,' (',I7,')',20X,'C= ',I5,' (',I7,')',/)") indxB, indxB_gl, &
                 indxC, indxC_gl
            !        write(UNIT_LOG,"(' \',34X,'/')")
            !        write(UNIT_LOG,"(2X,'\',32X,'/')")
            write(11,"(3X,'F= ',I5,' (',I7,')',12X,'G= ',I5,' (',I7,')')") indxF, indxF_gl, &
                 indxG, indxG_gl
            write(11,"(4(9X,'|',29X,'|',/))")
            write(11,"(3X,'E= ',I5,' (',I7,')',12X,'H= ',I5,' (',I7,')',/)") indxE, indxE_gl, &
                 indxH, indxH_gl
            !        write(UNIT_LOG,"(2X,'/',32X,'\')")
            !        write(UNIT_LOG,"('/',34X,'\')")
            write(11,"('A= ',I5,' (',I7,')',20X,'D= ',I5,' (',I7,')',/,/)") indxA, indxA_gl, &
                 indxD, indxD_gl

            !        write(UNIT_LOG,"(' (',I4,' , ',I4,' , ',I4,') => ',2(I7,' , '),6(I7,2X))") &
            !                                         i,j,k,ijk,ijk_GL,bottom_of(ijk),south_of(ijk),west_of(ijk),&
            !                                        east_of(ijk),north_of(ijk),top_of(ijk)

         ENDDO

         !      write(UNIT_LOG,"(/,' (   i ,    j,     k) =>    ijk (Active Fluid)')")
         !      write(UNIT_LOG,"(' ====================      =====')")
         !       DO ijk = ijkstart3, ijkend3
         !         I = I_OF(IJK)
         !         J = J_OF(IJK)
         !         K = K_OF(IJK)

         !         IF (FLOW_AT_E(IJK)) THEN
         !         IF (FLUID_AT(IJK)) THEN
         !           write(UNIT_LOG,"(' (',I4,' , ',I4,' , ',I4,') => ',I8)") I,J,K,ijk
         !         ENDIF
         !      END DO


      endif   ! end if(amgdbg .or. bdist_io)

      M = 0
      !      CALL WRITE_AB_M (A_M, B_M, IJKMAX2, M, IER)

      IF (AMGDBG .OR. bDist_IO) THEN
         write(11,"(/,/,'BLK6: ========= ORIGINAL MFIX VARIABLES ===========')")
         write(11,"('PE ',I5,': imin1  = ',I6,3X,'imax1= ',I6,/,'PE ',I5,': jmin1  = ',I6,3X,'jmax1= ',I6)") &
              myPE,imin1,imax1,myPE,jmin1,jmax1
         write(11,"('PE ',I5,': kmin1  = ',I6,3X,'kmax1= ',I6)") myPE,kmin1,kmax1
         write(11,"('-----')")
         write(11,"('PE ',I5,': imin2  = ',I6,3X,'imax2= ',I6,/,'PE ',I5,': jmin2  = ',I6,3X,'jmax2= ',I6)") &
              myPE,imin2,imax2,myPE,jmin2,jmax2
         write(11,"('PE ',I5,': kmin2  = ',I6,3X,'kmax2= ',I6)") myPE,kmin2,kmax2
         write(11,"('----- Below xxx3 set is DMP extension ------------')")
         write(11,"('PE ',I5,': imin3  = ',I6,3X,'imax3= ',I6,/,'PE ',I5,': jmin3  = ',I6,3X,'jmax3= ',I6)") &
              myPE,imin3,imax3,myPE,jmin3,jmax3
         write(11,"('PE ',I5,': kmin3  = ',I6,3X,'kmax3= ',I6)") myPE,kmin3,kmax3
         write(11,"('----- End of Below xxx3 set is DMP extension -----')")
         !      write(11,"('PE ',I5,': ijkmax2= ',I6)") myPE,ijkmax2
         write(11,"('PE ',I5,': ijmax2 = ',I6)") myPE,ijmax2
         write(11,"('PE ',I5,': ijkmin1= ',I6,' ijkmax1= ',I12)") myPE,ijkmin1, ijkmax1
         write(11,"('PE ',I5,':          ',6X,' ijkmax2= ',I12)") myPE,ijkmax2
         write(11,"('PE ',I5,':          ',6X,' ijkmax3= ',I12)") myPE,ijkmax3


         write(11,"(/,/,' ========= DMP EXTENSION VARIABLES ===========')")
         !      write(UNIT_LOG,"('PE ',I5,': ijksize  = ',I6)") myPE,ijksize
         write(11,"('PE ',I5,': ijksize3 = ',I6,3X,'ijksize3_all = ',I6)") myPE,ijksize3,ijksize3_all(myPE)
         write(11,"('PE ',I5,': ijkstart3  = ',I6,3X,'ijkend3  = ',I6)") myPE,ijkstart3, ijkend3
         write(11,"('PE ',I5,': ijkstart3_all = ',I6)") myPE,ijkstart3_all(myPE)
         write(11,"('PE ',I5,': istart_all = ',I6,3X,'iend_all = ',I6,/,'PE ',I5,': jstart_all = ',I6,3X,'jend_all = ',I6)") &
              myPE,istart_all(myPE),iend_all(myPE),myPE,jstart_all(myPE),jend_all(myPE)
         write(11,"('PE ',I5,': kstart_all = ',I6,3X,'kend_all = ',I6,/,'----------------------')") &
              myPE,kstart_all(myPE),kend_all(myPE)

         write(11,"('PE ',I5,': istart1_all= ',I6,3X,'iend1_all= ',I6,/,'PE ',I5,': jstart1_all= ',I6,3X,'jend3_all= ',I6)") &
              myPE,istart1_all(myPE),iend1_all(myPE),myPE,jstart1_all(myPE),jend1_all(myPE)
         write(11,"('PE ',I5,': kstart1_all= ',I6,3X,'kend1_all= ',I6,/,'----------------------')") &
              myPE,kstart1_all(myPE),kend1_all(myPE)

         write(11,"('PE ',I5,': istart2_all= ',I6,3X,'iend2_all= ',I6,/,'PE ',I5,': jstart2_all= ',I6,3X,'jend3_all= ',I6)") &
              myPE,istart2_all(myPE),iend2_all(myPE),myPE,jstart2_all(myPE),jend2_all(myPE)
         write(11,"('PE ',I5,': kstart2_all= ',I6,3X,'kend2_all= ',I6,/,'----------------------')") &
              myPE,kstart2_all(myPE),kend2_all(myPE)

         write(11,"('PE ',I5,': istart3_all= ',I6,3X,'iend3_all= ',I6,/,'PE ',I5,': jstart3_all= ',I6,3X,'jend3_all= ',I6)") &
              myPE,istart3_all(myPE),iend3_all(myPE),myPE,jstart3_all(myPE),jend3_all(myPE)
         write(11,"('PE ',I5,': kstart3_all= ',I6,3X,'kend3_all= ',I6,/,'----------------------')") &
              myPE,kstart3_all(myPE),kend3_all(myPE)

         write(11,"('PE ',I5,': istart1= ',I6,3X,'iend1= ',I6,/,'PE ',I5,': jstart1= ',I6,3X,'jend1= ',I6)") &
              myPE,istart1,iend1,myPE,jstart1,jend1
         write(11,"('PE ',I5,': kstart1= ',I6,3X,'kend1= ',I6,/,'----------------------')") &
              myPE,kstart1,kend1
         write(11,"('PE ',I5,': istart2= ',I6,3X,'iend2= ',I6,/,'PE ',I5,': jstart2= ',I6,3X,'jend2= ',I6)") &
              myPE,istart2,iend2,myPE,jstart2,jend2
         write(11,"('PE ',I5,': kstart2= ',I6,3X,'kend2= ',I6,/,'----------------------')") &
              myPE,kstart2,kend2
         write(11,"('PE ',I5,': istart3= ',I6,3X,'iend3= ',I6,/,'PE ',I5,': jstart3= ',I6,3X,'jend3= ',I6)") &
              myPE,istart3,iend3,myPE,jstart3,jend3
         write(11,"('PE ',I5,': kstart3= ',I6,3X,'kend3= ',I6,/,'----------------------')") &
              myPE,kstart3,kend3

      ENDIF   ! end if(amgdbg .or. bdist_io)

      close(unit=11)


      RETURN
   END SUBROUTINE DEBUG_WRITE_LAYOUT

   !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC

   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

   SUBROUTINE write_parallel_info()

      !-----------------------------------------------
      !   M o d u l e s
      !-----------------------------------------------
      USE compar
      USE functions
      USE funits
      USE geometry
      USE indices
      USE leqsol
      USE mpi_utility
      USE parallel
      USE param
      USE param1
      USE run
      USE sendrecv
      USE time_cpu
      IMPLICIT NONE
      !-----------------------------------------------
      ! Dummy arguments
      !-----------------------------------------------
      ! Local Variables
      !-----------------------------------------------
      ! phase index
      INTEGER :: M
      ! indices
      INTEGER :: i, j, k, ijk, ijk_GL, ijk_PROC, ijk_IO
      !
      character(LEN=80) :: fname
      !-----------------------------------------------

      !DISTIO
      !      fname = "p_info_xxxx.txt"
      !      write (fname(8:11),'(i4.4)') myPE
      fname = "p_info_xxxxx.txt"
      write (fname(8:12),'(i5.5)') myPE
      open (unit=11,file=fname,status='unknown')

      write (11,*) myPe , ' = myPE'

      write (11,*) myPE , istart3,iend3
      write (11,*) myPE , jstart3,jend3
      write (11,*) myPE , kstart3,kend3

      write(11,"('BLK1: Running from istart3,iend3 .AND. jstart3, jend3 .AND. kstart3, kend3')")
      write(11,"(' (   i ,    j,     k)       ijk      ijk_GL     ijk_PROC    ijk_IO')")
      write(11,"(' ====================      =====     =======    ========    ======')")
      DO k = kstart3, kend3
         DO i = istart3,iend3
            DO j = jstart3, jend3
               ijk = FUNIJK(i,j,k)
               ijk_GL = FUNIJK_GL(i,j,k)
               ijk_PROC = FUNIJK_PROC(i,j,k,myPE)
               ijk_IO = FUNIJK_IO(i,j,k)
               write(11,"('  ',I4,'   ',I4,'   ',I4,'     ',4(I8,'   '))" ) &
                    i,j,k,ijk,ijk_GL,ijk_PROC,ijk_IO
            ENDDO
         ENDDO
      ENDDO

      M = 0
      !      CALL WRITE_AB_M (A_M, B_M, IJKMAX2, M, IER)

      close(unit=11)

      RETURN
   END SUBROUTINE write_parallel_info

      END MODULE dbg

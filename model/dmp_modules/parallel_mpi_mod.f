! -*- f90 -*-
module parallel_mpi

      use geometry
      use compar

      implicit none

      contains

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: parallel_init                                          !
!  Author:                                            Date: XX-XXX-XX  !
!                                                                      !
!  Purpose: Wrapper function to initiailize OpenMP and MPI.            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      subroutine parallel_init()

      use compar, only: fbname
      USE funits, only: dmp_log, unit_log
      USE machine, only: get_run_id

      implicit none

      integer :: ierr

!$    integer :: num_threads, threads_specified, omp_id
!$    integer :: omp_get_num_threads
!$    integer :: omp_get_thread_num
!$    character(len=512) :: omp_num_threads
!$    integer :: length
!$    integer :: status

! Variables for generating file basename with processor id
      integer :: i1, i10, i100, i1000, i10000

! No need to initialize MPI when adjusting partition
      IF(ADJUST_PARTITION) RETURN

      numPEs = 1
      myPE = 0

#ifdef MPI
      call MPI_Init(ierr)
      call MPI_Check( 'parallel_init:MPI_Init ', ierr)

      call MPI_COMM_SIZE( MPI_COMM_WORLD, numPEs, ierr )
      call MPI_Check( 'parallel_init:MPI_Comm_size ', ierr )

      call MPI_COMM_RANK( MPI_COMM_WORLD, myPE, ierr )
      call MPI_Check( 'parallel_init:MPI_Comm_size ', ierr )
#endif

! Only PE_IO to write out common error messages
      DMP_LOG = (myPE == PE_IO)

! PAR_I/O Generate file basename for LOG files
      i10000 = int(myPE/10000)
      i1000  = int((myPE-i10000*10000)/1000)
      i100   = int((myPE-i10000*10000-i1000*1000)/100)
      i10    = int((myPE-i10000*10000-i1000*1000-i100*100)/10)
      i1     = int((myPE-i10000*10000-i1000*1000-i100*100-i10*10)/1)

      i10000 = i10000 + 48
      i1000  = i1000  + 48
      i100   = i100   + 48
      i10    = i10    + 48
      i1     = i1     + 48

      fbname=char(i10000)//char(i1000)//char(i100)//char(i10)//char(i1)

! Specify the number of processors to be used
!$    call get_environment_variable("OMP_NUM_THREADS", &
!$       omp_num_threads,length,status, .true.)
!$    if (status.eq.0 .and. length.ne.0) then
!$      read(omp_num_threads,*) threads_specified
!$    else
!$      WRITE(*,'(A)') 'Enter the number of threads to be used for SMP: '
!$      READ(*,*) threads_specified
!$    endif

!$    call omp_set_num_threads(threads_specified)

! Report the number of SMP threads used
!$omp parallel
!$    num_threads = omp_get_num_threads()
!$    if(omp_get_thread_num() == 0 .and. DMP_LOG) then
!$       write(*,2000) num_threads
!$       write(unit_log,2000) num_threads
!$    endif
!$omp end parallel

 2000 format(/1x,'Number of SMP threads: ',I0,2/)




! Get the date and time. They give the unique run_id in binary output
! files
      CALL GET_RUN_ID


      return
      end subroutine parallel_init


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: parallel_fin                                           !
!  Author:                                            Date: XX-XXX-XX  !
!                                                                      !
!  Purpose: Wrapper for MPI_Finalize calls.                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      subroutine parallel_fin()
      implicit none

#ifdef MPI
      integer :: ierr
      logical :: flag

      call MPI_Finalized(flag, ierr)
      if (.not. flag) then
         call MPI_Finalize(ierr)
         call MPI_Check( 'parallel_init:MPI_Finalize ', ierr)
      end if
#endif

      return
      end subroutine parallel_fin


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: MPI_Check                                              !
!  Author:                                            Date: XX-XXX-XX  !
!                                                                      !
!  Purpose: Check error flags returned by MPI calls.                   !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      subroutine MPI_Check( msg, ierr )

#ifdef MPI
      use mpi, only: MPI_SUCCESS
#endif

      implicit none
      character(len=*),intent(in) :: msg
      integer, intent(in) :: ierr

#ifdef MPI
      character(len=512) :: errmsg
      integer :: resultlen, ierror

      if (ierr .ne. MPI_SUCCESS ) then
         call MPI_Error_string( ierr, errmsg, resultlen, ierror )
         print*, 'Error: ', msg
         print*, errmsg(1:resultlen)
         stop '** ERROR ** '
      endif
#endif

      return
      end subroutine MPI_Check


end module parallel_mpi

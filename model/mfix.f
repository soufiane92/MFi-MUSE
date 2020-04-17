! -*- f90 -*-
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: MFIX                                                    !
!  Author: M. Syamlal                                 Date: 29-JAN-92  !
!                                                                      !
!  Purpose: The main module in the MFIX program                        !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
!
!> \mainpage Multiphase Flow with Interphase eXchanges
!!
!! MFIX is a general-purpose computer code developed at the National
!! Energy Technology Laboratory, NETL, for describing the hydrodynamics,
!! heat transfer, and chemical reactions in fluid-solid systems.
!!
!! It has been used for describing bubbling and circulating fluidized
!! beds and spouted beds. MFiX calculations give transient data on the
!! three-dimensional distribution of pressure, velocity, temperature,
!! and species mass fractions. MFiX code is based on a generally
!! accepted set of multiphase flow equations. The code is used as a
!! "test-stand" for testing and developing multiphase flow constitutive
!!  equations.
!!
!! \section Notice
!! Neither the United States Government nor any agency thereof, nor any
!! of their employees, makes any warranty, expressed or implied, or
!! assumes any legal liability or responsibility for the accuracy,
!! completeness, or usefulness of any information, apparatus, product,
!! or process disclosed or represents that its use would not infringe
!! privately owned rights.
!!
!! * MFIX is provided without any user support for applications in the
!!   user's immediate organization. It should not be redistributed in
!!   whole or in part.
!!
!! * The use of MFIX is to be acknowledged in any published paper based
!!   on computations using this software by citing the MFIX theory
!!   manual. Some of the submodels are being developed by researchers
!!   outside of NETL. The use of such submodels is to be acknowledged
!!   by citing the appropriate papers of the developers of the submodels.
!!
!! * The authors would appreciate receiving any reports of bugs or other
!!   difficulties with the software, enhancements to the software, and
!!   accounts of practical applications of this software.
!!
!! \section Disclaimer
!! This report was prepared as an account of work sponsored by an agency
!! of the United States Government. Neither the United States Government
!! nor any agency thereof, nor any of their employees, makes any
!! warranty, express or implied, or assumes any legal liability or
!! responsibility for the accuracy, completeness, or usefulness of any
!! information, apparatus, product, or process disclosed, or represents
!! that its use would not infringe privately owned rights. Reference
!! herein to any specific commercial product, process, or service by
!! trade name, trademark, manufacturer, or otherwise does not
!! necessarily constitute or imply its endorsement, recommendation, or
!! favoring by the United States Government or any agency thereof. The
!! views and opinions of authors expressed herein do not necessarily
!! state or reflect those of the United States Government or any
!! agency thereof.



SUBROUTINE RUN_MFIX(MFIX_DAT_FILENAME, MFIX_DAT_SIZE)
!f2py threadsafe

      USE COMPAR, only:ADJUST_PARTITION, MYPE, PE_IO
      USE DES_TIME_MARCH, ONLY: DES_TIME_INIT, DES_TIME_STEP, DES_TIME_END, FACTOR, EXIT_LOOP
      USE DISCRETELEMENT, ONLY: DES_CONTINUUM_COUPLED, DISCRETE_ELEMENT
      USE ERROR_MANAGER, ONLY: INIT_ERROR_MANAGER, FLUSH_ERR_MSG
      USE EXIT, ONLY: EXIT_FLAG, MFIX_EXIT, CHECK_EXIT_FLAG
      USE ITERATE, ONLY: CONVERGED, DIVERGED, ADJUSTDT
      USE ITERATE, ONLY: ITERATE_INIT, DO_ITERATION, POST_ITERATE
      USE ITERATE, ONLY: LOG_CONVERGED, LOG_DIVERGED, NIT, MAX_NIT
      USE MACHINE, ONLY: WALL_TIME
      USE MAIN, ONLY: GET_DATA, INITIALIZE, FINALIZE
      USE MAIN, ONLY: PRINT_FLAGS
      USE MPI_UTILITY
      USE PIC_TIME_MARCH_MOD, ONLY: PIC_TIME_MARCH
      USE RUN, ONLY:  DT, DEM_SOLIDS, PIC_SOLIDS, STEADY_STATE, TIME, TSTOP
      USE STEP, ONLY: CHECK_LOW_DT, CHEM_MASS, TIME_STEP_INIT, TIME_STEP_END
      USE iso_c_binding;
      USE param1, only: n_spx
      USE pause, only: check_pause

      IMPLICIT NONE
! Path to input file
      CHARACTER(LEN=1000), INTENT(IN) :: MFIX_DAT_FILENAME
      INTEGER, INTENT(IN) :: MFIX_DAT_SIZE

!-----------------------------------------------
! Local variables
!-----------------------------------------------

      DOUBLE PRECISION :: STARTTIME
      INTEGER :: II

      CHARACTER(MFIX_DAT_SIZE) :: MFIX_DAT

      INTEGER :: IOS

! Dynamic load balance loop
      DO

     ! Open the mfix.dat file. Report errors if the file is not located or
     ! there is difficulties opening it.

      OPEN(UNIT=UNIT_DAT, FILE=mfix_dat_filename, STATUS='OLD', IOSTAT=IOS, &
           FORM="UNFORMATTED", ACCESS="STREAM", ACTION="READ")
      IF(IOS /= 0) THEN
         IF(myPE == PE_IO) WRITE (*,1001) mfix_dat_FILENAME
         call flush_err_msg(abort=.TRUE.)
1001     FORMAT(2/,1X,70('*')/' From: RUN_MFIX',/' Error 1001: ',    &
              'Unable to open input data file: ',A/,'Aborting.',/1x,70('*'),2/)
      ENDIF

      READ(UNIT_DAT) MFIX_DAT
      CLOSE(UNIT_DAT)

! Read input data, check data, do computations for IC and BC locations
! and flows, and set geometry parameters such as X, X_E, DToDX, etc.
      CALL GET_DATA(MFIX_DAT)

      IF (CHECK_EXIT_FLAG()) RETURN

! Initialize the simulation
      CALL INITIALIZE(MFIX_DAT)

      IF (CHECK_EXIT_FLAG()) RETURN

! Time march loop.

      IF(DISCRETE_ELEMENT .AND. .NOT.DES_CONTINUUM_COUPLED) THEN

! Uncoupled discrete element simulations
         IF (DEM_SOLIDS) THEN
            STARTTIME = WALL_TIME()
            CALL DES_TIME_INIT
            DO II = 1, FACTOR
               CALL DES_TIME_STEP(II)
               IF ( EXIT_LOOP ) EXIT
               IF ( CHECK_EXIT_FLAG() ) RETURN
               CALL CHECK_PAUSE
            ENDDO
            CALL DES_TIME_END
            CALL PRINT_WALLTIME("Timestep walltime, DEM solver:", STARTTIME)
         ENDIF
         IF (PIC_SOLIDS) CALL PIC_TIME_MARCH

      ELSE

! Transient or steady state simulation
         dt_loop: DO WHILE (TIME + 0.1d0*DT < TSTOP .AND. .NOT. EXIT_FLAG)
            CALL TIME_STEP_INIT(MFIX_DAT)
            CALL CHECK_PAUSE
            STARTTIME = WALL_TIME()
            IF (CHECK_EXIT_FLAG()) RETURN
            DO
               CALL ITERATE_INIT
               DO WHILE (NIT<MAX_NIT .AND. .NOT.(CONVERGED.OR.DIVERGED))

                  NIT = NIT + 1

                  CALL DO_ITERATION(MFIX_DAT)
                  IF (CHECK_EXIT_FLAG()) RETURN
                  CALL CHECK_PAUSE
               ENDDO

               CALL POST_ITERATE
               IF (CHECK_EXIT_FLAG()) RETURN
               IF (STEADY_STATE) EXIT
               IF (.NOT.ADJUSTDT(MFIX_DAT)) EXIT
            ENDDO
            CALL PRINT_WALLTIME("Timestep walltime, fluid solver:", STARTTIME)

! Exit if DT < DT_MIN
            CALL CHECK_LOW_DT
            IF (CHECK_EXIT_FLAG()) RETURN
! Stiff Chemistry Solver.
            CALL CHEM_MASS
            IF (CHECK_EXIT_FLAG()) RETURN
! DEM time loop
            IF (DEM_SOLIDS) THEN
               STARTTIME = WALL_TIME()
               CALL DES_TIME_INIT
               DO II = 1, FACTOR
                  CALL DES_TIME_STEP(II)
                  IF ( CHECK_EXIT_FLAG() ) RETURN
                  IF ( EXIT_LOOP ) EXIT
                  CALL CHECK_PAUSE
               ENDDO
               CALL DES_TIME_END
               CALL PRINT_WALLTIME("Timestep walltime, DEM solver:", STARTTIME)
            ENDIF

            CALL TIME_STEP_END
            IF (STEADY_STATE .OR. ADJUST_PARTITION) EXIT
            IF (CHECK_EXIT_FLAG()) RETURN

         ENDDO dt_loop

      ENDIF
      IF (CHECK_EXIT_FLAG()) RETURN

      CALL FINALIZE
      IF(.NOT.ADJUST_PARTITION) EXIT
      ENDDO
    END SUBROUTINE RUN_MFIX

SUBROUTINE PRINT_WALLTIME(LABEL, STARTTIME)
   USE ERROR_MANAGER, ONLY: ERR_MSG, FLUSH_ERR_MSG
   USE MACHINE, ONLY: WALL_TIME
   USE OUTPUT, ONLY: FULL_LOG

   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN) :: LABEL
   DOUBLE PRECISION, INTENT(IN) :: STARTTIME
   DOUBLE PRECISION :: ELAPSED_WALLTIME

   IF (FULL_LOG) THEN
      ELAPSED_WALLTIME = WALL_TIME() - STARTTIME
      WRITE(ERR_MSG, "(A, f9.3, A)" ) LABEL, ELAPSED_WALLTIME, " s"
      CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE., LOG=.FALSE.)
   ENDIF
END SUBROUTINE PRINT_WALLTIME


PROGRAM MFIX

      USE compar, only: mype, pe_io
      USE funits, only: unit_dat, file_size
      USE iso_c_binding, only: c_bool, c_char
      USE parallel_mpi, only: parallel_init
      USE error_manager, only: flush_err_msg

      IMPLICIT none
      CHARACTER(LEN=1000) :: MFIX_DAT_FILENAME = "mfix.dat"
      INTEGER :: MFIX_DAT_SIZE
      INTEGER :: II
      LOGICAL :: MFIX_DAT_EXISTS

#ifdef CROW
      character(kind=c_char) :: mfix_dat_c(1000)
      character(kind=c_char) :: port_c(81)
      logical(kind=c_bool) :: init_paused = .false.
      logical(kind=c_bool) :: init_clean = .false.
      logical(kind=c_bool) :: init_daemon = .false.
      logical(kind=c_bool) :: init_server = .true.

      include 'crowmfix.inc'
#endif

      CALL PARSE_COMMAND_LINE_ARGS

      INQUIRE(FILE=MFIX_DAT_FILENAME, EXIST=MFIX_DAT_EXISTS)
      IF (.NOT. MFIX_DAT_EXISTS) THEN
         WRITE(*,1000) mfix_dat_FILENAME
         call flush_err_msg(abort=.TRUE.)
1000     FORMAT(2/,1X,70('*')/' From: mfix.f',/' Error 1000: ',    &
              'Cannot find the input data file: ',A/,'Aborting.',/1x,   &
              70('*'),2/)
      ENDIF

      MFIX_DAT_SIZE = FILE_SIZE(MFIX_DAT_FILENAME)

      ! Invoke MPI/SMP initialization and get rank info.
      CALL PARALLEL_INIT

#ifdef CROW
      if(mype == pe_io .and. init_server) then
         call crow_init(mfix_dat_c, port_c, init_paused, init_clean, init_daemon)
      endif
#endif

      CALL RUN_MFIX(MFIX_DAT_FILENAME, MFIX_DAT_SIZE)

#ifdef CROW
      if(mype == pe_io .and. init_server) then
         call crow_shutdown()
      endif
#endif

   CONTAINS

   SUBROUTINE PARSE_COMMAND_LINE_ARGS

      USE main, only: add_command_line_keyword, print_flags
      USE run, only: id_version

#ifdef CROW
      USE crow, only: f_c_string_func
#endif

      IMPLICIT NONE

      LOGICAL :: reading_mfix_dat = .false.
#ifdef CROW
      character(len=80) :: port = "8080"
      logical :: reading_port = .false.
#endif
      CHARACTER(LEN=1000) :: tmp

      DO II=1, COMMAND_ARGUMENT_COUNT()

         IF (reading_mfix_dat) THEN
            call get_command_argument(ii,mfix_dat_filename)
            reading_mfix_dat = .false.
            CYCLE

#ifdef CROW
         ELSE IF (reading_port) THEN
            call get_command_argument(ii,port)
            reading_port = .false.
            CYCLE
#endif

         ELSE
            CALL GET_COMMAND_ARGUMENT(II,tmp)
         ENDIF

         IF (tmp=='-h'.or.tmp=='--help') THEN
            CALL PRINT_USAGE
            STOP
         ELSE IF (tmp=='-v'.or.tmp=='--version') THEN
            print *, ID_VERSION
            STOP
         ELSE IF (tmp=='-p'.or.tmp=='--print-flags') THEN
            CALL PRINT_FLAGS
            STOP

#ifdef CROW
         ELSE IF (tmp=='-P'.or.tmp=='--port') THEN
            reading_port = .true.
            CYCLE
         ELSE IF (tmp=='-w'.or.tmp=='--wait') THEN
            init_paused = .true.
            CYCLE
         ELSE IF (tmp=='-s'.or.tmp=='--server') THEN
            init_server = .true.
            CYCLE
         ELSE IF (tmp=='-c'.or.tmp=='--clean') THEN
            init_clean = .true.
            CYCLE
         ELSE IF (tmp=='-d'.or.tmp=='--daemon') THEN
            init_daemon = .true.
            CYCLE
#endif
         ELSE IF (tmp=='-f'.or.tmp=='--file') THEN
            reading_mfix_dat = .true.
            CYCLE
         ELSE IF (INDEX(tmp, '-')==1 .or. INDEX(tmp, '=')==0) THEN
            ! argument does not start with - or contain =
            print *, "Unknown option: ", tmp
            CALL PRINT_USAGE
            STOP
         ELSE
            CALL ADD_COMMAND_LINE_KEYWORD(tmp)
         ENDIF
      ENDDO

#ifdef CROW
      call f_c_string_func(mfix_dat_c, mfix_dat_filename)
      call f_c_string_func(port_c, port)
#endif

   END SUBROUTINE PARSE_COMMAND_LINE_ARGS

   SUBROUTINE PRINT_USAGE
      print *, "Usage: mfix [-h,--help] [-p,--print-flags] &
         &[-f,--file <filename>] [<KEYWORD>=<VALUE> ...]"


      print *, "       -h,--help: display this help message"
      print *, "       -p,--print-flags: print flags solver was &
         &built with, such as: dmp mkl netcdf python smp"
      print *, "       -f,--file <filename>: &
         &specify filename of input file (Default: mfix.dat)"
      print *, "       <KEYWORD>=<VALUE>: specify keyword on &
         &command line, overrides values in mfix.dat/<RUN_NAME>.mfx"
#ifdef CROW
      print *, "       -c,--clean: clean output files before starting run"
      print *, "       -d,--daemon: run detached with stdout/stderr redirection"
      print *, "       -s,--server: start HTTP server"
      print *, "       -P,--port: port number for HTTP server"
#endif
      print *, "       -v,--version: print version info"
   END SUBROUTINE PRINT_USAGE

END PROGRAM MFIX

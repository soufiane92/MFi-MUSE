! -*- f90 -*-

      MODULE PAUSE

         USE EXIT, ONLY : EXIT_FLAG, CHECK_EXIT_FLAG
         USE COMPAR, ONLY : MYPE
         USE MPI_UTILITY
         USE REINIT, ONLY: REINITIALIZE
         USE ERROR_MANAGER, ONLY: REINIT_ERROR
         USE MACHINE, ONLY: WALL_TIME
         USE TIME_CPU, ONLY:  WALL_PAUSE_START, WALL_PAUSED
         USE RESET_NEW_MOD, ONLY: RESET_NEW

#ifdef __INTEL_COMPILER
         USE IFPORT  ! for getpid, remove this when tmp file is gone
#endif

         IMPLICIT NONE

         LOGICAL :: PAUSE_FLAG = .FALSE.
         LOGICAL :: REINIT_FLAG = .FALSE.
         LOGICAL :: AUTOSTART_FLAG = .FALSE.  ! automatically restart after successful reinit
         INTEGER :: IER = 0

! TODO dynamically allocate this, instead of fixed-length
      CHARACTER(100000):: REINIT_DATA

      CONTAINS
      SUBROUTINE CHECK_PAUSE
#ifdef PYMFIX
         DOUBLE PRECISION :: WALL_PAUSED_PREV

         IF (CHECK_EXIT_FLAG()) RETURN

         WALL_PAUSED_PREV = WALL_PAUSED
         IF (CHECK_PAUSE_FLAG()) THEN
            WALL_PAUSE_START = WALL_TIME()
            IF (MYPE .EQ. 0) THEN
               PRINT *, "Paused"
!           ELSE
!              PRINT *, "Paused", MYPE
            END IF

            DO WHILE (CHECK_PAUSE_FLAG() .AND. .NOT. CHECK_EXIT_FLAG())
               IF (CHECK_REINIT_FLAG()) THEN
                  CALL DO_REINIT
                  REINIT_FLAG = .FALSE.
               ELSE
                  CALL USLEEP(100000) ! 10 Hz
!                 CALL SLEEP(1) ! 1 Hz
                  WALL_PAUSED = WALL_PAUSED_PREV + WALL_TIME() - WALL_PAUSE_START
               END IF
            END DO

            IF (CHECK_REINIT_FLAG()) THEN
               CALL DO_REINIT
               REINIT_FLAG = .FALSE.
            END IF

            IF (MYPE .EQ. 0) THEN
               PRINT *, "Resuming"
!           ELSE
!              PRINT *, "Resuming", MYPE
            END IF
            WALL_PAUSED = WALL_PAUSED_PREV + WALL_TIME() - WALL_PAUSE_START

         END IF
#endif
      END SUBROUTINE CHECK_PAUSE

#ifdef PYMFIX
      LOGICAL FUNCTION CHECK_PAUSE_FLAG()
         LOGICAL :: LOCAL
         LOCAL = PAUSE_FLAG ! Assuming atomic assignment of boolean
         CALL BCAST(LOCAL)
         CHECK_PAUSE_FLAG = LOCAL
      END FUNCTION CHECK_PAUSE_FLAG


      LOGICAL FUNCTION CHECK_REINIT_FLAG()
         LOGICAL :: LOCAL
         LOCAL = REINIT_FLAG ! Assuming atomic assignment of boolean
         CALL BCAST(LOCAL)
         CHECK_REINIT_FLAG = LOCAL

         IF(CHECK_REINIT_FLAG) CALL BCAST(REINIT_DATA)  ! Really should use mutex for here and SET_REINIT_DATA

      END FUNCTION CHECK_REINIT_FLAG


      SUBROUTINE SET_REINIT_DATA(MFIX_DAT)
         CHARACTER(LEN=*) MFIX_DAT
 !f2py intent(in) MFIX_DAT
         REINIT_DATA = MFIX_DAT
      END SUBROUTINE SET_REINIT_DATA


      SUBROUTINE DO_REINIT

         IF (MYPE .EQ. 0) THEN
            PRINT *, "Reinitializing"
         END IF


         CALL BCAST(REINIT_DATA)
         CALL RESET_NEW(REINIT_DATA) ! we might be paused in the middle of a time step


         CALL REINITIALIZE(REINIT_DATA, IER)

         IF (CHECK_EXIT_FLAG() .OR. REINIT_ERROR()) THEN
            IF (MYPE .EQ. 0) THEN
               PRINT *, "Reinitialization failed"
            END IF
            EXIT_FLAG = .FALSE.  !! allow user to retry. TODO review this
         ELSE
            IF (AUTOSTART_FLAG) THEN
               AUTOSTART_FLAG = .FALSE.
               PAUSE_FLAG = .FALSE.
            END IF
         END IF

      END SUBROUTINE DO_REINIT


      subroutine usleep(useconds) bind(C)
         use iso_c_binding
         implicit none
         integer(c_int32_t), value :: useconds
      end subroutine usleep


#endif
      END MODULE PAUSE

! -*- f90 -*-
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module: ambm                                                        !
!  Purpose:                                                            !
!     IMPORTANT:  For using these arrays in a subroutine               !
!     -lock the module in the beginning of the subroutine              !
!      call lock_ambm                                                  !
!     -and unlock the module at the end of the subroutine              !
!      call unlock_ambm                                                !
! Contains the following subroutines:                                  !
!      lock_ambm, unlock_ambm                                          !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

      MODULE ambm

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE compar
      use error_manager, only: err_msg, init_err_msg, flush_err_msg, finl_err_msg
      USE funits
!-----------------------------------------------

! linear equation matrix and vector
      DOUBLE PRECISION, DIMENSION(:, :, :), ALLOCATABLE :: A_m
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE :: B_m

      LOGICAL :: ambm_locked = .false.

      CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE lock_ambm
         IMPLICIT NONE
      IF(ambm_locked) THEN
         WRITE(ERR_MSG,*) &
            'Error:  Multiple use of ambm (ambm_mod.f)'
         call flush_err_msg(abort=.true.)
      ELSE
         ambm_locked = .true.
      ENDIF
      END SUBROUTINE lock_ambm

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE unlock_ambm
      ambm_locked = .false.
      END SUBROUTINE unlock_ambm

      END MODULE ambm

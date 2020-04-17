!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: WRITE_USR0                                             !
!  Purpose: Write initial part of user-defined output                  !
!                                                                      !
!  Author:                                            Date: dd-mmm-yy  !
!  Reviewer:                                          Date: dd-mmm-yy  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE WRITE_USR0

      use compar, only: myPE, PE_IO
      IMPLICIT NONE

      IF(myPE /= PE_IO) RETURN

      CALL WRITE_TKE_HEADER('POST_TKE.dat')
      CALL WRITE_DAT_HEADER('POST_UG.dat')
      CALL WRITE_DAT_HEADER('POST_VG.dat')
      CALL WRITE_DAT_HEADER('POST_PG.dat')

      RETURN

      CONTAINS

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WRITE_TKE_HEADER(FNAME)

      use run, only: DESCRIPTION

      IMPLICIT NONE

      CHARACTER(len=*) :: FNAME

! logical used for testing is the data file already exists
      LOGICAL :: EXISTS
! file unit for heat transfer data
      INTEGER, PARAMETER :: fUNIT = 2030

      INQUIRE(FILE=FNAME,EXIST=EXISTS)
      IF (.NOT.EXISTS) THEN
         OPEN(UNIT=fUNIT,FILE=FNAME,STATUS='NEW')
         WRITE(fUNIT, 1000) trim(DESCRIPTION)
         WRITE(fUNIT, 1200)
         CLOSE(fUNIT)
      ENDIF

 1000 FORMAT('#',/'#',/'#',25x,A)


 1200 FORMAT('#',/'#','Scheme',11x,'MFIX TKE',7X,&
         'ABS Error',7x,'REL Error')

      RETURN
      END SUBROUTINE WRITE_TKE_HEADER

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WRITE_DAT_HEADER(FNAME)

      use run, only: DESCRIPTION

      IMPLICIT NONE

      CHARACTER(len=*) :: FNAME

! logical used for testing is the data file already exists
      LOGICAL :: EXISTS
! file unit for heat transfer data
      INTEGER, PARAMETER :: fUNIT = 2030

      INQUIRE(FILE=FNAME,EXIST=EXISTS)
      IF (.NOT.EXISTS) THEN
         OPEN(UNIT=fUNIT,FILE=FNAME,STATUS='NEW')
         WRITE(fUNIT, 1000) trim(DESCRIPTION)
         WRITE(fUNIT, 1200)
         CLOSE(fUNIT)
      ENDIF

 1000 FORMAT('#',/'#',/'#',25x,A)


1200  FORMAT('#',/'#','Scheme',12x,'L1 Norm',8x,'L2 Norm', &
         7x,'L-Inf Norm')

      RETURN
      END SUBROUTINE WRITE_DAT_HEADER

      END SUBROUTINE WRITE_USR0

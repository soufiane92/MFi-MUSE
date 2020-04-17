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

      CALL WRITE_DAT_HEADER('POST_VEL.dat','Gas velocity')

      RETURN

      CONTAINS

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WRITE_DAT_HEADER(FNAME, VAR)

      use run, only: DESCRIPTION

      use param1, only: UNDEFINED
      use geometry, only: IMAX, JMAX

      IMPLICIT NONE

      CHARACTER(len=*) :: FNAME
      CHARACTER(len=*) :: VAR

! logical used for testing is the data file already exists
      LOGICAL :: EXISTS
! file unit for heat transfer data
      INTEGER, PARAMETER :: fUNIT = 2030

      INQUIRE(FILE=FNAME,EXIST=EXISTS)
      IF (.NOT.EXISTS) THEN
         OPEN(UNIT=fUNIT,FILE=FNAME,STATUS='NEW')
         WRITE(fUNIT, 1000) trim(DESCRIPTION)
      ELSE
         OPEN(UNIT=fUNIT,FILE=FNAME,POSITION="APPEND",STATUS='OLD')
      ENDIF

      WRITE(fUnit,"('#')")
      WRITE(fUnit,"('#',10x,'Mesh:          ',I13)") IMAX

!      WRITE(fUNIT, 1200) VAR, VAR

 1000 FORMAT('#',/'#',/'#',25x,A)

 1200 FORMAT('#',/'#',7X,'Height',7x,A,13X,A,'_MFIX',9X,&
         '%REL DIFF',9x,'ABS DIFF')

      CLOSE(fUNIT)
      RETURN
      END SUBROUTINE WRITE_DAT_HEADER

      END SUBROUTINE WRITE_USR0

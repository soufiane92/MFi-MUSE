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

      CALL WRITE_DAT_HEADER('POST_VEL.dat','Vel')
      CALL WRITE_NRM_HEADER('POST_VEL_NORMS.dat')


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
      use geometry, only: JMAX
      use bc, only: DELP_X
      use toleranc, only: tol_resid
      use iterate, only: max_nit

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
      WRITE(fUnit,"('#',10x,'Mesh:          ',I13)") JMAX
      WRITE(fUnit,"('#',10x,'Max Nit:       ',I13)") MAX_NIT
      WRITE(fUnit,"('#',10x,'Tol Residual:  ',es13.6)") TOL_RESID
      WRITE(fUnit,"('#',10x,'Pressure Grad: ',es13.6)") DELP_X

      WRITE(fUNIT, 1200) VAR, VAR

 1000 FORMAT('#',/'#',/'#',25x,A)

 1200 FORMAT('#',/'#',7X,'Height',7x,A,13X,A,'_MFIX',9X,&
         'ABS DIFF',9x,'REL DIFF')

      CLOSE(fUNIT)
      RETURN
      END SUBROUTINE WRITE_DAT_HEADER

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WRITE_NRM_HEADER(FNAME)

      use run, only: DESCRIPTION

      use param1, only: UNDEFINED
      use geometry, only: JMAX
      use bc, only: DELP_X
      use toleranc, only: tol_resid
      use iterate, only: max_nit

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
      ENDIF

 1000 FORMAT('#',/'#',/'#',25x,A)

      CLOSE(fUNIT)
      RETURN
      END SUBROUTINE WRITE_NRM_HEADER

      END SUBROUTINE WRITE_USR0

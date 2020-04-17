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
! Gas phase species database names.
      use rxns, only: SPECIES_g
! Total number of species
      use physprop, only: NMAX

      IMPLICIT NONE

      integer :: lc1

      IF(myPE /= PE_IO) RETURN

      DO lc1=1, nmax(0)
         CALL WRITE_DAT_HEADER &
            ('POST_SPECIES_'//trim(SPECIES_G(lc1))//'.dat')
      ENDDO

      RETURN

      CONTAINS

!----------------------------------------------------------------------!
!                                                                      !
!                                                                      !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE WRITE_DAT_HEADER(FNAME)

      use run, only: DESCRIPTION

      use param1, only: UNDEFINED

      IMPLICIT NONE

      CHARACTER(len=*), INTENT(IN) :: FNAME

! logical used for testing is the data file already exists
      LOGICAL :: EXISTS
! file unit for heat transfer data
      INTEGER, PARAMETER :: fUNIT = 2030

      INQUIRE(FILE=FNAME,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
         OPEN(UNIT=fUNIT,FILE=FNAME,STATUS='NEW')
         WRITE(fUNIT, 1000) trim(DESCRIPTION)
         CLOSE(fUNIT)
      ENDIF

1000  FORMAT(2('#',/),'#',25x,A,2(/'#'),/'#', &
         'IDX         Xg         Xg_MFIX      abs Error')

      RETURN
      END SUBROUTINE WRITE_DAT_HEADER

      END SUBROUTINE WRITE_USR0

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_AXIS                                              !
!  Author: P. Nicoletti                               Date: 27-NOV-91  !
!                                                                      !
!  Purpose: check geometry data for one axis                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_AXIS(NA, DIMEN, ALENGTH, DA, AXIS, &
         AXIS_INDEX, NO_IJK, SHIFT)

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE param
      USE param1
!      USE funits

      use error_manager


      IMPLICIT NONE
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
! number of axis cells (IMAX,JMAX,KMAX)
      INTEGER, INTENT(INOUT) :: NA
! maximum number of cells along axis based on domain decomposition
      INTEGER, INTENT(IN) :: DIMEN
! axis length (XLENGTH,YLENGTH,ZLENGTH)
      DOUBLE PRECISION, INTENT(INOUT) :: ALENGTH
! flag that specifies whether variation along that axis is
! considered (passed variable for NO_I, NO_J, or NO_K)
      LOGICAL, INTENT(IN) :: NO_IJK
! shift dx, dy and dz values (true only for new and restart_1 runs)
      LOGICAL, INTENT(IN) :: SHIFT
! axis checked ('X','Y','Z')
      CHARACTER, INTENT(IN) :: AXIS
! index associated with AXIS ('I','J','K')
      CHARACTER, INTENT(IN) :: AXIS_INDEX
! cell sizes (DX,DY,DZ);
! use explicit dimension for DA
! DA should be dimensioned DA(DIMEN) rather than DA(0:DIMEN+1) to be
! able to use the logic from previous versions that assumed DA(1)
! as the first element.  An error check has been added to ensure that
! DX, DY and DZ definitions in project settings starts with the zeroth
! element; i.e. DA(1).
      DOUBLE PRECISION, INTENT(INOUT), DIMENSION(DIMEN) :: DA
!-----------------------------------------------
! Local parameters
!-----------------------------------------------
! percent error allowed in axis length checks
      DOUBLE PRECISION, PARAMETER :: PERCENT_ERROR = 1.0
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! number of items specified from NA, ALENGTH, DA
      INTEGER :: N_SPECIFIED
! loop counter
      INTEGER :: LC
! temporary storage
      DOUBLE PRECISION :: TEMP_STOR, lSUM, lERR
!-----------------------------------------------


      CALL INIT_ERR_MSG("CHECK_AXIS")

! 0) Ensure that if DA is defined then it starts with DA(1); i.e. DX(0), DY(0) or DZ(0)
      IF(.NOT.NO_IJK)THEN
        IF( DA(2) /= UNDEFINED .AND. DA(1) == UNDEFINED) THEN
          WRITE(ERR_MSG, 1001) AXIS
          CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
        ENDIF
      ENDIF

! 1) MAKE SURE AT LEAST TWO OF NA, ALENGTH, DA ARE SPECIFIED
      N_SPECIFIED = 0
      IF (NA /= UNDEFINED_I) N_SPECIFIED = N_SPECIFIED + 1
      IF (ALENGTH /= UNDEFINED) N_SPECIFIED = N_SPECIFIED + 1
      IF (DA(1) /= UNDEFINED) N_SPECIFIED = N_SPECIFIED + 1
      IF (N_SPECIFIED < 2) THEN
         print*,ALENGTH,DA,NA
         WRITE(ERR_MSG, 1101) AXIS, AXIS, AXIS, AXIS_INDEX
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

 1101 FORMAT('Error 1101: Insufficient grid information for ',A1,'-',   &
         'axis. You must',/'specify at least two of the following: ',  &
         A1,'LENGTH, D',A1,', and ',A1,'MAX.',/'Please correct the ',    &
         'project settings.')


! 2) NUMBER OF CELLS NOT SPECIFIED - calculate NA based on
!    input that was specified
      IF(NA == UNDEFINED_I) THEN
        IF(no_ijk) THEN
          na = 1
        ELSE
          IF(DA(2) == UNDEFINED) THEN
            TEMP_STOR = ALENGTH/DA(1)
            NA = NINT(TEMP_STOR)
            IF(NA - 1 > 0) DA(2:NA) = DA(1)
          ELSE
            NA = DIMEN
            DO LC = 2, DIMEN
               IF (DA(LC) == UNDEFINED) THEN
                  NA = LC - 1
                  EXIT
               ENDIF
            ENDDO
          ENDIF
        ENDIF
        GO TO 700
      ENDIF


      IF(NA>=0 .AND. NA<=DIMEN) THEN

! 3) AXIS LENGTH NOT SPECIFIED - calculate ALENGTH based on
!    input that was specified
         IF (ALENGTH == UNDEFINED) THEN
            IF(no_ijk) THEN
               ALENGTH = DA(1)
            ELSE
               IF(DA(2) == UNDEFINED) THEN
                  IF(NA - 1 > 0) DA(2:NA) = DA(1)
               ENDIF
               ALENGTH = 0.0
               IF (NA > 0) ALENGTH = SUM(DA(:NA))
            ENDIF
         ENDIF

! 4) CELL SIZE NOT SPECIFIED - calculate NON_VARIABLE DA based on
!    input that was specified
         IF(DA(1) == UNDEFINED) THEN
            TEMP_STOR = ALENGTH/DBLE(NA)
            IF(NA > 0) DA(:NA) = TEMP_STOR
         ENDIF

! 5) ALL 3 SPECIFIED
         IF(.NOT.NO_IJK) THEN
            IF(DA(2) == UNDEFINED) THEN
               IF (NA - 1 > 0) DA(2:NA) = DA(1)
            ENDIF
         ENDIF

      ENDIF

! 6) CHECK CONSISTENCY OF AXIS INPUT
  700 CONTINUE

! This must be a legacy check because the code shouldn't get here
! without exiting and DIMEN is calculated, not a hard-coded param.
      IF (NA<0 .OR. .NOT.NO_IJK .AND. NA>DIMEN-2) THEN
         WRITE(ERR_MSG, 1001) AXIS_INDEX//'MAX', trim(iVal(NA))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

      IF(ALENGTH <= 0.0) THEN
         WRITE(ERR_MSG, 1001) AXIS//'LENGTH'
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

      lSUM = 0.0
      DO LC = 1, NA
         IF (DA(LC)<=0.0 .OR. DA(LC)==UNDEFINED) THEN
            WRITE(ERR_MSG, 1201) trim(iVar(AXIS,LC))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
         lSUM = lSUM + DA(LC)
      ENDDO

 1201 FORMAT('Error 1201: D',A,' is not specified or negative. ',      &
         'Please correct',/'the project settings.')


      lERR = 100.0*ABS(lSUM - ALENGTH)/ALENGTH
      IF(lERR > PERCENT_ERROR) THEN
         WRITE(ERR_MSG,1202) AXIS, AXIS, AXIS, ALENGTH, AXIS, lSUM, &
            lERR, PERCENT_ERROR
         CALL FLUSH_ERR_MSG(FOOTER=.FALSE.)

         DO LC = 1, NA
            WRITE(ERR_MSG,"(4x,A,' = ',A)") trim(iVar('D'//AXIS,LC)),   &
               trim(iVal(DA(LC)))
            CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE.)
         ENDDO
         WRITE(ERR_MSG,"('Please correct the project settings')")
         CALL FLUSH_ERR_MSG(HEADER=.FALSE., ABORT=.TRUE.)
      ENDIF

 1202 FORMAT('Error 1202: ',A1,'LENGTH and sum(D',A1,') are not ',     &
         'consistent.',/3x,A1,'LENGTH = ',g12.5,3x,'sum(D',A1,') = ',  &
         g12.5,/3x,'ERROR   = ',g12.5,3x,'ERR TOL = ',g12.5,/'  ')

      DO LC = NA + 1, DIMEN
         IF(SHIFT .AND. DA(LC)/=UNDEFINED) THEN
            WRITE(ERR_MSG, 1205) AXIS, AXIS_INDEX
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDDO

 1205 FORMAT('Error 1205: Too many D',A1,' values specified. Only ',A1,&
         'MAX permitted.',/'Please correct the project settings.')


      CALL FINL_ERR_MSG

      RETURN


 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/      &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_AXIS



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_MFIX_DOMAIN                                       !
!  Author: Jeff Dietiker                          Date: 30-SEPT-2016   !
!                                                                      !
!  Purpose: check MFIX domain data for one direction                   !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_MFIX_DOMAIN(A_MIN, A_MAX, ALENGTH, AXIS)

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE param
      USE param1
!      USE funits

      use error_manager
      use toleranc


      IMPLICIT NONE
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
! axis min (X_MIN,Y_MIN,Z_MIN)
      DOUBLE PRECISION, INTENT(INOUT) :: A_MIN
! axis min (X_MAX,Y_MAX,Z_MAX)
      DOUBLE PRECISION, INTENT(INOUT) :: A_MAX
! axis length (XLENGTH,YLENGTH,ZLENGTH)
      DOUBLE PRECISION, INTENT(INOUT) :: ALENGTH
! axis checked ('X','Y','Z')
      CHARACTER, INTENT(IN) :: AXIS
!-----------------------------------------------
! Local parameters
!-----------------------------------------------
!-----------------------------------------------
! Local variables
!-----------------------------------------------


      CALL INIT_ERR_MSG("CHECK_MFIX_DOMAIN")

! First, if both A_MIN and A_MAX are defined, make sure A_MAX>A_MIN
      IF( A_MIN /= UNDEFINED .AND. A_MAX /= UNDEFINED) THEN
         IF(A_MIN>=A_MAX) THEN
            WRITE(ERR_MSG, 1106) AXIS, AXIS, AXIS
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

1106        FORMAT('Error 1106: Incorrect domain size information in ',A1,'-'   , &
            'direction.',/'The upper bound must be larger than the lower bound.'   , &
            /'Currently, ',A1,'_MIN >= ',A1,'_MAX. Please correct the '        , &
            'project settings.')
         ENDIF
      ENDIF
! Legacy case
! If A_MIN and A_MAX are not defined and ALENGTH is defined
! then set A_MIN to ZERO and A_MAX to ALENGHT
      IF( ALENGTH /= UNDEFINED .AND. A_MIN == UNDEFINED .AND. A_MAX == UNDEFINED) THEN
         A_MIN = ZERO
         A_MAX = ALENGTH

! If A_MIN and ALENGTH are defined and A_MAX is not defined
! then set A_MAX to A_MIN + ALENGTH
      ELSEIF( ALENGTH /= UNDEFINED .AND. A_MIN /= UNDEFINED .AND. A_MAX == UNDEFINED) THEN
         A_MAX = A_MIN + ALENGTH

! If A_MAX and ALENGTH are defined and A_MIN is not defined
! then set A_MIN to A_MAX - ALENGTH
      ELSEIF( ALENGTH /= UNDEFINED .AND. A_MIN == UNDEFINED .AND. A_MAX /= UNDEFINED) THEN
         A_MIN = A_MAX - ALENGTH

! If A_MAX and A_MIN are defined and ALENGTH is not defined
! then set ALENGTH = A_MAX - A_MIN
      ELSEIF( ALENGTH == UNDEFINED .AND. A_MIN /= UNDEFINED .AND. A_MAX /= UNDEFINED) THEN
         ALENGTH = A_MAX - A_MIN

! If A_MIN and A_MAX and ALENGHT are defined, make sure they are consistent
      ELSEIF( ALENGTH /= UNDEFINED .AND. A_MIN /= UNDEFINED .AND. A_MAX /= UNDEFINED) THEN
         IF(.NOT.COMPARE(A_MAX-A_MIN,ALENGTH)) THEN
            WRITE(ERR_MSG, 1107) AXIS, AXIS, AXIS, AXIS
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
1107        FORMAT('Error 1107: Incorrect domain size information in ',A1,'-',   &
            'direction.',/A1,'LENGTH, ',A1,'_MIN, and ',A1,'_MAX are not consistent.', &
            /'Please correct the project settings.')
         ENDIF

! Otherwise, flag as error and quit
      ELSE
         WRITE(ERR_MSG, 1108) AXIS,AXIS,AXIS,AXIS,AXIS,A_MIN,AXIS,A_MAX,AXIS,ALENGTH
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

1108     FORMAT('Error 1108: Insufficient domain size information in ',A1,'-',   &
         'direction. You must',/'specify at least two of the following: ',  &
         A1,'LENGTH, ',A1,'_MIN, and ',A1,'_MAX.', &
         /'Currently, ',A1,'_MIN   = ',F14.8, &
         /'           ',A1,'_MAX   = ',F14.8, &
         /'           ',A1,'LENGTH = ',F14.8, &
         /'Please correct the project settings.')
      ENDIF

      CALL FINL_ERR_MSG

      RETURN

      END SUBROUTINE CHECK_MFIX_DOMAIN

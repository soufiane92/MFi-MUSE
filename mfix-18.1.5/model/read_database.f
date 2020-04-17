MODULE READ_DATABASE_MOD

  USE discretelement, only: discrete_element, des_continuum_hybrid
  USE error_manager, only: err_msg, init_err_msg, flush_err_msg, finl_err_msg
  USE funits, only: unit_dat, unit_log, dmp_log, file_size
  USE param1, only: zero, undefined, small_number, undefined_c
  USE physprop, only: t_ref, icpor_h, icpor_l, tcom, ahigh, alow, thigh, tlow, hfrefor, database_read, nmax, mmax, mw_g, mw_s
  USE read_thermochemical, only: read_therm, calc_ICpoR, calc_CpoR
  USE run, only: REINITIALIZING
  USE rxns, only: species_name

  CONTAINS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: read_database(Ier)                                     C
!  Purpose: read thermochemical database                               C
!                                                                      C
!  Author: M. Syamlal                                 Date: 21-OCT-05  C
!                                                                      C
!  Modification 1: J. Musser                          Date: 02-May-11  C
!  Purpose: Provided support for DEM access to database.               C
!                                                                      C
!  Modification 2: J. Musser                          Date: 02-Oct-12  C
!  Purpose: Calls to READ_DATABASE were moved to check_gas_phase and   C
!  check_solids_common_all during input data integrity checks for the  C
!  gas and solids phases.                                              C
!  Rather than looping through all species for each phase, the model   C
!  (TFM/DEM), phase index, species index, species name, and molecular  C
!  weight are passed as dummy arguments so that only information for   C
!  referenced species (lName) is obtained.                             C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE READ_DATABASE(MFIX_DAT, lM, lN, lName, lMW)

      IMPLICIT NONE

! project filename
      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! Phase and species indices
      INTEGER, INTENT(IN) :: lM, lN
! Species name from data file.
      CHARACTER(len=*), INTENT(IN) :: lName
! Species molecular weight from the data file (if any)
      DOUBLE PRECISION, INTENT(INOUT) :: lMW

! Molecular weight read from database.
      DOUBLE PRECISION dbMW

! Error message returned from Read_Therm and sent to calling routine
      INTEGER IER

! Identifies if the species was found in any of the databases.
      LOGICAL SPECIES_FOUND

! Tcom +/- SMALL_NUMBER: This is done so that the specific heat
! polynomial can be evaluated at Tcom with the high and low
! coefficients.
      DOUBLE PRECISION :: xTc

! Various integrations of the specific heat polynomials:
      DOUBLE PRECISION :: ICpoR_TrL  ! 0.0 --> Tref using Alow
      DOUBLE PRECISION :: ICpoR_TcL  ! 0.0 --> Tcom using Alow
      DOUBLE PRECISION :: ICpoR_TcH  ! 0.0 --> Tcom using Ahigh

      LOGICAL :: testCp = .FALSE.

      ! Full path to Burcat and Ruscic database
      CHARACTER(len=142) :: THERM = 'BURCAT.THR'
      INTEGER :: THERMO_SIZE

#ifdef BURCAT_THR
      THERM = BURCAT_THR
#endif

      CALL INIT_ERR_MSG('READ_DATABASE')

! Check for thermochemical data in the mfix.dat file.
      CALL READ_A_DATABASE("mfix.dat", MFIX_DAT)

      IF(.NOT.SPECIES_FOUND) THEN
! Check for thermochemical data from the BURCAT.THR database in the local
! run directory.
         THERMO_SIZE = FILE_SIZE(THERM)
         CALL READ_THERMO_DATABASE(THERMO_SIZE)
      ENDIF

! Error message control.
!-----------------------------------------------------------------------
! Write remaining error message if needed.
      IF(.NOT.SPECIES_FOUND) THEN
         CALL FLUSH_ERR_MSG(HEADER=.FALSE.)
#ifdef PYMFIX
         WRITE(ERR_MSG,'(A,A)') 'Unable to find species: ', trim(lName)
#else
         WRITE(ERR_MSG,1010) trim(lName), trim(THERM)
#endif
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

      CALL FINL_ERR_MSG

      RETURN

! Error Flags
!-----------------------------------------------------------------------
 1010 FORMAT('Message 1010: Species "',A,'" was not matched to any ',  &
         'entry in the',/'thermochemical databases.',2/,'SUGGESTION: ',&
         'Search the database for the exact species name. The ',/      &
         'species names are case sensitive and should match the names',&
         ' in',/'BURCAT.THR exactly excluding trailing blanks and ',   &
         'tabs. Also verify',/'that the data section in the project ', &
         'file (if any) is below a line',/'that starts with THERMO ',  &
         'DATA.',2/'Database location:', /A)

      CONTAINS

! Avoid using Fortran 2003 deferred-length string by passing in size to a subroutine
      SUBROUTINE READ_THERMO_DATABASE(THERM_SIZE)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: THERM_SIZE
        CHARACTER(THERM_SIZE) :: THERM_DB

        OPEN(UNIT=UNIT_DAT, FILE=TRIM(THERM), ACTION='read', FORM="unformatted", ACCESS="stream")
        READ(UNIT_DAT) THERM_DB
        CLOSE(UNIT_DAT)

        CALL READ_A_DATABASE(THERM, THERM_DB)

      END SUBROUTINE READ_THERMO_DATABASE


      SUBROUTINE READ_A_DATABASE(THERMO_DB_LABEL, THERMO_DB)
         IMPLICIT NONE
         CHARACTER(LEN=*), INTENT(IN) :: THERMO_DB_LABEL
         CHARACTER(LEN=*), INTENT(IN) :: THERMO_DB

         CALL READ_THERM(THERMO_DB, lName, Thigh(lM,lN), Tlow(lM,lN),      &
            Tcom(lM,lN), dbMW, Ahigh(:,lM,lN), Alow(:,lM,lN),          &
            HfrefoR(lM,lN), IER)

         IF(IER == 0) THEN
! If the user did not supply a value for the gas phase molecular weight
! in the mfix.dat file, use the value from the database.
            IF(lMW == UNDEFINED) lMW = dbMW
! There are a number of species with Tlow as 300, for which the
! following calculation will produce an error because T_ref = 298.  So
! slightly extend validity of the correlation.
            IF(ABS(Tlow(lM,lN)-T_ref)<=2.0D0 .AND. &
               Tlow(lM,lN) > T_ref) Tlow(lM,lN) = T_ref

! Initialize the reference integrals.
            ICpoR_l(lM,lN) = ZERO
            ICpoR_h(lM,lN) = ZERO

! Calculate the integral of specific heat from zero to Tref using the
! Alow coefficients.
               ICpoR_TrL = calc_ICpoR(T_ref, lM, lN, IER)
! Calculate the integral of specific heat from zero to Tcom using the
! Alow coefficients.
            xTc = Tcom(lM,lN)-SMALL_NUMBER
            ICpoR_TcL = calc_ICpoR(xTc, lM, lN, IER)
! Calculate the integral of specific heat from zero to Tcom using the
! Ahigh coefficients.
            xTc = Tcom(lM,lN)+SMALL_NUMBER
            ICpoR_TcH = calc_ICpoR(xTc, lM, lN, IER)
! Store the integrals in global variables.
            ICpoR_l(lM,lN) = ICpoR_TrL
            ICpoR_h(lM,lN) = ICpoR_TcH - (ICpoR_TcL - ICpoR_TrL)
         ENDIF

         SPECIES_FOUND = (IER == 0)
         IF(.NOT.SPECIES_FOUND) THEN
            IF(.NOT.REINITIALIZING)THEN
               WRITE(ERR_MSG,1001) trim(adjustl(THERMO_DB_LABEL)), 'Not found.'
               CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE.)
            ENDIF

         ELSE
            IF(.NOT.REINITIALIZING)THEN
               WRITE(ERR_MSG,1001) "Checking ", trim(adjustl(THERMO_DB_LABEL)), 'Found!'
               CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE.)
            ENDIF

            if(testCP) CALL writeCp(lM, lN, lName, lMW)

         ENDIF

! Messages
!-----------------------------------------------------------------------
 1001 FORMAT(8X,A,1X,' :: ',A)

      END SUBROUTINE READ_A_DATABASE

      END SUBROUTINE READ_DATABASE

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: READ_DATABASE0(IER)                                    C
!  Purpose: Provides legacy support for rrates files.                  C
!                                                                      C
!  Author: J. Musser                                  Date: 02-Oct-12  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE READ_DATABASE0(MFIX_DAT)

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

! Loop indices for mass phase and species
      INTEGER M, NN
! Loop counter for continuum and discrete species
      INTEGER Nsp, DES_Nsp

! Return to the calling routine if the database has already been called.
      IF(database_read)RETURN

! Set the flag identifying that the database has been read.
      database_read = .TRUE.

! Initialize counters
      Nsp = 0
      DES_Nsp = 0

! Read species data for the gas phase.
!-----------------------------------------------------------------------
      DO NN = 1, NMAX(0)
         Nsp = Nsp + 1
! If a species name was not specified in mfix.dat, flag error and exit.
          IF(SPECIES_NAME(Nsp) == UNDEFINED_C) THEN
            WRITE(*,1010) NN         ! screen
            IF(DMP_LOG) WRITE(UNIT_LOG,1010) NN  ! log file
             call flush_err_msg(abort=.TRUE.)
          ENDIF
! Read the database.
         CALL READ_DATABASE(MFIX_DAT, 0, NN, SPECIES_NAME(Nsp), MW_g(NN))
       ENDDO

! Read species data for the continuum solids phases.
!-----------------------------------------------------------------------
! Skip reading the database for the continuum solids phase if the
! simulation is only employing discrete solids.
      IF(.NOT.DISCRETE_ELEMENT .OR. DES_CONTINUUM_HYBRID)THEN
          DO M = 1, MMAX
            DO NN = 1, NMAX(M)
                Nsp = Nsp + 1
! If a species name was not specified in mfix.dat, flag error and exit.
                IF(SPECIES_NAME(Nsp) == UNDEFINED_C)THEN
                  WRITE(*,1011)'continuum', M, NN ! screen
                  IF(DMP_LOG) WRITE(UNIT_LOG,1011)'continuum', M, NN
                   call flush_err_msg(abort=.TRUE.)
                ENDIF
               CALL READ_DATABASE(MFIX_DAT, M, NN, SPECIES_NAME(Nsp), MW_s(M,NN))
             ENDDO   ! N=1, NMAX(M)
          ENDDO   ! M=1, MMAX
      ENDIF

      RETURN

! Error Messages
!-----------------------------------------------------------------------
 1010 FORMAT(/1X,70('*')/, ' From: READ_DATABASE0',/, ' Message: ',    &
         'No SPECIES_NAME provided for gas phase species ',I3,'.',/' ',&
         'Check mfix.dat.',/1X,70('*')/)
 1011 FORMAT(/1X,70('*')/, ' From: READ_DATABASE0',/, ' Message: ',    &
         'No SPECIES_NAME provided for ',A,' solids phase ',I2,', ',/  &
         ' species ',I3,'.',/' Check mfix.dat.',/1X,70('*')/)

      END SUBROUTINE READ_DATABASE0


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: read_database(Ier)                                     C
!  Purpose: read thermochemical database                               C
!                                                                      C
!  Author: M. Syamlal                                 Date: 21-OCT-05  C
!                                                                      C
!  Modification 1: J. Musser                          Date: 02-May-11  C
!  Purpose: Provided support for DEM access to database.               C
!                                                                      C
!  Modification 2: J. Musser                          Date: 02-Oct-12  C
!  Purpose: Calls to READ_DATABASE were moved to CHECK_DATA_04/05      C
!  duing input data integrity checks for the gas and solids phases.    C
!  Rather than looping through all species for each phase, the model   C
!  (TFM/DEM), phase index, species index, species name, and molecular  C
!  weight are passed as dummy arguments so that only infomration for   C
!  referenced species (lName) is obtained.                             C                             C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE writeCp(lM, lN, lName, lMW)

! Universal gas constant in cal/mol.K
      use constant, only: RGAS => GAS_CONST_cal

      IMPLICIT NONE

! Phase and species indices
      INTEGER, INTENT(IN) :: lM, lN
! Species name from data file.
      CHARACTER(len=*), INTENT(IN) :: lName
! Species molecular weight from the data file (if any)
      DOUBLE PRECISION, INTENT(in) :: lMW

      INTEGER :: IER1, IER2, lc

      DOUBLE PRECISION :: T
      DOUBLE PRECISION :: lCP, lICP

      write(*,"(2/,3x,'Specific Heat report for ',A)")trim(lName)

      write(*,"(/12x,'Low',9x,'High')")
      write(*,"(6x,'T',3x,g12.5,2x,g12.5)") Tlow(lM,lN), Thigh(lM,lN)
      DO lc=1,5
         write(*,"(4x,'A(',I1,')',2(2x,g12.5))") lc, &
            Alow(lc,lM,lN), Ahigh(lc,lM,lN)
      ENDDO
      write(*,"('')")
      write(*,"(5x,'Tcom: ',g12.5)")Tcom(lM,lN)
      write(*,"('')")

      write(*,"(5x,'Temperature',8x,'Cp',11x,'ICp')")

      T = Tcom(lM,lN) - 100.0
      DO WHILE(T <= Tcom(lM,lN) - SMALL_NUMBER)

         IER1 = 0
         IER2 = 0

         write(*,"(7x,g12.5)",ADVANCE="NO") T
         lCP  = calc_CpoR(T, lM, lN) * RGAS / lMW
         lICP = calc_ICpoR(T, lM, lN, IER2) * RGAS / lMW
         write(*,"(2(3x,g12.5))",ADVANCE="NO")lCP, lICP

         IF(IER1 /= 0) write(*,"(3x,'Cp Error!')",ADVANCE="NO")
         IF(IER2 /= 0) write(*,"(3x,'ICp Error!')",ADVANCE="NO")
         write(*,"('')")

         T = T + 5.0
      ENDDO


      T = Tcom(lM,lN) + SMALL_NUMBER
      DO WHILE(T <= Tcom(lM,lN) + 100.0)

         IER1 = 0
         IER2 = 0

         write(*,"(7x,g12.5)",ADVANCE="NO") T
         lCP  = calc_CpoR(T, lM, lN) * RGAS / lMW
         lICP = calc_ICpoR(T, lM, lN, IER2) * RGAS / lMW
         write(*,"(2(3x,g12.5))",ADVANCE="NO")lCP, lICP

         IF(IER1 /= 0) write(*,"(3x,'Cp Error!')",ADVANCE="NO")
         IF(IER2 /= 0) write(*,"(3x,'ICp Error!')",ADVANCE="NO")
         write(*,"('')")

         T = T + 5.0
      ENDDO

      write(*,"('')")
      END SUBROUTINE writeCp

END MODULE READ_DATABASE_MOD

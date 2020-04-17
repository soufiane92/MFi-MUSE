#include "version.inc"

MODULE output_man

   use get_smass_mod, only: get_smass, get_dem_smass, get_tfm_smass

CONTAINS
!----------------------------------------------------------------------!
!                                                                      !
!  Subroutine: OUTPUT_MANAGER                                          !
!  Author: J.Musser                                   Date:            !
!                                                                      !
!  Purpose: Relocate calls to write output files (RES, SPx, VTP). This !
!  was done to simplify the time_march code.                           !
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE OUTPUT_MANAGER(EXIT_FLAG, FINISHED)

! Global Variables:
!---------------------------------------------------------------------//

      use compar, only: myPE, PE_IO
      use discretelement, only: DISCRETE_ELEMENT,DLB_DT
      use machine, only: wall_time
      use output, only: DISK, DISK_TOT
      use output, only: DLB,DLB_TIME
      use output, only: OUT_TIME, OUT_DT
      use output, only: REPORT_SOLID_INVENTORY
      use output, only: REPORT_SOLID_INVENTORY_DT
      use output, only: RES_BACKUP_TIME, RES_BACKUP_DT
      use output, only: RES_TIME, RES_DT
      use output, only: SOLID_INVENTORY_TIME
      use output, only: SPX_TIME, SPX_DT
      use output, only: USR_TIME, USR_DT
      use param, only: DIMENSION_USR
      use param1, only: N_SPX
      use param1, only: UNDEFINED
      use qmom_kinetic_equation, only: QMOMK
      use run, only: TIME, DT, TSTOP, STEADY_STATE
      use time_cpu, only: CPU_IO
      use vtk, only:    VTK_TIME, VTK_DT
      use vtk, only: DIMENSION_VTK
      use vtk, only: WRITE_VTK_FILES
      use vtp, only: write_vtp_file
      use write_res1_mod, only: write_res1

      use monitor, only: dimension_monitor, monitor_defined
      use monitor, only: monitor_dt, monitor_time
      use write_monitor_mod, only: write_monitor

      IMPLICIT NONE

! Dummy Arguments:
!---------------------------------------------------------------------//
! Flag that the the user specified batch time (plus buffer) is met.
      LOGICAL, INTENT(IN) :: EXIT_FLAG
! Flag that a steady state case is completed.
      LOGICAL, INTENT(IN) :: FINISHED

! Local Variables:
!---------------------------------------------------------------------//
! Loop counter and counter
      INTEGER :: LC, IDX
! Flag to write NetCDF output
      LOGICAL :: bWRITE_NETCDF_FILES
! Flag that the header (time) has not be written.
      LOGICAL :: HDR_MSG
! SPX file extensions.
      CHARACTER(LEN=35) ::  EXT_END
! Wall time at the start of IO operations.
      DOUBLE PRECISION :: WALL_START

!......................................................................!

! Initialize the SPx file extension array.
      EXT_END = '123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
! Initial the header flag.
      HDR_MSG = .TRUE.

! Get the current time before any IO operations begin
      WALL_START = WALL_TIME()

! Create a backup copy of the RES file.
      IF(TIME+0.1d0*DT>=RES_BACKUP_TIME) THEN
         RES_BACKUP_TIME = NEXT_TIME(RES_BACKUP_DT)
         CALL BACKUP_RES
      ENDIF

! Write SPx files, if needed
      IDX = 0
      bWRITE_NETCDF_FILES = .FALSE.

      DO LC=1, N_SPX
         IF(CHECK_TIME(SPX_TIME(LC)).AND.SPX_DT(LC)<=TSTOP) THEN
            SPX_TIME(LC) = NEXT_TIME(SPX_DT(LC))

            CALL WRITE_SPX1(LC, 0)
            CALL NOTIFY_USER('SPx:',EXT_END(LC:LC))

            DISK_TOT = DISK_TOT + DISK(LC)
            IDX = IDX + 1

            bWRITE_NETCDF_FILES = .TRUE.
         ENDIF
      ENDDO
      IF(IDX /=0) CALL FLUSH_LIST

! Write standard output, if needed
      IF(CHECK_TIME(OUT_TIME)) THEN
         OUT_TIME = NEXT_TIME(OUT_DT)
         CALL WRITE_OUT1
         CALL NOTIFY_USER('.OUT;')
      ENDIF

! Write special output, if needed
      IDX = 0
      DO LC = 1, DIMENSION_USR
         IF(CHECK_TIME(USR_TIME(LC))) THEN
            USR_TIME(LC) = NEXT_TIME(USR_DT(LC))
            CALL WRITE_USR1 (LC)
            CALL NOTIFY_USER('.USR:',EXT_END(LC:LC))
            IDX = IDX + 1
         ENDIF
      ENDDO
      IF(IDX /=0) CALL FLUSH_LIST

! Write restart file, if needed
      IF(CHECK_TIME(RES_TIME) .OR. EXIT_FLAG) THEN

         RES_TIME = NEXT_TIME(RES_DT)
         CALL WRITE_RES1
         CALL NOTIFY_USER('.RES;')

         IF(DISCRETE_ELEMENT) THEN
            CALL WRITE_RES0_DES
            CALL NOTIFY_USER('DES.RES;')
         ENDIF

         IF(QMOMK) THEN
            CALL QMOMK_WRITE_RESTART
            CALL NOTIFY_USER('QMOMK.RES;')
         ENDIF

      ENDIF

      CALL FLUSH_NOTIFY_USER

      IF(DLB) THEN
         IF(CHECK_TIME(DLB_TIME)) THEN
            DLB_TIME = NEXT_TIME(DLB_DT)
            CALL DISPLAY_PARTICLE_LOAD
         ENDIF
      ENDIF
      DLB = .FALSE.


! Write vtk file, if needed
! Only regular (not debug) files are written (second argument is zero)
      IF(WRITE_VTK_FILES) THEN
         DO LC = 1, DIMENSION_VTK
            IF(CHECK_TIME(VTK_TIME(LC))) THEN
               VTK_TIME(LC) = NEXT_TIME(VTK_DT(LC))
               CALL WRITE_VTU_FILE(LC,0)
               IF(DISCRETE_ELEMENT) CALL WRITE_VTP_FILE(LC,0)
            ENDIF
         ENDDO
      ENDIF

! Write NetCDF files.
      IF(bWRITE_NETCDF_FILES) CALL WRITE_NETCDF(0,0,TIME)

      IF(any(MONITOR_DEFINED)) THEN
         DO LC = 1, DIMENSION_MONITOR
            IF(CHECK_TIME(MONITOR_TIME(LC))) THEN
               MONITOR_TIME(LC) = NEXT_TIME(MONITOR_DT(LC))
               CALL WRITE_MONITOR(LC)
            ENDIF
         ENDDO
      ENDIF

! Write Solids inventory, if needed
      IF(REPORT_SOLID_INVENTORY.AND.(CHECK_TIME(SOLID_INVENTORY_TIME) .OR. EXIT_FLAG)) THEN

         SOLID_INVENTORY_TIME = NEXT_TIME(REPORT_SOLID_INVENTORY_DT)

         IF(DISCRETE_ELEMENT) THEN
            CALL GET_DEM_SMASS
         ELSE
            CALL GET_TFM_SMASS
         ENDIF
      ENDIF

! Add the amount of time needed for all IO operations to total.
      CPU_IO = CPU_IO + (WALL_TIME() - WALL_START)

      RETURN

      contains

!----------------------------------------------------------------------!
!                                                                      !
!----------------------------------------------------------------------!
      LOGICAL FUNCTION CHECK_TIME(lTIME)
      USE RUN, ONLY: INTERACTIVE

      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN) :: lTIME

      IF(STEADY_STATE) THEN
         CHECK_TIME = FINISHED
      ELSE
         CHECK_TIME = (TIME+0.1d0*DT>=TSTOP .AND. .NOT.INTERACTIVE  ) &
                  .OR. TIME+0.1d0*DT>=lTIME
      ENDIF

      RETURN
      END FUNCTION CHECK_TIME

!----------------------------------------------------------------------!
!                                                                      !
!----------------------------------------------------------------------!
      DOUBLE PRECISION FUNCTION NEXT_TIME(lWRITE_DT)

      DOUBLE PRECISION, INTENT(IN) :: lWRITE_DT

      IF (.NOT.STEADY_STATE) THEN
         NEXT_TIME = (INT((TIME + 0.1d0*DT)/lWRITE_DT)+1)*lWRITE_DT
      ELSE
         NEXT_TIME = lWRITE_DT
      ENDIF

      RETURN
      END FUNCTION NEXT_TIME

!----------------------------------------------------------------------!
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE NOTIFY_USER(MSG, EXT)

      use output, only: FULL_LOG
      use funits, only: DMP_LOG
      use funits, only: UNIT_LOG

      CHARACTER(len=*), INTENT(IN) :: MSG
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EXT


      LOGICAL :: SCR_LOG

      SCR_LOG = (FULL_LOG .and. myPE.eq.PE_IO)

      IF(HDR_MSG) THEN
         IF(DMP_LOG) WRITE(UNIT_LOG, 1000, ADVANCE='NO') TIME
         IF(SCR_LOG) WRITE(*, 1000, ADVANCE='NO') TIME
         HDR_MSG = .FALSE.
      ENDIF

 1000 FORMAT(' ',/' t=',F12.6,' Wrote')

      IF(.NOT.present(EXT)) THEN
         IF(DMP_LOG) WRITE(UNIT_LOG, 1100, ADVANCE='NO') MSG
         IF(SCR_LOG) WRITE(*, 1100, ADVANCE='NO') MSG
      ELSE
         IF(IDX == 0) THEN
            IF(DMP_LOG) WRITE(UNIT_LOG, 1110, ADVANCE='NO') MSG, EXT
            IF(SCR_LOG) WRITE(*, 1110, ADVANCE='NO') MSG, EXT
         ELSE
            IF(DMP_LOG) WRITE(UNIT_LOG, 1120, ADVANCE='NO') EXT
            IF(SCR_LOG) WRITE(*, 1120, ADVANCE='NO') EXT
         ENDIF
      ENDIF

 1100 FORMAT(1X,A)
 1110 FORMAT(1X,A,1x,A)
 1120 FORMAT(',',A)

      RETURN
      END SUBROUTINE NOTIFY_USER

!----------------------------------------------------------------------!
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE FLUSH_LIST

      use output, only: FULL_LOG
      use funits, only: DMP_LOG
      use funits, only: UNIT_LOG

      LOGICAL :: SCR_LOG

      SCR_LOG = (FULL_LOG .and. myPE.eq.PE_IO)

      IF(DMP_LOG) WRITE(UNIT_LOG,1000, ADVANCE='NO')
      IF(SCR_LOG) WRITE(*,1000, ADVANCE='NO')

 1000 FORMAT(';')

      RETURN
      END SUBROUTINE FLUSH_LIST


!----------------------------------------------------------------------!
!                                                                      !
!----------------------------------------------------------------------!
      SUBROUTINE FLUSH_NOTIFY_USER

#ifndef PYMFIX
      use discretelement, only: DISCRETE_ELEMENT, DES_CONTINUUM_COUPLED
      use discretelement, only: DTSOLID
      use error_manager
      use funits, only: DMP_LOG
      use funits, only: UNIT_LOG
      use machine, only: wall_time
      use run, only: get_tunit
      use output, only: FULL_LOG
      use output, only: NLOG
      use run, only: TIME, NSTEP, STEADY_STATE
      use time_cpu, only: TIME_START
      use time_cpu, only: WALL_START

      DOUBLE PRECISION :: WALL_ELAP, WALL_LEFT, WALL_NOW
      CHARACTER(LEN=9) :: CHAR_ELAP, CHAR_LEFT
      CHARACTER(LEN=4) :: UNIT_ELAP, UNIT_LEFT

      INTEGER :: TNITS
      LOGICAL :: SCR_LOG

      SCR_LOG = (FULL_LOG .and. myPE.eq.PE_IO)

      IF(.NOT.HDR_MSG) THEN
         IF(DMP_LOG) WRITE(UNIT_LOG,1000)
         IF(SCR_LOG) WRITE(*,1000)
      ENDIF

 1000 FORMAT(' ',/' ')

! Write the elapsed time and estimated remaining time
      IF(MOD(NSTEP,NLOG) == 0) THEN

         IF(DISCRETE_ELEMENT .AND. .NOT.DES_CONTINUUM_COUPLED) THEN
            TNITs = CEILING(real((TSTOP-TIME)/DTSOLID))
            WRITE(ERR_MSG, 1100) TIME, DTSOLID, trim(iVal(TNITs))
            CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE., LOG=.FALSE.)
         ENDIF


 1100 FORMAT(/'Time: ',g12.5,3x,'DT: ',g12.5,3x,'Remaining DEM NITs: ',A)

         WALL_NOW = WALL_TIME()
! Calculate the elapsed wall time.
         WALL_ELAP = WALL_NOW - WALL_START
         CALL GET_TUNIT(WALL_ELAP, UNIT_ELAP)
         CHAR_ELAP=''; WRITE(CHAR_ELAP,"(F9.2)") WALL_ELAP
         CHAR_ELAP = trim(adjustl(CHAR_ELAP))
! Estimate the remaining wall time.
         WALL_LEFT = (WALL_NOW-WALL_START)*(TSTOP-TIME)/               &
            max(TIME-TIME_START,1.0d-6)
         CALL GET_TUNIT(WALL_LEFT, UNIT_LEFT)

         IF (.NOT.STEADY_STATE) THEN
            CHAR_LEFT=''; WRITE(CHAR_LEFT,"(F9.2)") WALL_LEFT
            CHAR_LEFT = trim(adjustl(CHAR_LEFT))
         ELSE
            CHAR_LEFT = '0.0'
            UNIT_LEFT = 's'
         ENDIF

! Notify the user of usage/remaining wall times.
         WRITE(ERR_MSG,2000)                                           &
            'Elapsed:', trim(CHAR_ELAP), trim(UNIT_ELAP),              &
            'Est. Remaining:',trim(CHAR_LEFT), trim(UNIT_LEFT)
         CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE.)
      ENDIF

 2000 FORMAT('Wall Time - ',2(A,1X,A,A,4X))
#endif
      RETURN
      END SUBROUTINE FLUSH_NOTIFY_USER

      END SUBROUTINE OUTPUT_MANAGER

!----------------------------------------------------------------------!
! Subroutine: INIT_OUTPUT_VARS                                         !
! Purpose: Initialize variables used for controling ouputs of the      !
! various files.                                                       !
!----------------------------------------------------------------------!
      SUBROUTINE INIT_OUTPUT_VARS

      use geometry, only: IJKMAX2
      use machine, only: wall_time
      use output, only: DISK, DISK_TOT
      use output, only: ONEMEG
      use output, only: OUT_TIME, OUT_DT
      use output, only: RES_TIME, RES_DT
      use output, only: SPX_TIME, SPX_DT
      use output, only: USR_TIME, USR_DT
      use output, only: RES_BACKUP_TIME, RES_BACKUP_DT
      use output, only: RES_BACKUPS
      use output, only: DLB_TIME
      use output, only: SOLID_INVENTORY_TIME
      use output, only: REPORT_SOLID_INVENTORY_DT
      use param, only: DIMENSION_USR
      use param1, only: N_SPX
      use param1, only: UNDEFINED
      use param1, only: ZERO
      use physprop, only: MMAX, NMAX
      use run, only: RUN_TYPE
      use run, only: TIME, DT, STEADY_STATE
      use rxns, only: nRR
      use scalars, only: NScalar
      use time_cpu, only: CPU_IO
      use time_cpu, only: TIME_START
      use time_cpu, only: WALL_START
      use turb, only: K_EPSILON
      use vtk, only:    VTK_TIME, VTK_DT
      use vtk, only: DIMENSION_VTK
      use vtk, only: DIMENSION_VTK
      use vtk, only: VTK_TIME, VTK_DT
      use vtk, only: WRITE_VTK_FILES
      use monitor, only: DIMENSION_MONITOR, MONITOR_TIME, MONITOR_DT

      use param1, only:  UNDEFINED_I

      use funits, only: CREATE_DIR
      use discretelement, only: DLB_DT
      use compar, only:ADJUST_PARTITION

      IMPLICIT NONE

! Disk space needed for one variable and each SPX file
      DOUBLE PRECISION :: DISK_ONE

! Loop counter
      INTEGER :: LC

! Initialize times for writing outputs
      OUT_TIME = merge(TIME, UNDEFINED, OUT_DT /= UNDEFINED)

! Initialize the amount of time spent on IO
      CPU_IO = 0.0d0

! Initialize disk space calculations
      DISK_TOT = ZERO
      DISK_ONE = 4.0*IJKMAX2/ONEMEG

      DISK(1) = 1.0*DISK_ONE                           ! EPg
      DISK(2) = 2.0*DISK_ONE                           ! Pg, Ps
      DISK(3) = 3.0*DISK_ONE                           ! Ug, Vg, Wg
      DISK(4) = 3.0*DISK_ONE*MMAX                      ! Us, Vs, Ws
      DISK(5) = 1.0*DISK_ONE*MMAX                      ! ROPs
      DISK(6) = 1.0*DISK_ONE*(MMAX+1)                  ! Tg, Ts
      DISK(7) = 1.0*DISK_ONE*(sum(NMAX(0:MMAX)))       ! Xg, Xs
      DISK(8) = 1.0*DISK_ONE*MMAX                      ! Theta
      DISK(9) = 1.0*DISK_ONE*NScalar                   ! User Scalars
      DISK(10) = nRR*DISK_ONE                          ! ReactionRates
      DISK(11) = merge(2.0*DISK_ONE, ZERO, K_EPSILON)  ! K-Epsilon


! Initizle RES and SPX_TIME
      IF (RUN_TYPE == 'NEW') THEN
         RES_TIME = TIME
         SPX_TIME(:N_SPX) = TIME
      ELSE
         IF (.NOT. STEADY_STATE) THEN
            RES_TIME = RES_DT *                                        &
               (INT((TIME + 0.1d0*DT)/RES_DT) + 1)
            SPX_TIME(:N_SPX) = SPX_DT(:N_SPX) *                        &
               (INT((TIME + 0.1d0*DT)/SPX_DT(:N_SPX)) + 1)
         ENDIF
      ENDIF

! Initialize RES_BACKUP_TIME
      RES_BACKUP_TIME = UNDEFINED
      IF(RES_BACKUP_DT /= UNDEFINED) RES_BACKUP_TIME =                 &
         RES_BACKUP_DT * (INT((TIME+0.1d0*DT)/RES_BACKUP_DT)+1)

! Initialize USR_TIME
      DO LC = 1, DIMENSION_USR
         USR_TIME(LC) = UNDEFINED
         IF (USR_DT(LC) /= UNDEFINED) THEN
            IF (RUN_TYPE == 'NEW') THEN
               USR_TIME(LC) = TIME
            ELSE
               USR_TIME(LC) = USR_DT(LC) *                             &
                  (INT((TIME+0.1d0*DT)/USR_DT(LC))+1)
            ENDIF
         ENDIF
      ENDDO

! Initialize DLB_TIME
      IF (RUN_TYPE == 'NEW'.OR.RUN_TYPE=='RESTART_2') THEN
         DLB_TIME = TIME
      ELSE
         DLB_TIME = (INT((TIME + 0.1d0*DT)/DLB_DT)+1)*DLB_DT
         ADJUST_PARTITION = .FALSE.
      ENDIF

! Initialize VTK_TIME
      IF(WRITE_VTK_FILES) THEN
         DO LC = 1, DIMENSION_VTK
            VTK_TIME(LC) = UNDEFINED
            IF (VTK_DT(LC) /= UNDEFINED) THEN
               IF (RUN_TYPE == 'NEW'.OR.RUN_TYPE=='RESTART_2') THEN
                  VTK_TIME(LC) = TIME
               ELSE
                  VTK_TIME(LC) = VTK_DT(LC) *                          &
                     (INT((TIME + 0.1d0*DT)/VTK_DT(LC))+1)
               ENDIF
            ENDIF
         ENDDO
      ENDIF

! Initialize MONITOR_TIME
      DO LC = 1, DIMENSION_MONITOR
         MONITOR_TIME(LC) = UNDEFINED
         IF (MONITOR_DT(LC) /= UNDEFINED) THEN
            IF (RUN_TYPE == 'NEW'.OR.RUN_TYPE=='RESTART_2') THEN
               MONITOR_TIME(LC) = TIME
            ELSE
               MONITOR_TIME(LC) = MONITOR_DT(LC) *                     &
               (INT((TIME + 0.1d0*DT)/MONITOR_DT(LC))+1)
            ENDIF
         ENDIF
      ENDDO

! Initialize SOLID INVENTORY TIME
      IF (RUN_TYPE == 'NEW') THEN
         SOLID_INVENTORY_TIME = TIME
      ELSE
         SOLID_INVENTORY_TIME = (INT((TIME + 0.1d0*DT)/REPORT_SOLID_INVENTORY_DT)+1)&
                                *REPORT_SOLID_INVENTORY_DT
      ENDIF

! Create a subdir for RES backup files.
      IF(RES_BACKUPS /= UNDEFINED_I) CALL CREATE_DIR('BACKUP_RES')

      WALL_START = WALL_TIME()
      TIME_START = TIME

      RETURN
      END SUBROUTINE INIT_OUTPUT_VARS

!----------------------------------------------------------------------!
! Subroutine: BACKUP_RES                                               !
! Purpose: Shift existing RES file backup files by one index, then     !
! create a copy of the current RES file.                               !
!----------------------------------------------------------------------!
      SUBROUTINE BACKUP_RES

      use compar, only: myPE, PE_IO
      use output, only: RES_BACKUPS
      use discretelement, only: DISCRETE_ELEMENT
      use param1, only: UNDEFINED_I

      IMPLICIT NONE

      CHARACTER(len=256) :: FNAME0, FNAME1

      INTEGER :: LC

      IF(myPE /= PE_IO) RETURN
      IF(RES_BACKUPS == UNDEFINED_I) RETURN

! Shift all the existing backups by one.
      DO LC=RES_BACKUPS,2,-1
         CALL SET_FNAME(FNAME0,'.RES', LC-1)
         CALL SET_FNAME(FNAME1,'.RES', LC)
         CALL SHIFT_RES(FNAME0, FNAME1, 'mv')

         IF(DISCRETE_ELEMENT) THEN
            CALL SET_FNAME(FNAME0,'_DES.RES', LC-1)
            CALL SET_FNAME(FNAME1,'_DES.RES', LC)
            CALL SHIFT_RES(FNAME0, FNAME1, 'mv')
         ENDIF
      ENDDO

! Copy RES to RES1
      CALL SET_FNAME(FNAME0, '.RES')
      CALL SET_FNAME(FNAME1, '.RES' ,1)
      CALL SHIFT_RES(FNAME0, FNAME1, 'cp')

      IF(DISCRETE_ELEMENT) THEN
         CALL SET_FNAME(FNAME0, '_DES.RES')
         CALL SET_FNAME(FNAME1, '_DES.RES' ,1)
         CALL SHIFT_RES(FNAME0, FNAME1, 'cp')
      ENDIF

      RETURN

      contains

!----------------------------------------------------------------------!
! Subroutine: SHIFT_RES                                                !
! Purpose: Shift RES(LC-1) to RES(LC)                                  !
!----------------------------------------------------------------------!
      SUBROUTINE SHIFT_RES(pFN0, pFN1, ACT)

      implicit none

      CHARACTER(LEN=*), INTENT(IN) :: pFN0, pFN1, ACT
      CHARACTER(len=1024) :: CMD
      LOGICAL :: EXISTS

      INQUIRE(FILE=trim(pFN0),EXIST=EXISTS)
      IF(EXISTS) THEN
         CMD=''; WRITE(CMD,1000)trim(ACT), trim(pFN0),trim(pFN1)
         CALL EXECUTE_COMMAND_LINE(trim(CMD))
      ENDIF

 1000 FORMAT(A,1x,A,1X,A)

      RETURN
      END SUBROUTINE SHIFT_RES

!----------------------------------------------------------------------!
! Subroutine: SET_FNAME                                                !
! Purpose: Set the backup RES file name based on pINDX.                !
!----------------------------------------------------------------------!
      SUBROUTINE SET_FNAME(pFNAME, pEXT, pINDX)

      use run, only: RUN_NAME

      implicit none

      CHARACTER(LEN=*), INTENT(OUT) :: pFNAME
      CHARACTER(LEN=*), INTENT(IN) ::  pEXT
      INTEGER, INTENT(IN), OPTIONAL :: pINDX

! Set the file format for backup copies
      pFNAME=''
      IF(.NOT.PRESENT(pINDX)) THEN
         WRITE(pFNAME,1000) trim(RUN_NAME),pEXT
      ELSE
         IF(RES_BACKUPS < 10) THEN
            WRITE(pFNAME,1001) trim(RUN_NAME), pEXT, pINDX
         ELSEIF(RES_BACKUPS < 100) THEN
            WRITE(pFNAME,1002) trim(RUN_NAME), pEXT, pINDX
         ELSEIF(RES_BACKUPS < 1000) THEN
            WRITE(pFNAME,1003) trim(RUN_NAME), pEXT, pINDX
         ELSEIF(RES_BACKUPS < 10000) THEN
            WRITE(pFNAME,1004) trim(RUN_NAME), pEXT, pINDX
         ELSEIF(RES_BACKUPS < 10000) THEN
            WRITE(pFNAME,1005) trim(RUN_NAME), pEXT, pINDX
         ELSE
            WRITE(pFNAME,1006) trim(RUN_NAME), pEXT, pINDX
         ENDIF
      ENDIF

 1000 FORMAT(2A)
 1001 FORMAT('BACKUP_RES/',2A,I1.1)
 1002 FORMAT('BACKUP_RES/',2A,I2.2)
 1003 FORMAT('BACKUP_RES/',2A,I3.3)
 1004 FORMAT('BACKUP_RES/',2A,I4.4)
 1005 FORMAT('BACKUP_RES/',2A,I5.5)
 1006 FORMAT('BACKUP_RES/',2A,I6.6)

      RETURN
      END SUBROUTINE SET_FNAME

      END SUBROUTINE BACKUP_RES


!----------------------------------------------------------------------!
! Subroutine: DISPLAY_PARTICLE_LOAD                                    !
! Purpose: Display the particle load balance (based on particle count) !
!----------------------------------------------------------------------!
      SUBROUTINE DISPLAY_PARTICLE_LOAD

      use discretelement
      use error_manager
      use functions
      use machine
      use mpi_utility
      use run, only: TIME
      use sendrecv
      use compar, only:MyPE,ADJUST_PARTITION
      use usr
      USE param1, only: UNDEFINED

      implicit none
      INTEGER :: IERR,L,ACTIVE_PEs,INACTIVE_PEs
      INTEGER :: NP=0  ! Number of particles
      INTEGER, DIMENSION(:), ALLOCATABLE :: NP_ALL
      INTEGER :: MIN_NP, MIN_NPP, MAX_NP, MAX_NPP, IDEAL_NP
      DOUBLE PRECISION :: MIN_LOAD, MAX_LOAD



      IF(NumPEs==1) RETURN  ! Nothing to do in serial

      IF(DLB_DT==UNDEFINED) RETURN ! Nothing to do if DLB_DT is not defined

      CALL PARTICLES_IN_CELL

      ALLOCATE( NP_ALL(0:NumPEs-1))
! Get load statistics based on particle count, excluding ghost particles
      NP = PIP - IGHOST_CNT
      CALL ALLGATHER_1I (NP,NP_ALL,IERR)
      CALL GLOBAL_ALL_SUM(NP)
      MIN_NP   = MINVAL(NP_ALL)
      MIN_NPP  = MINLOC(NP_ALL,1)-1
      MAX_NP   = MAXVAL(NP_ALL)
      MAX_NPP  = MAXLOC(NP_ALL,1)-1
      IDEAL_NP = INT(NP/NumPEs)
      IF(IDEAL_NP<1) RETURN    ! Nothing to do if the number of particles is less than NumPes
      MIN_LOAD = dble(MIN_NP)/dble(IDEAL_NP)
      MAX_LOAD = dble(MAX_NP)/dble(IDEAL_NP)
      CURRENT_MAX_LOAD = MAX_LOAD
      WRITE(ERR_MSG, 1000) trim(iVAL(NP)), &
                           trim(iVal(MIN_NP)), trim(iVal(MIN_NPP)), &
                           trim(iVal(MAX_NP)), trim(iVal(MAX_NPP)), &
                           trim(ival(IDEAL_NP))
      CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE., LOG=.FALSE.)
      WRITE(ERR_MSG, 1100) TIME,MIN_LOAD,MAX_LOAD
      CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE., LOG=.FALSE.)
      IF(MyPE==0) THEN
         ACTIVE_PEs = 0
         DO L = 0,NumPEs - 1
            IF(NP_ALL(L)>0) ACTIVE_PEs = ACTIVE_PEs + 1
         ENDDO
         INACTIVE_PEs = NumPEs - ACTIVE_PEs
         WRITE(ERR_MSG, 1200) trim(iVal(ACTIVE_PEs)), trim(iVal(INACTIVE_PEs)), trim(iVal(NumPes))
         CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE., LOG=.FALSE.)
      ENDIF

! Repeat above, including ghost particles
      NP = PIP
      CALL ALLGATHER_1I (NP,NP_ALL,IERR)
      CALL GLOBAL_ALL_SUM(NP)
      MIN_NP   = MINVAL(NP_ALL)
      MIN_NPP  = MINLOC(NP_ALL,1)-1
      MAX_NP   = MAXVAL(NP_ALL)
      MAX_NPP  = MAXLOC(NP_ALL,1)-1
      IDEAL_NP = INT(NP/NumPEs)
      IF(IDEAL_NP<1) RETURN    ! Nothing to do if the number of particles is less than NumPes
      MIN_LOAD = dble(MIN_NP)/dble(IDEAL_NP)
      MAX_LOAD = dble(MAX_NP)/dble(IDEAL_NP)
      CURRENT_MAX_LOAD = MAX_LOAD
      WRITE(ERR_MSG, 2000) trim(iVAL(NP)), &
                           trim(iVal(MIN_NP)), trim(iVal(MIN_NPP)), &
                           trim(iVal(MAX_NP)), trim(iVal(MAX_NPP)), &
                           trim(ival(IDEAL_NP))
      CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE., LOG=.FALSE.)
      WRITE(ERR_MSG, 2100) TIME,MIN_LOAD,MAX_LOAD
      CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE., LOG=.FALSE.)
      IF(MyPE==0) THEN
         ACTIVE_PEs = 0
         DO L = 0,NumPEs - 1
            IF(NP_ALL(L)>0) ACTIVE_PEs = ACTIVE_PEs + 1
         ENDDO
         INACTIVE_PEs = NumPEs - ACTIVE_PEs
         WRITE(ERR_MSG, 2200) trim(iVal(ACTIVE_PEs)), trim(iVal(INACTIVE_PEs)), trim(iVal(NumPes))
         CALL FLUSH_ERR_MSG(HEADER=.FALSE., FOOTER=.FALSE., LOG=.FALSE.)
      ENDIF

      DEALLOCATE( NP_ALL)
      CALL REPORT_BEST_DES_IJK_SIZE

1000 FORMAT(/'Particle count: Total: ', A,1x,', Min: ',A,1x,'(PE=',A,')', &
              'Max: ',A,1x,'(PE=',A,'), Ideal: ',A)
1100 FORMAT('Particle load at Time = ', G12.5,1x,', Min: ',G10.3,1x,', Max: ',G10.3)
1200 FORMAT('Number of active/inactive PEs = ', A,1x,'/ ',A,1x,'of ',A//)

2000 FORMAT(/'Particle count (with ghost): Total: ', A,1x,', Min: ',A,1x,'(PE=',A,')', &
              'Max: ',A,1x,'(PE=',A,'), Ideal: ',A)
2100 FORMAT('Particle load (with ghost) at Time = ', G12.5,1x,', Min: ',G10.3,1x,', Max: ',G10.3)
2200 FORMAT('Number of active/inactive PEs (with ghost) = ', A,1x,'/ ',A,1x,'of ',A//)
      RETURN
      END SUBROUTINE DISPLAY_PARTICLE_LOAD



!----------------------------------------------------------------------!
! Subroutine: REPORT_BEST_DES_IJK_SIZE                                 !
! Purpose: Reports best des grid decomposition based on particle
! distribution in the domain
!----------------------------------------------------------------------!
      SUBROUTINE REPORT_BEST_DES_IJK_SIZE

      use discretelement
      use error_manager
      use functions
      use machine
      use mpi_utility
      use run, only: TIME
      use sendrecv
      use desgrid
      use gridmap
      use mpi_init_des
      use usr
      use param1, only: UNDEFINED, ZERO
      use compar, only:ADJUST_PARTITION,ISIZE_ALL,JSIZE_ALL,KSIZE_ALL

      implicit none
      INTEGER :: IERR
      INTEGER :: NP=0  ! Number of particles

      INTEGER,DIMENSION(IMIN2:IMAX2) :: NP_I
      INTEGER,DIMENSION(JMIN2:JMAX2) :: NP_J
      INTEGER,DIMENSION(KMIN2:KMAX2) :: NP_K

      INTEGER :: I,J,K,IJK,IPROC,JPROC,KPROC

      INTEGER :: ilistsize,jlistsize,klistsize             ! size of list of cells

      INTEGER, ALLOCATABLE, DIMENSION(:) :: ALL_NP_I      ! Number of Useful Cells at I for all processors
                                                           ! (I will repeat if decomposing in J or K direction)
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ALL_LIST_I     ! List of I for all processors
      INTEGER, ALLOCATABLE, DIMENSION(:) :: GLOBAL_NP_I   ! Number of Useful Cells at Global I

      INTEGER, ALLOCATABLE, DIMENSION(:) :: ALL_NP_J      ! Number of Useful Cells at J for all processors
                                                           ! (J will repeat if decomposing in I or K direction)
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ALL_LIST_J     ! List of J for all processors
      INTEGER, ALLOCATABLE, DIMENSION(:) :: GLOBAL_NP_J   ! Number of Useful Cells at Global J

      INTEGER, ALLOCATABLE, DIMENSION(:) :: ALL_NP_K      ! Number of Useful Cells at K for all processors
                                                           ! (K will repeat if decomposing in I or J direction)
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ALL_LIST_K     ! List of K for all processors
      INTEGER, ALLOCATABLE, DIMENSION(:) :: GLOBAL_NP_K   ! Number of Useful Cells at Global K

      INTEGER :: I_OFFSET,J_OFFSET,K_OFFSET

      INTEGER, DIMENSION(0:numPEs-1) :: disp,rcount

      INTEGER, DIMENSION(0:NumPEs-1) :: BEST_I_SIZE,BEST_J_SIZE,BEST_K_SIZE
      INTEGER, DIMENSION(0:NumPEs-1) :: TEST_I_SIZE,TEST_J_SIZE,TEST_K_SIZE

      DOUBLE PRECISION :: LIP_I,LIP_J,LIP_K,LIP_IJK, BEST_LIP_IJK
      INTEGER :: P,BEST_PARTITION

      DOUBLE PRECISION :: BEST_DG_XE(1:1000) ! NEED BETTER UPPER BOUND
      DOUBLE PRECISION :: BEST_DG_YN(1:1000)
      DOUBLE PRECISION :: BEST_DG_ZT(1:1000)

      INTEGER :: NUMBER_OF_PARTITIONS_TO_TEST
      LOGICAL :: PARTITION_CHANGED
      LOGICAL :: INCLUDE_GHOST_PARTICLES = .TRUE.

      INTEGER :: iDLB_EGW


! Local Parameters:
!---------------------------------------------------------------------//
! The minimum number of computational cell layers required.
! Must be the same as defined in check_data/check_dmp_prereqs.f
      INTEGER, PARAMETER :: DMP_MIN = 1

      IF(NumPEs==1) RETURN  ! Nothing to do in serial

      iDLB_EGW = INT(DLB_EGW)

! GET A LIST OF NUMBER OF PARTICLES ALONG EACH I-PLANE

      DO I = ISTART1, IEND1
         NP_I(I) = 0
         DO J = JSTART1, JEND1
            DO K = KSTART1, KEND1
               IJK = FUNIJK(I,J,K)
               NP_I(I) = NP_I(I) + PINC(IJK)
               IF(INCLUDE_GHOST_PARTICLES) NP_I(I) = NP_I(I) + GPINC(IJK)
               NP_I(I) = NP_I(I) + iDLB_EGW
            ENDDO
         ENDDO
      ENDDO

! Gather NP onto the head node
! rcount is the size for each processor
! disp is the cumulative sum for each processor

      CALL allgather_1i (IEND1-ISTART1+1,rcount,IERR)

      IF (myPE == 0) THEN
         I_OFFSET = 0
      ELSE
         I_OFFSET = 0
         DO iproc=0,myPE-1
            I_OFFSET = I_OFFSET + rcount(iproc)
         ENDDO
      ENDIF

      CALL allgather_1i (I_OFFSET,disp,IERR)

      ilistsize=SUM(rcount)

      allocate( ALL_LIST_I(ilistsize))
      allocate( GLOBAL_NP_I(IMIN1:ilistsize+1))
      allocate( ALL_NP_I(ilistsize))

! Gather list of I and NP, each processor has its own list
      call gatherv_1i( (/(I,I=ISTART1,IEND1)/), IEND1-ISTART1+1, ALL_LIST_I(:), rcount, disp, PE_IO, ierr )
      call gatherv_1i( NP_I(ISTART1:IEND1), IEND1-ISTART1+1, ALL_NP_I(:), rcount, disp, PE_IO, ierr )

! Get the glocal NP for each unique value of I
      IF (myPE == 0) THEN
         GLOBAL_NP_I = 0
         DO I=1,ilistsize
            GLOBAL_NP_I(ALL_LIST_I(I)) = GLOBAL_NP_I(ALL_LIST_I(I)) + ALL_NP_I(I)
         ENDDO

      ENDIF



! GET A LIST OF NUMBER OF PARTICLES ALONG EACH J-PLANE

      DO J = JSTART1, JEND1
         NP_J(J) = 0
         DO I = ISTART1, IEND1
            DO K = KSTART1, KEND1
               IJK = FUNIJK(I,J,K)
               NP_J(J) = NP_J(J) + PINC(IJK)
               IF(INCLUDE_GHOST_PARTICLES) NP_J(J) = NP_J(J) + GPINC(IJK)
               NP_J(J) = NP_J(J) + iDLB_EGW
            ENDDO
         ENDDO
      ENDDO

! Gather NP onto the head node
! rcount is the size for each processor
! disp is the cumulative sum for each processor

      CALL allgather_1i (JEND1-JSTART1+1,rcount,IERR)

      IF (myPE == 0) THEN
         J_OFFSET = 0
      ELSE
         J_OFFSET = 0
         DO jproc=0,myPE-1
            J_OFFSET = J_OFFSET + rcount(jproc)
         ENDDO
      ENDIF

      CALL allgather_1i (J_OFFSET,disp,IERR)

      jlistsize=SUM(rcount)

      allocate( ALL_LIST_J(jlistsize))
      allocate( GLOBAL_NP_J(JMIN1:jlistsize+1))
      allocate( ALL_NP_J(jlistsize))

! Gather list of J and NP, each processor has its own list
      call gatherv_1i( (/(J,J=JSTART1,JEND1)/), JEND1-JSTART1+1, ALL_LIST_J(:), rcount, disp, PE_IO, ierr )
      call gatherv_1i( NP_J(JSTART1:JEND1), JEND1-JSTART1+1, ALL_NP_J(:), rcount, disp, PE_IO, ierr )

! Get the glocal NP for each unique value of J
      IF (myPE == 0) THEN
         GLOBAL_NP_J = 0
         DO J=1,jlistsize
            GLOBAL_NP_J(ALL_LIST_J(J)) = GLOBAL_NP_J(ALL_LIST_J(J)) + ALL_NP_J(J)
         ENDDO

      ENDIF

      ! Deallocate( ALL_LIST_J)
      ! Deallocate( GLOBAL_NP_J)
      ! Deallocate( ALL_NP_J)



! GET A LIST OF NUMBER OF PARTICLES ALONG EACH K-PLANE

      DO K = KSTART1, KEND1
         NP_K(K) = 0
         DO J = JSTART1, JEND1
            DO I = ISTART1, IEND1
               IJK = FUNIJK(I,J,K)
               NP_K(K) = NP_K(K) + PINC(IJK)
               IF(INCLUDE_GHOST_PARTICLES) NP_K(K) = NP_K(K) + GPINC(IJK)
               NP_K(K) = NP_K(K) + iDLB_EGW
            ENDDO
         ENDDO
      ENDDO

! Gather NP onto the head node
! rcount is the size for each processor
! disp is the cumulative sum for each processor

      CALL allgather_1i (KEND1-KSTART1+1,rcount,IERR)

      IF (myPE == 0) THEN
         K_OFFSET = 0
      ELSE
         K_OFFSET = 0
         DO Kproc=0,myPE-1
            K_OFFSET = K_OFFSET + rcount(Kproc)
         ENDDO
      ENDIF

      CALL allgather_1i (K_OFFSET,disp,IERR)

      Klistsize=SUM(rcount)

      allocate( ALL_LIST_K(Klistsize))
      allocate( GLOBAL_NP_K(KMIN1:Klistsize+1))
      allocate( ALL_NP_K(Klistsize))

! Gather list of K and NP, each processor has its own list
      call gatherv_1i( (/(K,K=KSTART1,KEND1)/), KEND1-KSTART1+1, ALL_LIST_K(:), rcount, disp, PE_IO, ierr )
      call gatherv_1i( NP_K(KSTART1:KEND1), KEND1-KSTART1+1, ALL_NP_K(:), rcount, disp, PE_IO, ierr )

! Get the glocal NP for each unique value of K
      IF (myPE == 0) THEN
         GLOBAL_NP_K = 0
         DO K=1,Klistsize
            GLOBAL_NP_K(ALL_LIST_K(K)) = GLOBAL_NP_K(ALL_LIST_K(K)) + ALL_NP_K(K)
         ENDDO

      ENDIF

      ! Deallocate( GLOBAL_NP_K)
      ! Deallocate( ALL_LIST_K)
      ! Deallocate( ALL_NP_K)


!      RETURN

#ifdef MPI
      call MPI_barrier(MPI_COMM_WORLD,mpierr)
#endif

      IF (myPE == 0) THEN
! Get number of valid partitions to test.
! Will quit the first time  NODESI x NODESJ x NODESK is not equal to NUmPEs

         NUMBER_OF_PARTITIONS_TO_TEST = 0
         DO P = 1,100  ! NEED BETTER UPPER BOUND
            IF(DLB_NODESI(P)*DLB_NODESJ(P)*DLB_NODESK(P)==NumPEs) THEN
               NUMBER_OF_PARTITIONS_TO_TEST = P
            ELSE
               EXIT
            ENDIF
         ENDDO

! Need to test at least the current partition layout
         IF(NUMBER_OF_PARTITIONS_TO_TEST ==0) THEN
            NUMBER_OF_PARTITIONS_TO_TEST = 1
            DLB_NODESI(1) = NODESI
            DLB_NODESJ(1) = NODESJ
            DLB_NODESK(1) = NODESK
         ENDIF


         BEST_LIP_IJK = UNDEFINED

         DO P =1,NUMBER_OF_PARTITIONS_TO_TEST

! I-DIRECTION
            IF(DLB_NODESI(P)>1) THEN

               CALL MINIMIZE_DES_LOAD_IMBALANCE(DLB_NODESI(P),IMIN1,IMAX1,GLOBAL_NP_I,TEST_I_SIZE,LIP_I)

            ELSE

               TEST_I_SIZE(0) = IMAX !isize_all(0)
               LIP_I = ZERO

            ENDIF


   ! J-DIRECTION
            IF(DLB_NODESJ(P)>1) THEN

               CALL MINIMIZE_DES_LOAD_IMBALANCE(DLB_NODESJ(P),JMIN1,JMAX1,GLOBAL_NP_J,TEST_J_SIZE,LIP_J)

            ELSE

               TEST_J_SIZE(0) = JMAX !JSIZE_ALL(0)
               LIP_J = ZERO

            ENDIF


   ! K-DIRECTION
            IF(DLB_NODESK(P)>1) THEN

               CALL MINIMIZE_DES_LOAD_IMBALANCE(DLB_NODESK(P),KMIN1,KMAX1,GLOBAL_NP_K,TEST_K_SIZE,LIP_K)

            ELSE

               TEST_K_SIZE(0) = KMAX !KSIZE_ALL(0)
               LIP_K = ZERO

            ENDIF


! Pick the best partition sizes in each direction

            LIP_IJK = LIP_I + LIP_J + LIP_K

            IF(LIP_IJK < BEST_LIP_IJK) THEN

              BEST_I_SIZE(0:DLB_NODESI(P)-1) = TEST_I_SIZE(0:DLB_NODESI(P)-1)
              BEST_J_SIZE(0:DLB_NODESJ(P)-1) = TEST_J_SIZE(0:DLB_NODESJ(P)-1)
              BEST_K_SIZE(0:DLB_NODESK(P)-1) = TEST_K_SIZE(0:DLB_NODESK(P)-1)

              BEST_DG_XE(1:SIZE(DG_XE)) = DG_XE(:)
              BEST_DG_YN(1:SIZE(DG_YN)) = DG_YN(:)
              IF(DO_K) BEST_DG_ZT(1:SIZE(DG_ZT)) = DG_ZT(:)

              BEST_PARTITION = P
              BEST_LIP_IJK = LIP_IJK

            ENDIF

         ENDDO ! PARTITION TO TEST


      Deallocate( ALL_LIST_I)
      Deallocate( GLOBAL_NP_I)
      Deallocate( ALL_NP_I)

      Deallocate( ALL_LIST_J)
      Deallocate( GLOBAL_NP_J)
      Deallocate( ALL_NP_J)

      Deallocate( ALL_LIST_K)
      Deallocate( GLOBAL_NP_K)
      Deallocate( ALL_NP_K)


          ADJUST_PARTITION = .TRUE.

! We do not need to do a restart when the partition did not change
         PARTITION_CHANGED = .FALSE.
         IF(NODESI/=DLB_NODESI(BEST_PARTITION)) THEN
            PARTITION_CHANGED = .TRUE.
         ELSE
            DO IPROC = 0,NODESI-1
               IF(ISIZE_ALL(IPROC)/=BEST_I_SIZE(IPROC)) PARTITION_CHANGED = .TRUE.
            ENDDO
         ENDIF

         IF(NODESJ/=DLB_NODESJ(BEST_PARTITION)) THEN
            PARTITION_CHANGED = .TRUE.
         ELSE
            DO IPROC = 0,NODESJ-1
               IF(JSIZE_ALL(IPROC)/=BEST_J_SIZE(IPROC)) PARTITION_CHANGED = .TRUE.
            ENDDO
         ENDIF

         IF(DO_K) THEN
            IF(NODESK/=DLB_NODESK(BEST_PARTITION)) THEN
               PARTITION_CHANGED = .TRUE.
            ELSE
               DO IPROC = 0,NODESK-1
                  IF(KSIZE_ALL(IPROC)/=BEST_K_SIZE(IPROC)) PARTITION_CHANGED = .TRUE.
               ENDDO
            ENDIF
         ENDIF


         IF(.NOT.PARTITION_CHANGED) THEN
            ADJUST_PARTITION = .FALSE.
            WRITE (*, 1000) '================================================='
            WRITE (*, 1000) ' PARTITION DID NOT CHANGE.'
            WRITE (*, 1000) '================================================='
         ENDIF


! Now save the new partition to gridmap.dat before restarting
         IF(ADJUST_PARTITION) THEN
! JFD: Comment line above and uncomment next two lines to
!      temporarily disabling DLB for debuging purposes.
!         ADJUST_PARTITION = .TRUE.
!         IF(.FALSE.) THEN
            OPEN(UNIT=777, FILE='gridmap.dat')
            WRITE (777, 1005) DLB_NODESI(BEST_PARTITION), &
                              DLB_NODESJ(BEST_PARTITION), &
                              DLB_NODESK(BEST_PARTITION), '     ! NODESI, NODESJ, NODESK'
            DO IPROC = 0,DLB_NODESI(BEST_PARTITION)-1
                  WRITE(777,1060) IPROC,BEST_I_SIZE(IPROC)
            ENDDO
            DO IPROC = 0,DLB_NODESJ(BEST_PARTITION)-1
                  WRITE(777,1060) IPROC,BEST_J_SIZE(IPROC)
            ENDDO
            DO IPROC = 0,DLB_NODESK(BEST_PARTITION)-1
                  WRITE(777,1060) IPROC,BEST_K_SIZE(IPROC)
            ENDDO
            CLOSE(777)

            WRITE (*, 1000) '================================================='
            WRITE (*, 1000) 'GRID PARTITION SAVED IN FILE: gridmap.dat'
            WRITE (*, 1000) 'MFIX WILL DO AN INTERNAL RESTART_1 NOW.'
            WRITE (*, 1000) '================================================='
            WRITE (*,*) DLB_NODESI(BEST_PARTITION), &
                        DLB_NODESJ(BEST_PARTITION), &
                        DLB_NODESK(BEST_PARTITION), '     ! NODESI, NODESJ, NODESK'
            DO IPROC = 0,DLB_NODESI(BEST_PARTITION)-1
                  WRITE(*,*) IPROC,BEST_I_SIZE(IPROC)
            ENDDO
            DO IPROC = 0,DLB_NODESJ(BEST_PARTITION)-1
                  WRITE(*,*) IPROC,BEST_J_SIZE(IPROC)
            ENDDO
            DO IPROC = 0,DLB_NODESK(BEST_PARTITION)-1
                  WRITE(*,*) IPROC,BEST_K_SIZE(IPROC)
            ENDDO
            WRITE (*, 1000) '================================================='
         ELSEIF(PARTITION_CHANGED) THEN
            WRITE (*, 1000) '================================================='
            WRITE (*, 1000) 'INVALID GRID PARTITION '
            WRITE (*, 1000) '================================================='
            WRITE (*,*) DLB_NODESI(BEST_PARTITION), &
                        DLB_NODESJ(BEST_PARTITION), &
                        DLB_NODESK(BEST_PARTITION), '     ! NODESI, NODESJ, NODESK'
            DO IPROC = 0,DLB_NODESI(BEST_PARTITION)-1
                  WRITE(*,*) IPROC,BEST_I_SIZE(IPROC)
            ENDDO
            DO IPROC = 0,DLB_NODESJ(BEST_PARTITION)-1
                  WRITE(*,*) IPROC,BEST_J_SIZE(IPROC)
            ENDDO
            DO IPROC = 0,DLB_NODESK(BEST_PARTITION)-1
                  WRITE(*,*) IPROC,BEST_K_SIZE(IPROC)
            ENDDO
            WRITE (*, 1000) '================================================='
         ENDIF

      ENDIF ! MyPE==0

      CALL BCAST(ADJUST_PARTITION)

1000  FORMAT(1x,A)
1005  FORMAT(1x,I10,I10,I10,A)
1010  FORMAT(1x,A,I10,I10)
1020  FORMAT(1X,I8,2(I12),F12.2)
1030  FORMAT(1X,A,2(F10.1))
1040  FORMAT(F10.1)
1050  FORMAT(1X,3(A))
1060  FORMAT(1x,I10,I10)

      RETURN

      END SUBROUTINE REPORT_BEST_DES_IJK_SIZE

      SUBROUTINE MINIMIZE_DES_LOAD_IMBALANCE(NODESL,LMIN1,LMAX1,NP_L,BEST_L_SIZE,BEST_LIP)

      use discretelement
      use error_manager
      use functions
      use machine
      use mpi_utility
      use run, only: TIME
      use sendrecv
      use desgrid



      IMPLICIT NONE

      INTEGER :: LMIN1,LMAX1,lsize,lremain
      INTEGER :: DMP_MIN
      INTEGER :: NODESL     ! COULD BE NODESI, NODESJ, OR NODESK
      INTEGER, DIMENSION(LMIN1:LMAX1) :: NP_L
      INTEGER :: NN, NAMAX   ! Number of adjustments, max number of adjustments
      INTEGER :: NOIMPROVEMENT

      INTEGER, DIMENSION(0:NODESL-1) :: NPPP, BEST_NPPP

      INTEGER ::IPROC_OF_MAX,IPROC_OF_MIN

      DOUBLE PRECISION :: LIP, BEST_LIP

      INTEGER, DIMENSION(0:NODESL-1) :: BEST_L_SIZE, NEW_L_SIZE


! Initialize the size with uniform (plus remainder) distribution
      lsize = (lmax1-lmin1+1)/nodesl
      BEST_L_SIZE(0:nodesl-1) = lsize
      lremain = (lmax1-lmin1+1) - nodesl*lsize
      IF (lremain.ge.1) BEST_L_SIZE( 0:(lremain-1) ) = lsize + 1

      CALL GET_LIP_DES(NODESL,NP_L,LMIN1,LMAX1,BEST_L_SIZE,NPPP,LIP,IPROC_OF_MAX,IPROC_OF_MIN)

      BEST_LIP = LIP
      NEW_L_SIZE = BEST_L_SIZE
      BEST_NPPP   = NPPP

      NOIMPROVEMENT=0

! At each iteration, the processor that has the maximum number
! of particles is reduced in size (by one cell),
! and the processor that has the minimum number of particles
! is increased in size (by one cell).
! This is rpeated at most NAMAX times until no improvement is found.

      DMP_MIN = 1

      NAMAX = 2000

      DO NN = 1,NAMAX

1050     FORMAT(A,13(I4),A,2(I4))

         NEW_L_SIZE(IPROC_OF_MAX) = NEW_L_SIZE(IPROC_OF_MAX) - 1
         NEW_L_SIZE(IPROC_OF_MIN) = NEW_L_SIZE(IPROC_OF_MIN) + 1

         CALL GET_LIP_DES(NODESL,NP_L,LMIN1,LMAX1,NEW_L_SIZE,NPPP,LIP,IPROC_OF_MAX,IPROC_OF_MIN)


         IF(LIP<BEST_LIP) THEN
            BEST_LIP    = LIP
            BEST_L_SIZE = NEW_L_SIZE
            BEST_NPPP   = NPPP
            NOIMPROVEMENT=0
         ELSE
            NOIMPROVEMENT = NOIMPROVEMENT + 1
         ENDIF

         IF(NOIMPROVEMENT==1000) THEN
!            WRITE (*, 1000) 'OPTIMIZED LOAD BALANCE REACHED.'
            EXIT
         ENDIF

      ENDDO

1000  FORMAT(1x,A)
1010  FORMAT(1x,A,I10,I10)
1020  FORMAT(1X,I8,2(I12),F12.2)



      END SUBROUTINE MINIMIZE_DES_LOAD_IMBALANCE

      SUBROUTINE GET_LIP_DES(NODESL,NP_L,LMIN1,LMAX1,L_SIZE,NPPP,LIP,IPROC_OF_MAX,IPROC_OF_MIN)
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE gridmap
      USE param
      USE param1
      USE constant
      USE run
      USE physprop
      USE indices
      USE scalars
      USE funits
      USE leqsol
      USE compar
      USE mpi_utility
      USE bc
      USE DISCRETELEMENT
      USE desgrid

      USE cutcell
      USE quadric
      USE vtk
      USE polygon
      USE dashboard
      USE stl


      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: NODESL,LMIN1,LMAX1,TOTAL_NP,IPROC_OF_MAX,IPROC_OF_MIN

      INTEGER :: LCOUNT1,LCOUNT2,MINVAL_NPPP,MAXVAL_NPPP,IDEAL_NPPP
      INTEGER, DIMENSION(LMIN1:LMAX1) :: NP_L

      INTEGER :: IPROC

      INTEGER, DIMENSION(0:NODESL-1) :: NPPP,L_SIZE,L1,L2

      DOUBLE PRECISION :: LIP

      INTEGER :: DMP_MIN = 1

!-----------------------------------------------

      LCOUNT1 = LMAX1 - LMIN1 + 1
      LCOUNT2 = SUM(L_SIZE(0:NODESL-1))

      IF(LCOUNT1/=LCOUNT2) THEN
         WRITE(ERR_MSG,*)' ERROR: SUM OF CELLS DO NOT MATCH:',LCOUNT1,LCOUNT2
         call flush_err_msg(abort=.TRUE.)
      ENDIF

      L1(0) = LMIN1
      L2(0) = L1(0) + L_SIZE(0) - 1

      DO IPROC = 1,NODESL-1
         L1(IPROC) = L2(IPROC-1) + 1
         L2(IPROC) = L1(IPROC) + L_SIZE(IPROC) - 1
      ENDDO

      TOTAL_NP = 0
      DO IPROC = 0,NODESL-1
         NPPP(IPROC) = SUM(NP_L(L1(IPROC):L2(IPROC)))
         TOTAL_NP = TOTAL_NP + NPPP(IPROC)
      ENDDO

      IDEAL_NPPP = TOTAL_NP / NODESL

      MAXVAL_NPPP = MAXVAL(NPPP)
      MINVAL_NPPP = MINVAL(NPPP)

      LIP = DBLE(MAXVAL_NPPP-IDEAL_NPPP)/DBLE(IDEAL_NPPP)*100.0D0

      IPROC_OF_MAX = MAXLOC(NPPP,1)-1
      IPROC_OF_MIN = MINLOC(NPPP,1)-1

      IF(L_SIZE(IPROC_OF_MAX)<=DMP_MIN) THEN ! Need to find the next processor to be reduced in size
         MAXVAL_NPPP = 0
         DO IPROC = 0,NODESL-1
            IF(IPROC==IPROC_OF_MIN) CYCLE
            IF(IPROC==IPROC_OF_MAX) CYCLE
            IF(L_SIZE(IPROC)<=DMP_MIN) CYCLE
            IF(NPPP(IPROC)>MAXVAL_NPPP) THEN
               MAXVAL_NPPP = NPPP(IPROC)
               IPROC_OF_MAX = IPROC
            ENDIF
         ENDDO
      ENDIF
1050     FORMAT(A,12(I8),A,2(I4))

      RETURN
      END SUBROUTINE GET_LIP_DES
END MODULE output_man

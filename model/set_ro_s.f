MODULE SET_RO_s_MOD
   CONTAINS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: SET_RO_s                                                !
!                                                                      !
!  Author: J.Musser                                   Date: 09-Oct-13  !
!  Reviewer:                                                           !
!                                                                      !
!  Purpose: Initialize solids densities.                               !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE SET_RO_S

! Global Variables:
!---------------------------------------------------------------------/
! Number of solids phases.
      use physprop, only: MMAX
      use discretelement, only: DES_MMAX
! Solids density field variable.
      use fldvar, only: RO_s, ROP_s
! Solid phase species mass fractions.
      use fldvar, only: X_s
! Initial mass fraction of inert species
      use physprop, only: X_S0
! Index of inert solids phase species.
      use physprop, only: INERT_SPECIES
! Inert solids phase species mass fraction in dilute region.
      use physprop, only: DIL_INERT_X_VSD
! Factor to define dilute region where DIL_INERT_X_VSD is used
      use physprop, only: DIL_FACTOR_VSD
! Run-time flag for variable soilds density
      use run, only: SOLVE_ROs
! Constant solids density.
      use physprop, only: RO_s0
! Minimum solids volume fraction
      use toleranc, only: DIL_EP_s

! Function for evaluating solids density.
      use eos, only: EOSS

      use usr_prop, only: usr_ros
! Modules needed to support function.inc
      use compar
      use geometry
      use indices
      use functions

      implicit none

! Local Variables:
!---------------------------------------------------------------------/
! Solids phase index
      INTEGER :: M
! Fluid cell index
      INTEGER :: IJK
! Index of the inert solids species.
      INTEGER :: IIS
! Flag for debugging.
      LOGICAL, parameter :: dbgMode = .FALSE.

      DOUBLE PRECISION :: minROPs

! Loop over all solids
      DO M=1,MMAX

! Variable solids density.
         IF (SOLVE_ROs(M)) THEN
! Set the index of the intert phase.
            IIS = INERT_SPECIES(M)
! Calculate the minimum solids denisty.
!            minROPs = RO_s0(M)*DIL_EP_s
            minROPs = RO_s0(M)*(DIL_FACTOR_VSD*DIL_EP_s)
! Debug/Development option.
            IF(dbgMode) CALL CHECK_SET_ROs()

! Calculate Ro_s in all fluid and flow boundary cells.
            DO IJK = ijkStart3, ijkEnd3
               IF(WALL_AT(IJK)) CYCLE
               IF(ROP_s(IJK,M) > minROPs) THEN
                  RO_S(IJK,M) = EOSS(RO_s0(M), X_s0(M,IIS),         &
                     X_s(IJK,M,IIS))
               ELSE
!                  RO_s(IJK,M) = RO_s0(M)
                  RO_S(IJK,M) = EOSS(RO_s0(M), X_s0(M,IIS),            &
                     DIL_INERT_X_VSD(M))
               ENDIF
            ENDDO
         ELSEIF (USR_ROs(M)) THEN
            DO IJK=IJKSTART3,IJKEND3
               IF (WALL_AT(IJK)) CYCLE
               CALL USR_PROP_ROS(IJK,M)
            ENDDO
! Correct volume fraction to user speciciation that was introduced in
! using an arbitrary ic_ros.
            CALL USR_ROS_CORRECTION(M)
         ELSE
! Constant solids density.
            DO IJK = ijkstart3, ijkend3
               IF (WALL_AT(IJK)) CYCLE
               RO_S(IJK,M) = RO_S0(M)
            ENDDO
         ENDIF
      ENDDO

! Set a phase density for each cell. The value isn't overly important,
! but it is needed to use the EP_s() function.
      DO M=MMAX+1, DES_MMAX+MMAX
         IF (SOLVE_ROs(M)) THEN
            DO IJK = ijkStart3, ijkEnd3
               IF(.NOT.WALL_AT(IJK)) RO_S(IJK,M) = RO_s0(M)
            ENDDO
         ENDIF
      ENDDO

      RETURN

      CONTAINS

!``````````````````````````````````````````````````````````````````````!
!  Subroutine: USR_ROS_CORRECTION                                      !
!  Author: J.CARNEY                                                    !
!                                                                      !
!  Purpose:  In the case of variable density, it is perhaps more       !
!  rational for the user to specify ep_s (ic_ep_s/bc_ep_s) as opposed  !
!  to rop_s. The reason being that rop_s is itself derived from ro_s,  !
!  which has not been calculated yet. MFIX, however, does not store    !
!  ep_s as its own variable but makes it a function of rop_s at        !
!  any time. So imposing the user specified ic/bc_ep_s on the field    !
!  ep_s is not readily possible; we must first assign rop_s which      !
!  cannot be done without knowing ro_s. And, the call to usr_ro_s      !
!  only becomes pratical once the field variables are defined          !
!  throughout the domain.                                              !
!  Thus, issues may arise during the very fist call to usr_ros (via    !
!  set_ro_s). In particular, if the user invokes rop_s as any part     !
!  of their function call it will be undefined.                        !
!  To avoid such problems we allow rop_s to be set throughout the      !
!  domain based on the ic_ep_s and an arbitrary ic_ros. Note that      !
!  BC routines are set up so that flow boundaries will reflect their   !
!  fluid neighbor if bc_ep_s/bc_rop_s are undefined). The existing     !
!  rop_s is then overwritten here based on the ic_ep_s/bc_ep_s         !
!  and the now available/updated ros.                                  !
!                                                                      !
!  Note: In general the rop_s value in an outflow boundary is set to   !
!  reflect its fluid neighbor (through set_bc1).  However, if a MI     !
!  boundary is selected w/o requiring bc_ep_s (as in the case where    !
!  the user specified velocities), then ep_s in the boundary will      !
!  effectively become fixed to the IC of the neighbor fluid cell.      !
!  Ultimately, this value has no meaning (it is not involved in the    !
!  solver but it may be misinterpreted should a user use the boundary  !
!  ep_s value (or rop_s) in some sort of post processing analysis.     !
!                                                                      !
!                                                                      !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE USR_ROS_CORRECTION(M)

! Global variables
!---------------------------------------------------------------------

      use fldvar, only: RO_s, ROP_s

      use param1, only: undefined
      use param, only: dimension_ic, dimension_bc
      use ic, only: ic_defined, ic_ep_s
      use bc, only: bc_defined, bc_ep_s

      use ic, only: ic_i_e, ic_i_w, ic_j_n, ic_j_s, ic_k_t, ic_k_b
      use bc, only: bc_i_e, bc_i_w, bc_j_n, bc_j_s, bc_k_t, bc_k_b

      use functions, only: is_on_mype_plus2layers
      use functions, only: funijk
      use compar, only: dead_cell_at

     IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------
! solids phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!---------------------------------------------------------------------
! Loop counters
      INTEGER :: ICV, BCV
      INTEGER :: I, J, K
!......................................................................!

      DO ICV = 1, DIMENSION_IC
         IF (IC_DEFINED(ICV)) THEN
            DO K = IC_K_B(ICV), IC_K_T(ICV)
            DO J = IC_J_S(ICV), IC_J_N(ICV)
            DO I = IC_I_W(ICV), IC_I_E(ICV)
               IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
               IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
               IJK = FUNIJK(I,J,K)

! This check is probably unnecessary as ic_ep_s must be specified
! when using usr_ros
               IF (IC_EP_S(ICV,M) /= UNDEFINED) THEN
                  ROP_S(IJK,M) = IC_EP_S(ICV,M)*RO_S(IJK,M)
               ENDIF
            ENDDO
            ENDDO
            ENDDO
         ENDIF
      ENDDO

      DO BCV = 1, DIMENSION_BC
         IF (BC_DEFINED(BCV)) THEN
            DO K = BC_K_B(BCV), BC_K_T(BCV)
            DO J = BC_J_S(BCV), BC_J_N(BCV)
            DO I = BC_I_W(BCV), BC_I_E(BCV)
               IF (.NOT.IS_ON_myPE_plus2layers(I,J,K)) CYCLE
               IF (DEAD_CELL_AT(I,J,K)) CYCLE  ! skip dead cells
               IJK = FUNIJK(I,J,K)

! This check is more necessary as in many boundaries bc_ep_s is not
! required and therefore may not be set...
               IF (BC_EP_S(BCV,M) /= UNDEFINED) THEN
                  ROP_S(IJK,M) = BC_EP_S(BCV,M)*RO_S(IJK,M)
               ENDIF
            ENDDO
            ENDDO
            ENDDO
         ENDIF
      ENDDO

      RETURN

      END SUBROUTINE USR_ROS_CORRECTION


!``````````````````````````````````````````````````````````````````````!
!  Subroutine: CHECK_SET_ROs                                           !
!  Author: J.Musser                                   Date:            !
!                                                                      !
!  Purpose: Verify that all the variable solids density information is !
!           present for solids phase M.                                !
!                                                                      !
!           Note: The check_data routines should have caught any       !
!           problematic IC/BC specifications. This is included mainly  !
!           for development efforts.                                   !
!``````````````````````````````````````````````````````````````````````!
      SUBROUTINE CHECK_SET_ROs()

! Flag for who writes
      use funits, only: DMP_LOG
! Solids species mass fractions.
      use fldvar, only: X_s

      use param1, only: zero
! Number of phase species.
      use physprop, only: NMAX
! Index of inert species.
      use physprop, only: INERT_SPECIES

      use toleranc
      use error_manager, only: init_err_msg, flush_err_msg

      implicit none

! Sum of solids phase mass fractions.
      DOUBLE PRECISION :: SUM_Xs
! Index of inert solids phase species.
      INTEGER :: INERT
! Integer Error Flag.
      INTEGER :: IER(2)

! Error file log.
      INTEGER, parameter :: lUnit = 8454
      LOGICAL :: lExists
      CHARACTER(LEN=64) :: lFName

! Initialize error flags.
      IER = 0

! Set the inert species index.
      INERT = INERT_SPECIES(M)

! Check all computational cells.
      DO IJK = ijkStart3, ijkEnd3
! Skip walls.
         IF (WALL_AT(IJK)) CYCLE
! Calculate the solids species mass fraction sum.
         SUM_Xs = sum(X_s(IJK,M,:NMAX(M)))
! Verify that the species mass fractions are specified and valid.
         IF(.NOT.compare(ONE,SUM_Xs)) IER(1) = IER(1)+1
! Verify that the inert species mass fraction is greater than zero.
         IF(X_s(IJK,M,INERT) <= ZERO) IER(2) = IER(2)+1

      ENDDO

! An error was detected. Open a log file.
      IF(sum(IER) /= 0) THEN
         lFName=''
         IF(numPEs == 1) THEN
            WRITE(lFName,"('setROs.log')")
         ELSE
            WRITE(lFName,"('setROs_',I6.6,'.log')") myPE
         ENDIF
         inquire(file=trim(lFName),exist=lExists)
         IF(lExists) THEN
            OPEN(unit=lUnit,file=trim(lFName),status='replace')
         ELSE
            OPEN(unit=lUnit,file=trim(lFName),status='new')
         ENDIF
      ENDIF


! An error was detected in species mass fraction sum.
      IF(IER(1) /= 0)THEN
         WRITE(lUnit,1100) myPE
! Skip walls.
         DO IJK = ijkStart3, ijkEnd3
            IF (WALL_AT(IJK)) CYCLE
! Calculate the solids species mass fraction sum.
            SUM_Xs = sum(X_s(IJK,M,:NMAX(M)))
! Verify that the species mass fractions are specified and valid.
            IF(.NOT.compare(ONE,SUM_Xs)) WRITE(lUnit,1101) IJK, SUM_Xs
         ENDDO
         WRITE(lUnit,9999)
      ENDIF

! An error was detected in inert species mass fraction.
      IF(IER(2) /= 0)THEN
         WRITE(lUnit,1200) myPE
         WRITE(lUnit,1201) M
         WRITE(lUnit,1202) INERT
! Skip walls.
         DO IJK = ijkStart3, ijkEnd3
            IF (WALL_AT(IJK)) CYCLE
! Calculate the solids species mass fraction sum.
! Verify that the species mass fractions are specified and valid.
            IF(X_s(IJK,M,INERT) <= ZERO) WRITE(lUnit,1203)             &
               IJK, X_s(IJK,M,INERT)
         ENDDO
         WRITE(lUnit,9999)
      ENDIF

! Close the file, cleanup, and exit.
      IF(sum(IER) /= 0) THEN
         CLOSE(lUnit)
         IF(DMP_LOG) THEN
         ENDIF
         call flush_err_msg(abort=.true.)
      ENDIF


      RETURN

 1100 FORMAT(//1X,70('*')/' From: CHECK_SET_ROs',/,' Error 1100:',     &
         ' One or more fluid cells contain invalid species mass',/     &
         ' fractions which do NOT sum to one.'/,'   > myPE = ',I6)

 1101 FORMAT('   > sum(X_s(',I6,')) = ',g12.5)

 1200 FORMAT(//1X,70('*')/' From: CHECK_SET_ROs',/,' Error 1200:',     &
         ' One or more fluid cells contain an invalid species mass',/  &
         ' fraction for the inert material.'/,'   > myPE = ',I6)

 1201 FORMAT('   > Solid Phase: ',I2)

 1202 FORMAT('   > Inert species index: ',I4)

 1203 FORMAT('   > X_s(',I6,',INERT) = ',g12.5)

 9999 FORMAT(1x,70('*')/)

      END SUBROUTINE CHECK_SET_ROs

      END SUBROUTINE SET_RO_S

   END MODULE SET_RO_s_MOD

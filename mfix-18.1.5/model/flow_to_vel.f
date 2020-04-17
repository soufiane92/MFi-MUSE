!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: FLOW_TO_VEL                                             !
!  Author: M. Syamlal                                 Date: 28-JUL-92  !
!                                                                      !
!  Purpose: Convert volumetric and mass flow rates to velocities       !
!     A specified mass flow rate is first converted to volumetric      !
!     flow rate. The volumetric flow rate is then converted to a       !
!     velocity.                                                        !
!                                                                      !
!    When both flow rates and velocities are specified, a consistency  !
!    check is done. The first time flow_to_vel is called in by setting !
!    the logical DO_VEL_CHECK to .TRUE.. If cut-cells are not used,    !
!    flow_to_vel is only called once.  When cut-cells are used,        !
!    flow_to_vel is called another time after the cut-cell pre-        !
!    processing stage. During, the second call, the velocity check     !
!    should not be performed, because the velocity assigned suring the !
!    first call will not match the flow rate. Therfore, when called    !
!    from cut_cell_preprocessing.f DO_VEL_CHECK is set to .FALSE..     !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE FLOW_TO_VEL(DO_VEL_CHECK, M_TOT, SKIP, BCV)

      use param, only: DIM_M
      use param1, only: UNDEFINED
      use geometry, only: NO_I, NO_J, NO_K
      use bc, only: BC_MASSFLOW_G
      use bc, only: BC_VOLFLOW_G
      use bc, only: BC_MASSFLOW_S
      use bc, only: BC_VOLFLOW_S
      use run, only: REINITIALIZING

      use error_manager
      use toleranc

      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
      LOGICAL, intent(in) :: DO_VEL_CHECK

! loop/variable indices
      INTEGER, intent(in) :: M_TOT
      LOGICAL, intent(in) :: SKIP(DIM_M)

! loop/variable indices
      INTEGER, intent(in) :: BCV

! Whether any volumetric flow conversion was done
      LOGICAL :: CONVERTED = .FALSE.

! Loop index
      INTEGER :: M


      CALL INIT_ERR_MSG("FLOW_TO_VEL")

! Mass flows rates are converted to volumetric flow rates.
      IF(BC_MASSFLOW_G(BCV) /= UNDEFINED) &
         CALL GAS_MASSFLOW_TO_VOLFLOW(BCV)

      DO M=1,M_TOT
         IF(BC_MASSFLOW_S(BCV,M) /= UNDEFINED) &
            CALL SOLIDS_MASSFLOW_TO_VOLFLOW(BCV,M,SKIP(M))
      ENDDO

! Volumetric flow rates are converted to velocities.
      IF(BC_VOLFLOW_G(BCV) /= UNDEFINED) THEN
         CALL GAS_VOLFLOW_TO_VELOCITY(DO_VEL_CHECK, BCV)
! Set the conversion flag.
         CONVERTED = .TRUE.
      ENDIF

      DO M=1,M_TOT
         IF(BC_VOLFLOW_S(BCV,M) /= UNDEFINED) THEN
            CALL SOLIDS_VOLFLOW_TO_VELOCITY(DO_VEL_CHECK,BCV,M,SKIP(M))
! Set the conversion flag.
            CONVERTED = .TRUE.
         ENDIF
      ENDDO

      IF(CONVERTED .AND. .NOT.REINITIALIZING .AND. &
         (NO_I.OR.NO_J.OR.NO_K)) THEN
         WRITE(ERR_MSG, 1100)
         CALL FLUSH_ERR_MSG
      ENDIF

      CALL FINL_ERR_MSG

      RETURN

 1100 FORMAT('Warning 1100: Some volumetric or mass flow rates have ', &
         'been converted',/'velocity. Ensure that the third (unused) ',&
         'dimension in 2D simulations',/'is correctly specified (e.g.',&
         ', in axisymmetric cylindrical coordinates',/'ZLENGTH = 2*Pi)')

      END SUBROUTINE FLOW_TO_VEL



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: GAS_MASSFLOW_TO_VOLFLOW                                 !
!  Author: M. Syamlal                                 Date: 28-JUL-92  !
!                                                                      !
!  Purpose: Convert a gas phase BC input from a mass flow rate to      !
!  a volumetric flow rate.                                             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GAS_MASSFLOW_TO_VOLFLOW(BCV)

      use bc, only: BC_MASSFLOW_g
      use bc, only: BC_P_g
      use bc, only: BC_T_g
      use bc, only: BC_VOLFLOW_g
      use bc, only: BC_X_g
      use eos, only: EOSG
      use error_manager
      use param, only: DIMENSION_BC
      use param1, only: UNDEFINED
      use param1, only: ZERO
      use physprop, only: CALC_MW
      use physprop, only: MW_AVG, MW_g
      use physprop, only: NMAX
      use physprop, only: RO_g0
      use usr_prop, only: usr_rog
      use scales, only: P_REF
      use toleranc

      IMPLICIT NONE

      INTEGER, INTENT(in) :: BCV

! Volumetric flow rate computed from mass flow rate
      DOUBLE PRECISION :: VOLFLOW
! Average molecular weight
      DOUBLE PRECISION :: MW

      CALL INIT_ERR_MSG("GAS_MASSFLOW_TO_VOLFLOW")

! No need to convert if the mass flow is zero.
      IF(COMPARE(BC_MASSFLOW_G(BCV),ZERO)) THEN
         VOLFLOW = ZERO

! Incompressible gas BC.
      ELSEIF(RO_G0 /= UNDEFINED) THEN
         VOLFLOW = BC_MASSFLOW_G(BCV)/RO_G0

      ELSEIF(USR_ROg) THEN
! prior error checks should catch this. a massflow rate cannot be
! specified in the case of a udf for density at this time

! Well-defined compresible gas BC.
      ELSEIF(BC_P_G(BCV)/=UNDEFINED .AND. BC_T_G(BCV)/=UNDEFINED) THEN
         IF(MW_AVG == UNDEFINED) THEN
            MW = CALC_MW(BC_X_G,DIMENSION_BC,BCV,NMAX(0),MW_G)
         ELSE
            MW = MW_AVG
         ENDIF
         VOLFLOW = BC_MASSFLOW_G(BCV) / &
            EOSG(MW,(BC_P_G(BCV)-P_REF),BC_T_G(BCV))

! Fails. This shouldn't happen as previous checks should catch any
! errors leading to this routine.
      ELSE
         WRITE(ERR_MSG, 1100) BCV
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

 1100 FORMAT('Error 1100: Boundary condition ',I3,' failed sanity ',   &
      'check.',/'Please report this to the MFIX mailing list.')

      ENDIF

! Check that a specified volumetric flow matches the calculated value.
      IF(BC_VOLFLOW_G(BCV) /= UNDEFINED) THEN
         IF(.NOT.COMPARE(VOLFLOW,BC_VOLFLOW_G(BCV))) THEN
            WRITE(ERR_MSG,1101) trim(iVar('BC_MASSFLOW_g',BCV)), BCV,  &
               VOLFLOW, BC_VOLFLOW_g(BCV)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ELSE

! Store the calculated volumetric flow rate.
         BC_VOLFLOW_G(BCV) = VOLFLOW
      ENDIF

 1101 FORMAT('Error 1101: Volumetric flow rate calculated from ',A,/   &
         'does NOT equal the specified volumetric flow rate for BC',I3,&
         /3x,'>>> Calculated: ',G14.7,/3x,'>>> Specified:  ',G14.7,/   &
         'Please correct the mfix.dat file.')

! Clean up and return.
      CALL FINL_ERR_MSG

      RETURN

      END SUBROUTINE GAS_MASSFLOW_TO_VOLFLOW



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: SOLIDS_MASSFLOW_TO_VOLFLOW                              !
!  Author: M. Syamlal                                 Date: 28-JUL-92  !
!                                                                      !
!  Purpose: Convert solids phase BC input from a mass flow rate to     !
!  a volumetric flow rate.                                             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE SOLIDS_MASSFLOW_TO_VOLFLOW(BCV,M, SKIP_M)

      USE bc, only: BC_MASSFLOW_s
      USE bc, only: BC_VOLFLOW_s
      USE bc, only: BC_X_s
      USE param1, only: UNDEFINED, ZERO
      USE physprop, only: INERT_SPECIES
      USE physprop, only: RO_s0
      USE physprop, only: X_s0
      use usr_prop, only: usr_ros
      use eos, only: EOSS
      use error_manager
      use toleranc

      IMPLICIT NONE

! loop/variable indices
      INTEGER, INTENT(in) :: BCV, M
      LOGICAL, INTENT(in) :: SKIP_M

! Volumetric flow rate computed from mass flow rate
      DOUBLE PRECISION :: VOLFLOW
! Index of inert species
      INTEGER :: INERT

      CALL INIT_ERR_MSG("SOLIDS_MASSFLOW_TO_VOLFLOW")


      IF(SKIP_M) THEN
         WRITE(ERR_MSG,1100) M, BCV, trim(iVar("BC_MASSFLOW_S",BCV,M))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

 1100 FORMAT('Error 1100: Solids phase ',I2,' has a specified mass ',  &
         'flow rate',/'at BC ',I3,', ',A,'. But, both BC_ROP_s and ',&
         'BC_EP_s are zero or undefined.',/'Please correct the ',&
         'mfix.dat file.')

      IF(COMPARE(BC_MASSFLOW_S(BCV,M),ZERO)) THEN
         VOLFLOW = ZERO
! Constant solids density.
      ELSEIF(RO_S0(M) /= UNDEFINED) THEN
         VOLFLOW = BC_MASSFLOW_S(BCV,M)/RO_S0(M)
      ELSEIF(USR_ROs(M)) THEN
! prior error checks should catch this. a massflow rate cannot be
! specified in the case of a udf for density at this time
      ELSE
! Set an alias for the inert species.
         INERT = INERT_SPECIES(M)
! Variable solids density.
         VOLFLOW = BC_MASSFLOW_S(BCV,M)/EOSS(RO_s0(M),              &
            X_s0(M,INERT), BC_X_S(BCV,M,INERT))
      ENDIF

! If volumetric flow is also specified compare both
      IF(BC_VOLFLOW_S(BCV,M) /= UNDEFINED) THEN
         IF(.NOT.COMPARE(VOLFLOW,BC_VOLFLOW_S(BCV,M))) THEN
            WRITE(ERR_MSG,1101) trim(iVar('BC_MASSFLOW_S',BCV,M)), BCV, &
               VOLFLOW, BC_VOLFLOW_S(BCV,M)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ELSE
         BC_VOLFLOW_S(BCV,M) = VOLFLOW
      ENDIF

      CALL FINL_ERR_MSG

      RETURN

 1101 FORMAT('Error 1101: Volumetric flow rate calculated from ',A,/   &
         'does NOT equal the specified volumetric flow rate for BC',I3,&
         /3x,'>>> Calculated: ',G14.7,/3x,'>>> Specified:  ',G14.7,/   &
         'Please correct the mfix.dat file.')


      END SUBROUTINE SOLIDS_MASSFLOW_TO_VOLFLOW




!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: GAS_VOLFLOW_TO_VELOCITY                                 !
!  Author: M. Syamlal                                 Date: 28-JUL-92  !
!                                                                      !
!  Purpose: Convert gas phase volumetric rate to a velocity.           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE GAS_VOLFLOW_TO_VELOCITY(DO_VEL_CHECK, BCV)

      USE bc
      USE compar
      USE discretelement
      use error_manager, only: err_msg, init_err_msg, flush_err_msg, finl_err_msg
      USE fldvar
      USE funits
      USE geometry
      USE indices
      USE mfix_pic
      USE param
      USE param1
      USE physprop
      USE run
      USE scales
      USE toleranc

      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! loop/variable indices
      INTEGER, INTENT(in) :: BCV

! Whether any volumetric flow conversion was done
      LOGICAL, INTENT(in) :: DO_VEL_CHECK


      DOUBLE PRECISION :: SGN, OFF

! Velocity computed from volumetric flow rate
      DOUBLE PRECISION :: VEL
!-----------------------------------------------
      CALL INIT_ERR_MSG("GAS_VOLFLOW_TO_VELOCITY")

      SELECT CASE (BC_TYPE_ENUM(BCV))
      CASE (MASS_INFLOW);  SGN =  ONE; OFF = ZERO
      CASE (MASS_OUTFLOW); SGN = -ONE; OFF = ONE
      CASE DEFAULT
        write(ERR_MSG,*) 'error in GAS_VOLFLOW_TO_VELOCITY'
        call flush_err_msg(abort=.TRUE.)
      END SELECT

      SELECT CASE (BC_PLANE(BCV))
      CASE ('W'); SGN = -SGN
      CASE ('S'); SGN = -SGN
      CASE ('B'); SGN = -SGN
      END SELECT

! Calculate the velocity based on the volumetric flow rate,
! BC area and BC volume fraction.
      VEL = SGN*BC_VOLFLOW_G(BCV)/(BC_AREA(BCV)*BC_EP_G(BCV))

! if the user also defined the boundary velocity through the plane, then
! check that the calculated value agrees with the specified value. if
! the user did not define the boundary velocity through the plane, then
! if mass_inflow set the value of the boundary velocity to the
! calculated value. otherwise do nothing.
      IF(BC_PLANE(BCV) == 'W' .OR. BC_PLANE(BCV)== 'E') THEN

         IF(BC_U_G(BCV) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL,BC_U_G(BCV))) THEN
               WRITE(ERR_MSG,1100) BCV, VEL, 'BC_U_g', BC_U_G(BCV)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ELSE
            BC_U_G(BCV) = VEL
            BC_V_G(BCV) = OFF * BC_V_G(BCV)
            BC_W_G(BCV) = OFF * BC_W_G(BCV)
         ENDIF

      ELSEIF(BC_PLANE(BCV) == 'S' .OR. BC_PLANE(BCV)== 'N') THEN
         IF(BC_V_G(BCV) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL,BC_V_G(BCV))) THEN
               WRITE(ERR_MSG, 1100) BCV, VEL, 'BC_V_g', BC_V_G(BCV)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ELSE
            BC_V_G(BCV) = VEL
            BC_U_G(BCV) = OFF * BC_U_G(BCV)
            BC_W_G(BCV) = OFF * BC_W_G(BCV)
         ENDIF

      ELSEIF(BC_PLANE(BCV) == 'B' .OR. BC_PLANE(BCV)== 'T') THEN
         IF(BC_W_G(BCV) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL, BC_W_G(BCV))) THEN
               WRITE(ERR_MSG, 1100) BCV, VEL, 'BC_W_g', BC_W_G(BCV)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ELSE
            BC_W_G(BCV) = VEL
            BC_U_G(BCV) = OFF * BC_U_G(BCV)
            BC_V_G(BCV) = OFF * BC_V_G(BCV)
         ENDIF

      ENDIF

      CALL FINL_ERR_MSG

      RETURN

 1100 FORMAT(/1X,70('*')//' From: FLOW_TO_VEL',/' Message: BC No:',I2,/,&
         ' Computed velocity is not equal to specified value',/,&
         ' Value computed from vol. or mass flow  = ',G14.7,/,&
         ' Specified value (',A,') = ',G14.7,/1X,70('*')/)

      END SUBROUTINE GAS_VOLFLOW_TO_VELOCITY

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: SOLIDS_VOLFLOW_TO_VELOCITY                              !
!  Author: M. Syamlal                                 Date: 28-JUL-92  !
!                                                                      !
!  Purpose: Convert volumetric and mass flow rates to velocities       !
!     A specified mass flow rate is first converted to volumetric      !
!     flow rate. The volumetric flow rate is then converted to a       !
!     velocity.                                                        !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE SOLIDS_VOLFLOW_TO_VELOCITY(DO_VEL_CHECK, BCV, M, SKIP_M)

      USE param
      USE param1
      USE geometry
      USE fldvar
      USE physprop
      USE run
      USE bc
      USE scales
      USE indices
      USE funits
      USE compar
      USE discretelement
      USE mfix_pic

      use error_manager
      use toleranc

      IMPLICIT NONE
!-----------------------------------------------
! Local variables
!-----------------------------------------------
! loop/variable indices
      INTEGER, INTENT(in) :: BCV, M
! Logical to preform velocity check.
      LOGICAL, INTENT(in) :: DO_VEL_CHECK, SKIP_M

! Velocity computed from volumetric flow rate
      DOUBLE PRECISION :: VEL

      DOUBLE PRECISION :: SGN, OFF

!-----------------------------------------------

      CALL INIT_ERR_MSG("SOLIDS_VOLFLOW_TO_VELOCITY")

      IF(SKIP_M) THEN
         WRITE(ERR_MSG,1100) M, BCV, trim(iVar("BC_VOLFLOW_S",BCV,M))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

 1100 FORMAT('Error 1100: Solids phase ',I2,' has a specified ',       &
         'volumetric flow rate',/'at BC ',I3,', ',A,'. But, both ',&
         'BC_ROP_s and BC_EP_s are zero or undefined.',/'Please ',&
         'update the mfix.dat file.')

      SELECT CASE (BC_TYPE_ENUM(BCV))
      CASE (MASS_INFLOW);  SGN =  ONE; OFF = ZERO
      CASE (MASS_OUTFLOW); SGN = -ONE; OFF = ONE
      CASE DEFAULT
        write(ERR_MSG,*) 'error in SOLIDS_VOLFLOW_TO_VELOCITY'
        call flush_err_msg(abort=.TRUE.)
      END SELECT

      SELECT CASE (BC_PLANE(BCV))
      CASE ('W'); SGN = -SGN
      CASE ('S'); SGN = -SGN
      CASE ('B'); SGN = -SGN
      END SELECT

      IF(BC_EP_S(BCV,M) /= ZERO) THEN
         VEL = SGN * BC_VOLFLOW_S(BCV,M)/(BC_AREA(BCV)*BC_EP_S(BCV,M))
      ELSE
         IF(BC_VOLFLOW_S(BCV,M) == ZERO) THEN
            VEL = ZERO
         ELSE
            IF(DMP_LOG)WRITE (UNIT_LOG, 1101) BCV, M
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDIF

 1101 FORMAT('Error 1101: BC No:',I2,' Non-zero vol. or mass flow ',&
         'specified with BC_ROP_s', I1,' = 0.')

      IF(BC_PLANE(BCV) == 'W' .OR. BC_PLANE(BCV)== 'E') THEN
         IF(BC_U_S(BCV,M) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL, BC_U_S(BCV,M))) THEN
              WRITE(ERR_MSG, 1300) BCV, (-VEL), 'BC_U_s', M, BC_U_S(BCV,M)
              CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ELSE
            BC_U_S(BCV,M) = VEL
            BC_V_S(BCV,M) = OFF * BC_V_S(BCV,M)
            BC_W_S(BCV,M) = OFF * BC_W_S(BCV,M)
         ENDIF

      ELSEIF(BC_PLANE(BCV) == 'S' .OR. BC_PLANE(BCV)== 'N') THEN
         IF(BC_V_S(BCV,M) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL,BC_V_S(BCV,M))) THEN
               WRITE(ERR_MSG,1300) BCV, VEL, 'BC_V_s', M, BC_V_S(BCV,M)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ELSE
            BC_V_S(BCV,M) = VEL
            BC_U_S(BCV,M) = OFF * BC_U_S(BCV,M)
            BC_W_S(BCV,M) = OFF * BC_W_S(BCV,M)
         ENDIF

      ELSEIF(BC_PLANE(BCV) == 'B' .OR. BC_PLANE(BCV)== 'T') THEN
         IF(BC_W_S(BCV,M) /= UNDEFINED .AND. DO_VEL_CHECK) THEN
            IF(.NOT.COMPARE(VEL,BC_W_S(BCV,M))) THEN
               WRITE(ERR_MSG, 1300) BCV, VEL, 'BC_W_s', M, BC_W_S(BCV,M)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ELSE
            BC_W_S(BCV,M) = VEL
            BC_U_S(BCV,M) = OFF * BC_U_S(BCV,M)
            BC_V_S(BCV,M) = OFF * BC_V_S(BCV,M)
         ENDIF
      ENDIF


      CALL FINL_ERR_MSG

      RETURN

 1300 FORMAT(/1X,70('*')//' From: FLOW_TO_VEL',/' Message: BC No:',I2,/,&
         ' Computed velocity is not equal to specified value',/,&
         ' Value computed from vol. or mass flow  = ',G14.7,/,&
         ' Specified value (',A,I1,') = ',G14.7,/1X,70('*')/)

      END SUBROUTINE SOLIDS_VOLFLOW_TO_VELOCITY

MODULE REINIT
   CONTAINS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: REINITIALIZE                                            !
!  Purpose: read and verify input data, open files                     !
!                                                                      !
!  Author: P. Nicoletti                               Date: 04-DEC-91  !
!  Reviewer: M.SYAMLAL, W.ROGERS, P.NICOLETTI         Date: 24-JAN-92  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE REINITIALIZE(MFIX_DAT, IER)

      use read_namelist_mod, only: read_namelist
      use run, only: REINITIALIZING

      use error_manager

      IMPLICIT NONE

      CHARACTER(LEN=*), intent(in) :: MFIX_DAT

      INTEGER, INTENT(OUT) :: IER

      IER = 0
      REINITIALIZING = .TRUE.

! Clean up reaction data if needed
      CALL REINIT_RXN_DATA

! Read in the namelist variables from the ascii input file.
      CALL READ_NAMELIST(2, MFIX_DAT)

      CALL REINITIALIZE0(MFIX_DAT, IER)

      REINITIALIZING = .FALSE.

      RETURN
      END SUBROUTINE REINITIALIZE

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: REINITIALIZE0                                           !
!  Purpose: read and verify input data, open files                     !
!                                                                      !
!  Author: P. Nicoletti                               Date: 04-DEC-91  !
!  Reviewer: M.SYAMLAL, W.ROGERS, P.NICOLETTI         Date: 24-JAN-92  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE REINITIALIZE0(MFIX_DAT, pIER)

      use check_boundary_conditions_mod, only: check_boundary_conditions
      use check_chemical_rxns_mod, only: check_chemical_rxns
      use check_data_20_mod, only: check_data_20
      use check_data_30_mod, only: check_data_30
      use check_gas_phase_mod, only: check_gas_phase
      use check_initial_conditions_mod, only: check_initial_conditions
      use check_internal_surfaces_mod, only: check_internal_surfaces
      use check_numerics_mod, only: check_numerics
      use check_odepack_stiff_chem_mod, only: check_odepack_stiff_chem
      use check_output_control_mod, only: check_output_control
      use check_point_sources_mod, only: check_point_sources
      use check_run_control_mod, only: check_run_control
      use check_solids_phases_mod, only: check_solids_phases
      use coeff, only: INIT_COEFF
      use cutcell, only: CARTESIAN_GRID
      use error_manager
      use parse_resid_string_mod, only: parse_resid_string
      use rrates_init_mod, only: rrates_init
      use set_bc0_mod, only: set_bc0
      use set_bc1_mod, only: set_bc1
      use set_bc_flow_mod, only: set_bc_flow
      use set_constprop_mod, only: set_constprop
      use set_ic_mod, only: set_ic
      use set_mw_mix_g_mod, only: set_mw_mix_g
      use set_ps_mod, only: set_ps
      use set_ro_g_mod, only: set_ro_g
      use set_ro_s_mod, only: set_ro_s
      use write_out0_mod, only: write_out0
      use write_res0_mod, only: write_res0
      use zero_norm_vel_mod, only: zero_norm_vel

      IMPLICIT NONE

! Path to input file
      CHARACTER(LEN=*), INTENT(IN) :: MFIX_DAT

      INTEGER, INTENT(OUT) :: pIER
      INTEGER :: IER

! Set the default error flag to ERROR state.
      pIER = 1

      CALL CHECK_RUN_CONTROL()
      IF(REINIT_ERROR()) RETURN

      CALL CHECK_NUMERICS()
      IF(REINIT_ERROR()) RETURN

      CALL CHECK_OUTPUT_CONTROL()
      IF(REINIT_ERROR()) RETURN

      CALL CHECK_GAS_PHASE(MFIX_DAT)
      IF(REINIT_ERROR()) RETURN

      CALL CHECK_SOLIDS_PHASES(MFIX_DAT)
      IF(REINIT_ERROR()) RETURN

      CALL CHECK_INITIAL_CONDITIONS
      IF(REINIT_ERROR()) RETURN
      CALL CHECK_BOUNDARY_CONDITIONS
      IF(REINIT_ERROR()) RETURN
      CALL CHECK_INTERNAL_SURFACES
      IF(REINIT_ERROR()) RETURN
      CALL CHECK_POINT_SOURCES

      CALL CHECK_CHEMICAL_RXNS(MFIX_DAT)
      IF(REINIT_ERROR()) RETURN
      CALL CHECK_ODEPACK_STIFF_CHEM
      IF(REINIT_ERROR()) RETURN

! Convert (mass, volume) flows to velocities.
      CALL SET_BC_FLOW
      IF(REINIT_ERROR()) RETURN

! Set constant physical properties
      CALL SET_CONSTPROP
      IF(REINIT_ERROR()) RETURN

! Set initial conditions
      CALL SET_IC
      IF(REINIT_ERROR()) RETURN

! Set point sources.
      CALL SET_PS(MFIX_DAT)
      IF(REINIT_ERROR()) RETURN

! Set boundary conditions
      CALL ZERO_NORM_VEL
      IF(REINIT_ERROR()) RETURN
      CALL SET_BC0
      IF(REINIT_ERROR()) RETURN
      IF(CARTESIAN_GRID) CALL CG_SET_BC0
      IF(REINIT_ERROR()) RETURN

! Set gas mixture molecular weight
      CALL SET_MW_MIX_G
      IF(REINIT_ERROR()) RETURN

! Initialize densities.
      CALL SET_RO_G
      IF(REINIT_ERROR()) RETURN
      CALL SET_RO_S
      IF(REINIT_ERROR()) RETURN

! Initialize time dependent boundary conditions
      CALL SET_BC1
      IF(REINIT_ERROR()) RETURN

! Check the field variable data and report errors.
      IF(.NOT.CARTESIAN_GRID) CALL CHECK_DATA_20
      IF(REINIT_ERROR()) RETURN

! Parse residual strings
      CALL PARSE_RESID_STRING ()
      IF(REINIT_ERROR()) RETURN

      CALL RRATES_INIT
      IF(REINIT_ERROR()) RETURN

! Calculate all the coefficients once before entering the time loop
      CALL INIT_COEFF(MFIX_DAT, IER)
      IF(REINIT_ERROR()) RETURN

! After reinitialization, the field vars should pass these checks too
      CALL CHECK_DATA_30()
      IF(REINIT_ERROR()) RETURN

      CALL WRITE_RES0()
      CALL WRITE_OUT0()

! Keep a copy of field variables in case of divergence
! Needed for example if mass inlet bc velocity was changed
      CALL UPDATE_OLD

! Made it here without error.
      pIER = 0

      RETURN
      END SUBROUTINE REINITIALIZE0


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  SUBROUTINE: REINIT_RXN_DATA                                         !
!  Purpose: read and verify input data, open files                     !
!                                                                      !
!  Author: P. Nicoletti                               Date: 04-DEC-91  !
!  Reviewer: M.SYAMLAL, W.ROGERS, P.NICOLETTI         Date: 24-JAN-92  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE REINIT_RXN_DATA

      use parse, only: RXN_NAME, DES_RXN_NAME
      use parse, only: RXN_CHEM_EQ, DES_RXN_CHEM_EQ
      use parse, only: usrDH, DES_usrDH
      use parse, only: usrfDH, DES_usrfDH

      use rxns, only: NO_OF_RXNS
      use rxns, only: REACTION
      use des_rxns, only: NO_OF_DES_RXNS
      use des_rxns, only: DES_REACTION

      use error_manager

      IMPLICIT NONE

      INTEGER :: LC

! Reaction Names: Allocate/Initialize
      IF(allocated( RXN_NAME )) deallocate(RXN_NAME)
! Chemical Equations: Allocate/Initialize
      IF(allocated( RXN_CHEM_EQ )) deallocate(RXN_CHEM_EQ)
! User defined heat of reaction: Allocate/Initialize
      IF(allocated( usrDH ))deallocate(usrDH)
! User defined heat of reaction partitions: Allocate/Initialize
      IF(allocated( usrfDH )) deallocate(usrfDH)


      IF(allocated(Reaction)) THEN
         DO LC=1,NO_OF_RXNS
            IF(allocated(Reaction(LC)%HoR)) &
               deallocate(Reaction(LC)%HoR)
            IF(allocated(Reaction(LC)%rPhase)) &
               deallocate(Reaction(LC)%rPhase)
            IF(allocated(Reaction(LC)%Species)) &
               deallocate(Reaction(LC)%Species)
         ENDDO
         deallocate(Reaction)
      ENDIF
      NO_OF_RXNS = 0

! Reaction Names: Allocate/Initialize
      IF(allocated( DES_RXN_NAME )) deallocate(DES_RXN_NAME)
! Chemical Equations: Allocate/Initialize
      IF(allocated( DES_RXN_CHEM_EQ )) deallocate(DES_RXN_CHEM_EQ)
! User defined heat of reaction: Allocate/Initialize
      IF(allocated( DES_usrDH )) deallocate( DES_usrDH)
! User defined heat of reaction partitions: Allocate/Initialize
      IF(Allocated( DES_usrfDH )) deallocate( DES_usrfDH)

      IF(allocated(DES_Reaction)) THEN
         DO LC=1,NO_OF_DES_RXNS
            IF(allocated(DES_Reaction(LC)%HoR)) &
               deallocate(DES_Reaction(LC)%HoR)
            IF(allocated(DES_Reaction(LC)%rPhase)) &
               deallocate(DES_Reaction(LC)%rPhase)
            IF(allocated(DES_Reaction(LC)%Species)) &
               deallocate(DES_Reaction(LC)%Species)
         ENDDO
         deallocate(DES_Reaction)
      ENDIF
      NO_OF_DES_RXNS = 0


      RETURN
      END SUBROUTINE REINIT_RXN_DATA


END MODULE REINIT

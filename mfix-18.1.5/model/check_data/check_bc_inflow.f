!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: CHECK_BC_INFLOW                                          !
! Author: J.Musser                                    Date: 01-Mar-14  !
!                                                                      !
! Purpose: Provided a detailed error message on common inflow BC       !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_BC_INFLOW(M_TOT, SKIP, BCV)

! Modules
!---------------------------------------------------------------------//
      use bc, only: bc_x_g, bc_x_s
      use bc, only: bc_t_g, bc_t_s, bc_theta_m
      use bc, only: bc_k_turb_g, bc_e_turb_g
      use bc, only: bc_scalar
      use param, only: dim_m
      use param1, only: undefined, one, zero
      use physprop, only: inert_species
      use physprop, only: mu_g0
      use physprop, only: mw_avg, nmax
      use physprop, only: ro_g0
      use run, only: energy_eq, granular_energy
      use run, only: solids_model, solve_ros, species_eq
      use scalars, only: nscalar
      use toleranc, only: compare
      use turb, only: k_epsilon
      use error_manager
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      INTEGER, INTENT(in) :: BCV
      INTEGER, INTENT(in) :: M_TOT
      LOGICAL, INTENT(in) :: SKIP(DIM_M)

! Local variables
!---------------------------------------------------------------------//
! loop/variable indices
      INTEGER :: M, N
      DOUBLE PRECISION SUM_X
! Index of inert species
      INTEGER :: INERT
!---------------------------------------------------------------------//

      CALL INIT_ERR_MSG("CHECK_BC_INFLOW")

! Check temperature dependency.
      IF((ENERGY_EQ .OR. RO_G0==UNDEFINED .OR.MU_G0==UNDEFINED) .AND. &
         BC_T_G(BCV)==UNDEFINED) THEN
         WRITE(ERR_MSG, 1000) trim(iVar('BC_T_g',BCV))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

! Sum together defined gas phase species mass fractions.
      SUM_X = ZERO
      DO N = 1, NMAX(0)
         IF(BC_X_G(BCV,N) /= UNDEFINED) THEN
            SUM_X = SUM_X + BC_X_G(BCV,N)
         ELSE
            BC_X_G(BCV,N) = ZERO
         ENDIF
      ENDDO

! Enforce that the species mass fractions must sum to one.
      IF(.NOT.COMPARE(ONE,SUM_X)) THEN

         IF(SPECIES_EQ(0)) THEN
            WRITE(ERR_MSG, 1110) BCV
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1110 FORMAT('Error 1110: BC_X_g(',I3,',:) do NOT sum to ONE and the ',&
         'gas phase',/'species equations are solved. Please correct ', &
         'the project settings.')

         ELSEIF(RO_G0 == UNDEFINED .AND. MW_AVG == UNDEFINED) THEN
            WRITE(ERR_MSG, 1111) BCV
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1111 FORMAT('Error 1111: BC_X_g(',I3,',:) do NOT sum to ONE and the ',&
         'gas phase',/'is compressible and MW_AVG is UNDEFINED.',/     &
         'Please correct the project settings.')

         ELSEIF(.NOT.COMPARE(SUM_X,ZERO)) THEN
            WRITE(ERR_MSG, 1112) BCV
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1112 FORMAT('Error 1112: BC_X_g(',I3,',:) do not sum to ONE or ZERO ',&
         'and they',/'are not needed. Please correct ',   &
         'the project settings.')

         ELSE
            BC_X_G(BCV,:) = ZERO
            BC_X_G(BCV,1) = ONE
         ENDIF
      ENDIF


! Verify that species mass fractions are defined for mass flow BCs when
! using variable solids density. Needed to calculation RO_s
      DO M = 1, M_TOT

! If this phase is not present, clear out x_s for the BC and
! cycle the solids loop. No need to continue checks.
         IF(SKIP(M)) THEN
           IF(SPECIES_EQ(M))THEN
               BC_X_S(BCV,M,:) = ZERO
               BC_X_S(BCV,M,1) = ONE
            ENDIF
            CYCLE
         ENDIF

! Sum together defined species mass fractions.
         SUM_X = ZERO
         DO N = 1, NMAX(M)
            IF(BC_X_S(BCV,M,N) /= UNDEFINED) THEN
               SUM_X = SUM_X + BC_X_S(BCV,M,N)
            ELSE
               BC_X_S(BCV,M,N) = ZERO
            ENDIF
         ENDDO

! Enforce that the species mass fractions must sum to one.
         IF(.NOT.COMPARE(ONE,SUM_X)) THEN
            IF(SPECIES_EQ(M)) THEN
               WRITE(ERR_MSG, 1210) BCV, M
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1210 FORMAT('Error 1210: BC_X_s(',I3,',',I2,':) do NOT sum to ONE ',  &
         'and the solids phase',/'species equations are solved. ',     &
         'Please correct the project settings.')

            ELSEIF(SOLVE_ROS(M)) THEN
               WRITE(ERR_MSG, 1211) BCV, M
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1211 FORMAT('Error 1211: BC_X_s(',I3,',',I2,':) do NOT sum to ONE ',  &
         'and the solids phase',/'density is calculated. Please ',     &
         'correct the project settings.')

            ELSEIF(.NOT.COMPARE(SUM_X,ZERO)) THEN
               WRITE(ERR_MSG, 1212) BCV, M
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1212 FORMAT('Error 1212: BC_X_s(',I3,',',I2,':) do NOT sum to ONE ',  &
         'or ZERO and',/'they are not needed. Please correct the ',    &
         'project settings.')

            ELSE
               BC_X_S(BCV,M,:) = ZERO
               BC_X_S(BCV,M,1) = ONE
            ENDIF
         ENDIF

! Set the solids density for the BC region.
         IF(SOLVE_ROs(M)) THEN
! Verify that the species mass fraction for the inert material is not
! zero in the IC region when the solids is present.
            INERT = INERT_SPECIES(M)
            IF(BC_X_S(BCV,M,INERT) == ZERO) THEN
               WRITE(ERR_MSG,1213) M, BCV
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1213 FORMAT('Error 1213: No inert species for phase ',I2,' in BC ',   &
         'region',I3,'.',/'Unable to calculate solids phase density. ',&
         'Please refer to the Readme',/' file for required variable ', &
         'solids density model input parameters and',/' make the ',   &
         'necessary corrections to the project settings.')

            ENDIF
         ENDIF
      ENDDO


      DO M = 1, M_TOT
! Check solids phase temperature dependency.
         IF(ENERGY_EQ .AND. BC_T_S(BCV,M)==UNDEFINED) THEN
            IF(SKIP(M)) THEN
               BC_T_S(BCV,M) = BC_T_G(BCV)
            ELSE
               WRITE(ERR_MSG, 1000) trim(iVar('BC_T_s',BCV,M))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ENDIF

! Check granular energy dependency
         IF(GRANULAR_ENERGY) THEN
            IF(BC_THETA_M(BCV,M) == UNDEFINED) THEN
               IF(SKIP(M) .OR. SOLIDS_MODEL(M) /= 'TFM') THEN
                  BC_THETA_M(BCV,M) = ZERO
               ELSE
                  WRITE(ERR_MSG,1000) trim(iVar('BC_Theta_m',BCV,M))
                  CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
               ENDIF
            ENDIF
         ENDIF
      ENDDO

! Check K-Epsilon BCs.
      IF(K_Epsilon) THEN
         IF(BC_K_Turb_G(BCV) == UNDEFINED) THEN
            WRITE(ERR_MSG, 1000) trim(iVar('BC_K_Turb_G',BCV))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

         ELSEIF(BC_E_Turb_G(BCV) == UNDEFINED) THEN
            WRITE(ERR_MSG, 1000) trim(iVar('BC_E_Turb_G',BCV))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDIF

! Check scalar equation BCs.
      DO N = 1, NScalar
         IF(BC_Scalar(BCV,N) == UNDEFINED) THEN
            WRITE(ERR_MSG, 1001) trim(iVar('BC_Scalar',BCV,N))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDDO

      CALL FINL_ERR_MSG

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/   &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_BC_INFLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: CHECK_BC_MASS_FLOW                                       !
! Author: J.Musser                                    Date: 01-Mar-14  !
!                                                                      !
! Purpose: Provided a detailed error message on BC                     !
!                                                                      !
! Comments:                                                            !
!     The velocities at the inflow/outflow face are fixed and the      !
!     momentum equations are not solved in the MI/MO cells. Depending  !
!     on user specification (mass flow or volume flow or velocity)     !
!     different checks need to be made. Consistenty on the sum of      !
!     the volume fractions at the boundary are made as needed.         !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_BC_MASS_FLOW(M_TOT, SKIP, BCV)

! Modules
!---------------------------------------------------------------------//
      use bc, only: bc_ep_g, bc_p_g
      use bc, only: bc_rop_s, bc_ep_s, bc_x_s
      use bc, only: bc_massflow_g, bc_massflow_s
      use bc, only: bc_volflow_g, bc_volflow_s
      use eos, only: eoss
      use param, only: dim_m
      use param1, only: undefined, one, zero
      use physprop, only: ro_g0, ro_s0
      use physprop, only: inert_species, x_s0
      use run, only: solve_ros
      use toleranc, only: compare
      use usr_prop, only: usr_ros, usr_rog
      use error_manager
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      INTEGER, INTENT(in) :: BCV
      INTEGER, INTENT(in) :: M_TOT
      LOGICAL, INTENT(in) :: SKIP(DIM_M)

! Local variables
!---------------------------------------------------------------------//
! loop/variable indices
      INTEGER :: M
      DOUBLE PRECISION :: SUM_EP
! Solids phase density in BC region.
      DOUBLE PRECISION :: BC_ROs(DIM_M)
! Index of inert species
      INTEGER :: INERT
! count of number of phases skipped from boundary volume fraction
! checks due to a user variable density model
      INTEGER :: lskip

!---------------------------------------------------------------------//

      CALL INIT_ERR_MSG("CHECK_BC_MASS_FLOW")

! Check gas phase volume fraction.
! Technically, bc_ep_g should not be needed if the user specifies
! velocities. bc_ep_g is needed, however, when either bc_massflow_g
! or bc_volflow_g are set. Ultimately the check on velocities, among
! other checks, is made slightly later in set_bc_flow by
! check_bc_vel_inflow.
! TODO: This check can be lifted for the _outflow_ condition if/when
! we are able to move flow_to_vel after the check and set_ic/bc routines,
! such that bc_ep_g is set in an outflow boundary according to its
! neighbor...
      IF(BC_EP_G(BCV) == UNDEFINED) THEN
         IF(BC_MASSFLOW_G(BCV) /= UNDEFINED .AND. &
            BC_MASSFLOW_G(BCV) /= ZERO) THEN
            WRITE(ERR_MSG, 1096) trim(iVar('BC_EP_G',BCV)), &
            trim(iVar('BC_MASSFLOW_G',BCV))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
         IF(BC_VOLFLOW_G(BCV) /= UNDEFINED .AND. &
            BC_VOLFLOW_G(BCV) /= ZERO) THEN
            WRITE(ERR_MSG, 1096) trim(iVar('BC_EP_G',BCV)), &
            trim(iVar('BC_MASSFLOW_G',BCV))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF

 1096 FORMAT('Error 1096: ',A,' must be defined when requesting',/, &
         'a non-zero', A,' Please correct the project settings.')
      ENDIF


      IF(USR_ROG) THEN
! unfortunately it will be difficult to lift this restriction since
! we need ro_g in the boundary to convert mass flow rate and the
! boudnary values are the user function is based on defined field
! variables which are not initialized until well after the conversion
! is done..
! TODO: This check can be lifted for an inflow/outflow if/when we move
! flow_to_vel to after the check/set ic/bc routines (and after call to
! set_rog).
         IF(BC_MASSFLOW_G(BCV) /= UNDEFINED .AND.                   &
            BC_MASSFLOW_G(BCV) /= ZERO) THEN
            WRITE(ERR_MSG, 1098) trim(iVar('BC_MASSFLOW_G',BCV))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
 1098 FORMAT('Error 1098: ',A,' cannot be used when invoking',/, &
         'a user specified density function via USR_ROg.',&
         'The conversion',/,'to volumetric flow rate is not readily',&
         'possible.',/,'Please correct the project settings.')
      ENDIF

! Verify compressible boundary condition variables.
! TODO: It may be possible to update this check so that bc_p_g is
! not needed for usr_rog
      IF(RO_G0 == UNDEFINED) THEN
         IF(BC_P_G(BCV) == UNDEFINED) THEN
            IF(BC_MASSFLOW_G(BCV) /= UNDEFINED .AND.                   &
               BC_MASSFLOW_G(BCV) /= ZERO) THEN
               WRITE(ERR_MSG, 1100) trim(iVar('BC_P_g',BCV))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
 1100 FORMAT('Error 1100: ',A,' must be specified for compressible ',  &
         'flows',/'when specifying BC_MASSFLOW_g to make the ',        &
         'conversion to velocity.',/'Please correct the project '&
         'settings.')

         ELSEIF(BC_P_G(BCV) <= ZERO) THEN
            WRITE(ERR_MSG, 1101) BCV, trim(iVal(BC_P_G(BCV)))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
 1101 FORMAT('Error 1101: Pressure must be greater than zero for ',    &
         'compressible flow',/' >>>  BC_P_g(',I3,') = ',A,/'Please ',  &
         'correct the project settings.')
      ENDIF

! Check solids phase volume fraction; the following is a complicated
! series of checks depending on user configuration. Technically,
! bc_ep_s/bc_rop_s are only needed if bc_massflow_s or bc_volflow_s
! is specified. However, the code checks for consistency of the
! definitions whenever they are set (needed or not).

! Calculate the solids volume fraction from the gas phase if there is
! only one solids phase. Do this check regardless of whether it needs
! to be set or not.
      IF(M_TOT == 1 .AND. BC_EP_S(BCV,1) == UNDEFINED) THEN
         IF (BC_EP_G(BCV) /= UNDEFINED) BC_EP_S(BCV,1) = ONE - BC_EP_g(BCV)
      ENDIF

      DO M = 1, M_TOT

! If variable density via a udf is requested then massflow_s cannot
! be specified at this time; it would be difficult to lift this
! restriction w/o some major work around to evaluate ro_s in the
! boundary
! TODO: revisit this check if/when we can move flow_to_vel to after
! the checks/sets of ic_bc and set_ros.
         IF(USR_ROS(M)) THEN
            IF(BC_MASSFLOW_S(BCV,M) /= UNDEFINED .AND.  &
               BC_MASSFLOW_S(BCV,M) /= ZERO) THEN
               WRITE(ERR_MSG, 1199) trim(iVar('BC_MASSFLOW_S',BCV,M)), &
                  trim(iVAR('USR_ROS',M))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
 1199 FORMAT('Error 1199: ',A,' cannot be used when invoking a',/, &
         'user specified density function via', A,&
         '. The conversion',/,'to volumetric flow rate is not readily',&
         'possible.',/,'Please correct the project settings.')

            IF(BC_ROP_s(BCV,M) /= UNDEFINED) THEN
               WRITE(ERR_MSG,1200) trim(iVar('BC_ROP_s',BCV,M)), &
                  trim(iVar('USR_ROs',M))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
 1200 FORMAT('Error 1200: ',A,' must not be specified when also ',&
         'requesting ',/,A,' for the same phase. Please correct ',&
         'the project settings.')
         ENDIF

! Checking if massflow or volflow has been specified for phase m
         IF((BC_MASSFLOW_S(BCV,M) /= UNDEFINED .AND. &
             BC_MASSFLOW_S(BCV,M) /= ZERO) .OR.      &
            (BC_VOLFLOW_S(BCV,M) /= UNDEFINED .AND.  &
             BC_VOLFLOW_S(BCV,M) /= ZERO)) THEN
! If so, then check if rop_s or ep_s are also be specified or can be
! calculated based on ep_g
             IF (BC_ROP_S(BCV,M) == UNDEFINED .AND. &
                 BC_EP_S(BCV,M) == UNDEFINED) THEN
! For M>1, rop_s/ep_s can only be calculated if ep_g=1; If M=1, then
! ep_s will have already been set if undefined (see above).
                IF (BC_EP_G(BCV) /= UNDEFINED) THEN
                   IF (M_TOT > 1 .AND. .NOT. COMPARE(BC_EP_G(BCV),ONE)) THEN
                     WRITE(ERR_MSG, 1201) M, BCV, 'BC_ROP_s or BC_EP_s'
                     CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
                   ENDIF
                ELSE
! bc_ep_g is undefined and bc_rop_s/bc_ep_s is undefined while volflow_s
! and/or massflow_s are defined; error not enough information at boundary
                   WRITE(ERR_MSG, 1201) M, BCV, 'BC_ROP_s or BC_EP_s'
                   CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
                ENDIF
             ENDIF
         ELSE
! In this case velocities should be specified. Ultimately the check on
! velocities, among other checks, is made slightly later in set_bc_flow
! by check_bc_vel_inflow.
         ENDIF
      ENDDO
 1201 FORMAT('Error 1201: Insufficient solids phase ',I2,' data ',     &
         'for BC',I3,'. ',/A,' not specified.',/'Please correct the ', &
         'project settings.')

! At this point a check has been made to ensure that bc_rop_s or bc_ep_s
! have been set if they are needed. Similarly for bc_ep_g. However, if
! they are not needed then they may not necessarily be defined.

! If the user has provided bc_ep_g but not defined bc_rop_s or bc_ep_s
! (and they cannot be readily calculated based on bc_ep_g), then the
! boundary may evolve inconsistent values for these quantities (simply
! due to setting such outflow bc cells based on neighbor fluid cells
! when undefined). Even though this information is not used by any
! calculation, request the user redefine their bc more appropriately.
      IF (BC_EP_G(BCV) /= UNDEFINED) THEN
! This is a very similar check as above but covers the case when
! massflow or volflow were not specified
         IF(M_TOT > 1 .AND. .NOT.COMPARE(BC_EP_g(BCV),ONE)) THEN
! Bulk density or solids volume fraction should be explicitly defined
! if there are more than one solids phase and bc_ep_g is defined but
! not 1.
            DO M = 1, M_TOT
               IF(BC_ROP_S(BCV,M) == UNDEFINED .AND. &
                  BC_EP_S(BCV,M) == UNDEFINED) THEN
                  WRITE(ERR_MSG, 1205) BCV, M, 'BC_ROP_s or BC_EP_s'
                  CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
               ENDIF
            ENDDO
         ENDIF
 1205 FORMAT('Error 1205: Incomplete specification for BC', I3,'.',/&
         'BC_EP_G is defined but insufficient corresponding solids ',&
         'phase ',I2,' data:',/,A,' not specified. Please correct ',&
         'the project settings.')
! If bc_ep_g is 1, then we can set/clear any undefined bc_ep_s and
! bc_rop_s to zero for a consistent bc definition.
         DO M = 1, M_TOT
            IF(SKIP(M)) THEN
               BC_EP_S(BCV,M)  = ZERO
               BC_ROP_S(BCV,M) = ZERO
               CYCLE
            ENDIF
         ENDDO
      ENDIF

! At this point, if bc_ep_g was defined then bc_rop_s and/or bc_ep_s must
! also be fully specified for all phases. However, bc_ep_g and bc_rop_s/
! bc_ep_s still may not necessarily be defined if they are not needed. So
! now check if any bc_ep_s or bc_rop_s are defined. If any one phase is
! specified, then all should be set otherwise return an error. If they
! are all set, we can also calculate bc_ep_g (if undefined).
! Initialize the sum of the total volume fraction.
      SUM_EP = ZERO
      lskip = 0
      DO M = 1, M_TOT
         IF(BC_ROP_S(BCV,M) == UNDEFINED .AND. &
            BC_EP_S(BCV,M) == UNDEFINED) THEN
            lskip = lskip + 1
         ENDIF
      ENDDO

      IF (lskip /= M_TOT .AND. lskip /= ZERO) then
! Some phases are set and some are not. Return an error; do not attempt
! to guess user intention.
         WRITE(ERR_MSG, 1206) BCV, 'BC_ROP_s or BC_EP_s', M
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1206 FORMAT('Error 1206: Incomplete specification for BC', I3,'.',/&
         A,' is not specified for solids phase ',I2,' but is given ',/&
         'other solids phases. Please correct the project settings.')
      ELSEIF (lskip == M_TOT) THEN
! No phase specification has been made at the boundary, that is, all
! bc_rop_s/bc_ep_s are undefined as is bc_ep_g. This is ok. Do nothing.
! Set mass inflow routine will set bc values to neighbor fluid value.
         RETURN
      ELSEIF (lskip == zero) THEN
         IF (BC_EP_G(BCV) /= UNDEFINED) THEN
! perform checks (below) for consistency of boundary specification
! Initialize the sum of the total volume fraction.
            SUM_EP = BC_EP_G(BCV)
         ELSEIF (BC_EP_G(BCV) == UNDEFINED) THEN
! perform checks (below) for consistency, and define bc_ep_g according
! to sum of bc_rops/bc_ep_s
            SUM_EP = ZERO
         ENDIF
      ENDIF

! At this point we have made a number of checks to ensure one of the
! following:
! 1) either all bc_ep_g, bc_rop_s/bc_ep_s are defined or
! 2) all bc_rop_s/bc_ep_s are defined such that we might back out
!    bc_ep_g
! So now conduct checks to ensure any user settings of volume/void
! fraction are self-consistent (whether needed or not)
      DO M = 1, M_TOT

! Set the solids density for the BC region.
         IF(SOLVE_ROs(M)) THEN
! presence of non-zero inert species is checked by bc_inflow
            INERT = INERT_SPECIES(M)
            BC_ROs(M) = EOSS(RO_s0(M), X_s0(M,INERT),&
               BC_X_S(BCV,M,INERT))
         ELSEIF (USR_ROs(M)) THEN
! cannot readily set solids density in bc when using udf for density
! specification of rop_s will have been caught above.
         ELSE
            BC_ROs(M) = RO_s0(M)
         ENDIF

! If both input parameters are defined. Make sure they are equivalent.
! (Not possible when udf for density is requested).
         IF(BC_ROP_S(BCV,M) /= UNDEFINED .AND.                         &
            BC_EP_S(BCV,M) /= UNDEFINED) THEN

            IF(.NOT.COMPARE(BC_EP_S(BCV,M)*BC_ROs(M),                  &
               BC_ROP_S(BCV,M))) THEN
               WRITE(ERR_MSG,1214) BCV
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
 1214 FORMAT('Error 1214: Illegal initial condition region : ',I3,/    &
         'BC_EP_s and BC_ROP_s are inconsistent. Please correct the ',/&
         'project settings.')

! Compute BC_EP_s from BC_ROP_s
         ELSEIF(BC_EP_S(BCV,M) == UNDEFINED .AND. .NOT. USR_ROS(M)) THEN
! at this point bc_ep_s should have to be defined in the case of a udf
! for density if volflow_s was defined as bc_rop_s will be rejected.
            BC_EP_S(BCV,M) = BC_ROP_S(BCV,M) / BC_ROs(M)

! Compute BC_ROP_s from BC_EP_s and BC_ROs
         ELSEIF(BC_ROP_S(BCV,M) == UNDEFINED .AND. .NOT. USR_ROS(M)) THEN
! bc_ros it not defined when allowing a udf for density; so bc_rop_s will
! still be undefined here..
            BC_ROP_S(BCV,M) = BC_EP_S(BCV,M) * BC_ROs(M)

         ENDIF
! Add this phase to the total volume fraction.
         SUM_EP = SUM_EP + BC_EP_S(BCV,M)
      ENDDO

! Verify that the volume fractions sum to one.
      IF(BC_EP_G(BCV) /= UNDEFINED) THEN
         IF(.NOT.COMPARE(SUM_EP,ONE)) THEN
            WRITE(ERR_MSG,1215) BCV, trim(iVal(SUM_EP))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ELSE
        BC_EP_G(BCV) = ONE - SUM_EP
         IF( BC_EP_G(BCV) <= ZERO) THEN
            WRITE(ERR_MSG,1216) BCV, trim(iVal(SUM_EP))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDIF
 1215 FORMAT('Error 1215: Illegal boundary condition region: ',I3,'. ',&
         'Sum of volume',/'fractions does NOT equal ONE. (SUM = ',A,   &
         ')',/'Please correct the project settings.')
 1216 FORMAT('Error 1216: Illegal boundary condition region: ',I3,'. ',&
         'Sum of solids ',/,'volume fractions exceeds ONE. (SUM = ',A, &
         ')',/'Please correct the project settings.')

      CALL FINL_ERR_MSG

      RETURN

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/   &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_BC_MASS_FLOW


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: CHECK_BC_P_INFLOW                                        !
! Author: J.Musser                                    Date: 01-Mar-14  !
!                                                                      !
! Purpose: Provided detailed error message on bc                       !
!                                                                      !
! Comments:                                                            !
!     Unlike the MI boundary, for the PI boundary the velocities at    !
!     the inflow face are calculated by solving the momentum eqns      !
!     and are not fixed. In this way, the PI is similar to the PO      !
!     except that the flow is into the domain and hence all other      !
!     scalars (e.g., mass fractions, void fraction, temperature,       !
!     etc.,) at the inflow cells need to be specified. To satisfy      !
!     the error routines at the start of the simulation, both the      !
!     tangential and normal components at the inflow also need to      !
!     be specified. The velocities values essentially serve as IC.     !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE CHECK_BC_P_INFLOW(M_TOT, SKIP, BCV)

! Modules
!---------------------------------------------------------------------//
      use bc, only: bc_p_g, bc_rop_s
      use bc, only: bc_u_g, bc_v_g, bc_w_g
      use bc, only: bc_u_s, bc_v_s, bc_w_s
      use geometry, only: no_i, no_j, no_k
      use param, only: dim_m
      use param1, only: undefined, zero
      use physprop, only: ro_g0
      use error_manager
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
      INTEGER, INTENT(in) :: BCV
      INTEGER, INTENT(in) :: M_TOT
      LOGICAL, INTENT(in) :: SKIP(DIM_M)

! Local variables
!---------------------------------------------------------------------//
      INTEGER :: M

!---------------------------------------------------------------------//

      CALL INIT_ERR_MSG("CHECK_BC_P_INFLOW")

! Remove checks on bc_ep_g/bc_rop_s; using routine check_bc_outflow

      IF (BC_P_G(BCV) == UNDEFINED) THEN
         WRITE(ERR_MSG,1000) 'BC_P_g', BCV
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
         'correct the project settings.')

      ELSEIF (BC_P_G(BCV)<=ZERO .AND. RO_G0==UNDEFINED) THEN
         WRITE(ERR_MSG, 1101) BCV, trim(iVal(BC_P_G(BCV)))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1101 FORMAT('Error 1101: Pressure must be greater than zero for ',    &
         'compressible flow',/' >>>  BC_P_g(',I3,') = ',A,/'Please ',  &
         'correct the project settings.')
      ENDIF

! Check that velocities are also specified. These are essentially used
! as initial conditions for the boundary region. If they are not
! specified then a default value is set here otherwise check_data_20
! will complain and cause MFIX to exit.
      IF(BC_U_G(BCV) == UNDEFINED) THEN
         BC_U_G(BCV) = ZERO
         IF(.NOT.NO_I) THEN
            WRITE(ERR_MSG, 1300) trim(iVar('BC_U_g',BCV))
            CALL FLUSH_ERR_MSG
         ENDIF
      ENDIF

      IF(BC_V_G(BCV) == UNDEFINED) THEN
         BC_V_G(BCV) = ZERO
         IF(.NOT.NO_J) THEN
            WRITE(ERR_MSG, 1300) trim(iVar('BC_V_g',BCV))
            CALL FLUSH_ERR_MSG
         ENDIF
      ENDIF

      IF(BC_W_G(BCV) == UNDEFINED) THEN
         BC_W_G(BCV) = ZERO
         IF(.NOT.NO_K) THEN
            WRITE(ERR_MSG, 1300) trim(iVar('BC_W_g',BCV))
            CALL FLUSH_ERR_MSG
         ENDIF
      ENDIF

      DO M = 1, M_TOT
         IF (SKIP(M)) THEN
            BC_U_S(BCV,M) = ZERO
            BC_V_S(BCV,M) = ZERO
            BC_W_S(BCV,M) = ZERO
         ELSE
            IF(BC_U_S(BCV,M) == UNDEFINED) THEN
               BC_U_S(BCV,M) = ZERO
               IF(BC_ROP_S(BCV,M) /= ZERO .AND. .NOT.NO_I) THEN
                  WRITE(ERR_MSG, 1300) trim(iVar('BC_U_s',BCV,M))
                  CALL FLUSH_ERR_MSG
               ENDIF
            ENDIF

            IF(BC_V_S(BCV,M) == UNDEFINED) THEN
               BC_V_S(BCV,M) = ZERO
               IF(BC_ROP_S(BCV,M) /= ZERO .AND. .NOT.NO_J) THEN
                  WRITE(ERR_MSG, 1300) trim(iVar('BC_V_s',BCV,M))
                  CALL FLUSH_ERR_MSG
               ENDIF
            ENDIF

            IF(BC_W_S(BCV,M) == UNDEFINED) THEN
               BC_W_S(BCV,M) = ZERO
               IF(BC_ROP_S(BCV,M) /= ZERO .AND. .NOT.NO_K) THEN
                  WRITE(ERR_MSG, 1300) trim(iVar('BC_W_s',BCV,M))
                  CALL FLUSH_ERR_MSG
               ENDIF
            ENDIF
         ENDIF
      ENDDO

 1300 FORMAT('Warning 1300: ',A,' was undefined. This variable was ', &
         'set ',/ 'to zero to be used as the initial value in the BC ',&
         'region.')

      CALL FINL_ERR_MSG

      RETURN
      END SUBROUTINE CHECK_BC_P_INFLOW

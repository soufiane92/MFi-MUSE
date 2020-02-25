!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR0                                                   C
!  Purpose: This routine is called before the time loop starts and is  C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.  This        C
!           can be used for setting constants and checking errors in   C
!           data.  This routine is not called from an IJK loop, hence  C
!           all indices are undefined.                                 C
!                                                                      C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR0

! Global Variables
!---------------------------------------------------------------------//

! Global parameters
!---------------------------------------------------------------------//

! Modules procedures
!---------------------------------------------------------------------//
      use error_manager
      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//

! Functions
!---------------------------------------------------------------------//
! External Function for comparing two numbers.
      LOGICAL, EXTERNAL :: COMPARE

! Include statement functions
!---------------------------------------------------------------------//

      CALL CHECK_USR_INPUT
      CALL ALLOCATE_USR_ARRAYS
      CALL SET_INIT_USR_PROPERTIES


      RETURN
      END SUBROUTINE USR0


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: check_usr_input                                         C
!  Purpose: Check the user input that is possible for the solvent      C
!  absorption model.                                                   C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CHECK_USR_INPUT

! Global variables/parameters
!---------------------------------------------------------------------//
      use constant, only: c

      use usr, only: solvent_absorption
      use usr, only: index_sol, index_liq
      use usr, only: mech_dispersion, spread_factor
      use usr, only: enhancement_factor
      use usr, only: sa_pack, d_pack, omega_pack
      use usr, only: omega_l0
      use usr, only: lam_mu_s0, lam_mu_g0

      use usr, only: CAP_PRESS_TYPE, CAP_PRESS_TYPE_ENUM
      use usr, only: UNDEFINED_CAP_PRESS_TYPE
      use usr, only: GROSSER_1988

      use usr, only: USR_DRAG_TYPE, USR_DRAG_TYPE_ENUM
      use usr, only: UNDEFINED_USR_DRAG_TYPE
      use usr, only: ATTOU_99, ATTOU_99_MOD, SOLOMENKO_15
      use usr, only: LAPPALAINEN_09, LAPPALAINEN_09_MOD

      use usr, only: wetarea_type, wetarea_type_enum
      use usr, only: UNDEFINED_WETAREA_TYPE
      use usr, only: LAPPALAINEN_08, ONDA_68, BILLET_95, CONSTANT_WAF
      use usr, only: wetareafrac0, apply_waf, ch_pack

      use usr, only: absorption_chem_type, absorption_chem_type_enum
      use usr, only: undefined_absorption_chem_type
      use usr, only: single_step, equilibrium_segregated
      use usr, only: equilibrium_coupled

      use geometry, only: ylength, no_k
      use run, only: UNITS
      use run, only: drag_type, drag_type_enum, user_drag
      use rxns, only : NO_OF_RXNS
      use param1, only: undefined, undefined_c, zero, one
      use physprop, only: d_p0, mu_s0, mu_g0
      use error_manager
      use usr_src, only: call_usr_source
      use usr_prop, only: usr_fss
      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
! local index
      INTEGER :: L, Ll
!---------------------------------------------------------------------//

      IF (.NOT.SOLVENT_ABSORPTION) THEN
         CAP_PRESS_TYPE_ENUM = UNDEFINED_CAP_PRESS_TYPE
         USR_DRAG_TYPE_ENUM = UNDEFINED_USR_DRAG_TYPE
         ABSORPTION_CHEM_TYPE_ENUM = UNDEFINED_ABSORPTION_CHEM_TYPE
         RETURN
      ENDIF

      CALL INIT_ERR_MSG('USR0 - CHECK_USER_INPUT')


! check usr_drag_type is set to an available option
!-------------------------------------------------//
      IF (DRAG_TYPE_ENUM /= USER_DRAG) THEN
         write(err_msg, 1140)
 1140 format('Error 1140: DRAG_TYPE must be set to USER_DRAG when ',/,&
         'invoking solvent_absorption model.')
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

      IF (USR_DRAG_TYPE /= UNDEFINED_C) THEN
         SELECT CASE(trim(Adjustl(USR_DRAG_TYPE)))
         CASE ('ATTOU_99')
            USR_DRAG_TYPE_ENUM = ATTOU_99
         CASE ('ATTOU_99_MOD')
            USR_DRAG_TYPE_ENUM = ATTOU_99_MOD
         CASE ('SOLOMENKO_15')
            USR_DRAG_TYPE_ENUM = SOLOMENKO_15
            IF (.NOT.APPLY_WAF) THEN
               WRITE(ERR_MSG,1149)
               CALL FLUSH_ERR_MSG
            ENDIF
         CASE ('LAPPALAINEN_09')
            USR_DRAG_TYPE_ENUM = LAPPALAINEN_09
            IF (.NOT.CALL_USR_SOURCE(3) .OR. .NOT.CALL_USR_SOURCE(4) .OR. &
               (.NOT.CALL_USR_SOURCE(5) .AND. .NOT.NO_K)) THEN
               WRITE(ERR_MSG,1147)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
            IF (.NOT.APPLY_WAF) THEN
               WRITE(ERR_MSG,1149)
               CALL FLUSH_ERR_MSG
            ENDIF
         CASE ('LAPPALAINEN_09_MOD')
            USR_DRAG_TYPE_ENUM = LAPPALAINEN_09_MOD
            IF (.NOT.CALL_USR_SOURCE(3) .OR. .NOT.CALL_USR_SOURCE(4) .OR. &
               (.NOT.CALL_USR_SOURCE(5) .AND. .NOT.NO_K)) THEN
               WRITE(ERR_MSG,1147)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
            IF (.NOT.APPLY_WAF) THEN
               WRITE(ERR_MSG,1149)
               CALL FLUSH_ERR_MSG
            ENDIF
         CASE DEFAULT
            USR_DRAG_TYPE_ENUM = UNDEFINED_USR_DRAG_TYPE
         END SELECT

         IF (USR_DRAG_TYPE_ENUM == UNDEFINED_USR_DRAG_TYPE) THEN
            WRITE(ERR_MSG,1106) 'USR_DRAG_TYPE', &
                trim(adjustl(USR_DRAG_TYPE))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF

         IF (.NOT.USR_FSS(1)) THEN
            WRITE(ERR_MSG,1148)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ELSE
         USR_DRAG_TYPE_ENUM = UNDEFINED_USR_DRAG_TYPE
      ENDIF
 1147 format("Error 1147: the selected drag model results in ",&
        "additional",/"source terms that require CALL_USR_SOURCE,"&
        "be set to true for all momentum equations.")
 1148 format("Error 1148: USR_FSS(1) must be true when invoking ",&
        "this module",/"so that the liquid-solid drag is calculated ",&
        "appropriately.")
 1149 format("Warning 1149: APPLY_WAF should be .TRUE. to remain ",&
       "consistent",/"with the published form of the selected drag ",&
       "model.")

! check settings for 'calibration factors' for momentum interaction f
! forces
      if (c(5) == undefined) then
         write(err_msg, 1141)
         call flush_err_msg
 1141 format("Warning 1141: setting the calibration factor, c(5), to ",&
        "1 for the",/"viscous term in gas-liquid interaction model")
            c(5) = one
      elseif (c(5) < 0) then
         WRITE(err_msg, 1106) 'C(5)', ival(C(5))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      endif
      if (c(6) == undefined) then
         write(err_msg, 1142)
         call flush_err_msg
 1142 format("Warning 1142: setting the calibration factor, c(6), to ",&
        "1 for the",/"inertial term in gas-liquid interaction model")
            c(6) = one
      elseif (c(6) < 0) then
         WRITE(err_msg, 1106) 'C(6)', ival(C(6))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      endif
      if (c(7) == undefined) then
         write(err_msg, 1143)
         call flush_err_msg
 1143 format("Warning 1143: setting the calibration factor, c(7), to ",&
        "1 for the",/"viscous term in gas-solids interaction model")
            c(7) = one
      elseif (c(7) < 0) then
         WRITE(err_msg, 1106) 'C(7)', ival(C(7))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      endif
      if (c(8) == undefined) then
         write(err_msg, 1144)
         call flush_err_msg
 1144 format("Warning 1144: setting the calibration factor, c(8), to ",&
        "1 for the",/"inertial term in gas-solids interaction model")
            c(8) = one
      elseif (c(8) < 0) then
         WRITE(err_msg, 1106) 'C(8)', ival(C(8))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      endif
      if (c(9) == undefined) then
         write(err_msg, 1145)
         call flush_err_msg
 1145 format("Warning 1145: setting the calibration factor, c(9), to ",&
        "1 for the",/"viscous term in liquid-solids interaction model")
            c(9) = one
      elseif (c(9) < 0) then
         WRITE(err_msg, 1106) 'C(9)', ival(C(9))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      endif
      if (c(10) == undefined) then
         write(err_msg, 1146)
         call flush_err_msg
 1146 format("Warning 1146: setting the calibration factor, c(10), to ",&
        "1 for the",/"inertial term in liquid-solids interaction model")
            c(10) = one
      elseif (c(10) < 0) then
         WRITE(err_msg, 1106) 'C(10)', ival(C(10))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      endif


! check settings for mechanical dispersion
!-------------------------------------------------//
      IF (MECH_DISPERSION ) THEN
! currently cannot invoke mechanical dispersion unless specific user
! drag model is also invoked
         IF (USR_DRAG_TYPE_ENUM == UNDEFINED_USR_DRAG_TYPE) THEN
            write(err_msg, 1150)
 1150 format('Error 1150: If MECH_DISPERSION = .TRUE. then ',&
         'USR_DRAG_TYPE',/,'must be defined')
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF (USR_DRAG_TYPE_ENUM /= ATTOU_99 .OR. &
                 USR_DRAG_TYPE_ENUM /= ATTOU_99_MOD .OR. &
                 USR_DRAG_TYPE_ENUM /= SOLOMENKO_15 .OR. &
                 USR_DRAG_TYPE_ENUM /= LAPPALAINEN_09_MOD .OR. &
                 USR_DRAG_TYPE_ENUM /= LAPPALAINEN_09) THEN
           WRITE(ERR_MSG,1151)
 1151 format('Error 1151: Selected USR_DRAG_TYPE is not valid ',&
         'when',/'MECH_DISPERSION = .TRUE.')
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
! check value of spread_factor
         IF (spread_factor == undefined) THEN
            IF (d_pack == undefined) THEN
                write(err_msg, 1152) index_sol, ival(d_p0(index_sol))
                call flush_err_msg
                d_pack = d_p0(index_sol)
            ELSE
               IF(d_pack <= 0) THEN
                  WRITE(err_msg, 1106) 'd_pack', ival(d_pack)
                  CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
               ENDIF
            ENDIF

            IF (UNITS == 'CGS') THEN
               spread_factor = 0.15*sqrt(d_pack)
            ELSEIF (UNITS == 'SI') THEN
               spread_factor = 0.015*sqrt(d_pack)
            ENDIF
         ELSE
            IF (spread_factor < 0) THEN
               WRITE(err_msg, 1106) 'spread_factor', ival(spread_factor)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ENDIF
         IF (.NOT.CALL_USR_SOURCE(3) .OR. .NOT.CALL_USR_SOURCE(4) .OR. &
            (.NOT.CALL_USR_SOURCE(5) .AND. .NOT.NO_K)) THEN
            WRITE(ERR_MSG,1153)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDIF
 1152 format('Warning 1152: setting the nominal size of the packing ',&
            'to the ',/,'diameter of solids phase: d_pack = d_p0(',i2,&
            ') = ',A)
 1153 format("Error 1153: invoking a mechanical dispersion model ",&
        "requires",/"that CALL_USR_SOURCE be set to true for all,"&
        "momentum equations.")


! check surface tension
!-------------------------------------------------//
      IF (omega_l0 == undefined) THEN
         write(err_msg, 1160)
         call flush_err_msg
 1160 format('Warning 1160: The value of surface tension will be ',&
         'calculated ',/'based on the provided model.')
      ELSE
         IF (omega_l0 < 0) THEN
            WRITE(err_msg, 1106) 'omega_l0', ival(omega_l0)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDIF


! check wetted area fraction is set to available option
!-------------------------------------------------//
      IF (wetarea_type /= undefined_c) THEN
         SELECT CASE(trim(Adjustl(WETAREA_TYPE)))
         CASE ('ONDA_68')
            WETAREA_TYPE_ENUM = ONDA_68
         CASE ('LAPPALAINEN_08')
            WETAREA_TYPE_ENUM = LAPPALAINEN_08
         CASE ('BILLET_95')
            WETAREA_TYPE_ENUM = BILLET_95
! Must specify ch_pack if using BILLET_95 wetted area model
            IF (ch_pack == undefined .OR. ch_pack < 0) THEN
               WRITE(err_msg, 1106) 'ch_pack', ival(ch_pack)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
            IF (sa_pack == undefined .OR. sa_pack <=0) THEN
               WRITE(err_msg, 1106) 'sa_pack', ival(sa_pack)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF

         CASE ('CONSTANT')
            WETAREA_TYPE_ENUM = CONSTANT_WAF
            if (wetareafrac0 == undefined) then
               write(err_msg, 1170)
               call flush_err_msg(ABORT=.TRUE.)
 1170 format("Error 1170: Constant fraction wetted area is requested",&
        "This",/"requires defining wetareafrac0.")
            elseif (wetareafrac0 > 1 .or. wetareafrac0<=0) then
               WRITE(err_msg, 1106) 'wetareafrac0', ival(wetareafrac0)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            endif
         CASE DEFAULT
            WRITE(ERR_MSG,1106) 'WETAREA_TYPE', &
                trim(adjustl(WETAREA_TYPE))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         END SELECT
      ELSE
         WETAREA_TYPE_ENUM = UNDEFINED_WETAREA_TYPE
         write(err_msg, 1171)
 1171 format('Warning 1171: Undefined wetarea_type. No fractional ',&
         'wetted area ',/, 'factor will be used in any calculation.')
         call flush_err_msg
         wetareafrac0 = ONE
         IF (apply_WAF) THEN
            write(err_msg,1172)
 1172 format('Error 1172: Undefined wetarea_type but requesting ',&
         'apply_waf.',/, 'Clarify intent with appropriate settings.')
            call flush_err_msg(abort=.true.)
         ENDIF
         APPLY_WAF = .FALSE.
      ENDIF

! if the user chooses to apply a fractional area then they must
! select a model
      IF (apply_waf) THEN
         IF(wetarea_type_enum == undefined_wetarea_type) THEN
            write(err_msg, 1173)
 1173 format("Error 1173: Fractional wetted area is requested for ",&
        "interaction",/"terms. This requires defining WETAREA_TYPE.")
            call flush_err_msg(ABORT=.TRUE.)
         ENDIF
      ENDIF


! check capillary pressure model is set to one available
!-------------------------------------------------//
      IF (CAP_PRESS_TYPE /= UNDEFINED_C) THEN
         SELECT CASE(trim(Adjustl(CAP_PRESS_TYPE)))
         CASE ('GROSSER_1988')
            CAP_PRESS_TYPE_ENUM = GROSSER_1988
         CASE DEFAULT
            CAP_PRESS_TYPE_ENUM = UNDEFINED_CAP_PRESS_TYPE
         END SELECT

         IF (CAP_PRESS_TYPE_ENUM == UNDEFINED_CAP_PRESS_TYPE) THEN
            WRITE(ERR_MSG,1106) 'CAP_PRESS_TYPE', &
                trim(adjustl(CAP_PRESS_TYPE))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF

         IF (.NOT.CALL_USR_SOURCE(3) .OR. .NOT.CALL_USR_SOURCE(4) .OR. &
            (.NOT.CALL_USR_SOURCE(5) .AND. .NOT.NO_K)) THEN
            WRITE(ERR_MSG,1191)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ELSE
         CAP_PRESS_TYPE_ENUM = UNDEFINED_CAP_PRESS_TYPE
      ENDIF
 1191 format("Error 1191: invoking a capillary pressure model ",&
        "requires",/"that CALL_USR_SOURCE be set to true for all,"&
        "momentum equations.")

! check/set effective liquid viscosity
!-------------------------------------------------//
      IF (lam_mu_s0 == undefined) THEN
         IF (mu_s0(index_liq) == undefined) THEN
            write(err_msg, 1111)
            call flush_err_msg
 1111 format('Warning 1111: The viscosity of the liquid used in the ',&
         'liquid',/,'solids drag correlation is set to the calculated ',&
         'viscosity',/,'of the liquid phase: lambda_mu_s0 = mu_s = ',&
         'calculated.')
         ELSE
            write(err_msg, 1112) index_liq, trim(ival(mu_s0(index_liq)))
            call flush_err_msg
 1112 format('Warning 1112: The viscosity of the liquid used in the ',&
         'liquid',/,'solids drag correlation is set to the viscosity ',&
         'of the liquid',/,'phase: lam_mu_s0 = mu_s0(',i2,') = ',A)
         ENDIF
      ELSE
         IF (lam_mu_s0 < 0) THEN
            WRITE(err_msg, 1106) 'lam_mu_s0', ival(lam_mu_s0)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
         IF (MU_s0(index_liq) /= undefined) THEN
            write(err_msg, 1113) trim(ival(lam_mu_s0)), index_liq, &
               trim(ival(mu_s0(index_liq)))
            call flush_err_msg
 1113 format('Warning 1113: The viscosity of the liquid phase used ',&
         'in the drag ',/,'correlation (lam_mu_s0= ',A,') is set ',&
         'independent of the ',/,'viscosity used in the stress ',&
         'calculation (mu_s0(',i2,')= ', A,').')
         ELSE
            write(err_msg, 1114) trim(ival(lam_mu_s0))
            call flush_err_msg
 1114 format('Warning 1114: The viscosity of the liquid phase used ',&
         'in the drag ',/,'correlation (lam_mu_s0= ',A,') is set ',&
         'independent of the ',/,'viscosity used in the stress ',&
         'calculation (mu_s= calculated).')
         ENDIF
      ENDIF


! check/set effective gas viscosity
!-------------------------------------------------//
      IF (lam_mu_g0 == UNDEFINED) THEN
         IF (MU_G0 == UNDEFINED) THEN
            write(err_msg, 1115)
            call flush_err_msg
 1115 format('Warning 1115: The viscosity of the gas used in the gas-',&
         'solids',/,'drag correlation is set to the calculated visco',&
         'sity of the',/, 'gas phase: lam_mu_g0 = mu_g = calculated.')
         ELSE
            write(err_msg, 1116) trim(ival(mu_g0))
            call flush_err_msg
 1116 format('Warning 1116: The viscosity of the gas used in the gas-',&
         'solids',/,'drag correlation is set to the viscosity of the ',&
         'gas phase: ',/,'lam_mu_g0 = mu_g0 = ',A)
            lam_Mu_g0 = mu_g0
         ENDIF
      ELSE
         IF (lam_mu_g0 < 0) THEN
            WRITE(err_msg, 1106) 'lam_mu_g0', ival(lam_mu_g0)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
         IF (mu_g0 /= undefined) THEN
            write(err_msg, 1117) trim(ival(lam_mu_g0)), &
               trim(ival(mu_g0))
            call flush_err_msg
 1117 format('Warning 1117: The viscosity of the gas phase used ',&
         'in the drag ',/,'correlation (lam_mu_g0= ',A,') is set ',&
         'independent of the ',/,'viscosity used in the stress ',&
         'calculation (mu_g0= ', A,').')
         ELSE
            write(err_msg, 1118) trim(ival(lam_mu_g0))
            call flush_err_msg
 1118 format('Warning 1118: The viscosity of the gas phase used ',&
         'in the drag ',/,'correlation (lam_mu_g0= ',A,') is set ',&
         'independent of the ',/,'viscosity used in the stress ',&
         'calculation (mu_g= calculated).')
         ENDIF
      ENDIF



! check reaction model specific quantities
!-------------------------------------------------//
      IF (NO_OF_RXNS > 0) THEN

! check absorption_chem_type is set to available option
!-------------------------------------------------//
         IF (ABSORPTION_CHEM_TYPE /= UNDEFINED_C) THEN
            SELECT CASE(trim(Adjustl(ABSORPTION_CHEM_TYPE)))
            CASE ('SINGLE_STEP')
               ABSORPTION_CHEM_TYPE_ENUM = SINGLE_STEP
               IF (ENHANCEMENT_FACTOR /= UNDEFINED) THEN
                  IF (ENHANCEMENT_FACTOR /= ONE) THEN
                    write(err_msg, 1188) 'ENHANCEMENT_FACTOR',&
                       ival(ENHANCEMENT_FACTOR)
 1188 format("Error 1188: ",A,"=",A,". For consistency with",/&
             "ABSORPTION_CHEM_TYPE=""SINGLE_STEP"", ENHANCEMENT_",&
             "FACTOR should be ",/,"set to 1. Please correct the ",&
             "input file.")
                     CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
                  ENDIF
               ELSE
                  write(err_msg, 1189)
 1189 format("Warning 1189: For consistency with ABSORPTION_CHEM_",&
             "TYPE=",/,"""SINGLE_STEP"", ENHANCEMENT_FACTOR is set ",&
             "1.")
                  call flush_err_msg
                  ENHANCEMENT_FACTOR = ONE
               ENDIF
            CASE ('EQUILIBRIUM_SEGREGATED')
               ABSORPTION_CHEM_TYPE_ENUM = EQUILIBRIUM_SEGREGATED
            CASE ('EQUILIBRIUM_COUPLED')
               ABSORPTION_CHEM_TYPE_ENUM = EQUILIBRIUM_COUPLED
            CASE DEFAULT
               ABSORPTION_CHEM_TYPE_ENUM=UNDEFINED_ABSORPTION_CHEM_TYPE
            END SELECT
         ELSE
            ABSORPTION_CHEM_TYPE_ENUM=UNDEFINED_ABSORPTION_CHEM_TYPE
         ENDIF
         IF (ABSORPTION_CHEM_TYPE_ENUM == &
            UNDEFINED_ABSORPTION_CHEM_TYPE) THEN
            WRITE(ERR_MSG,1106) 'ABSORPTION_CHEM_TYPE', &
                trim(adjustl(ABSORPTION_CHEM_TYPE))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF

         IF (ENHANCEMENT_FACTOR < ZERO) THEN
            WRITE(ERR_MSG,1106) 'ENHANCEMENT_FACTOR', &
               iVal(ENHANCEMENT_FACTOR)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF

! set defaults for equilbrium chemistry if using the
! EQUILIBRIUM_SEGREGATED scheme
         IF(ABSORPTION_CHEM_TYPE_ENUM == EQUILIBRIUM_SEGREGATED) THEN
            if (c(11) == undefined) then
               write(err_msg, 1183)
               call flush_err_msg
 1183 format("Warning 1183: setting the forward reaction rate constant",&
           " C(11) for",/"the carabmate (RNHCOO-) reversion equilbrium ",&
           "reaction to 1.")
               c(11) = one
            elseif (c(11) <= 0) then
               WRITE(err_msg, 1106) 'C(11)', ival(C(11))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            endif

            if (c(12) == undefined) then
               write(err_msg, 1184)
               call flush_err_msg
 1184 format("Warning 1184: setting the forward reaction rate constant",&
           " C(12) for",/"the dissociation of dissolved CO2 equilbrium",&
           " reaction to 1.")
               c(12) = one
            elseif (c(12) <= 0) then
               WRITE(err_msg, 1106) 'C(12)', ival(C(12))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            endif

            if (c(13) == undefined) then
               write(err_msg, 1185)
               call flush_err_msg
 1185 format("Warning 1185: setting the forward reaction rate constant",&
           " C(13) for",/"the dissociation of bicarbonate (HCO3-)",&
           " equilbrium reaction to 1.")
               c(13) = one
            elseif (c(13) <= 0) then
               WRITE(err_msg, 1106) 'C(13)', ival(C(13))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            endif

            if (c(14) == undefined) then
               write(err_msg, 1186)
               call flush_err_msg
 1186 format("Warning 1186: setting the forward reaction rate constant",&
           " C(14) for",/"the dissociation of protonated MEA (RNH2)",&
           " equilbrium reaction to 1.")
               c(14) = one
            elseif (c(14) <= 0) then
               WRITE(err_msg, 1106) 'C(14)', ival(C(14))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            endif

            if (c(15) == undefined) then
               write(err_msg, 1187)
               call flush_err_msg
 1187 format("Warning 1187: setting the forward reaction rate constant",&
        " C(15) for",/"the ionization of water equilibrium reaction",&
        " to 1.")
               c(15) = one
            elseif (c(15) <= 0) then
               WRITE(err_msg, 1106) 'C(15)', ival(C(15))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            endif
         ENDIF   ! endif EQUILIBRIUM_SEGREGATED scheme


! check user settings for additional calibration parameters
!-------------------------------------------------
         if (c(1) == undefined) then
            write(err_msg, 1180)
            call flush_err_msg
 1180 format("Warning 1180: setting the calibration factor, c(1), to ",&
           "1 for Henry's",/"constant for CO2 in aequous MEA.")
               c(1) = one
            elseif (c(1) <= 0) then
               WRITE(err_msg, 1106) 'C(1)', ival(C(1))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         endif

         if (c(2) == undefined) then
             write(err_msg, 1181)
             call flush_err_msg
 1181 format('Warning 1181: setting the calibration factor, c(2), to ',&
         '1 for the ',/'gas phase mass transfer coefficient.')
             c(2) = one
         elseif (c(2) <= 0) then
            WRITE(err_msg, 1106) 'C(2)', ival(C(2))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         endif

         if (c(3) == undefined) then
             write(err_msg, 1182)
             call flush_err_msg
 1182 format('Warning 1182: setting the calibration factor, c(3), to ',&
         '1 for the ',/'liquid phase mass transfer coefficient.')
             c(3) = one
         elseif (c(3) <= 0) then
            WRITE(err_msg, 1106) 'C(3)', ival(C(3))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         endif

! must select a wetted area model
! prior checks have already been conducted on wetted area selection
! as needed for pure hydrodynamics; here are a few additional checks
! needed as chemistry is included
         IF(wetarea_type_enum == undefined_wetarea_type) THEN
            write(err_msg, 1108) 'WETAREA_TYPE'
            call flush_err_msg(ABORT=.TRUE.)
         ENDIF

         IF (sa_pack == undefined) THEN
            WRITE(err_msg, 1108) 'sa_pack'
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSE
            IF(sa_pack <= 0) THEN
               WRITE(err_msg, 1106) 'sa_pack', ival(sa_pack)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ENDIF

         IF (d_pack == undefined) THEN
             write(err_msg, 1152) index_sol, ival(d_p0(index_sol))
             call flush_err_msg
             d_pack = d_p0(index_sol)
         ELSE
            IF(d_pack <= 0) THEN
               WRITE(err_msg, 1106) 'd_pack', ival(d_pack)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ENDIF

         IF (omega_pack == undefined) THEN
            WRITE(err_msg, 1108) 'omega_pack'
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSE
            IF(omega_pack < 0) THEN
               WRITE(err_msg, 1106) 'omega_pack', ival(omega_pack)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ENDIF

      ENDIF   ! end if no_of_rxns >0

 1106 FORMAT('Error 1106: Illegal or unknown input: ',A,' = ',A,/ &
         'Please correct the input file')

 1108 FORMAT('Error 1108: Required input not specified: ', A,/,&
         'Please correct the input file.')

      CALL FINL_ERR_MSG

      RETURN
      END SUBROUTINE CHECK_USR_INPUT


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: allocate_usr_arrays                                     C
!  Purpose: Allocate user arrays                                       C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE ALLOCATE_USR_ARRAYS

! Global variables/parameters
!---------------------------------------------------------------------//
      use usr, only: Omega_L, fWetArea_Pack
      use param, only: dimension_3
      use error_manager
      IMPLICIT NONE
!---------------------------------------------------------------------//

! Surface tension
      Allocate( Omega_L(DIMENSION_3) )
! Wetted area
      Allocate( fWetArea_Pack(DIMENSION_3) )

      RETURN
      END SUBROUTINE ALLOCATE_USR_ARRAYS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: set_init_usr_propertie                                  C
!  Purpose: Set/Initialize certain user properties                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE SET_INIT_USR_PROPERTIES

! Global variables/parameters
!---------------------------------------------------------------------//
      use compar, only: ijkstart3, ijkend3
      use functions, only: fluid_at
      use param1, only: zero, undefined
      use usr, only: Omega_L, omega_l0
      use usr, only: fWetArea_Pack
      use error_manager
      IMPLICIT NONE

! Local variables
!---------------------------------------------------------------------//
! index
      INTEGER :: IJK
!---------------------------------------------------------------------//

      omega_l = zero
      fWetArea_Pack = ZERO

      DO IJK = IJKSTART3,IJKEND3
         IF (FLUID_AT(IJK)) THEN
            IF (omega_l0 /= UNDEFINED) omega_l(IJK) = omega_l0
         ENDIF
      ENDDO

      RETURN
      END SUBROUTINE SET_INIT_USR_PROPERTIES

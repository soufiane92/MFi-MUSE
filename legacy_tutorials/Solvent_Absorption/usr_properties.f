!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_ROg                                            !
!  Purpose: User hook for calculating the gas phase density.           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_ROg(IJK)

! Modules
!---------------------------------------------------------------------//
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use fldvar, only: t_g, x_g, p_g, ro_g
      use param1, only: undefined_i, zero, one, half
      use physprop, only: mw_g, mw_avg, nmax
      use run, only: units
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=40) :: err_prop
!......................................................................!
! if using this quantity then remove definition of ier
      ier = 1

! Assign the fluid density
      RO_G(IJK) = ZERO

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("gas density")')
         CALL INIT_ERR_MSG('USR_PROP_ROg')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_ROg


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_CPg                                            !
!  Purpose: User hook for calculating the gas phase constant pressure  !
!  specific heat.                                                      !
!                                                                      !
!  Comments:                                                           !
!  - The specific heat assigned in this routine only applies to the    !
!    mixture average specific heat invoked in the general energy       !
!    equations. MFIX has no global variable representing species       !
!    specific heats. The reason for this limitation is explained.      !
!                                                                      !
!    Species specific heat values are locally evaluated based on a     !
!    specific polynominal format set by the Burcat database. Values    !
!    for the polynominal coefficients are read from either the         !
!    database or the mfix.dat. Species specific heats are needed by    !
!    reacting flow simulations.                                        !
!                                                                      !
!    Inconsistencies may arise in reacting flow systems if a user      !
!    specifies a phase average specific heat that is not consistent    !
!    with the values of the species specific heats that comprise that  !
!    phase.                                                            !
!    - IT may be possible to circumvent the matter long as the formula !
!      for the specific heat follows the burcat database form and      !
!      the quantites Thigh(M,N), Tlow(M,N), Tcom, Alow(M,N) and        !
!      Ahigh(M,N) are all appropriately assigned. However, the same    !
!      can be achieved by simply entering the data into the mfix.dat   !
!      as indicated by the user guide.                                 !
!    - To permit different forms of the polynominal for species        !
!      specific heats would require more code development to ensure    !
!      reacting flow simulations evaluate/reference the species        !
!      specific heats accordingly.                                     !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_CPg(IJK)

! Modules
!---------------------------------------------------------------------//
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use constant, only: RGAS => GAS_CONST_cal
      use fldvar, only: t_g, x_g
      use param1, only: undefined_i, zero, one, half
      use physprop, only: mw_g, c_pg, nmax
      use read_thermochemical, only: calc_CpoR
      use run, only: units
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=40) :: err_prop
!......................................................................!
! if using this quantity then remove definition of ier
      ier = 1

! Assign the fluid density
      C_PG(IJK) = ZERO

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("gas specific heat")')
         CALL INIT_ERR_MSG('USR_PROP_CPg')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_CPg


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_Mug                                            !
!  Purpose: User hook for calculating the gas phase viscosity.         !
!                                                                      !
!  Comments:                                                           !
!  - Assign a value to gas phase viscosity (mu_g). MFIX uses the       !
!    concept of second viscosity (lambda_g) for its internal           !
!    calculations of stress. Second viscosity is defined as follows:   !
!       lambda_g = mu_gbulk - 2/3mu_g                                  !
!       where mu_gbulk is the gas phase bulk viscosity.                !
!    MFIX automatically calculates a value for the gas phase second    !
!    viscosity by assuming the gas phase bulk viscosity is zero. As a  !
!    result, gas phase second  viscosity is evaluated as follows:      !
!       lambda_g = -2/3mu_g                                            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_Mug(IJK)

! Modules
!---------------------------------------------------------------------//
      use constant, only: to_si
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use fldvar, only: t_g, x_g, p_g, ro_g
      use param1, only: undefined_i, zero, one, half
      use physprop, only: mu_g, mw_g, mw_avg, nmax
      use run, only: units
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=40) :: err_prop
!......................................................................!
! if using this quantity then remove definition of ier
      ier = 1

! Assign the fluid viscosity
      Mu_g(IJK) = ZERO

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("gas viscosity")')
         CALL INIT_ERR_MSG('USR_PROP_MUG')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_Mug


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_Kg                                             !
!  Purpose: User hook for calculating the gas phase conductivity.      !
!                                                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_Kg(IJK)

! Modules
!---------------------------------------------------------------------//
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use fldvar, only: t_g, x_g, p_g, ro_g
      use param1, only: undefined_i, zero, one, half
      use physprop, only: k_g, mw_g, mw_avg, nmax
      use run, only: units
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=40) :: err_prop
!......................................................................!
! if using this quantity then remove definition of ier
      ier = 1

! Assign the fluid conductivity
      K_g(IJK) = ZERO

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("gas conductivity")')
         CALL INIT_ERR_MSG('USR_PROP_KG')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_Kg


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_Difg                                           !
!  Purpose: User hook for calculating the diffusivity of the gas phase !
!  species.                                                            !
!                                                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_Difg(IJK,N)

! Modules
!---------------------------------------------------------------------//
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use fldvar, only: t_g, x_g, p_g, ro_g, rop_g
      use param1, only: undefined_i, zero, one, half
      use physprop, only: dif_g, mw_g, mw_avg, nmax
      use run, only: units
      use scales, only: unscale_pressure
      use toleranc, only: zero_x_gs
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK
! species index
      INTEGER, INTENT(IN) :: N

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=40) :: err_prop
!......................................................................!
! if using this quantity then remove definition of ier
      ier = 1

! Assign the fluid phase species diffusivity
      Dif_g(IJK,N) = ZERO

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("gas phase species ",I2," diffusivity")') N
         CALL INIT_ERR_MSG('USR_PROP_DIFG')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_Difg


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_ROs                                            !
!  Purpose: User hook for calculating solids phase density.            !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_ROs(IJK,M)

! Modules
!---------------------------------------------------------------------//
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use fldvar, only: ro_s, T_s, X_s, rop_s
      use param1, only: undefined_i, zero, one, half
      use physprop, only: mw_s, nmax
      use run, only: units
      use usr, only: index_liq
      use usr, only: calc_rho_liq, calc_mw_mix_l
      use usr, only: solvent_absorption
      use toleranc, only: tmax
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK
! solids phase index
      INTEGER, INTENT(IN) :: M

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=40) :: err_prop

! bounded phase temperatures (K)
      DOUBLE PRECISION :: xTl
! liquid phase mole fraction
      DOUBLE PRECISION :: y_lH2O
      DOUBLE PRECISION :: y_lRNH2
      DOUBLE PRECISION :: y_lCO2
! liquid phase average molecular weight (g/gmol)
      DOUBLE PRECISION :: MW_MIX_L
! bulk density of liquid
      DOUBLE PRECISION :: ep_liq
! MFIX solids phases indices representing liquid phase
      INTEGER :: iliquid
! Minimum phase volume fraction required for density calculation
      DOUBLE PRECISION, parameter :: v_Limiter = 1.0d-6
! density of water (g/cm^3)
      DOUBLE PRECISION, PARAMETER :: rho_h2o = 1.d0
! minimum bulk density required for density calculation
      DOUBLE PRECISION :: minRops

      INCLUDE 'species.inc'
!......................................................................!
! if using this quantity then remove definition of ier
!      ier = 1

      IF (.NOT. SOLVENT_ABSORPTION) RETURN
      iliquid=index_liq

! exit routine if not being called for liquid phase only
      IF (M /= iliquid) ier =1

! initialize
      y_lCO2 = zero
      y_lRNH2 = zero
      y_lH2O = zero

! Volume fraction of liquid and bed
! (Volume fraction may not be accessible the first time this routine
!  is called and therefore should be avoided)
      minRops = rho_h2O*v_Limiter

! if the fluid cell contains the liquid phase, calculate the properties
! otherwise assume density of water
      IF (ROP_s(ijk,iliquid) > minRops) THEN

! bounded liquid phase temperature
         xTl = min(TMAX,T_s(IJK,iliquid))

! Compute average molecular weight of liquid phase
         MW_MIX_L = calc_MW_mix_l(ijk, iliquid)

! compute the mole fraction of following liquid components
         y_lCO2 = X_s(IJK,iliquid,lCO2)*&
                  (MW_MIX_L/MW_s(iliquid,lCO2))     ! (mol-CO2/mol-Mix)

         y_lRNH2 = X_s(IJK,iliquid,lRNH2)*&
                  (MW_MIX_L/MW_s(iliquid,lRNH2))    ! (mol-RNH2/mol-Mix)

         y_lH2O = X_s(IJK,iliquid,lH2O)*&
                  (MW_MIX_L/MW_s(iliquid,lH2O))     ! (mol-H2O/mol-Mix)

! Assign the fluid density
         RO_S(IJK,M) =  calc_rho_liq(ijk, xTl, y_lrnh2, y_lh2o, y_lco2,&
                                     iliquid)
      ELSE
! when the liquid volume fraction is very dilute set the liquid density
! to that of water until a better mechanism can be devised for the very
! dilute limit. note that simply assigning a zero value will cause many
! error messages to be reported. this may not be necessary provided
! mass fraction of h2o is always present.
         RO_S(IJK,M) = rho_h2O
         IF (UNITS=='SI') RO_S(IJK,M)=RO_S(IJK,M)*1000.d0
      ENDIF


      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("solids phase ",I2," density")') M
         CALL INIT_ERR_MSG('USR_PROP_ROS')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_ROs


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_CPs                                            !
!  Purpose: User hook for calculating solids phase constant pressure   !
!  specific heat.                                                      !
!                                                                      !
!  Comments:                                                           !
!  - See comments under USER_PROP_CPg                                  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_CPs(IJK, M)
! Modules
!---------------------------------------------------------------------//
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use constant, only: RGAS => GAS_CONST_cal
      use fldvar, only: t_s, x_s
      use param1, only: undefined_i, zero, one, half
      use physprop, only: mw_s, c_ps, nmax
      use read_thermochemical, only: calc_CpoR
      use run, only: units
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK
! solids phase index
      INTEGER, INTENT(IN) :: M

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=40) :: err_prop
!......................................................................!
! if using this quantity then remove definition of ier
      ier = 1

! Assign the fluid density
      C_PS(IJK,M) = ZERO

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("solids phase ",I2," specific heat")') M
         CALL INIT_ERR_MSG('USR_PROP_CPs')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_CPs


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_Mus                                            !
!  Purpose: User hook for calculating the solids phase viscosity.      !
!                                                                      !
!  Comments:                                                           !
!  - Assign a value to solids phase viscosity (mu_g), second viscosity !
!    (lambda_s) and solids pressure (p_s). MFIX uses the concept of    !
!    second viscosity for its internal calculations of stress. Second  !
!    viscosity is defined as follows:                                  !
!       lambda_s = mu_sbulk - 2/3 mu_s                                 !
!       where mu_sbulk is the solids phase bulk viscosity.             !
!                                                                      !
!  - In assigning a value to solids pressure consider how MFIX's       !
!    governing equations have been posed and what physics at are       !
!    involved. Nominally the governing equations are written assuming  !
!    a gas-solids system wherein the gradient in gas phase pressure    !
!    is present in the solids phase momentum balances.                 !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_Mus(IJK,M)

! Modules
!---------------------------------------------------------------------//
      use constant, only: to_si
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use fldvar, only: t_s, x_s, rop_s, ro_s, ep_s, p_s
      use param1, only: undefined_i, zero, one, half
      use physprop, only: mw_s, nmax
      use run, only: units
      use visc_s, only: mu_s, lambda_s
      use usr, only: index_liq
      use usr, only: calc_mu_liq, calc_mw_mix_l
      use usr, only: solvent_absorption
      use toleranc, only: tmax
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK
! solids phase index
      INTEGER, INTENT(IN) :: M

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=40) :: err_prop

! bounded phase temperatures (K)
      DOUBLE PRECISION :: xTl
! liquid phase mole fraction
      DOUBLE PRECISION :: y_lCO2
      DOUBLE PRECISION :: y_lRNH2
! load of CO2 in aq amine (gmol CO2/gmol amine)
      DOUBLE PRECISION :: load_CO2
! mass percentage of amine (%)
      DOUBLE PRECISION :: xp_lRNH2
! liquid phase average molecular weight (g/gmol)
      DOUBLE PRECISION :: MW_MIX_L
! volume fraction of liquid volume fraction of liquid
      DOUBLE PRECISION :: ep_liq
!
      INTEGER :: iliquid
! Minimum phase volume fraction required for density calculation
      DOUBLE PRECISION, parameter :: v_Limiter = 1.0d-6
! viscosity of water (poise or g/cm.s)
      DOUBLE PRECISION, PARAMETER :: mu_H2O = 0.01d0

      INCLUDE 'species.inc'
!......................................................................!
! if using this quantity then remove definition of ier
!      ier = 1

      IF (.NOT.SOLVENT_ABSORPTION) RETURN

      iliquid=index_liq
! routine will fail if not being called for liquid phase only
      IF (M /= iliquid) ier =1

! initialize
      y_lCO2 = zero
      y_lRNH2 = zero
      load_CO2 = zero
      xp_lRNH2 = zero

! if the fluid cell contains the liquid phase, calculate the properties
! otherwise assume density of water
      IF (ep_liq > v_limiter) THEN
! mass percentage of mea
         xp_lRNH2 = X_S(IJK,iliquid,lRNH2)*100

! bounded liquid phase temperature
         xTl = min(TMAX,T_s(IJK,iliquid))

! Compute average molecular weight of liquid phase
         MW_MIX_L = calc_MW_mix_l(ijk, iliquid)

! compute the mole fraction of following liquid components
         y_lCO2 = X_s(IJK,iliquid,lCO2)*&
                  (MW_MIX_L/MW_s(iliquid,lCO2))     ! (mol-CO2/mol-Mix)

         y_lRNH2 = X_s(IJK,iliquid,lRNH2)*&
                  (MW_MIX_L/MW_s(iliquid,lRNH2))    ! (mol-RNH2/mol-Mix)

! CO2 loading
         IF (Y_LRNH2 > ZERO) THEN
            load_CO2 = y_lco2/y_lrnh2
         ELSE
            load_CO2 = zero
         ENDIF

! viscosity of liquid phase
         mu_s(ijk,M) = calc_mu_liq(xTl, xp_lrnh2, load_CO2)

      ELSE
         mu_s(IJK,M) = mu_h2O    ! in poise or g/cm.s
         IF (UNITS=='SI') Mu_s(IJK,M)=Mu_s(IJK,M)/10.d0
      ENDIF

! Assign second viscosity and pressure:
! Here it is assumed that the bulk viscosity is zero:
      LAMBDA_S(IJK,M) = -2./3.*MU_S(IJK,M)
! At this point a capillary pressure model is used to supply the
! liquid phase pressure bsed on the gas phase pressure. so do
! nothing here.
      P_S(IJK,M) = ZERO

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("solids phase ",I2," viscosity")') M
         CALL INIT_ERR_MSG('USR_PROP_Mus')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_Mus


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_Ks                                             !
!  Purpose: User hook for calculating solids phase conductivity.       !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_Ks(IJK,M)

! Modules
!---------------------------------------------------------------------//
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use fldvar, only: ro_s, T_s, X_s, ep_s, ep_g, rop_s
      use param1, only: undefined_i, zero, one, half
      use physprop, only: mw_s, nmax, k_s, k_g
      use run, only: units
      use usr, only: solvent_absorption
      use usr, only: index_liq
      use usr, only: calc_cond_liq
      use toleranc, only: dil_ep_s, tmax
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK
! solids phase index
      INTEGER, INTENT(IN) :: M

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=40) :: err_prop
! bounded phase temperatures (K)
      DOUBLE PRECISION :: xTl
! liquid phase mass fraction
      DOUBLE PRECISION :: x_lH2O
      DOUBLE PRECISION :: x_lRNH2
! volume fraction of liquid volume fraction of liquid
      DOUBLE PRECISION :: ep_liq
! thermal conductivity
      DOUBLE PRECISION :: K_aqMEA

      INTEGER :: iliquid
      INCLUDE 'species.inc'
!......................................................................!
! if using this quantity then remove definition of ier
!      ier = 1
      IF (.NOT.SOLVENT_ABSORPTION) RETURN

      iliquid=index_liq
! routine will fail if not being called for liquid phase only
      IF (M /= iliquid) ier =1

! bounded liquid phase temperature
      xTl = min(TMAX,T_s(IJK,iliquid))

! mass fraction of MEA
      x_lRNH2 = X_S(IJK,iliquid,lRNH2)

! mass fraction of H2O
      x_lH2O = X_S(IJK,iliquid,lH2O)

! conductivity of MEA solution
      K_aqMEA = calc_cond_liq(xTl, x_lrnh2, x_lh2o)

! Assign the fluid phase thermal conductivity
      K_S(IJK,M) = K_aqMEA

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("solids phase ",I2," conductivity")') M
         CALL INIT_ERR_MSG('USR_PROP_KS')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_Ks


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_Difs                                           !
!  Purpose: User hook for calculating diffusivity of 'solids' phase    !
!  species.                                                            !
!                                                                      !
!  Comments:                                                           !
!  - As always consider whether such a term is meaningful in the       !
!    system being modeled.                                             !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_Difs(IJK,M,N)

! Modules
!---------------------------------------------------------------------//
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use fldvar, only: ro_s, T_s, X_s, ep_s, ep_g, rop_s
      use param1, only: undefined_i, zero, one, half
      use physprop, only: mw_s, nmax, dif_s
      use run, only: units
      use toleranc, only: tmax
      use usr, only: index_liq
      use usr, only: calc_D_CO2inMEA
      use usr, only: solvent_absorption
      use visc_s, only: mu_s
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK
! solids phase index
      INTEGER, INTENT(IN) :: M
! species index
      INTEGER, INTENT(IN) :: N

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=40) :: err_prop
! bounded phase temperatures (K)
      DOUBLE PRECISION :: xTl
! viscosity of liquid phase
      DOUBLE PRECISION :: mu_liq
! diffusivity
      DOUBLE PRECISION :: D_CO2inMEA
! mfix index of liquid phase
      INTEGER :: iliquid
!......................................................................!
! if using this quantity then remove definition of ier
!      ier = 1

      IF (.NOT.SOLVENT_ABSORPTION) RETURN

      iliquid=index_liq
! routine will fail if not being called for liquid phase only
      IF (M /= iliquid) ier =1

! bounded liquid phase temperature
      xTl = min(TMAX,T_s(IJK,iliquid))

! viscosity of liquid phase
! diffusivity of CO2 in MEA solution
      D_CO2inMEA = calc_D_CO2inMEA(xTl, mu_liq)

! Assign the fluid phase diffusivity
! multiphase by rop_s at this time for unit consistency
      DIF_S(IJK,M,N) = D_CO2inMEA*ROP_S(IJK,iliquid)

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("solids phase ",I2," species ", I2, &
                           " diffusivity")') M, N
         CALL INIT_ERR_MSG('USR_PROP_DIFS')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_Difs


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_Gama                                           !
!  Purpose: User hook for calculating the gas-solids heat transfer     !
!  coefficient due to relative temperature differences between the     !
!  gas phase (M=0) and each 'solids' phase (M=1 to MMAX)               !
!                                                                      !
!  Comments:                                                           !
!  - No solids-solids heat transfer is allowed at this time. To        !
!    account for this term would require appropriate closure and       !
!    additional code development.                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_Gama(IJK,M)

! Modules
!---------------------------------------------------------------------//
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use energy, only: gama_gs
      use fldvar, only: u_g, v_g, w_g, u_s, v_s, w_s
      use fldvar, only: ep_s, ep_g, rop_s, rop_g, ro_s, ro_g
      use fldvar, only: T_s, T_g, d_p
      Use fun_avg, only: avg_x_e, avg_y_n, avg_z_t
      use functions, only: im_of, jm_of, km_of
      use indices, only: i_of
      use param1, only: undefined_i, zero, one, half
      use param1, only: small_number, large_number
      use physprop, only: mu_g, k_g, c_pg
      use run, only: units
      use rxns, only: r_phase
      use usr, only: index_liq, index_sol
      use usr, only: solvent_absorption
      use usr, only: fwetarea_pack, sa_pack, apply_waf

      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK
! solids phase index
      INTEGER, INTENT(IN) :: M

! Local Variables:
!---------------------------------------------------------------------//
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=50) :: err_prop
! indices
      INTEGER :: I, IMJK, IJMK, IJKM
      INTEGER :: isolid, iliquid
! cell center velocities
      DOUBLE PRECISION :: UGC, VGC, WGC, USCM, VSCM, WSCM
! relative velocity
      DOUBLE PRECISION :: VREL
! gas-liquid and gas-solids reynolds number
      DOUBLE PRECISION :: RE_GL, RE_GS
! gas prandlt number
      DOUBLE PRECISION :: PR_G
! gas-liquid and gas-solids nusselt number
      DOUBLE PRECISION :: NU_G
! packing characteristics
      DOUBLE PRECISION :: dp_pack, ep_pack
! wetting factor to apply
      DOUBLE PRECISION :: wetfac
! heat transfer area
      DOUBLE PRECISION :: tran_area
! characteristic length scale
      DOUBLE PRECISION :: l_SCALE
! effective 'diameter' of liquid phase
      DOUBLE PRECISION :: d_pl
! square of gas volume fraction
      DOUBLE PRECISION :: EP_G2
! liquid volume fraction
      DOUBLE PRECISION :: EP_LIQ
! heat transfer coefficient per unit area
      DOUBLE PRECISION :: GAMMA_G
!......................................................................!
! if using this quantity then remove definition of ier
!      ier = 1

      IF (.NOT.SOLVENT_ABSORPTION) RETURN

      iliquid=index_liq
      isolid=index_sol

      I = I_OF(IJK)
      IMJK = IM_OF(IJK)
      IJMK = JM_OF(IJK)
      IJKM = KM_OF(IJK)

! Calculate velocity components at i, j, k
      UGC = AVG_X_E(U_G(IMJK),U_G(IJK),I)
      VGC = AVG_Y_N(V_G(IJMK),V_G(IJK))
      WGC = AVG_Z_T(W_G(IJKM),W_G(IJK))

      USCM = AVG_X_E(U_S(IMJK,M),U_S(IJK,M),I)
      VSCM = AVG_Y_N(V_S(IJMK,M),V_S(IJK,M))
      WSCM = AVG_Z_T(W_S(IJKM,M),W_S(IJK,M))

! magnitude of gas-solids relative velocity
      VREL = SQRT((UGC-USCM)**2 + (VGC-VSCM)**2 + (WGC-WSCM)**2)

! 'packing' characteristics
      dp_pack = d_p(ijk,isolid)
      EP_pack = ep_s(ijk,isolid)

! gas prandlt number
      PR_G = C_PG(IJK)*MU_G(IJK)/K_G(IJK)

! gas volume fraction squared
      EP_G2 = EP_G(IJK)*EP_G(IJK)
! liquid volume fraction
      ep_liq = ep_s(ijk,iliquid)

! apply fractional wetted area to interphase terms
      IF (APPLY_WAF) THEN
         IF (M == isolid) THEN
            wetfac = ONE-fwetarea_pack(ijk)
         ELSE
            wetfac = fwetarea_pack(ijk)
         ENDIF
      ELSE
         wetfac = one
      ENDIF

! liquid-gas heat transfer
! -----------------------------------------------//
      IF (M == iliquid) THEN

! if we are in the packing region then assume length scale based
! on packing
         IF (ep_pack > zero) THEN

! effective diameter
            IF (dp_pack > 0) THEN
               D_PL = ((ONE-EP_G(IJK))/(ep_pack))**0.333*dp_pack
            ELSE
               D_PL = LARGE_NUMBER
            ENDIF

! gas-liquid reynolds number
            IF (MU_G(IJK) > ZERO) THEN
               RE_GL = ROP_G(IJK)*VREL*D_PL/MU_G(IJK)
            ELSE
               RE_GL = LARGE_NUMBER
            ENDIF
! Nusselt number
            NU_G = 2.+0.6*(Re_GL**0.5)*(PR_G**0.333)

! characteristic length scale
            L_scale = D_PL

! determine the heat transfer coefficient per unit area
            IF (L_SCALE > 0) THEN
               Gamma_G = K_G(IJK)*NU_G/L_SCALE
            ELSE
               GAMMA_G = ZERO
            ENDIF
! heat transfer area (wetted area == interfacial area)
            tran_area = WetFac*sa_pack

! Assign the heat transfer coefficient
            GAMA_GS(IJK,M) = gamma_g*tran_area

! we are not in a packing region so the length scale is based on some
! liquid phase diameter assuming liquid drops? or bubble diameter?
         ELSE
            IF (D_P(IJK,iliquid) >0) THEN
               D_PL = D_P(IJK,iliquid)
            ELSE
               D_PL = LARGE_NUMBER
            ENDIF

! gas-liquid reynolds number
            IF (MU_G(IJK) > ZERO) THEN
               RE_GL = ROP_G(IJK)*VREL*D_PL/MU_G(IJK)
            ELSE
               RE_GL = LARGE_NUMBER
            ENDIF

! Nusselt number
            NU_G = 2.+0.6*(Re_GL**0.5)*(PR_G**0.333)

! characteristic length scale
            L_scale = D_PL

! determine the heat transfer coefficient per unit area
            IF (L_SCALE > 0) THEN
               Gamma_G = K_G(IJK)*NU_G/L_SCALE
            ELSE
               GAMMA_G = ZERO
            ENDIF

! heat transfer area ()
! following FLUENT implementation: 16.5.10
            tran_area = (6.*ep_liq/D_PL)

! Assign the heat transfer coefficient
! Multiply by void fraction of primary phase so that the heat transfer
! coefficient tends to zero when the phase is not present
            GAMA_GS(IJK,M) = gamma_g*tran_area*EP_G(IJK)
         ENDIF
      ENDIF


! solids-gas heat transfer
! -----------------------------------------------//
      IF (M == isolid) THEN

! ensure we are in the packing region
         IF (ep_pack > ZERO) THEN

! gas-solids reynolds number
            IF (MU_G(IJK) > ZERO) THEN
               Re_GS = ROP_G(IJK)*VREL*dp_pack/MU_G(IJK)
            ELSE
               RE_GS = LARGE_NUMBER
            ENDIF

! Nusselt number
            NU_G = ((7.D0 - 10.D0*EP_G(IJK) + 5.D0*EP_G2)*&
                    (ONE + 0.7D0*RE_GS**0.2*PR_G**0.333) + &
                    (1.33D0 - 2.4D0*EP_G(IJK) + 1.2D0*EP_G2)*&
                    RE_GS**0.7*PR_G**0.333)

! characteristic length scale
            L_scale = dp_pack

! determine the heat transfer coefficient per unit area
            IF (L_SCALE > 0) THEN
               Gamma_G = K_G(IJK)*NU_G/L_SCALE
            ELSE
               GAMMA_G = ZERO
            ENDIF

! heat transfer area (specific area); this should be replaced with
! something more appropriate
            tran_area = WetFac*sa_pack

! Assign the heat transfer coefficient
            GAMA_GS(IJK,M) = gamma_g*tran_area
         ELSE
            GAMA_GS(IJK,M) = ZERO
         ENDIF
      ENDIF

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("gas-solids phase ",I2," heat transfer ", &
                           "coefficient")') M
         CALL INIT_ERR_MSG('USR_PROP_Gama')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF
      RETURN
      END SUBROUTINE USR_PROP_GAMA


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_PROP_FSS                                            !
!  Purpose: User hook for calculating the solids-solids drag           !
!  coefficient due to relative velocity differences between each of    !
!  solids phases (M = 1 to MMAX).                                      !
!                                                                      !
!  Comments:                                                           !
!  - solids-solids drag is momentum transfer due to relative velocity  !
!    differences between solids phases M and L, where M and L range    !
!    from 1 to MMAX and M!=L.                                          !
!  - this implementation is currently restricted to solids-solids      !
!    drag between continuum solids phases. Further development is      !
!    needed to interact with discrete phases.                          !
!                                                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_PROP_Fss(IJK,L,M)

! Modules
!---------------------------------------------------------------------//
      use constant, only: segregation_slope_coefficient
      use drag, only: f_ss
      use error_manager, only: init_err_msg, err_msg, flush_err_msg
      use fldvar, only: u_s, v_s, w_s
      use fldvar, only: ep_s, ep_g, rop_s, ro_s
      use fldvar, only: d_p, p_star
      use fun_avg, only: avg_x_e, avg_y_n, avg_z_t
      use functions, only: funlm
      use functions, only: im_of, jm_of, km_of
      use indices, only: i_of
      use param1, only: undefined_i, zero, one, half
      use param1, only: small_number, large_number
      use physprop, only: smax, close_packed
      use run, only: units
      use rdf, only: g_0

      use param1, only: undefined
      use usr, only: index_liq, index_sol, lam_mu_s0
      use usr, only: usr_drag_type, usr_drag_type_enum
      use usr, only: attou_99, attou_99_mod, solomenko_15
      use usr, only: lappalainen_09, lappalainen_09_mod
      use usr, only: attou_LS_momExchange
      use usr, only: attou_mod_ls_momExchange
      use usr, only: solomenko_ls_momexchange
      use usr, only: lappalainen_ls_momexchange
      use usr, only: lappalainen_mod_ls_momexchange
      use usr, only: fwetarea_pack, apply_waf
      use visc_s, only: mu_s
      use machine, only: start_log, end_log
      use compar, only: mype
      use funits, only: dmp_log, unit_log
      use exit, only: mfix_exit
      implicit none

! Dummy arguments
!---------------------------------------------------------------------//
! index
      INTEGER, INTENT(IN) :: IJK
! solids phase indices
      INTEGER, INTENT(IN) :: M, L

! Local Variables:
!---------------------------------------------------------------------//
! indices
      INTEGER :: I, IMJK, IJMK, IJKM
! solids phase index corresponding to solids phase (packing)
      INTEGER :: mM
! solids phase index corresponding to other calling phase (liquid)
      INTEGER :: lL
! index for storing solids-solids drag coefficients in the upper
! triangle of the matrix
      INTEGER :: LM
! local momentum exchange coefficient for liquid-solid
      DOUBLE PRECISION :: lDlA
! cell center value of U_sm, U_sl, V_sm, V_sl, W_sm, W_sl
      DOUBLE PRECISION :: USCM, USCL, VSCM, VSCL, WSCM, WSCL
! relative velocity between solids phase m and liquid phase l
      DOUBLE PRECISION :: VREL
! volume fraction of gas phase
      DOUBLE PRECISION :: EP_GAS
! volume fractions of liquid and solids phase
      DOUBLE PRECISION :: EP_SL, EP_SM
! nominal diameter of packing material and volume fraction of packing
      DOUBLE PRECISION :: dp_pack, ep_pack
! material and bulk density of liquid phase L
      DOUBLE PRECISION :: RO_L, ROP_L
! liquid viscosity
      DOUBLE PRECISION :: Mu_sL
! wetting factor to apply
      DOUBLE PRECISION :: wetfac
! error flag
      INTEGER :: IER = undefined_i
      CHARACTER(LEN=60) :: err_prop

!......................................................................!
! if using this quantity then remove definition of ier
!      ier = 1

! find out which index is liquid and which is solids
      IF (M==index_sol) THEN
         mM = M
         lL = L
      ELSEIF (L==index_sol) THEN
         mM = L
         lL = M
      ELSE
         CALL INIT_ERR_MSG('USR_PROP_FSS')
         WRITE(ERR_MSG,9998) M, L
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9998 FORMAT('ERROR 9998: Unknown solid-liquid indices for case ',&
         'solids phase',/I2,' and solids phase ', I2,'.')
      ENDIF
! evaluate index
      LM = FUNLM(L,M)

! apply fractional wetted area to interaction terms
      IF (APPLY_WAF) THEN
         wetfac = fwetarea_pack(ijk)
      ELSE
         wetfac = one
      ENDIF

! Evaluate at all flow boundaries and fluid cells
! This is unlike the fluid-solid drag coefficient, which is only
! evluated in fluid cells and pressure inflow cells
      I = I_OF(IJK)
      IMJK = IM_OF(IJK)
      IJMK = JM_OF(IJK)
      IJKM = KM_OF(IJK)

! 'packing' characteristics where lM is the solids phase index
      dp_pack = d_p(ijk,mM)
      ep_pack = ep_s(ijk,mM)
      EP_sM = ep_s(ijk,mM)

! gas phase
      EP_Gas = EP_G(IJK)

! liquid properties
      RO_L = RO_S(IJK,lL)
      ROP_L = ROP_S(IJK,lL)
      EP_sL = EP_S(IJK,lL)
      IF (LAM_mu_S0 /= UNDEFINED) THEN
         Mu_sL = LAM_Mu_s0
      ELSE
         MU_sL = MU_S(IJK,lL)
      ENDIF

! calculating velocity components at i, j, k (cell center)
      USCL = AVG_X_E(U_S(IMJK,lL),U_S(IJK,lL),I)
      VSCL = AVG_Y_N(V_S(IJMK,lL),V_S(IJK,lL))
      WSCL = AVG_Z_T(W_S(IJKM,lL),W_S(IJK,lL))

      USCM = AVG_X_E(U_S(IMJK,mM),U_S(IJK,mM),I)
      VSCM = AVG_Y_N(V_S(IJMK,mM),V_S(IJK,mM))
      WSCM = AVG_Z_T(W_S(IJKM,mM),W_S(IJK,mM))

! magnitude of solids-solids relative velocity
      VREL = SQRT((USCL - USCM)**2 + (VSCL - VSCM)**2 + &
                  (WSCL - WSCM)**2)


! determining the liquid-solids drag coefficient
      IF((EP_SL > ZERO) .AND. (EP_SM > ZERO)) THEN
         SELECT CASE (USR_DRAG_TYPE_ENUM)
         CASE(ATTOU_99)
            CALL ATTOU_LS_MOMEXCHANGE(IJK, lDlA, EP_sl, Mu_sl, &
                         RO_l, VREL, dp_pack, ep_pack, wetfac)
            F_SS(IJK,LM) = lDlA
         CASE(ATTOU_99_MOD)
            CALL ATTOU_MOD_LS_MOMEXCHANGE(IJK, lDlA, EP_sl, Mu_sl,&
                         RO_l, VREL, dp_pack, ep_pack, wetfac)
            F_SS(IJK,LM) = lDlA
         CASE(SOLOMENKO_15)
            CALL SOLOMENKO_LS_MOMEXCHANGE(IJK, lDlA, EP_sl, Mu_sl, &
                         RO_l, VREL, dp_pack, ep_pack, wetfac)
            F_SS(IJK,LM) = lDlA
         CASE(LAPPALAINEN_09)
            CALL LAPPALAINEN_LS_MOMEXCHANGE(IJK, lDlA, EP_gas, EP_sl,&
                         Mu_sl, RO_l, VREL, dp_pack, ep_pack, wetfac)
            F_SS(IJK,LM) = lDlA
         CASE(LAPPALAINEN_09_MOD)
            CALL LAPPALAINEN_MOD_LS_MOMEXCHANGE(IJK, lDlA, EP_gas, EP_sl,&
                         Mu_sl, RO_l, VREL, dp_pack, ep_pack, wetfac)
            F_SS(IJK,LM) = lDlA
         CASE DEFAULT
            CALL START_LOG
            IF(.NOT.DMP_LOG) call open_pe_log(ier)
            IF(DMP_LOG) WRITE (*, '(A,A)') &
               'Unknown DRAG_TYPE: ',USR_DRAG_TYPE
            WRITE (UNIT_LOG, '(A,A)')&
               'Unknown DRAG_TYPE: ', USR_DRAG_TYPE
            CALL END_LOG
            CALL mfix_exit(myPE)
         END SELECT
      ELSE
         F_ss(IJK,LM) = ZERO
      ENDIF

      IF (IER /= UNDEFINED_I) THEN
         write(err_prop, '("solids phase ",I2," solids phase ", I2, &
                           " drag coefficient")') M, L
         CALL INIT_ERR_MSG('USR_PROP_FSS')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exi',&
         'sts. Either choose a different model or correct',/,'mfix/,'&
         'model/usr_properties.f')
      ENDIF


      END SUBROUTINE USR_PROP_FSS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: USR_PROPERTIES                                          C
!  Purpose: Hook for user defined physical properties and transport    C
!  coefficients including interphase exchange coefficients for all     C
!  phases. The exact quantities are listed here for clarity.           C
!                                                                      C
!         Quantity      GAS (M=0)     SOLIDS (M=1toMMAX)               C
!         density       RO_G(:)        RO_S(:,M)                       C
!      specific heat    C_PG(:)        C_PS(:,M)                       C
!       conductivity    K_G(:)         K_S(:,M)                        C
!       diffusivity     DIF_G(:,N)     DIF_S(:,M,N)                    C
!        viscosity      MU_G(:)        MU_S(:,M)                       C
!     gas-solids drag                  F_GS(:,M)                       C
!   solids-solids drag                 F_SS(:,LM)                      C
!   gas-solids heat tr.                GAMA(:,M)                       C
!                                                                      C
!  Comments:                                                           C
!  - gas-solids drag is momentum transfer due to relative velocity     C
!    differences (skin friction and form drag) between the gas phase   C
!    (M=0) and each solids phase (M=1 to MMAX).                        C
!  - solids-solids drag is momentum transfer due to relative velocity  C
!    differences between solids phases M and L, where M and L range    C
!    from 1 to MMAX and M!=L.                                          C
!  - gas-solids heat transfer is heat transfer due to relative         C
!    temperature differences between the gas phase phase (M=0) and     C
!    each solids phase (M=1 to MMAX).                                  C
!  - No solids-solids heat transfer is allowed. To account for such    C
!    would require appropriate closures and additional code            C
!    development.                                                      C
!  - the specific heat assigned in this routine only applies to the    C
!    mixture average specific heat invoked in the general energy       C
!    equations. reacting flow simulations require values for species   C
!    specific heat and are taken from the Burcat database or read      C
!    in that format from the mfix.dat file. therefore inconsitencies   C
!    may arise in calculations involving species specific heat. this   C
!    requires further development to fully address.                    C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR_PROPERTIES(lprop, IJK, M, N)

! Modules
!-----------------------------------------------
      use constant, only: pi, gas_const, gravity
      use error_manager

      use fldvar, only: u_g, v_g, w_g, ep_g
      use fldvar, only: u_s, v_s, w_s, ep_s
      use fldvar, only: p_g, rop_g, ro_g, T_g, X_g
      use fldvar, only: p_s, rop_s, ro_s, T_s, X_s
      use fldvar, only: d_p, theta_m

      use fldvar, only: scalar
      use functions
      use geometry

      use indices, only: i_of, j_of, k_of
      use indices, only: im1, ip1, jm1, jp1, km1, kp1

      use param1, only: zero, one, half, undefined, undefined_i
      use physprop, only: k_g, c_pg, dif_g, mu_g
      use physprop, only: k_s, c_ps, dif_s
      use scalars, only: phase4scalar
      use visc_s, only: mu_s, lambda_s
      use usr_prop
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! reference equation
      INTEGER, INTENT(IN) :: lprop
! index
      INTEGER, INTENT(IN) :: IJK
! Phase index
      INTEGER, INTENT(IN) :: M
! Species index or second solids phase index for solids-solids drag
! (if applicable otherwise undefined_i)
      INTEGER, INTENT(IN) :: N

! Local variables
!-----------------------------------------------
! Error flag
      INTEGER :: IER
      CHARACTER(len=40):: err_prop

!-----------------------------------------------
! initialize
      ier = undefined_i

! in each case the ier flag is set to ensure that if a user defined
! quantity is invoked that the associated user defined quantity is
! actually specified. this flag must be removed by the user or the
! code will fail.

      SELECT CASE(lprop)

!
      CASE (GAS_DENSITY)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, '("gas density")')
!        RO_G(IJK) =


      CASE (GAS_SPECIFICHEAT)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, '("gas specific heat")')
!        C_PG(IJK,M) =


! assign gas viscosity value mu_g. bulk viscosity is taken as zero.
! second viscosity (lambda_g) is automatically defined as
! lambda_g = -2/3mu_g
      CASE (GAS_VISCOSITY)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, '("gas viscosity")')
!        MU_G(IJK) =


      CASE (GAS_CONDUCTIVITY)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, '("gas conductivity")')
!        K_G(IJK) =


      CASE (GAS_DIFFUSIVITY)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, '("gas diffusivity")')
!        DIF_G(IJK,N) =


      CASE (SOLIDS_DENSITY)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, '("solids phase ",I2," density")') M
!        RO_S(IJK,M) =


      CASE (SOLIDS_SPECIFICHEAT)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, '("solids phase ",I2," specific heat")') M
!        C_PS(IJK,M) =


! assign solids phase M viscosity mu_s.
! assign second viscosity (lambda_s): lambda_s = mu_sbulk - 2/3mu_s
! assign solids pressure p_s.
      CASE (SOLIDS_VISCOSITY)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, '("solids phase ", I2," viscosity")') M
!        MU_S(IJK,M) =
!        LAMBDA_S(IJK,M) =
!        P_S(IJK,M) =


      CASE (SOLIDS_CONDUCTIVITY)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, '("solids phase ",I2," conductivity")') M
!        K_S(IJK,M) =


      CASE (SOLIDS_DIFFUSIVITY)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, '("solids phase",I2," diffusivity")') M
!        DIF_S(IJK,M,N) =


      CASE (GASSOLIDS_HEATTRANSFER)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, &
               '("gas-solids phase ",I2," heat transfer coefficient")') M
!       GAMA_GS(IJK,M) =


      CASE (GASSOLIDS_DRAG)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, &
               '("gas-solids phase ",I2," drag coefficient")') M
!        F_GS(IJK,M) =


      CASE (SOLIDSSOLIDS_DRAG)
! if using this quantity then remove definition of ier
         ier = 1
         write(err_prop, &
            '("solids-solids phases ",I2," & ",I2," drag coefficient")') M, N
!        F_SS(IJK,LM) =


      END SELECT

      IF (IER /= UNDEFINED_I) THEN
         CALL INIT_ERR_MSG('USR_PROPERTIES')
         WRITE(ERR_MSG,9999) trim(err_prop)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

 9999 FORMAT('ERROR 9999: The user-defined properties routine was ',&
         'invoked for',/,A,' but this generic error',/,'message exits.',&
         'Either choose a different model or correct',/,'mfix/model/',&
         'usr_properties.f')
      ENDIF

      RETURN
      END SUBROUTINE USR_PROPERTIES


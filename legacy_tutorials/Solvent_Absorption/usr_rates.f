!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Module name: USR_RATES                                              !
!                                                                      !
!  Purpose: Hook for user defined reaction rates.                      !
!                                                                      !
!  Author: J.Musser                                   Date: 10-Oct-12  !
!                                                                      !
!  Comments: Write reaction rates in units of moles/sec.cm^3 (cgs) or  !
!  kmoles/sec.m^3 (SI). Units should match those specified in the data !
!  file.
!                                                                      !
!  Example reaction: Methane combustion                                !
!                                                                      !
!  mfix.dat input:                                                     !
!``````````````````````````````````````````````````````````````````````!
!    @(RXNS)                                                           !
!      CH4_Comb { chem_eq = "CH4 + 2.0*O2 --> CO2 + 2.0*H2O" }         !
!    @(END)                                                            !
!``````````````````````````````````````````````````````````````````````!
!                                                                      !
!  usr_rates.f input:                                                  !
!``````````````````````````````````````````````````````````````````````!
!    c_O2  = (RO_g(IJK)*X_g(IJK,O2)/MW_g(O2))                          !
!    c_CH4 = (RO_g(IJK)*X_g(IJK,CH4)/MW_g(CH4))                        !
!    RATES(CH4_Comb) = 2.0d5 * EP_g(IJK) * c_O2 * c_CH4                !
!``````````````````````````````````````````````````````````````````````!
!  * Species alias and reaction names given in the data file can be    !
!    used in reference to the reaction index in RATES and a species    !
!    index in gas/solids phase variables.                              !
!                                                                      !
!  * Additional information is provided in section 5.11 of the code    !
!    Readme.                                                           !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_RATES(IJK, RATES)

! Modules
!---------------------------------------------------------------------//
      use constant, only: C
      use fldvar, only: p_g, t_g, x_g, ro_g, ep_g
      use fldvar, only: t_s, x_s, ro_s, ep_s

! geometry for debugging
      use geometry, only: imax, jmax
      use param1, only: one, zero
      use physprop, only: mu_g, mw_g, mw_mix_g
      use physprop, only: mw_s, nmax
      use physprop, only: dif_s

      use rxns, only: no_of_rxns, nrr, reactionrates
      use run, only: units
      use scales, only: unscale_pressure
      use toleranc, only: tmax, zero_x_gs
      use visc_s, only: mu_s

      use usr, only: index_liq, index_sol
      use usr, only: calc_mw_mix_l
      use usr, only: fwetarea_pack, sa_pack
      use usr, only: omega_l
! this routine is still being called here but is also being
! stored in the global variable for the liquid phase diffusivist
! therefore it can probably be removed
      use usr, only: calc_d_co2inmea

! these routines are called from rrates alone
      use usr, only: calc_d_meainmea
      use usr, only: calc_h_co2inmea
      use usr, only: calc_k2_hikita
! Flag for chemistry scheme
      use usr, only: absorption_chem_type_enum
      use usr, only: single_step, equilibrium_segregated
      use usr, only: equilibrium_coupled
! Function for equilibrium constants
      use usr, only: keq_aboudheir
      use error_manager
      IMPLICIT NONE
! Dummy arguments
!---------------------------------------------------------------------//
      INTEGER, INTENT(IN) :: IJK
      DOUBLE PRECISION, DIMENSION(NO_OF_RXNS), INTENT(OUT) :: RATES

! Local variables
!---------------------------------------------------------------------//
! local index for liquid and solids phases
      INTEGER :: iliquid
      INTEGER :: isolid

! loop indice
      INTEGER :: L

! Species loop index
      INTEGER :: N

! absolute gas phase pressure (barye)
      DOUBLE PRECISION:: Pg_abs

! bounded phase temperatures (K)
      DOUBLE PRECISION :: xTg
      DOUBLE PRECISION :: xTl

! gas phase partial pressures (barye or dyne/cm^2)
      DOUBLE PRECISION :: pp_g(nmax(0))

! gas phase molar concentrations (gmol/cm^3)
      DOUBLE PRECISION :: c_g(nmax(0))

! gas phase reaction limiters: (0,1]
      DOUBLE PRECISION :: r_g(nmax(0))

! liquid phase molar concentration (gmol/cm^3)
! in units per liquid vol not cell volume!
      DOUBLE PRECISION :: c_l(nmax(index_liq))

! liquid phase mole fraction
      DOUBLE PRECISION :: y_l(nmax(index_liq))

! liquid phase reaction limiters: (0,1]
      DOUBLE PRECISION :: r_l(nmax(index_liq))

! liquid phase average molecular weight (g/gmol)
      DOUBLE PRECISION :: MW_MIX_L

! partial pressure of (gas) CO2 at interface (barye)
! concentration of (liq) CO2 at interface and (mol/cm^3)
      DOUBLE PRECISION :: p_CO2int
      DOUBLE PRECISION :: c_lCO2int

! CO2 partial pressure drop from bulk gas to interface (barye)
      DOUBLE PRECISION :: delta_pp_CO2

! load of CO2 in aq amine (gmol CO2/gmol amine)
      DOUBLE PRECISION :: load_CO2

! mass percentage of amine (%)
      DOUBLE PRECISION :: xp_lRNH2

! volume fraction of packing (1-bed porosity)
      DOUBLE PRECISION :: eps_pack

! volume fraction of liquid
      DOUBLE PRECISION :: eps_liq

! volume (void) fraction of gas
      DOUBLE PRECISION :: ep_gas

! local aliases for physical properties of gas
      DOUBLE PRECISION :: mu_gas
      DOUBLE PRECISION :: rho_gas

! physical properties of liquid
      DOUBLE PRECISION :: mu_liq
      DOUBLE PRECISION :: rho_liq

! diffusivsities (cm^2/s)
      DOUBLE PRECISION :: D_CO2inMEA
      DOUBLE PRECISION :: D_MEAinMEA
      DOUBLE PRECISION :: D_CO2inAIR

! henry's law constant (barye.cm^3/mol)
      DOUBLE PRECISION :: H_CO2inMEA

! surface tension of solution (dyne/cm)
      DOUBLE PRECISION :: omega_liq

! second order rate constant for CO2 reacting with MEA
      DOUBLE PRECISION :: kl_rxn

! intermedate values for carabmate and bicarbonate formation calculation
      DOUBLE PRECISION :: k_carbf, k_bicarbf

! temporary reaction kinetics/rates
      DOUBLE PRECISION :: kfwd, krvs
      DOUBLE PRECISION :: fwd, rvs
      DOUBLE PRECISION :: keq

! gas phase mass transfer coefficient for CO2 (mol/(barye.cm^2.s))
! liquid phase mass transfer coefficient for CO2 (cm/s)
      DOUBLE PRECISION :: kg_abs
      DOUBLE PRECISION :: kl_abs

! wetted and interfacial area of packing (cm^2/cm^3)
! in units per volume of bed
      DOUBLE PRECISION :: wa_pack
      DOUBLE PRECISION :: int_area

! Minimum species mass fraction required to facilitate a reaction.
      DOUBLE PRECISION, parameter :: x_Limiter = 1.0d-6
! Minimum phase volume fraction required to facilitate a reaction.
      DOUBLE PRECISION, parameter :: v_Limiter = 1.0d-6
! Minimum gradient at interface required to facilitate a flux.
      DOUBLE PRECISION, parameter :: p_Limiter = 1.0d-6

! Reaction softener parameters.
      DOUBLE PRECISION :: rLimiter(NO_OF_RXNS)
      DOUBLE PRECISION, parameter :: r_pnt = 1.0d-4
      DOUBLE PRECISION, parameter :: r_exp = 3.0d0

!---------------------------------------------------------------------//
      INCLUDE 'species.inc'


! Constants that need to be set
!---------------------------------------------------------------------//
! solids phase index representing the liquid phase
! reaction occur between gas and liquid phase
      iliquid = index_liq

! solids phase index represent the solids (packing) phase
      isolid = index_sol

! Volume (void) fraction of gas phase
      ep_gas = EP_G(IJK)

! Volume fraction of liquid
      eps_liq = ep_s(ijk,iliquid)

! Volume fraction of packing
      eps_pack = ep_s(ijk,isolid)


! Gas phase quantities
!---------------------------------------------------------------------//
! initialize gas phase partial pressures and molar concentrations
      pp_g(:) = zero; c_g(:) = zero; r_g(:) = one;

! initialize
      Pg_abs = zero
      xTg = zero
      mu_gas = zero
      rho_gas = zero
      D_CO2inAIR = zero

      IF (ep_gas > v_limiter) THEN
! Make sure it is 'absolute pressure' (barye)
         Pg_abs = unscale_pressure(P_g(ijk))

! Bounded gas phase temperature (K)
         xTg   = min(TMAX,T_g(IJK))

! Local aliases
         mu_gas = MU_G(IJK)
         rho_gas = RO_G(IJK)

! diffusivsity of CO2 in Air (cm^2/s)
         D_CO2inAIR = 0.16   ! (cm^2/s)
         IF (UNITS=='SI') D_CO2inAIR=D_CO2inAIR/10000.d0 !(m^2/s)

! Compute the species concentrations and partial pressures:
! Note: concentration is in per gas volume NOT per cell volume
         DO N = 1, NMAX(0)
            IF (X_g(IJK,N) > x_limiter) THEN
               pp_g(N) = pg_abs*X_g(IJK,N)*&
                  (MW_MIX_g(IJK)/MW_g(N))                ! (barye)
               c_g(N) = ro_g(IJK)*X_g(IJK,N)/MW_g(N)  ! (gmol/cm^3)
               r_g(N) = (X_g(IJK,N)/&
                  (X_g(IJK,N)+r_pnt))**r_exp
            ENDIF
         ENDDO
      ENDIF

! Liquid phase quantities
!---------------------------------------------------------------------//
! initialize liquid phase molar concentrations and mole fractions
      c_l(:) = zero; y_l(:) = zero; r_l(:) = one

! initialize
      xTl = zero
      MW_MIX_l = zero
      omega_liq = zero
      rho_liq = zero
      mu_liq = zero
      D_CO2inMEA = zero
      D_MEAinMEA = zero
      H_CO2inMEA = zero
      load_CO2 = zero
      xp_lRNH2 = zero

! If the fluid cell contains the liquid phase, calculate the properties
      IF (eps_liq > v_limiter) THEN

! Compute average molecular weight of liquid phase
         MW_MIX_L = calc_MW_mix_l(ijk, iliquid)

! Compute the species concentrations, etc:
! Note: concentration is in per liquid volume NOT per cell volume
         DO N = 1, NMAX(iliquid)
            IF (X_s(IJK,iliquid,N) > zero_x_gs) THEN !x_limiter) THEN
               c_l(N) = (ro_s(IJK,iliquid)*X_s(IJK,iliquid,N))/&
                  MW_s(iliquid,N)                     ! (gmol/cm^3)
               y_l(N) = X_s(IJK,iliquid,N)*&
                  (MW_MIX_L/MW_s(iliquid,N))           ! (mol-CO2/mol-Mix)
               r_l(N) = (X_s(IJK,iliquid,N)/&
                  (X_s(IJK,iliquid,N)+r_pnt))**r_exp
            ENDIF
         ENDDO

! bounded liquid phase temperature
         xTl = min(TMAX,T_s(IJK,iliquid))

! mass percentage of mea
         xp_lRNH2 = X_S(IJK,iliquid,lRNH2)*100

! CO2 loading
         IF (y_l(lRNH2) > ZERO) THEN
            load_CO2 = y_l(lCO2)/y_l(lRNH2)
         ELSE
            load_CO2 = zero
         ENDIF

! Some of the physical property correlations will need to be placed in
! their appropriate routines (viscosity, density)

! surface tension of the solution  (dyne/cm or g/s^2)
         omega_liq = omega_l(ijk)

! density of liquid phase (g/cm^3)
         rho_liq = ro_s(ijk,iliquid)

! viscosity of liquid phase (poise or g/cm.s)
         mu_liq = mu_s(ijk,iliquid)

! diffusivity of CO2 in MEA solution (cm^2/s)
         D_CO2inMEA = calc_D_CO2inMEA(xTl, mu_liq)
! units of dif_s are incorrect for our purpose as it includes an
! additional factor of rop_s
!         D_CO2inMEA = dif_s(ijk,iliquid)

! diffusivity of MEA in MEA solution (cm^2/s)
         D_MEAinMEA = calc_D_MEAinMEA(xTl, c_l(lCO2))

! henry's law constant for CO2 in solution (barye.cm^3/mol)
         H_CO2inMEA = calc_H_CO2inMEA(xTl)

      ENDIF

! Packing phase and related quantities
!---------------------------------------------------------------------//
! initialize
      kl_abs = zero
      kg_abs = zero
      wa_pack = zero
      int_area = zero

      IF (ep_gas > v_limiter) THEN
! gas phase mass transfer coefficient for CO2 (mole/(barye.cm^2.s))
         call calc_kg_onda(ijk, xTg, D_CO2inAIR, mu_gas, rho_gas, kg_abs)
      ENDIF

      IF (eps_pack > v_limiter .AND. eps_liq > v_limiter) then
! compute the wetted area (cm^2/cm^3)
! Note: this is per unit volume of bed (not computational cell)
         wa_pack = sa_pack*fwetarea_pack(ijk)

         IF (wa_pack > 1.D-6) THEN
! suppress liquid phase mass transport if no wetting
! liquid phase mass transfer coefficient for CO2 (no rxn) (cm/s)
            call calc_kl_onda(ijk, wa_pack, D_CO2inMEA,&
                  mu_liq, rho_liq, eps_liq, iliquid, kl_abs)

! Compute the interfacial area: Assume the interfacial area equals the
! wetted area (cm^2/cm^3) which is per unit volume of bed NOT per unit
! volume of packing
            int_area = wa_pack
         ENDIF

      ENDIF

! initialize rates
      RATES(:) = ZERO


!**********************************************************************!
!                                                                      !
!                   Heterogenous Reaction Rates                        !
!                                                                      !
!**********************************************************************!
! Reactions are suppressed if both phases are not present

! CO2 absorption:  CO2(gas) --> CO2(liq)
!---------------------------------------------------------------------//
! Suppress mass transfer in regions where both gas and liquid are not
! present... In addition, explicitly limit this reaction to regions
! of packing as otherwise interfacial/wetted area has no meaning

      p_CO2int = zero
      c_lCO2int = zero
      delta_pp_CO2 = zero;

      IF (ep_gas > v_limiter .and. eps_liq > v_limiter .AND. &
          wa_pack > 1.D-6) THEN
! second order rate constant for CO2 reacting with MEA (cm^3/s.gmol)
         kl_rxn = calc_k2_hikita(xTl)

! Calculate the concentration of CO2 in the liquid at the interface
         CALL calc_lCO2int(ijk, kl_rxn, kl_abs, kg_abs, &
                   D_CO2inMEA, D_MEAinMEA, H_CO2inMEA, &
                   c_l(lRNH2), c_l(lCO2), pp_g(gCO2), c_lCO2int, p_CO2int)

! CO2 pressure difference, apply limiter for (irreversible) mass flux:
         delta_pp_CO2 = pp_g(gCO2) - p_CO2int     ! (barye)
!         IF (delta_pp_CO2 <= p_limiter) THEN
!            delta_pp_CO2 = zero
!         ENDIF

! Mass transfer rate. (moles/cm^3.sec)
! NA = kg(Pa-PAi)  in units (mol/cm^2.s)
! NA = kl(Cai-Ca)  in units (mol/cm^2.s)
! Note that the interfacial area has units surface area per reactor
! volume. Whether this should be multipled by the solids volume
! fraction of the packing (eps_pack) is unclear. It would seem
! implicit in the calculation and therefore unneeded but it is a
! matter to be aware of.
        RATES(CO2_absorption) = int_area*kg_abs*delta_pp_CO2
      ENDIF

! H2O mass transfer:    H2O(l) <--> H2O(g)
!---------------------------------------------------------------------//


!**********************************************************************!
!                                                                      !
!                Homogenous liquid phase reactions                     !
!                                                                      !
!**********************************************************************!
! Liquid phase reactions are suppressed if liquid phase is not present
! through calculation of concentrations (see earlier).
! Multiple by liquid phase reactions by ep_l to account for the fact
! that only part of the volume is occupied by liquid


! Single step carbamate formation:  CO2 + 2RNH2 --> RNHCO2- + RNH3+
!---------------------------------------------------------------------//
! Name (rxn ID): CARBAMATE_FORM_SINGLE_STEP
      IF(ABSORPTION_CHEM_TYPE_ENUM == SINGLE_STEP) THEN
! second order rate constant for CO2 reacting with MEA (cm^3/s.gmol)
         kl_rxn = calc_k2_hikita(xTl)
         K_CARBF = eps_liq*kl_rxn
         FWD = c_l(lCO2)*c_l(lRNH2)
         FWD = K_CARBF*FWD
         RATES(CARBAMATE_FORM_SINGLE_STEP) = FWD
      ELSE
         RATES(CARBAMATE_FORM_SINGLE_STEP) = ZERO
      ENDIF


! Additional instantaneous, equilbrium reactions may occur in the bulk
! liquid. List of 5 equilibrium reactions:
!   RNHCO2- + H2O <--> RNH2 + HCO3-    (3-4: Reversion of Carbamate)
!   CO2 + 2H2O <--> HCO3m + H3Op        (5-6: Dissociation of CO2)
!   HCO3m + H2O --> CO3m2 + H3Op        (7-8: Dissociation of Bicarbonate)
!   RNH3+ + H2O <--> RNH2 + H3O+        (9-10: Dissociation of MEAH+)
!   2H2O <--> H3O+ + OH-                (11-12: Ionization of Water)
!---------------------------------------------------------------------//
      IF(ABSORPTION_CHEM_TYPE_ENUM == EQUILIBRIUM_SEGREGATED) THEN
! Set the rates of these reactions according to the following:
!   krev = kfwd/Keq
!   kfwd = kconst
!   RATE(FWD) = kfwd*[reactants]
!   RATE(REV) = krev*[products]

! Carabamate reversion or Hydrolysis
! Reaction: RNHCO2- + H2O <--> RNH2 + HCO3-
!---------------------------------------------------------------------//
         KEQ = KEQ_ABOUDHEIR(FWD_REVERSION_CARBAMATE,xTl)
         KFWD = C(11)*eps_liq
         KRVS = KFWD/KEQ
         FWD = KFWD*c_l(RNHCO2m) !*c_l(lH2O)
         RVS = KRVS*c_l(lRNH2)*c_l(HCO3m)
         IF (FWD>RVS) THEN
            RATES(FWD_REVERSION_CARBAMATE)=FWD-RVS
         ELSE
            RATES(REV_REVERSION_CARBAMATE)=RVS-FWD
         ENDIF

! Dissociation of dissolved CO2 through carbonic acid:
! CO2 + 2H2O <--> HCO3- + H3O+
!---------------------------------------------------------------------//
         KEQ = KEQ_ABOUDHEIR(FWD_DISSOCIATION_CO2,xTl)
         KFWD = C(12)*eps_liq
         KRVS = KFWD/KEQ
         FWD = KFWD*c_l(lCO2) !*c_l(lH2O)*c_l(lH2O)
         RVS = KRVS*c_l(HCO3m)*c_l(H3Op)
         IF (FWD>RVS) THEN
            RATES(FWD_DISSOCIATION_CO2)=FWD-RVS
         ELSE
            RATES(REV_DISSOCIATION_CO2)=RVS-FWD
         ENDIF

! Dissociation of bicarbonate:
! HCO3- + H2O <--> CO3-2 + H3O+
!---------------------------------------------------------------------//
         KEQ = KEQ_ABOUDHEIR(FWD_DISSOCIATION_BICARBONATE,xTl)
         KFWD = C(13)*eps_liq
         KRVS = KFWD/KEQ
         FWD = KFWD*c_l(HCO3m) !*c_l(lH2O)
         RVS = KRVS*c_l(CO3m2)*c_l(H3Op)
         IF (FWD>RVS) THEN
            RATES(FWD_DISSOCIATION_BICARBONATE)=FWD-RVS
         ELSE
            RATES(REV_DISSOCIATION_BICARBONATE)=RVS-FWD
         ENDIF

! Dissociation of protonated MEA:
! RNH3+ + H2O <--> RNH2 + H3O+
!---------------------------------------------------------------------//
         KEQ = KEQ_ABOUDHEIR(FWD_DISSOCIATION_MEAH,xTl)
         KFWD = C(14)*eps_liq
         KRVS = KFWD/KEQ
         FWD = KFWD*c_l(RNH3p) !*c_l(lH2O)
         RVS = KRVS*c_l(lRNH2)*c_l(H3Op)
         IF (FWD>RVS) THEN
            RATES(FWD_DISSOCIATION_MEAH)=FWD-RVS
         ELSE
            RATES(REV_DISSOCIATION_MEAH)=RVS-FWD
         ENDIF

! Ionization of water:
! 2H2O <--> OH- + H3O+
!---------------------------------------------------------------------//
         KEQ = KEQ_ABOUDHEIR(FWD_IONIZATION_H2O,xTl)
         KFWD = C(15)*eps_liq
         KRVS = KFWD/KEQ
         FWD = KFWD !*c_l(lH2O)*c_l(lH2O)
         RVS = KRVS*c_l(OHm)*c_l(H3Op)
         IF (FWD>RVS) THEN
            RATES(FWD_IONIZATION_H2O)=FWD-RVS
         ELSE
            RATES(REV_IONIZATION_H2O)=RVS-FWD
         ENDIF

      ENDIF


!**********************************************************************!
!                     Setup/Apply Limiter                              !
!**********************************************************************!

      rLIMITER = ONE
      IF (delta_pp_co2 > p_limiter) THEN
         rLIMITER(CO2_absorption) = (delta_pp_CO2/(delta_pp_CO2+r_pnt))**r_exp
      ENDIF

      rLIMITER(CO2_absorption) = r_g(gCO2)

      rLIMITER(CARBAMATE_FORM_SINGLE_STEP) = min(r_l(lCO2),r_l(lRNH2))

      rLIMITER(FWD_REVERSION_CARBAMATE) = min(r_l(RNHCO2m), r_l(lH2O))
      rLIMITER(REV_REVERSION_CARBAMATE) = min(r_l(lRNH2), r_l(HCO3m))

      rLIMITER(FWD_DISSOCIATION_CO2) = min(r_l(lCO2),r_l(lH2O))
      rLIMITER(REV_DISSOCIATION_CO2) = min(r_l(HCO3m),r_l(H3Op))

      rLIMITER(FWD_DISSOCIATION_BICARBONATE) = min(r_l(HCO3m),r_l(lH2O))
      rLIMITER(REV_DISSOCIATION_BICARBONATE) = min(r_l(CO3m2),r_l(H3Op))

      rLIMITER(FWD_DISSOCIATION_MEAH) = min(r_l(RNH3p),r_l(lH2O))
      rLIMITER(REV_DISSOCIATION_MEAH) = min(r_l(lRNH2),r_l(H3Op))

      rLIMITER(FWD_IONIZATION_H2O) = r_l(lH2O)
      rLIMITER(REV_IONIZATION_H2O) = min(r_l(OHm),r_l(H3Op))

      DO L=1,2 !NO_OF_RXNS !(up to equilbrium reactions)
         RATES(L) = RATES(L) * rLimiter(L)
      ENDDO


!**********************************************************************!
!                         Save the Rates                               !
!**********************************************************************!
      DO L=1, min(nRR,NO_OF_RXNS)
         ReactionRates(IJK,L) = RATES(L)
      ENDDO

      RETURN

      END SUBROUTINE USR_RATES


!----------------------------------------------------------------------!
! Subroutine: calc_kg_onda                                             !
!                                                                      !
! Purpose: Calculate the gas phase mass transport coefficient for      !
! absorption of pure CO2 into an organic solvent in a packed bed.      !
!                                                                      !
! This reference studies CO2 absorbed into methanol and carbon         !
! tetracholoride. The column is random packed bed (raschig rings,      !
! berl saddles, spheres, etc). The mass transfer coefficient is        !
! returned in units (mol/(barye.cm^2.s). This is considered mass       !
! transfer due to physical absorption only (no rxn).                   !
!                                                                      !
! References:                                                          !
! Onda K. et al., Mass transfer coefficients between gas and liquid    !
!   phases in packed columns, Journal of Chemical Engineering of       !
!   Japan, 1968, 56-62.                                                !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      SUBROUTINE calc_kg_onda(ijk, Tg, diffg, mug, rog, kg_abs)

      use fldvar, only: u_g, v_g, w_g, ep_g
! nominal size of packing (cm)
      use usr, only: d_pack
! total surface area of packing per unit volume of bed (cm^2/cm^3)
! commonly referred to as the specific area
      use usr, only: sa_pack
      use constant, only: gas_const
      use constant, only: C
      use indices, only: i_of
      use functions, only: im_of, jm_of, km_of
      use fun_avg, only: avg_x_e, avg_y_n, avg_z_t
      use error_manager
      IMPLICIT NONE

! index
      INTEGER, INTENT(IN) :: IJK
! Gas temperature (K)
      DOUBLE PRECISION, INTENT(IN) :: Tg
! Gas diffusivity of CO2 (cm^2/s)
      DOUBLE PRECISION, INTENT(IN) :: diffg
! Gas viscosity (g/cm.s)
      DOUBLE PRECISION, INTENT(IN) :: mug
! Gas density (g/cm^3)
      DOUBLE PRECISION, INTENT(IN) :: rog
! gas phase mass transfer coefficient for CO2 (mol/(barye.cm^2.s))
      DOUBLE PRECISION, INTENT(OUT) :: kg_abs

! Various fluid cell indicies
      INTEGER :: I, IMJK, IJMK, IJKM
! Gas Velocity - cell centered
      DOUBLE PRECISION :: UGC, VGC, WGC
! Magnitude of gas velocity
      DOUBLE PRECISION :: vgas
! Gas superficial mass velocity (g/cm2.s)
      DOUBLE PRECISION :: ggas
! Calculation quantities
      DOUBLE PRECISION :: group1

! Initialize fluid cell variables
      I =  I_OF(IJK)
      IMJK  = IM_OF(IJK)
      IJMK  = JM_OF(IJK)
      IJKM  = KM_OF(IJK)

! Calculate velocity components at i, j, k
      UGC = AVG_X_E(U_G(IMJK),U_G(IJK),I)
      VGC = AVG_Y_N(V_G(IJMK),V_G(IJK))
      WGC = AVG_Z_T(W_G(IJKM),W_G(IJK))

! Calculate the magnitude of gas velocity
      vgas = dsqrt(UGC*UGC + VGC*VGC + WGC*WGC)

! Calculate superficial gas mass velocity (g/cm^2.s)
      ggas = vgas*rog*EP_G(IJK)

! Eq 3 calculation 1 (dimensionless)
      group1 = 5.23d0*(ggas/(sa_pack*mug))**(0.7) * &
               (mug/(rog*diffg))**(1./3.)* &
               (sa_pack*d_pack)**(-2.)
! Note that sa_pack*d_pack could be replaced using the relation
! sa_pack*d_pack = 6 * (1-porosity). As is we require a value for
! sa_pack and d_pack. However, this relation is based on porous
! medium consisting of uniformally packed spheres, which is not
! necessarily the case being simulated.

! In CGS the gas constant R is in units cm^3.barye/mol.K
! gas-phase mass transfer coefficient (mol/(barye.cm^2.s)
      kg_abs = C(2)*(sa_pack*diffg/(GAS_CONST*Tg))*group1

      if (kg_abs < 0) then
         CALL INIT_ERR_MSG("CALC_KG_ONDA in usr_rates")
         write(err_msg, 1102) IJK
         write(*,*) 'epg     ', ep_g(ijk)
         write(*,*) 'vgas    ', vgas
         write(*,*) 'ggas    ', ggas
         write(*,*) 'Tg      ', Tg
         write(*,*) 'rog     ', rog
         write(*,*) 'mug     ', mug
         write(*,*) 'diffg   ', diffg
         write(*,*) 'sa_pack ', sa_pack
         write(*,*) 'd_pack  ', d_pack
         write(*,*) 'GAS_CONST ', GAS_CONST
         write(*,*) 'group1  ', group1

         call flush_err_msg(ABORT=.TRUE.)
         CALL FINL_ERR_MSG
 1102 FORMAT('Error 1102: Gas phase mass transfer coefficient is ', &
         'less than',/'or equal to zero at IJK = ',i5)
      endif

      RETURN
      END SUBROUTINE calc_kg_onda


!----------------------------------------------------------------------!
! Subroutine: calc_kl_onda                                             !
!                                                                      !
! Purpose: Calculate the liquid phase mass transport coefficient for   !
! absorption of pure CO2 into an organic solvent in a packed bed.      !
!                                                                      !
! This reference studies CO2 absorbed into methanol and carbon         !
! tetracholoride. The column is random packed bed (raschig rings,      !
! berl saddles, spheres, etc). The mass transfer coefficient is        !
! returned in units (cm/s). This is considered mass transfer due to    !
! physical absorption only (no rxn).                                   !
!                                                                      !
! References:                                                          !
! Onda K. et al., Mass transfer coefficients between gas and liquid    !
!   phases in packed columns, Journal of Chemical Engineering of       !
!   Japan, 1968, 56-62.                                                !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      SUBROUTINE calc_kl_onda(ijk, wa_pack, diffl, mul, rol, epl, &
                              m, kl_abs)

      use fldvar, only: u_s, v_s, w_s
! nominal size of packing (cm)
      use usr, only: d_pack
! total surface area of packing per unit volume of bed (cm^2/cm^3)
! commonly referred to as the specific area
      use usr, only: sa_pack
      use param1, only: zero
      use constant, only: gravity
      use constant, only: C
      use indices, only: i_of
      use functions, only: im_of, jm_of, km_of
      use fun_avg, only: avg_x_e, avg_y_n, avg_z_t
      use error_manager
      IMPLICIT NONE

! index
      INTEGER, INTENT(IN) :: ijk
! Wetted surface area of packing per unit volume of bed (cm^2/cm^3)
      DOUBLE PRECISION, INTENT(IN) :: wa_pack
! Liquid diffusivity (cm^2/s)
      DOUBLE PRECISION, INTENT(IN) :: diffl
! Liquid viscosity (g/cm.s)
      DOUBLE PRECISION, INTENT(IN) :: mul
! Liquid density (g/cm^3)
      DOUBLE PRECISION, INTENT(IN) :: rol
! Liquid volume fraction (-)
      DOUBLE PRECISION, INTENT(IN) :: epl
! solids phase index representing liquid phase
      INTEGER, INTENT(IN) :: M
! liquid phase mass transfer coefficient for CO2 (cm/s)
      DOUBLE PRECISION, INTENT(OUT) :: kl_abs

! Various fluid cell indicies
      INTEGER :: I, IMJK, IJMK, IJKM
! Liquid Velocity - cell centered
      DOUBLE PRECISION :: ULC, VLC, WLC
! Magnitude of liquid velocity
      DOUBLE PRECISION :: vliq
! Liquid superficial mass velocity (g/cm2.s)
      DOUBLE PRECISION :: lliq
! Calculation quantities
      DOUBLE PRECISION :: group1
! Modified reynolds number
      DOUBLE PRECISION :: Rel

! Initialize fluid cell variables
      I =  I_OF(IJK)
      IMJK  = IM_OF(IJK)
      IJMK  = JM_OF(IJK)
      IJKM  = KM_OF(IJK)

! Calculate velocity components at i, j, k
      ULC = AVG_X_E(U_s(IMJK,M),U_s(IJK,M),I)
      VLC = AVG_Y_N(V_s(IJMK,M),V_s(IJK,M))
      WLC = AVG_Z_T(W_s(IJKM,M),W_s(IJK,M))

! Calculate the magnitude of liquid interstitial velocity
      vliq = dsqrt(ULC*ULC + VLC*VLC + WLC*WLC)

! Calculate superficial liquid mass velocity (g/cm^2.s)
      lliq = vliq*rol*epl

! Calculate the reynolds number
      Rel = lliq/(wa_pack*mul)

! Eq 2 calculation 1 (dimensionless)
      group1 = 0.0051d0*(lliq/(wa_pack*mul))**(2./3.) * &
               (mul/(rol*diffl))**(-0.5)* &
               (sa_pack*d_pack)**(0.4)
! Note that sa_pack*d_pack could be replaced using the relation
! sa_pack*d_pack = 6 * (1-porosity). As is we require a value for
! sa_pack and d_pack. However, this relation is based on porous
! medium consisting of uniformally packed spheres, which is not
! necessarily the case being simulated.

! Calculate the liquid-phase mass transfer coefficient (cm/s)
      kl_abs = c(3)*(mul*GRAVITY/rol)**(1./3.)*group1

      if (kl_abs < ZERO) then
         CALL INIT_ERR_MSG("CALC_KL_ONDA in usr_rates")
         write(err_msg, 1104) IJK
         write(*,*) 'epl     ', epl
         write(*,*) 'vliq    ', vliq
         write(*,*) 'lliq    ', lliq
         write(*,*) 'rol     ', rol
         write(*,*) 'mul     ', mul
         write(*,*) 'diffl   ', diffl
         write(*,*) 'GRAVITY ', GRAVITY
         write(*,*) 'wa_pack ', wa_pack
         write(*,*) 'sa_pack ', sa_pack
         write(*,*) 'd_pack  ', d_pack
         write(*,*) 'group1  ', group1
         call flush_err_msg(ABORT=.TRUE.)
         CALL FINL_ERR_MSG
 1104 FORMAT('Error 1104: Liquid phase mass transfer coefficient is ', &
         'less than',/'zero at IJK = ',i5)
      endif

      RETURN
      END SUBROUTINE calc_kl_onda





!----------------------------------------------------------------------!
! Subroutine: calc_lCO2int                                             !
!                                                                      !
! Purpose: Calculate the concentration of CO2 in the liquid at the     !
! interface. The concentration of CO2 is returned in units g-mol/cm^3  !
!                                                                      !
! References:                                                          !
! Pandya, J. D., Chemical Engineering Communications, Vol. 19, 1983,   !
!   343-361.                                                           !
!   equations 9, 12, 33                                                !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      SUBROUTINE calc_lCO2int(ijk, kl_rxn, kl_abs, kg_abs, &
          D_CO2, D_RNH2, H_CO2, C_RNH2, C_CO2, P_CO2, C_CO2i, P_CO2i)

      use param1, only: zero

      use error_manager
      IMPLICIT NONE

! ijk index
      INTEGER, INTENT(IN) :: IJK
! Kinetic reaction rate constant (cm^3/s.gmol)
      DOUBLE PRECISION, INTENT(IN) :: kl_rxn
! Liquid phase mass transfer coefficient for CO2 (cm/s)
! Physical absorption
      DOUBLE PRECISION, INTENT(IN) :: kl_abs
! Gas phase mass transfer coefficient for CO2 (mol/(barye.cm^2.s.))
      DOUBLE PRECISION, INTENT(IN) :: kg_abs
! Diffusivity of CO2 in liquid at given temperature (cm^2/s)
      DOUBLE PRECISION, INTENT(IN) :: D_CO2
! Diffusivity of solvent in solution at given temperature (cm^2/s)
      DOUBLE PRECISION, INTENT(IN) :: D_RNH2
! Henry's coefficient for CO2 in liquid solvent
      DOUBLE PRECISION, INTENT(IN) :: H_CO2

! Concentration of solvent in solution (bulk liquid)
      DOUBLE PRECISION, INTENT(IN) :: C_RNH2
! Concentration of CO2 in the liquid phase bulk (mol/cm^3)
      DOUBLE PRECISION, INTENT(IN) :: C_CO2
! Partial pressure of CO2 in gas phase bulk (barye)
      DOUBLE PRECISION, INTENT(IN) :: P_CO2
! Concentration of CO2 in liquid at the interface (gmol/cm^3)
      DOUBLE PRECISION, INTENT(OUT) :: C_CO2i
! Partial pressure of CO2 in gas at the interface (barye)
      DOUBLE PRECISION, INTENT(OUT) :: P_CO2i

! Enhancement factor (-)
      DOUBLE PRECISION :: E_num
! Partial pressure of CO2 at the interface
      DOUBLE PRECISION :: p_CO2i_hp, P_CO2i_hm
      DOUBLE PRECISION :: P_CO2i_n
! Corresponding concentration of CO2 at the interface
      DOUBLE PRECISION :: C_CO2i_hp, C_CO2i_hm
      DOUBLE PRECISION :: C_CO2i_0, C_CO2i_n, C_CO2i_np1
! Function to zero and derivative
      DOUBLE PRECISION :: func_n, deriv_func_n
! difference between previous and current guess
      DOUBLE PRECISION :: diff

! Numerical settings
! central derivative step size
      INTEGER :: NIT
      INTEGER, PARAMETER :: MAX_NIT = 50
      DOUBLE PRECISION, PARAMETER :: tol = 0.001d0
      DOUBLE PRECISION :: h_size


! First, calculate the enhancement factor. This requires knowing the
! concentration of CO2 at the interface. For an initial estimate of
! CO2 at the interface use either:
! the concentration of CO2 in bulk of gas (this is more likely to
! guarantee non-zero)
      C_CO2i_0 = P_CO2/H_CO2
! or simply assume it equals the bulk concentration in the liquid
!         C_LCO2i_0 = C_CO2

      NIT = 0
      diff = 1.0
      C_CO2i_n = C_CO2i_0
      DO WHILE (diff > tol .AND. NIT < MAX_NIT)
! Now calculate an enhancement factor
         E_num = calc_E_wellek(ijk, kl_rxn, kl_abs, D_CO2, &
                 D_RNH2, C_RNH2, C_CO2i_n)

! Second, calculate the partial pressure of CO2 at the interface
! based on the current value of the enhancement factor and the
! two film model
         p_CO2i_n= calc_PCO2int(kg_abs, kl_abs, H_CO2, E_num, &
                  P_CO2, C_CO2)

! Third, calculate the corresponding concentration of CO2 in the
! liquid at the interface that is in equilbrium with the partial
! pressure of CO2 in the gas at the interface using Henry's law
! (g-mol/cm^3)
! Henry's law: Pi = Hi*Ci or Ci = Pi/Hi
         C_CO2i_np1 = P_CO2i_n/H_CO2

! Finally, check whether the difference in the initial estimate and
! predicted interfacial value of CO2 are close. If so, no need to
! further iterate. If not, try again.
         diff = abs(C_CO2i_np1 - C_CO2i_n)
         C_CO2i_n = C_CO2i_np1
         NIT = NIT+1
      ENDDO

      C_CO2i = C_CO2i_np1
      P_CO2i = H_CO2*C_CO2i
      RETURN

! Alternative evaluation (overkill):
      NIT = 1
      h_size = 5.d-6*max(1.d0,C_CO2i_0)
      C_CO2i_n = C_CO2i_0
      DO NIT =1,MAX_NIT

! Calculate the function: C_CO2i - P_CO2i/H_CO2=0
         E_num = calc_E_wellek(ijk, kl_rxn, kl_abs, D_CO2, &
                 D_RNH2, C_RNH2, C_CO2i_n)
         p_CO2i_n = calc_PCO2int(kg_abs, kl_abs, H_CO2, E_num, &
                  P_CO2, C_CO2)
         func_n = C_CO2i_n - P_CO2i/H_CO2

! Numerically calculate the derivative using a central difference
! scheme:
         C_CO2i_hp = C_CO2i_n+h_size
         E_num = calc_E_wellek(ijk, kl_rxn, kl_abs, D_CO2, &
                 D_RNH2, C_RNH2, C_CO2i_hp)
         p_CO2i_hp = calc_PCO2int(kg_abs, kl_abs, H_CO2, E_num, &
                     P_CO2, C_CO2)

         C_CO2i_hm = C_CO2i_n-h_size
         E_num = calc_E_wellek(ijk, kl_rxn, kl_abs, D_CO2, &
                 D_RNH2, C_RNH2, C_CO2i_hm)
         p_CO2i_hm = calc_PCO2int(kg_abs, kl_abs, H_CO2, E_num, &
                     P_CO2, C_CO2)

         deriv_func_n = ((C_CO2i_hp - P_CO2i_hp/C_CO2i_hp) - &
                         (C_CO2i_hm - P_CO2i_hm/C_CO2i_hm)) /&
                        (2.d0*h_size)

! Calculate the next approximation to the root
         IF (deriv_func_n .NE. ZERO) THEN
            C_CO2i_np1 = C_CO2i_n - (func_n)/deriv_func_n
         ELSE
            C_CO2i_np1 = P_CO2i_n/H_CO2
         ENDIF

! Check for convergence
         DIFF = ABS(C_CO2i_np1 - C_CO2i_n)
! If converged exit
         IF (DIFF < TOL) EXIT

! If not update the guess and try again
         C_CO2i_n = C_CO2i_np1
      ENDDO

! Concentration of CO2 in the liquid at the interface
      C_CO2i = C_CO2i_np1
      P_CO2i = H_CO2*C_CO2i

      IF( NIT >= MAX_NIT .AND. DIFF > TOL) THEN
         CALL INIT_ERR_MSG("CALC_lCO2int in usr_rates")
         write(err_msg, 1108) IJK, trim(ival(C_CO2)), trim(ival(C_CO2i))
         CALL FLUSH_ERR_MSG
         CALL FINL_ERR_MSG
 1108 FORMAT('Warning 1108: Iterative calculation of interfacial CO2 ',&
         'concentration ',/,'not converging at IJK = ', i4,' with ',&
         'C_CO2 (bulk) = ', A,' and ',/,'C_CO2 (int) = ', A)
      ENDIF

      RETURN

      CONTAINS

!----------------------------------------------------------------------!
! Function: calc_E_wellek                                              !
!                                                                      !
! Purpose: Calculate the enhancement factor for second order reaction  !
! reaction which represents the ratio of mass transfer coefficient for !
! absorption when chemical reaction occurs to pure physical absorption !
! without chemical reaction. Dimensionless.                            !
!                                                                      !
! References:                                                          !
! Wellek, R. M., et al., the Canadian Journal of Chemical Engineering  !
!   56, 1978, 181-186                                                  !
!   equations 18a, 18b, 5, 4                                           !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_E_wellek(IJK, kl_rxn, kl_abs,&
            D_CO2, D_RNH2, C_RNH2, C_CO2i)

      use param1, only: small_number, large_number, undefined
      use usr, only: ENHANCEMENT_FACTOR
      use error_manager
      IMPLICIT NONE

! ijk indice
      INTEGER, INTENT(IN) :: IJK
! Kinetic reaction rate constant (cm^3/s.gmol)
      DOUBLE PRECISION, INTENT(IN) :: kl_rxn
! Liquid phase mass transfer coefficient for CO2 (cm/s)
! (physical absorption coefficient)
      DOUBLE PRECISION, INTENT(IN) :: kl_abs
! Diffusivity of CO2 in liquid at given temperature (cm^2/s)
      DOUBLE PRECISION, INTENT(IN) :: D_CO2
! Diffusivity of solvent in solution at given temperature (cm^2/s)
      DOUBLE PRECISION, INTENT(IN) :: D_RNH2
! Concentration of solvent in bulk liquid
      DOUBLE PRECISION, INTENT(IN) :: C_RNH2
! Concentration of CO2 in liquid at the interface
      DOUBLE PRECISION, INTENT(IN) :: C_CO2i

! Minimum amount of species concentration required for evaluation
      DOUBLE PRECISION, parameter :: c_Limiter = 1.0d-8

! Enhancement factors for pseudo first order and instantaneous
! reactions
      DOUBLE PRECISION :: E_1, E_inf
! Hatta number
      DOUBLE PRECISION :: M_Ha
!---------------------------------------------------------------------//

! Constant enhancement factor
      IF (ENHANCEMENT_FACTOR /= UNDEFINED) THEN
         calc_e_wellek = ENHANCEMENT_FACTOR
         RETURN
      ENDIF

! Calculate the enhancement factor for instantaneous reaction
! Equation 4
      if (C_CO2i > SMALL_NUMBER) then
         E_inf = 1.d0 + (C_RNH2/(2.d0*C_CO2i))*(D_RNH2/D_CO2)
      else
         E_inf = LARGE_NUMBER
      endif

! Calculate the Hatta number (dimensionless)
! Equation 5
! it is possible to have kl_abs ->0 at which point M_Ha may also
! approach infinity
      if (kl_abs > SMALL_NUMBER) then
         M_Ha = dsqrt(D_CO2*kl_rxn*C_RNH2)/(kl_abs)
      else
         M_Ha = LARGE_NUMBER
      endif

! Calculate the enhancement factor for pseudo-first-order reaction
! Equation 18b
      if (M_Ha == LARGE_NUMBER) then
         E_1 = LARGE_NUMBER
      elseif(M_Ha > SMALL_NUMBER) then
         E_1 = M_Ha/tanh(M_Ha)
      else
! limit of x/tanh(x) as x->0
        E_1 = 1.d0
      endif

! Calculate the enhancement factor
! Equation 18a (rearranged)
      if (e_inf == 1.d0 .OR. E_1 == 1.d0) then
! may want to report this...
         calc_e_wellek = 1.d0
      elseif (e_inf == large_number .and. e_1 == large_number) then
! may want to report this as a warning...
         calc_e_wellek = 1.d0+1.d0/( (1.d0/(E_inf-1.d0)**1.35) + &
                                   (1.d0/(E_1-1.d0)**1.35) )**(1./1.35)

         CALL INIT_ERR_MSG("CALC_E_WELLEK in usr_rates")
         write(err_msg, 1109) IJK
         write(*,*) 'M_Ha = ', M_ha
         write(*,*) 'kl_abs = ', kl_abs
         write(*,*) 'C_C02i = ', C_CO2i
         CALL FLUSH_ERR_MSG
         CALL FINL_ERR_MSG
 1109 FORMAT('Warning 1109: Enhancement factor at IJK = ', i4, ' is ',&
        'based on e_inf',/ 'and e_1 having large_number values. ',&
        'This is probably due',/'to one or more of the dependents: ',&
        'kl_abs->M/Ha and C_CO2i.')

      else
         calc_e_wellek = 1.d0+1.d0/( (1.d0/(E_inf-1.d0)**1.35) + &
                                   (1.d0/(E_1-1.d0)**1.35) )**(1./1.35)
      endif

      RETURN
      END FUNCTION calc_e_wellek


!----------------------------------------------------------------------!
! Function: calc_PCO2int                                               !
!                                                                      !
! Purpose: Calculate the partial pressure of CO2 in the gas phase      !
! at the interface. A number of assumptions are implicit in this       !
! model. Namely, the reaction is assumed to be fast enough for the     !
! reaction to occur primarily in the film. Therefore the concentration !
! of dissolved gas (CO2) in the bulk, and other species, are very near !
! in equilbrium with bulk of liquid. The partial pressure of CO2 is    !
! returned in units barye.                                             !
!                                                                      !
! References:                                                          !
! Pandya, J. D., Chemical Engineering Communications, Vol. 19, 1983,   !
!   343-361.                                                           !
!   equations 9, 12, 33                                                !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_PCO2int(kg_abs, kl_abs, H_CO2, &
                                E_num, P_CO2, C_lCO2)

      use param1, only: zero
      IMPLICIT NONE

! Gas phase mass transfer coefficient for CO2 (mol/(barye.cm^2.s.))
      DOUBLE PRECISION, INTENT(IN) :: kg_abs
! Liquid phase mass transfer coefficient for CO2 (cm/s)
! Physical absorption
      DOUBLE PRECISION, INTENT(IN) :: kl_abs
! Henry's coefficient for CO2 in liquid solvent
      DOUBLE PRECISION, INTENT(IN) :: H_CO2
! Enhancement factor (-)
      DOUBLE PRECISION, INTENT(IN) :: E_num
! Partial pressure of CO2 in gas phase bulk (barye)
      DOUBLE PRECISION, INTENT(IN) :: P_CO2
! Concentration of CO2 in the liquid phase bulk (mol/cm^3)
      DOUBLE PRECISION, INTENT(IN) :: C_lCO2

! Calculate the partial pressure of CO2 at the interface
! Equation 33
!      calc_PCO2int = (P_CO2 + (kl_abs*E_num/kg_abs)*C_lCO2) /&
!                     (1.d0 + (kl_abs*E_num/(kg_abs*H_CO2)))
! equivalent but different arrangement to allow for kg_abs ->0

      if (kg_abs > 0 .and. kl_abs > 0) then
         calc_PCO2int = (P_CO2 + (kl_abs/kg_abs)*E_num*C_lCO2) /&
                     (1.d0 + ((kl_abs/kg_abs)*E_num/H_CO2))
      else
         calc_PCO2int = zero
      endif

      RETURN
      END FUNCTION calc_PCO2int

      END SUBROUTINE calc_lCO2int


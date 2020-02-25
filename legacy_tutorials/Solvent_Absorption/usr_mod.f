!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: usr                                                    C
!  Purpose: Common block containing data for usr subroutines           C
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
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      MODULE usr
      use param, only: dim_I, dim_K

!---------------------------------------------------------------------//

      DOUBLE PRECISION DUMMY_DP

! CONSTANTS
! solids phase index corresponding to liquid phase
      INTEGER, PARAMETER :: index_liq = 1

! solids phase index corresponding to solids phase
      INTEGER, PARAMETER :: index_sol = 2


! These are included in the usrnlst:
! --------------------------------------------------------------------//

! If .TRUE. use solvent absorption type drag interactions
! gas-liquid, gas-solid, liquid-solid
      LOGICAL :: SOLVENT_ABSORPTION

! Flag for the coupled mass transfer/chemical reaction scheme
      CHARACTER(64):: ABSORPTION_CHEM_TYPE
      INTEGER:: ABSORPTION_CHEM_TYPE_ENUM
      INTEGER, PARAMETER:: UNDEFINED_ABSORPTION_CHEM_TYPE = 0
      INTEGER, PARAMETER:: SINGLE_STEP = 1
      INTEGER, PARAMETER:: EQUILIBRIUM_SEGREGATED = 2
      INTEGER, PARAMETER:: EQUILIBRIUM_COUPLED = 3

! If .TRUE. then apply mechanical dispersion forces to momentum
      LOGICAL :: MECH_DISPERSION
! Factor to describe spreading.
      DOUBLE PRECISION :: Spread_factor

! Specify a constant value for enhancement factor
! A value of 1 would enable modelling pure physical absorption
      DOUBLE PRECISION :: Enhancement_Factor

      CHARACTER(64) :: CAP_PRESS_TYPE
      INTEGER :: CAP_PRESS_TYPE_ENUM
      INTEGER, PARAMETER :: UNDEFINED_CAP_PRESS_TYPE = 0
      INTEGER, PARAMETER :: GROSSER_1988 = 1

      CHARACTER(64) :: USR_DRAG_TYPE
      INTEGER :: USR_DRAG_TYPE_ENUM
      INTEGER, PARAMETER :: UNDEFINED_USR_DRAG_TYPE = 0
      INTEGER, PARAMETER :: ATTOU_99_MOD = 1
      INTEGER, PARAMETER :: ATTOU_99 = 2
      INTEGER, PARAMETER :: SOLOMENKO_15 = 3
      INTEGER, PARAMETER :: LAPPALAINEN_09 = 4
      INTEGER, PARAMETER :: LAPPALAINEN_09_MOD = 5

! Surface tension of liquid on packing
      DOUBLE PRECISION :: OMEGA_L0
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: OMEGA_L

! Wetted area -fraction of liquid on packing
      CHARACTER(64) :: WETAREA_TYPE
      INTEGER :: WETAREA_TYPE_ENUM
      INTEGER, PARAMETER :: UNDEFINED_WETAREA_TYPE = 0
      INTEGER, PARAMETER :: ONDA_68 = 1
      INTEGER, PARAMETER :: LAPPALAINEN_08 = 2
      INTEGER, PARAMETER :: BILLET_95 = 3
      INTEGER, PARAMETER :: CONSTANT_WAF = 4
      DOUBLE PRECISION :: WetAreaFrac0
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: fWetArea_Pack
! Runtime flag to determine whether to apply a fractional wetted area
! factor to the interphase interaction terms
      LOGICAL :: APPLY_WAF

! The physical value of liquid viscosity
! If undefined it will be assigned mu_s0(liq). When specified
! this value is used in the drag correlation so that the variable
! mu_s0 can be used to specify an effective viscosity (sum of
! physical + pseudo turbulent viscosities). The effective viscosity
! is then used in the calculation of stresses.
      DOUBLE PRECISION :: LAM_MU_s0
! Same as above but for gas viscosity
      DOUBLE PRECISION :: LAM_MU_g0

! CHARACTERISTICS OF PACKED BED
! specific area of the packing: surface area of packing per
! unit volume of bed (cm^2/cm^3)
      DOUBLE PRECISION:: sa_pack

! nominal size of packing (cm)
! If undefined will be assigned d_p0(index_sol). The value of d_pack is
! only used in evaluation of mass transfer coefficients.  Otherwise
! d_p0(index_sol) is always used
      DOUBLE PRECISION :: d_pack

! critical surface tension of packing (surface energy) in (dyne/cm ~
! g/s^2)
      DOUBLE PRECISION :: omega_pack

! Packing specific wetted area parameter, only used for
! WETAREA_TYPE=BILLET_95
      DOUBLE PRECISION:: CH_PACK


! INTERFACE BLOCKS
! --------------------------------------------------------------------//



      CONTAINS

!----------------------------------------------------------------------!
! Function: calc_mw_mix_l                                              !
!                                                                      !
! Purpose: Calculate the average molecular weight of the liquid phase. !
! This is needed to calculate species mole fractions in the liquid     !
! phase.                                                               !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_mw_mix_l(ijk, m)

      use fldvar, only: x_s
      use physprop, only: mw_s, nmax
      use toleranc, only: omw_max
      use param1, only: one
      IMPLICIT NONE

! index
      INTEGER, INTENT(IN) :: IJK
! solids phase index representing liquid phase
      INTEGER, INTENT(IN) :: M
! average molecular weight of the liquid phase
      DOUBLE PRECISION :: MW

! Calculating the average molecular weight of the fluid.
!      IF (EP_S(IJK,M) > ZERO_EP_S) THEN
      MW = SUM(X_S(IJK,M,:NMAX(M))/MW_S(M,:NMAX(M)))
      MW = ONE/MAX(MW,OMW_MAX)
      calc_MW_MIX_l = MW

      RETURN
      END FUNCTION calc_mw_mix_l


!----------------------------------------------------------------------!
! Function: calc_mu_liq                                                !
!                                                                      !
! Purpose: Calculate the viscosity for partially loaded (w/ CO2) MEA   !
! solution at a given temperature. The viscosity of solution is        !
! returned in Poise (g/cm.s)                                           !
!                                                                      !
! This formula may be used to calculate MEA solution viscosity up to   !
! the following levels:                                                !
!    -amine concentrations up to 40 mass% MEA                          !
!    -CO2 loadings up to 0.6 mol of CO2/mol amine                      !
!    -temperature up to 398K.                                          !
!                                                                      !
! References:                                                          !
! Weiland, R. H., Dingman, J. C., Cronin, D. B., and Browning, G. J.,  !
!   Density and viscosity of some partially carbonated aqueous         !
!   alkanolamine solutions and their blends, Journal Chemical          !
!   Engineering Data, 1998, 43, 378-382                                !
!   Equations 5 and table 7.                                           !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_mu_liq(sol_T, omega, alpha)

      use param1, only: zero
      use run, only: units
      IMPLICIT NONE

! Temperature at which to calculate the viscosity (K)
      DOUBLE PRECISION, INTENT(IN) :: sol_T
! Mass percent of mea in liquid phase
      DOUBLE PRECISION, INTENT(IN) :: omega
! CO2 loading in (mol of CO2/mol mea)
      DOUBLE PRECISION, INTENT(IN) :: alpha

! parameters for viscosity correlation
! table 7 for MEA solution
      DOUBLE PRECISION, PARAMETER :: par_a = 0d0
      DOUBLE PRECISION, PARAMETER :: par_b = 0d0
      DOUBLE PRECISION, PARAMETER :: par_c = 21.186d0
      DOUBLE PRECISION, PARAMETER :: par_d = 2373.d0
      DOUBLE PRECISION, PARAMETER :: par_e = 0.01015
      DOUBLE PRECISION, PARAMETER :: par_f = 0.0093
      DOUBLE PRECISION, PARAMETER :: par_g = -2.2589

! viscosity of water (poise or g/cm.s)
      DOUBLE PRECISION, PARAMETER :: mu_H2O = 0.01d0
! ratio of viscosity of MEA solution to viscosity of water
      DOUBLE PRECISION :: ratio_mus


! calculate the ratio of viscosity
! equation 5
      ratio_mus = exp( ((par_a*omega+par_b)*sol_T + &
                        (par_c*omega+par_d))*&
                        (alpha*(par_e*omega+par_f*sol_T+par_g)+1.d0)*&
                        omega/(sol_T*sol_T) )

! calculate the viscosity of partially loaded mea solution (poise)
      calc_mu_liq = mu_H2O*ratio_mus
      IF (UNITS=='SI') calc_mu_liq=calc_mu_liq/10.d0

      RETURN
      END FUNCTION calc_mu_liq


!----------------------------------------------------------------------!
! Function: calc_omega_liq                                             !
!                                                                      !
! Purpose: Calculate the surface tension of an aqueous MEA solution.   !
! as function of solution temperature and mole fractions of mea and    !
! water. This is returned in units dyne/cm or g/s^2.                   !
!                                                                      !
! This work appears to be based entirely on water and MEA (no other    !
! species in solution). So the mole fractions in the present case      !
! are renormalized to a binary mixture of mea and h2o. Whether this    !
! normalization is appropriate or necessary is questionable.           !
!                                                                      !
! This quantity will affect the mass transfer rate. Some correlations  !
! for the liquid phase mass transfer coefficient require this property !
!                                                                      !
! References:                                                          !
! Hiwale, R., et al., IECR, 2012, 51, 4328-4346.                       !
!   equations 25, 26, 27                                               !
! Vazquez, G., Alvarez, E., Navaza, J. M. Rendo R., and Romero, E.,    !
!   Journal of Chemical Engineering Data, 42, 1, 1997, 57-59           !
!   See equations 1 and 2 and tables 4 and 5.                          !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_omega_liq(sol_T, y_lmea, y_lh2o)

      use param1, only: zero
      use run, only: units
      IMPLICIT NONE

! Temperature at which to calculate the surface tension (K)
      DOUBLE PRECISION, INTENT(IN) :: sol_T
! Mole fraction of mea in liquid phase
      DOUBLE PRECISION, INTENT(IN) :: y_lmea
! Mole fraction of h2o in liquid phase
      DOUBLE PRECISION, INTENT(IN) :: y_lh2o

! Surface tension of CO2
      DOUBLE PRECISION :: omega_H2O
! Surface tension of MEA
      DOUBLE PRECISION :: omega_MEA
! Renormalized mole fraction of h2o and mea.
      DOUBLE PRECISION :: yn_lmea, yn_lh2o


! Renormalize the liquid phase mole fractions to the 2 component
      if( (y_lmea +y_lh2O) > zero) then
         yn_lmea = y_lmea/(y_lmea+y_lh2O)
         yn_lh2o = y_lh2o/(y_lmea+y_lh2O)
      else
         yn_lmea = 0.d0
         yn_lh2o = 1.d0
      endif

! Calculate the surface tension of pure H2O
! Reference 1 eq 26
! Reference 2: Eq 1, table 4 (pure h2O)
      if (sol_T >=273.15) then
         omega_h2o = 76.0852d0-0.1609d0*(sol_T-273.15d0)  !(dyne/cm)
! Calculate the surface tension of pure MEA
! Reference 1 eq 27
! Reference 2: Eq 1, table 4 (pure mea)
         omega_mea = 53.082d0-0.1648d0*(sol_T-273.15d0)  !(dyne/cm)
      else
         omega_h2o = 76.0852d0
         omega_mea = 53.082d0
      endif

! Calculate the surface tension of aqueous MEA solution
! Reference 1 eq 25 : slightly modifies reference 2 to account for
!   temperature dependency indicated by ref 2 table 5
! Reference 2: Eq 2, table 5
      calc_omega_liq = omega_h2o - (omega_h2o-omega_mea)* (1.d0 +&
            (0.63036d0-1.3d-5*(sol_T-273.15d0))*yn_lh2O/&
            (1.d0-(0.947-2d-5*(sol_T-273.15d0))*yn_lh2O))*yn_lmea
           ! (dyne/cm)
      IF (UNITS == 'SI') calc_omega_liq = calc_omega_liq*1000.d0

      RETURN
      END FUNCTION calc_omega_liq

!----------------------------------------------------------------------!
! Function: calc_cond_liq                                              !
!                                                                      !
! Purpose: Calculate the conductivity of an aqueous MEA solution.      !
! as function of solution temperature and mass fractions of mea and    !
! water. This is returned in units cal/(s.cm.K)                        !
!                                                                      !
! This work appears to be based entirely on water and MEA (no other    !
! species in solution). So the mass fractions in the present case      !
! are renormalized to a binary mixture of mea and h2o. Whether this    !
! normalization is appropriate or necessary is questionable.           !
!                                                                      !
!                                                                      !
! References:                                                          !
! Cheng, S. et al, Hydrocarbon Processing, 1996, 81-84.                !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_cond_liq(sol_T, x_lmea, x_lh2o)

      use param1, only: zero
      use run, only: units
      IMPLICIT NONE

! Temperature at which to calculate the conductivity (K)
      DOUBLE PRECISION, INTENT(IN) :: sol_T
! Mass fraction of mea in liquid phase
      DOUBLE PRECISION, INTENT(IN) :: x_lmea
! Mass fraction of h2o in liquid phase
      DOUBLE PRECISION, INTENT(IN) :: x_lh2o

! thermal conductivity of water, mea and aqueous mea
      DOUBLE PRECISION :: kl_h2O, kl_mea, kl_aqmea
! Renormalized mass fraction of h2o and mea.
      DOUBLE PRECISION :: xn_lmea, xn_lh2o
! temperature in celsius
      DOUBLE PRECISION :: T_C

! Renormalize the liquid phase mole fractions to the 2 component
      if( (x_lmea +x_lh2O) > zero) then
         xn_lmea = x_lmea/(x_lmea+x_lh2O)
         xn_lh2o = x_lh2o/(x_lmea+x_lh2O)
      else
         xn_lmea = 0.0
         xn_lh2o = 1.0
      endif

! Convert temperature to celsius
      T_C = sol_T - 273.15

! thermal conductivity of 10% mass fraction aq MEA in (W/m/K)
      kl_mea = 0.2363

      IF (T_C>=ZERO) THEN
! thermal conductivity of pure H2O in (W/m/K)
         kl_h2o = 0.0054545*T_C**0.686+0.5562
! thermal conductivity of aqueous MEA solution in (W/m/K)
         kl_aqmea = (1.0-xn_lmea)*kl_h2o + xn_lmea*kl_mea + &
            xn_lmea*(1.0-xn_lmea)*(-0.2478 - 0.00061806*T_C + &
            0.02117*xn_lmea*T_C**0.2322)
      ELSE
         kl_h2O = 0.5562
         kl_aqmea = (1.0-xn_lmea)*kl_h2o + xn_lmea*kl_mea + &
            xn_lmea*(1.0-xn_lmea)*(-0.2478)
      ENDIF

      calc_cond_liq = kl_aqmea

! Convert to MFIX 'CGS' units for conducivity which is cal/(cm.s.K)
      IF (UNITS == 'CGS') calc_cond_liq = kl_aqmea/4.1868

      RETURN
      END FUNCTION calc_cond_liq


!----------------------------------------------------------------------!
! Function: calc_rho_liq                                               !
!                                                                      !
! Purpose: Calculate the density for partially loaded (w/ CO2) MEA     !
! solution. The density of solution is returned in g/cm^3.             !
!                                                                      !
! This is based on a correlation for density of amine solutions using  !
! pure-component molar volumes together with excess molar volumes.     !
! The expression may be used to calculate the density of MEA solutions !
! up to the following levels:                                          !
!    -CO2 loadings up to 0.6 mol of CO2/mol amine                      !
!    -temperature up to 398K.                                          !
!                                                                      !
! References:                                                          !
! Weiland, R. H., Dingman, J. C., Cronin, D. B., and Browning, G. J.,  !
!   Density and viscosity of some partially carbonated aqueous         !
!   alkanolamine solutions and their blends, Journal Chemical          !
!   Engineering Data, 1998, 43, 378-382                                !
!   Equations 1, 2, 3, 4 and table 6.                                  !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_rho_liq(ijk, sol_T, y_lmea, y_lh2o, &
                                y_lco2,m)

      use physprop, only: mw_s
      use param1, only: zero
      use run, only: units
      use error_manager
      IMPLICIT NONE
! index
      INTEGER, INTENT(IN) :: IJK
! Temperature at which to calculate density (K)
      DOUBLE PRECISION, INTENT(IN) :: sol_T
! Mole fraction of mea in liquid phase
      DOUBLE PRECISION, INTENT(IN) :: y_lmea
! Mole fraction of h2o in liquid phase
      DOUBLE PRECISION, INTENT(IN) :: y_lh2o
! Mole fraction of co2 in liquid phase
      DOUBLE PRECISION, INTENT(IN) :: y_lco2
! solids phase index representing liquid phase
      INTEGER, INTENT(IN) :: M

! parameters for density correlation
! table 6 for MEA
      DOUBLE PRECISION, PARAMETER :: par_a = -5.35162d-7
      DOUBLE PRECISION, PARAMETER :: par_b = -4.51417d-4
      DOUBLE PRECISION, PARAMETER :: par_c = 1.19451d0
      DOUBLE PRECISION, PARAMETER :: par_d = 0.0
      DOUBLE PRECISION, PARAMETER :: par_e = 0.0
      DOUBLE PRECISION, PARAMETER :: vstar = -1.8218
! molar volume for dissolved CO2 (cm^3/mol)
! (unrelated to its pure molar component value)
! table 6 for MEA
      DOUBLE PRECISION, PARAMETER :: vco2 = 0.04747

! molar volume of pure mea (cm^3/mol)
      DOUBLE PRECISION :: Vmea
! molar volume associated with interaction
      DOUBLE PRECISION :: vdstar
! molar volume of solution (cm^3/mol)
      DOUBLE PRECISION :: vsol
! molar volume of water (cm^3/mol)
      DOUBLE PRECISION :: vh2o
! density of water (g/cm^3)
      DOUBLE PRECISION, PARAMETER :: rho_h2o = 1.d0
! density of pure mea (g/cm^3)
      DOUBLE PRECISION :: rho_mea

      INCLUDE 'species.inc'

! The molar volume of a substance is its molecular weight divided by its
! mass density
! Calculate the molar volume of h2o (cm^3/mol)
      vh2o = MW_s(m,lH2O)/rho_h2o

! Calculate the molar volume of pure mea (cm^3/mol)
! equation 3
      rho_mea = par_a*sol_T*sol_T+par_b*sol_T+par_c
      Vmea = MW_s(M,lRNH2)/rho_mea

! Calculate the molar volume associated with the interaction between
! carbon dioxide and the mea
! equation 4
      vdstar = par_d+par_e*y_lmea    ! this is zero based on this ref.

! Calculate the molar volume of the solution. Assume vh2o represents
! the pure molar volume of water even though the similar term vco2
! does not represent pure molar volume of co2 but the molar volume of
! dissolved co2 (cm^3/mol)
! equation 2
      vsol = y_lmea*vmea + y_lh2o*vh2o + y_lco2*vco2 + &
         y_lmea*y_lh2o*vstar + y_lmea*y_lco2*vdstar

! Calculate the density of mea solution (g/cm^3)
      if (vsol >= zero) then
         calc_rho_liq = (y_lmea*MW_s(M,lRNH2) + y_lh2o*MW_s(M,lH2O) + &
                      y_lco2*MW_s(M,lCO2))/vsol
         IF (UNITS=='SI') calc_rho_liq = calc_rho_liq*1000.d0
      else
         CALL INIT_ERR_MSG("CALC_RHO_LIQ in usr_rates")
         write(err_msg, 1101) IJK
         write(*,*) 'vsol          ', vsol
         write(*,*) 'sol_T         ', sol_T
         write(*,*) 'rho_mea       ', rho_mea
         write(*,*) 'vstar, vdstar ', vstar, vdstar
         write(*,*) 'y_lmea, vmea  ', y_lmea, vmea
         write(*,*) 'y_lh2o, vh2o  ', y_lh2o, vh2o
         write(*,*) 'y_lco2, vco2  ', y_lco2, vco2
         call flush_err_msg(ABORT=.TRUE.)
         CALL FINL_ERR_MSG
 1101 FORMAT('Error 1101: Molar volume of solution is less than ', &
         'zero at',/'IJK = ',i5)
      endif

      RETURN
      END FUNCTION calc_rho_liq


!----------------------------------------------------------------------!
! Function: calc_D_CO2inMEA                                            !
!                                                                      !
! Purpose: Calculate the diffusivity of CO2 in MEA solution at given   !
! temperature. The diffusivity coefficient is returned in units        !
! cm^2/s.                                                              !
!                                                                      !
! Calculate the diffusivity of CO2 in MEA using the 'N2O' analogy      !
! combined with a stokes-einstein type relationship for diffusivity    !
! of N2O in water and in MEA                                           !
! N2O analogy : D_co2inmea = D_n2oinmea*(D_co2inh2O/D_n2oinh2o)        !
! stokes-einsten: D_n2oinmea*mu_mea^n = const                          !
!                 D_n2oinh2o*mu_h2o^n = const                          !
!                                                                      !
! References:                                                          !
! Versteeg, G., and van Swaaij, W., J. Chem. Eng. Data, 1988,          !
!   33, 29.                                                            !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_D_CO2inMEA(sol_T,mu_aqmea)
      use run, only: units

      IMPLICIT NONE

! Temperature at which to calculate the diffusivity (K)
      DOUBLE PRECISION, INTENT(IN) :: sol_T
! Viscosity of mea solution (poise)
      DOUBLE PRECISION, INTENT(IN) :: mu_aqmea

! viscosity of water (poise or g/cm.s)
      DOUBLE PRECISION, PARAMETER :: mu_h2O = 0.01d0

! ratio of viscosity of MEA solution to viscosity of water
      DOUBLE PRECISION :: ratio_mus
! Diffusivity of CO2 in water (cm^2/s)
      DOUBLE PRECISION :: D_CO2inH2O
! Diffusivity of N2O in water (cm^2/s)
      DOUBLE PRECISION :: D_N2OinH2O

! Calculate the diffusivity of CO2 in water; eq 8
      D_CO2inH2O = 2.35d-2 * exp (-2119.d0/sol_T)  !(cm^2/s)
! Calculate the diffusivity of N2O in water; eq 9
      D_N2OinH2O = 5.07d-2 * exp(-2371.d0/sol_T)   !(cm^2/s)
! Calculate ratio (water to mea) of viscosities
      ratio_mus = mu_aqmea/mu_H2O   !(-)

! Calculate the diffusivity of CO2 in mea solution using combination of:
! -N2O analogy to relate diffusivity of CO2 in MEA to diffusivity of
!  N2O in MEA and diffusivity of both CO2 and N2O in H2O
! -stokes-einstein relation to relate diffusivity of N2O in MEA and H2O
!  to the viscosity of N2O in MEA and H2O; eq 11
! The latter avoids the need for diffusivity of N2O in MEA
      calc_D_CO2inMEA = D_CO2inH2O*ratio_mus**0.80   !(cm^2/s)
      IF (UNITS=='SI') calc_D_CO2inMEA=calc_D_CO2inMEA/10000.d0

      RETURN
      END FUNCTION calc_D_CO2inMEA


!----------------------------------------------------------------------!
! Function: calc_D_MEAinMEA                                            !
!                                                                      !
! Purpose: Calculate the diffusivity of MEA in aqueous MEA solution at !
! given temperature and concentration. The diffusivity coefficient is  !
! returned in units cm^2/s.                                            !
!                                                                      !
! Calculate the diffusivity of MEA in MEA solution using correlation   !
! based on experimental data that is valid in the following ranges:    !
!     -temperature 298-333K                                            !
!     -concentration from 43 to 5016 mole/m^3                          !
!                                                                      !
! References:                                                          !
! Snijder, E. D., te Riele, M. M. M., Versteeg, G. F., and van Swaaij, !
! W. P. M., Diffusion coefficients of several aqueous alkanolamine     !
! solutions, J. Chem. Eng. Data, 1993, 38, 475-480                     !
!   Equation 8                                                         !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_D_MEAinMEA(sol_T,C_lCO2)
      use run, only: units

      IMPLICIT NONE
! Temperature at which to calculate the diffusivity (K)
      DOUBLE PRECISION, INTENT(IN) :: sol_T
! Concentration of MEA in solution (mol/cm^3)
      DOUBLE PRECISION, INTENT(IN) :: C_LCO2

      DOUBLE PRECISION :: D_MEAinMEA

! Calculate the diffusivity of MEA in aq MEA; eq 8
! In this expression the concentration was converted from
! mol/m^3 to mol/cm^3.
      D_MEAinMEA = exp(-13.275d0 - 2198.3d0/sol_T - &
                       78.142d0*C_LCO2) !(m^2/s)

! Convert to the necessary units
      calc_D_MEAinMEA = 100.d0*100.d0*D_MEAinMEA  !(cm^2/s)

      IF (UNITS=='SI') calc_D_MEAinMEA=D_MEAinMEA  !(m^2/s)

      RETURN
      END FUNCTION calc_D_MEAinMEA


!----------------------------------------------------------------------!
! Function: calc_H_CO2inMEA                                            !
!                                                                      !
! Purpose: Calculate Henry's law constant for CO2 in MEA as function   !
! temperature. The Henry's law constant is returned in barye*cm^3/mol. !
!                                                                      !
! References:                                                          !
! Hiwale, R., et al., IECR, 2012, 51, 4328-4346.                       !
!   equations 2, 3, 4, 5.                                              !
! Versteeg, G., and van Swaaij, W., J. Chem. Eng. Data, 1988,          !
!   33, 29.                                                            !
! Wang, Y., et al., Chem. Eng. J. 1992, 48, 31                         !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_H_CO2inMEA(sol_T)
      use constant, only: C
      use run, only: units

      IMPLICIT NONE

! Temperature at which to calculate the solubility
      DOUBLE PRECISION, INTENT(IN) :: sol_T

! Solubility of CO2 in water
      DOUBLE PRECISION :: H_CO2inH2O
! Solubility of N2O in water
      DOUBLE PRECISION :: H_N2OinH2O
! Solubility of N2O in MEA
      DOUBLE PRECISION :: H_N2OinMEA
! Solubility of CO2 in MEA
      DOUBLE PRECISION :: H_CO2inMEA

! Calculate the solubility of CO2 in water; eq 3
      H_CO2inH2O = 2.8249d6 * exp (-2044.d0/sol_T)  !(kPa.m^3/kmol)
! Calculate the solubility of N2O in water; eq 4
      H_N2OinH2O = 8.5470d6 * exp(-2284.d0/sol_T)   !(kPa.m^3/kmol)
! Calculate the solubility of N2O in MEA; eq 5
      H_N2OinMEA = 1.207d5 * exp(-1136.5d0/sol_T)   !(kPa.m^3/kmol)
! Calculate the solubility of CO2 in MEA using the 'N2O'
! analogy; eq 2
      H_CO2inMEA = H_N2OinMEA*(H_CO2inH2O/H_N2OinH2O) !(kPa.m^3/kmol)
! Convert to the necessary units.
      calc_H_CO2inMEA = C(1)*10.0d0*(100.d0*100.d0*100.d0)* &
                   H_CO2inMEA ! (barye*cm^3/mol)

      IF (UNITS=='SI') calc_H_CO2inMEA=c(1)*1000.d0*H_CO2inMEA  !(Pa.m^3/kmol)

      RETURN
      END FUNCTION calc_H_CO2inMEA


!----------------------------------------------------------------------!
! Function: calc_k2_hikita                                             !
!                                                                      !
! Purpose: Calculate the second order reaction rate constant for the   !
! reaction between CO2 and MEA. The kinetic reaction rate constant is  !
! returned in units (cm^3/s.gmol).                                     !
! The reaction mechanism is generally considered to consist of two     !
! steps:                                                               !
! 1) formation of the zwitterion:                                      !
!      CO2 + HOC2H4NH2 -> OHC2H4NH2+COO-                               !
! 2) zwitterion deprotonation (removal of proton by a base e.g., amine !
!    OH- or H2O):                                                      !
!      HOC2H4NH2+COO- + HOC2H4NH2 -> HOC2H4NH3+ + HOC2H4NHCOO-         !
! However, formation of the zwitterion is taken to be the rate         !
! governing step with zwitterion deprotation occuring much faster:     !
! So the rate can be simplified to be first order with respect to      !
! amine and CO2 concentration.                                         !
! This kinetics is assumed to govern the overall reaction:             !
!      CO2+2 HOC2H4NH2 -> HOC2H4NH3+ + HOC2H4NHCOO-                    !
!                                                                      !
! References:                                                          !
! Hiwale, R., et al., IECR, 2012, 51, 4328-4346.                       !
!   equations 52 and text between eqn 45-46.                           !
! Hikita, H., et al., CEJ, 1977, 13, 7-2.                              !
!   equations 2                                                        !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_k2_hikita(sol_T)
      use run, only: units

      IMPLICIT NONE

! Temperature at which to calculate the reaction rate (K)
      DOUBLE PRECISION, INTENT(IN) :: sol_T

      DOUBLE PRECISION :: k2_hikita

! Calculate the rate constant (l/gmol.s)
      k2_hikita = exp(10.99d0 - 2152d0/sol_T)
! Convert to necessary units (cm^3/gmol.s)
      calc_k2_hikita = 1000.d0*k2_hikita

      IF (UNITS=='SI') calc_k2_hikita=k2_hikita/1000.0  !(m^3/kmol.s)

      RETURN
      END FUNCTION calc_k2_hikita


!----------------------------------------------------------------------!
! Function: calc_fwa_onda                                              !
!                                                                      !
! Purpose: Calculate the fractional wetted surface area of the         !
!   packing. Column is random packed bed (raschig rings, berl saddles, !
!   spheres, etc).                                                     !
!   Returns fraction (0-1) of wetted area of packing which is the      !
!   ratio of wetted surface area to specific surface area. Is a        !
!   dimensionless quantity.  When multiplied by the specific surface   !
!   it should return units per volume of bed (length^2/length^3)       !
!                                                                      !
! References:                                                          !
! Onda K. et al., Mass transfer coefficients between gas and liquid    !
!   phases in packed columns, Journal of Chemical Engineering of       !
!   Japan, 1968, 56-62.                                                !
!                                                                      !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_fwa_onda(ijk, omega, mul, rol, &
                                              epl, M)

      use constant, only: gravity
      use fldvar, only: u_s, v_s, w_s
      use functions, only: im_of, jm_of, km_of
      use fun_avg, only: avg_x_e, avg_y_n, avg_z_t
      use indices, only: i_of
      use param1, only: small_number, zero
      use error_manager
      IMPLICIT NONE

! index
      INTEGER, INTENT(IN) :: IJK
! Liquid surface tension (dyne/cm or g/s^2)
      DOUBLE PRECISION, INTENT(IN) :: omega
! Liquid viscosity (g/cm.s)
      DOUBLE PRECISION, INTENT(IN) :: mul
! Liquid density (g/cm^3)
      DOUBLE PRECISION, INTENT(IN) :: rol
! Liquid volume fraction (-)
      DOUBLE PRECISION, INTENT(IN) :: epl
! mfix solids phase index representing liquid phase
      INTEGER, INTENT(IN) :: M


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
! Modified liquid Reynolds number
      DOUBLE PRECISION :: Rel
! Liquid Weber number
      DOUBLE PRECISION :: Wel
! Froude number
      DOUBLE PRECISION :: Frl
! local alias for gravity to accomodate special case of g=0
      DOUBLE PRECISION :: lgravity

! Initialize fluid cell variables
      I =  I_OF(IJK)
      IMJK  = IM_OF(IJK)
      IJMK  = JM_OF(IJK)
      IJKM  = KM_OF(IJK)

! Calculate velocity components at i, j, k
      ULC = AVG_X_E(U_s(IMJK,M),U_s(IJK,M),I)
      VLC = AVG_Y_N(V_s(IJMK,M),V_s(IJK,M))
      WLC = AVG_Z_T(W_s(IJKM,M),W_s(IJK,M))

! to handle special cases
      IF (GRAVITY /= ZERO) THEN
         lgravity = GRAVITY
      ELSE
         lgravity = GRAVITY+SMALL_NUMBER
      ENDIF

! Calculate the magnitude of liquid interstitial velocity
      vliq = dsqrt(ULC*ULC + VLC*VLC + WLC*WLC)
!      vliq = max(vliq,small_number)

! Calculate superficial liquid mass velocity (g/cm^2.s)
      lliq = Vliq*rol*epl

! sa_pack = total surface area of packing per unit volume of bed
! (cm^2/cm^3). commonly referred to as the specific area

! Calculate the Reynolds number (-)
      Rel = lliq/(sa_pack*mul)

! Calculate the Weber number (-)
      Wel = lliq*lliq/(rol*omega*sa_pack)

! Calculate the Froude number (-)
      Frl = sa_pack*lliq*lliq/(rol*rol*lgravity)

! Eq 1 calculation 1 (dimensionless)
!      group1 = 1.d0-exp(-1.45*(omega_pack/omega)**0.75*&
!               (Rel**(0.1))*(Frl**(-0.05))*(Wel**(0.2)))

!      group1 = 1.d0-exp(-1.45*(omega_pack/omega)**(0.75)*&
!                  (lliq/(sa_pack*mul))**(0.1)*&
!                  (sa_pack*lliq*lliq/(rol*rol*lgravity))**(-0.05)*&
!                  (lliq*lliq/(rol*omega*sa_pack))**(0.2))

! omega_pack = critical surface tension (surface energy of packing
! material). (dyne/cm or g/s^2)

! equivalent but different arrangement to allow for lliq->0
      group1 = 1.d0-exp(-1.45*(omega_pack**0.75 * lgravity**0.05 *&
                        lliq**0.4)/(omega**0.95 * sa_pack**0.35 *&
                        (mul*rol)**0.1))

! Calculate the fracitional wetted area (dimensionless)
      calc_fwa_onda= group1

      if (calc_fwa_onda < ZERO) then
         CALL INIT_ERR_MSG("CALC_FWA_ONDA in usr_mod")
         write(err_msg, 1103) IJK
         write(*,*) 'epl     ', epl
         write(*,*) 'omega_c ', omega_pack
         write(*,*) 'omega   ', omega
         write(*,*) 'lliq    ', lliq
         write(*,*) 'sa_pack ', sa_pack
         write(*,*) 'rol     ', rol
         write(*,*) 'GRAVITY ', lgravity
         write(*,*) 'omega_c/omega^0.75 ', (omega_pack/omega)**(0.75)
         write(*,*) 'Rel^0.1   ', Rel**0.1
         write(*,*) 'Frl^-0.05 ', Frl**(-0.05)
         write(*,*) 'Wel^0.2   ', Wel**0.2
         write(*,*) 'exp:      ', exp(-1.45*(omega_pack/omega)**0.75*&
               (Rel**(0.1))*(Frl**(-0.05))*(Wel**(0.2)))
         call flush_err_msg(ABORT=.TRUE.)
         CALL FINL_ERR_MSG
 1103 FORMAT('Error 1103: Fractional wetted area is <= 0 at IJK ', i5)
      endif

      RETURN
      END FUNCTION calc_fwa_onda


!----------------------------------------------------------------------!
! Function: calc_fwa_lapp                                              !
!                                                                      !
! Purpose: Calculate the fractional wetted surface area of the         !
!   packing. This model is based on fitting a large set of exp.        !
!   data on wetting efficiency from trickle bed reactors and also on   !
!   its suitability to predict overall column data.                    !
!   spheres, etc).                                                     !
!   Returns fraction (0-1) of wetted area of packing which is the      !
!   ratio of wetted surface area to specific surface area. Is a        !
!   dimensionless quantity. When multiplied by the specific surface    !
!   it should return units per volume of bed (length^2/length^3)       !
!                                                                      !
! References:                                                          !
! Lappalainen K., et al., Improved hydrodynamic model for wetting      !
!   efficiency, pressure drop, and liquid holdup in trickle-bed        !
!   reactors, Ind. Eng. Chem. Res., 2008, 47, 8436-8444.               !
!                                                                      !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_fwa_lapp(ijk, epg, mug, rog, omega, &
                                  epl, mul, rol, ep_pack, dp_pack, M)

      use constant, only: gravity
      use fldvar, only: u_s, v_s, w_s, u_g, v_g, w_g
      use functions, only: im_of, jm_of, km_of
      use fun_avg, only: avg_x_e, avg_y_n, avg_z_t
      use indices, only: i_of
      use param1, only: small_number, zero, one
      use error_manager
      IMPLICIT NONE

! index
      INTEGER, INTENT(IN) :: IJK
! Liquid surface tension (dyne/cm or g/s^2)
      DOUBLE PRECISION, INTENT(IN) :: omega
! Gas & Liquid viscosity (g/cm.s)
      DOUBLE PRECISION, INTENT(IN) :: mug, mul
! Gas & Liquid density (g/cm^3)
      DOUBLE PRECISION, INTENT(IN) :: rog, rol
! gas and liquid volume fraction (-)
      DOUBLE PRECISION, INTENT(IN) :: epg, epl
! packing volume fraction (-)
      DOUBLE PRECISION, INTENT(IN) :: ep_pack
! effective diameter of packing elements (cm)
      DOUBLE PRECISION, INTENT(IN) :: dp_pack
! mfix solids phase index representing liquid phase
      INTEGER, INTENT(IN) :: M


! Various fluid cell indicies
      INTEGER :: I, IMJK, IJMK, IJKM
! Gas & Liquid Velocity - cell centered
      DOUBLE PRECISION :: ULC, VLC, WLC, UGC, VGC, WGC
! Magnitude of gas & liquid interstitial velocity
      DOUBLE PRECISION :: vgas, vliq
! Gas & liquid superficial velocity
      DOUBLE PRECISION :: ugas, uliq
! Modified liquid Reynolds number
      DOUBLE PRECISION :: Rel
! Modified Eotvos number
      DOUBLE PRECISION :: Eol
! Modified Gas Galileo number
      DOUBLE PRECISION :: Gag
! Gas Froude number
      DOUBLE PRECISION :: Frg
! Calculation quantities
      DOUBLE PRECISION :: group1
! local alias for gravity to accomodate special case of g=0
      DOUBLE PRECISION :: lgravity

! Initialize fluid cell variables
      I =  I_OF(IJK)
      IMJK  = IM_OF(IJK)
      IJMK  = JM_OF(IJK)
      IJKM  = KM_OF(IJK)

! Calculate velocity components at i, j, k
      ULC = AVG_X_E(U_s(IMJK,M),U_s(IJK,M),I)
      VLC = AVG_Y_N(V_s(IJMK,M),V_s(IJK,M))
      WLC = AVG_Z_T(W_s(IJKM,M),W_s(IJK,M))

      UGC = AVG_X_E(U_g(IMJK),U_g(IJK),I)
      VGC = AVG_Y_N(V_g(IJMK),V_g(IJK))
      WGC = AVG_Z_T(W_g(IJKM),W_g(IJK))

! to handle special cases
      IF (GRAVITY /= ZERO) THEN
         lgravity = GRAVITY
      ELSE
         lgravity = GRAVITY+SMALL_NUMBER
      ENDIF

! Calculate the magnitude of gas&liquid interstitial velocity
      vliq = dsqrt(ULC*ULC + VLC*VLC + WLC*WLC)
      vgas = dsqrt(UGC*UGC + VGC*VGC + WGC*WGC)
!      vliq = max(vliq,small_number)

! Calculate superficial gas&liquid velocity (cm/s)
      uliq = Vliq**epl
      ugas = Vgas**epg

! Calculate the Reynolds number (-)
      if (mul > zero) then
         Rel = rol*dp_pack*uliq/(mul*ep_pack)
      else
         Rel = zero
      endif

! Calculate the Eotvos number (-)
      eol = rol*lgravity*dp_pack**2*(ONE-ep_pack)**2/(omega*(ep_pack**2))

! Calculate the Galileo number (-)
      if (mug > zero) then
         Gag = rog**2*lgravity*dp_pack**3*(ONE-ep_pack)**3/(mug**2*ep_pack**3)
      else
         Gag = zero
      endif

! Calculate the Froude number (-)
      Frg = ugas/sqrt(lgravity*dp_pack)

! Eq 13 and table 2
      group1 = 0.335 * Rel**0.185 * Eol**(-0.188) * Gag**0.027 * &
               (ONE+Frg)**(-0.014)

! Calculate the fracitional wetted area (dimensionless)
      calc_fwa_lapp = group1

      if (calc_fwa_lapp < ZERO) then
         CALL INIT_ERR_MSG("CALC_FWA_LAPP in usr_mod")
         write(err_msg, 1103) IJK
         write(*,*) 'epl, epg   ', epl, epg
         write(*,*) 'omega      ', omega
         write(*,*) 'uliq, ugas ', uliq, ugas
         write(*,*) 'rol, rog   ', rol, rog
         write(*,*) 'ep_pack, dp_pack   ', ep_pack, dp_pack
         write(*,*) 'GRAVITY ', lgravity
         write(*,*) 'Rel^0.185   ', Rel**0.185
         write(*,*) 'Eol^-0.188   ', Eol**(-0.188)
         write(*,*) 'Frl^-0.014 ', (ONE+Frg)**(-0.014)
         write(*,*) 'Gag^0.027   ', Gag**0.027
         write(*,*) 'exp:      ', 0.335*Rel**0.185*Eol**(-0.188)* &
               Gag**(0.027)*(ONE+Frg)**(-0.0514)
         call flush_err_msg(ABORT=.TRUE.)
         CALL FINL_ERR_MSG
 1103 FORMAT('Error 1103: Fractional wetted area is <= 0 at IJK ', i5)
      endif

      RETURN
      END FUNCTION calc_fwa_lapp

!----------------------------------------------------------------------!
! Function: calc_fwa_billet                                            !
!                                                                      !
! Purpose: Calculate the fractional wetted surface area of the         !
! packing according to the correlation of Billet for random and        !
! structured packings.                                                 !
!                                                                      !
! References:                                                          !
! Billet, R., Packet towers (In Processing and Environmental           !
! Technology),VCH Verlagsgesellschaft mbH, Weinheim (1995).            !
!   -See equations 4-74, 4-75 for fractional wetted area               !
!   -See equation 4-69 (or 4-27) for reynolds number for liquid        !
!   -See equation 4-27 for froude number for liquid                    !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION calc_fwa_billet(ijk, epl, mul, rol, M)
      use constant, only: gravity
      use fldvar, only: u_s, v_s, w_s
      use functions, only: im_of, jm_of, km_of
      use fun_avg, only: avg_x_e, avg_y_n, avg_z_t
      use indices, only: i_of
      use param1, only: small_number, zero, one
      use error_manager
      IMPLICIT NONE

! index
      INTEGER, INTENT(IN) :: IJK
! Liquid viscosity (g/cm.s)
      DOUBLE PRECISION, INTENT(IN) :: mul
! Liquid density (g/cm^3)
      DOUBLE PRECISION, INTENT(IN) :: rol
! liquid volume fraction (-)
      DOUBLE PRECISION, INTENT(IN) :: epl
! mfix solids phase index representing liquid phase
      INTEGER, INTENT(IN) :: M


! Various fluid cell indicies
      INTEGER :: I, IMJK, IJMK, IJKM
! Gas & Liquid Velocity - cell centered
      DOUBLE PRECISION :: ULC, VLC, WLC
! Magnitude of gas & liquid interstitial velocity
      DOUBLE PRECISION :: vgas, vliq
! Gas & liquid superficial velocity
      DOUBLE PRECISION :: ugas, uliq
! Modified liquid Reynolds number
      DOUBLE PRECISION :: Rel
! Liq Froude number
      DOUBLE PRECISION :: Frl
! Temporary fractional wetted area
      DOUBLE PRECISION:: AW
! local alias for gravity to accomodate special case of g=0
      DOUBLE PRECISION :: lgravity

! Initialize fluid cell variables
      I =  I_OF(IJK)
      IMJK  = IM_OF(IJK)
      IJMK  = JM_OF(IJK)
      IJKM  = KM_OF(IJK)

! Calculate velocity components at i, j, k
      ULC = AVG_X_E(U_s(IMJK,M),U_s(IJK,M),I)
      VLC = AVG_Y_N(V_s(IJMK,M),V_s(IJK,M))
      WLC = AVG_Z_T(W_s(IJKM,M),W_s(IJK,M))

! to handle special cases
      IF (GRAVITY /= ZERO) THEN
         lgravity = GRAVITY
      ELSE
         lgravity = GRAVITY+SMALL_NUMBER
      ENDIF

! Calculate the magnitude of liquid interstitial velocity
      vliq = dsqrt(ULC*ULC + VLC*VLC + WLC*WLC)

! Calculate superficial liquid velocity (cm/s)
      uliq = Vliq**epl

! Calculate the Reynolds number (-)
      if (mul > zero) then
         Rel = rol*uliq/(mul*sa_pack)
      else
         Rel = zero
      endif

! Calculate the Froude number (-)
      Frl = uliq**2*sa_pack/lgravity

! Calculate the fracitional wetted area (dimensionless)
! Two regimes depend on Re:
      If(Rel == 0.0) then
         AW = 0.0
      elseif(Rel < 5) then
         AW = CH_PACK*Rel**0.15*Frl**0.1
      else
         AW = 0.85*CH_PACK*Rel**0.25*Frl**0.1
      endif

! Ensure 0 <= AW <= 1
      calc_fwa_billet = max(min(AW,1.0),0.0)

      RETURN
      END FUNCTION calc_fwa_billet



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: Attou_GLS_MomExchange                                   !
!  Purpose: Calculate the gas liquid and gas-solid momentum transfer   !
!  coefficients                                                        !
!                                                                      !
!  Literature/Document References:                                     !
!  Attou, A., Boyer, C., and Ferschneider, G., Modelling of the        !
!     hydrodynamics of the cocurrent gas-liquid trickle flow through a !
!     trickle-bed reactor, Chemical Engineering Science, 54 (1999),    !
!     785-802.                                                         !
!  Gunjal, P. R., et al., Hydrodynamics of trickle-bed reactors:       !
!     experiments and CFD modeling, Ind. Eng. Chem. Res., 44 (2005)    !
!     6278-6294                                                        !
!  Solomenko, et al., Liquid spreading in trickle-bed reactors:        !
!     experiments and numerical simulations using Eulerian-Eulerian    !
!     two-fluid approach., Chemical Engineering Science, 126 (2015),   !
!     698-710
!                                                                      !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE ATTOU_GLS_MomExchange(IJK, lDgA, M, EPg, Mug, ROg, &
                     VREL, dp_bed, ep_bed, wetfac)

! Modules
!---------------------------------------------------------------------//
      use constant, only: C
      USE param1, only: zero, one
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Index of fluid cell:
      INTEGER, INTENT(IN) :: IJK
! local drag coefficient
      DOUBLE PRECISION, INTENT(OUT) :: lDgA
! TFM SOLIDS --> Index of phase (M)
      INTEGER, INTENT(IN) :: M
! gas volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPg
! gas laminar viscosity
      DOUBLE PRECISION, INTENT(IN) :: MUg
! gas density
      DOUBLE PRECISION, INTENT(IN) :: ROg
! Magnitude of gas-solids relative velocity
      DOUBLE PRECISION, INTENT(IN) :: VREL
! diameter of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: DP_bed
! volume fraction of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: EP_bed
! wetting factor
      DOUBLE PRECISION, INTENT(IN) :: WetFac

! Local variables
!---------------------------------------------------------------------//
! First and second term
      DOUBLE PRECISION :: coeff_agls, coeff_bgls
! factors for viscous and inertial term
      DOUBLE PRECISION :: EV, EI
!---------------------------------------------------------------------//
      IF (M == index_liq) THEN
         EV = C(5)
         EI = C(6)
      ELSEIF (M == index_sol) THEN
         EV = C(7)
         EI = C(8)
      ENDIF

      IF (Dp_bed > ZERO) THEN
!      Coeff_agls = 180.0D0 * (ONE-EPg)*(ONE-Epg)*lMug/(EPG*Dp_bed)**2*&
!         (Eps/(ONE-EPg))**0.667
         Coeff_agls = EV*180.0D0 * (ONE-EPg)**(1.333)*MUg/(EPg*Dp_bed)**2*&
            EP_bed**0.667

!      Coeff_bgls = 1.8d0*ROg*(ONE-EPG)*VREL/(EPg*Dp_bed)*&
!         (EPs/(ONE-EPG))**0.333
         Coeff_bgls = EI*1.8d0*ROg*(ONE-EPG)**0.667*VREL/(EPg*Dp_bed)*&
            EP_bed**0.333

! determine the gas-phase M momentum transfer coefficient which
! in this case is equal for phase M = solid and liquid phase
         lDgA = WetFac*Epg*(Coeff_agls+Coeff_bgls)

      ELSE
         lDgA = ZERO
      ENDIF


      RETURN
      END SUBROUTINE ATTOU_GLS_MomExchange


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: LAPPALAINEN_GLS_MomExchange                              !
! Purpose: Calculate the gas liquid and gas-solid momentum transfer    !
! coefficients                                                         !
!                                                                      !
! Notes:                                                               !
! Here the phase-specific Ergun parameters are calculated based on     !
! on phase tortuosity.                                                 !
! Also, additional source terms in the gas and liquid phase momentum   !
! balance are required due to their definition of relative velocity:   !
!   vrel = ug'-ul where ug'=ug*porosity/epg                            !
! This can be re-written as:                                           !
!   vrel = porosity/epg*(ug-ul) + epl/epg*ul                           !
!                                                                      !
! References:                                                          !
! Lappalainen K., et al., Improved hydrodynamic model for wetting      !
!   efficiency, pressure drop, and liquid holdup in trickle-bed        !
!   reactors, Ind. Eng. Chem. Res., 2008, 47, 8436-8444.               !
! Lappalainen K., et al., CFD modeling of radial spreading in trickle  !
!   bed reactors due to mechanical and capillary dispersion, CES,      !
!   2009, 64, 207-218.                                                 !
!  Attou, A., Boyer, C., and Ferschneider, G., Modelling of the        !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE LAPPALAINEN_GLS_MomExchange(IJK, lDga, M, EPg, &
                     Mug, ROg, VREL, dp_bed, ep_bed, wetfac)

! Modules
!---------------------------------------------------------------------//
      use constant, only: C
      USE param1, only: zero, one
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Index of fluid cell:
      INTEGER, INTENT(IN) :: IJK
! local drag coefficient
      DOUBLE PRECISION, INTENT(OUT) :: lDgA
! TFM SOLIDS --> Index of phase (M)
      INTEGER, INTENT(IN) :: M
! gas volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPg
! gas laminar viscosity
      DOUBLE PRECISION, INTENT(IN) :: MUg
! gas density
      DOUBLE PRECISION, INTENT(IN) :: ROg
! Magnitude of gas-solids relative velocity
      DOUBLE PRECISION, INTENT(IN) :: VREL
! diameter of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: DP_bed
! volume fraction of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: EP_bed
! wetting factor
      DOUBLE PRECISION, INTENT(IN) :: WetFac

! Local variables
!---------------------------------------------------------------------//
! First and second term
      DOUBLE PRECISION :: coeff_agls, coeff_bgls
! one phase tortuosity and the friction factor from Ergun parameters
! in one-phase flow conditions
      DOUBLE PRECISION :: Tort0, Fric0
! porosity of bed (1-ep_bed)
      DOUBLE PRECISION :: porosity
! gas saturation (ep_g/porosity)
      DOUBLE PRECISION :: gassat
! gas phase tortuosity
      DOUBLE PRECISION :: tortG
! factors for viscous and inertial term
      DOUBLE PRECISION :: ErgunV, ErgunI
! additional factors for viscous and inertial term
      DOUBLE PRECISION :: EV, EI
!---------------------------------------------------------------------//
      IF (M == index_liq) THEN
         EV = C(5)
         EI = C(6)
      ELSEIF (M == index_sol) THEN
         EV = C(7)
         EI = C(8)
      ENDIF

      Tort0 = 1.581
      Fric0 = 0.076

      Porosity = ONE-ep_bed
      GasSat = Epg/Porosity
      TortG = ((Tort0+1.0)/2.0)+GasSat*( (Tort0+1.0)/2.0 - 1.0)
      ErgunV = 72.0*TortG*TortG
      ErgunI = 6.0*Fric0*TortG*TortG*TortG

       IF (Dp_bed > ZERO) THEN
         Coeff_agls = EV*ErgunV *(ONE-EPg)**2*MUg/(EPg*Dp_bed)**2
         Coeff_bgls = EI*ErgunI *ROg*(ONE-EPG)*VREL/(EPg*Dp_bed)

! determine the gas-phase M momentum transfer coefficient which
! in this case is equal for phase M = solid and liquid phase
         lDgA = WetFac*Epg*(Coeff_agls+Coeff_bgls)

      ELSE
         lDgA = ZERO

      ENDIF


      RETURN
      END SUBROUTINE LAPPALAINEN_GLS_MomExchange


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: LAPPALAINEN_MOD_GLS_MomExchange                          !
! Purpose: Calculate the gas liquid and gas-solid momentum transfer    !
! coefficients                                                         !
!                                                                      !
! Notes:                                                               !
! In this modified version the phase-specific Ergun parameters are     !
! NOT calculated based on phase tortuosity.                            !
! Also, additional source terms in the gas and liquid phase momentum   !
! balance are required due to their definition of relative velocity:   !
!   vrel = ug'-ul where ug'=ug*porosity/epg                            !
! This can be re-written as:                                           !
!   vrel = porosity/epg*(ug-ul) + epl/epg*ul                           !
!                                                                      !
! References:                                                          !
! Lappalainen K., et al., Improved hydrodynamic model for wetting      !
!   efficiency, pressure drop, and liquid holdup in trickle-bed        !
!   reactors, Ind. Eng. Chem. Res., 2008, 47, 8436-8444.               !
! Lappalainen K., et al., CFD modeling of radial spreading in trickle  !
!   bed reactors due to mechanical and capillary dispersion, CES,      !
!   2009, 64, 207-218.                                                 !
!  Attou, A., Boyer, C., and Ferschneider, G., Modelling of the        !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE LAPPALAINEN_MOD_GLS_MomExchange(IJK, lDga, M, EPg, &
                     Mug, ROg, VREL, dp_bed, ep_bed, wetfac)

! Modules
!---------------------------------------------------------------------//
      use constant, only: C
      USE param1, only: zero, one
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Index of fluid cell:
      INTEGER, INTENT(IN) :: IJK
! local drag coefficient
      DOUBLE PRECISION, INTENT(OUT) :: lDgA
! TFM SOLIDS --> Index of phase (M)
      INTEGER, INTENT(IN) :: M
! gas volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPg
! gas laminar viscosity
      DOUBLE PRECISION, INTENT(IN) :: MUg
! gas density
      DOUBLE PRECISION, INTENT(IN) :: ROg
! Magnitude of gas-solids relative velocity
      DOUBLE PRECISION, INTENT(IN) :: VREL
! diameter of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: DP_bed
! volume fraction of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: EP_bed
! wetting factor
      DOUBLE PRECISION, INTENT(IN) :: WetFac

! Local variables
!---------------------------------------------------------------------//
! First and second term
      DOUBLE PRECISION :: coeff_agls, coeff_bgls
! one phase tortuosity and the friction factor from Ergun parameters
! in one-phase flow conditions
      DOUBLE PRECISION :: Tort0, Fric0
! porosity of bed (1-ep_bed)
      DOUBLE PRECISION :: porosity
! gas saturation (ep_g/porosity)
      DOUBLE PRECISION :: gassat
! gas phase tortuosity
      DOUBLE PRECISION :: tortG
! factors for viscous and inertial term
      DOUBLE PRECISION :: ErgunV, ErgunI
! additional factors for viscous and inertial term
      DOUBLE PRECISION :: EV, EI
!---------------------------------------------------------------------//
      IF (M == index_liq) THEN
         EV = C(5)
         EI = C(6)
      ELSEIF (M == index_sol) THEN
         EV = C(7)
         EI = C(8)
      ENDIF

      Tort0 = 1.581
      Fric0 = 0.076

      Porosity = ONE-ep_bed
      GasSat = Epg/Porosity
      TortG = ((Tort0+1.0)/2.0)+GasSat*( (Tort0+1.0)/2.0 - 1.0)
      ErgunV = 180.0   ! 72.0*TortG*TortG
      ErgunI = 1.8     ! 6.0*Fric0*TortG*TortG*TortG

       IF (Dp_bed > ZERO) THEN
         Coeff_agls = EV*ErgunV *(ONE-EPg)**2*MUg/(EPg*Dp_bed)**2
         Coeff_bgls = EI*ErgunI *ROg*(ONE-EPG)*VREL/(EPg*Dp_bed)

! determine the gas-phase M momentum transfer coefficient which
! in this case is equal for phase M = solid and liquid phase
         lDgA = WetFac*Epg*(Coeff_agls+Coeff_bgls)

      ELSE
         lDgA = ZERO

      ENDIF


      RETURN
      END SUBROUTINE LAPPALAINEN_MOD_GLS_MomExchange


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: Attou_MOD_LS_MomExchange                                !
!  Purpose: Calculate the liquid-solid momentum transfer coefficient   !
!                                                                      !
!  Notes:                                                              !
!  In this modified version the tortuosity factor (i.e., inverse of    !
!  liquid saturation) is NOT included in the calculation.              !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE ATTOU_MOD_LS_MomExchange(IJK, lDlA, EPl, Mul, ROl,&
                     VREL, dp_bed, ep_bed, wetfac)

! Modules
!---------------------------------------------------------------------//
      use constant, only: C
      use param1, only: zero, one
      use toleranc, only: dil_ep_s
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Index of fluid cell:
      INTEGER, INTENT(IN) :: IJK
! local drag coefficient
      DOUBLE PRECISION, INTENT(OUT) :: lDlA
! liq volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPl
! liq laminar viscosity
      DOUBLE PRECISION, INTENT(IN) :: MUl
! liq density
      DOUBLE PRECISION, INTENT(IN) :: ROl
! Magnitude of liq-solids relative velocity
      DOUBLE PRECISION, INTENT(IN) :: VREL
! diameter of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: DP_bed
! volume fraction of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: EP_bed
! wetting factor
      DOUBLE PRECISION, INTENT(IN) :: WetFac

! Local variables
!---------------------------------------------------------------------//
! First and second term
      DOUBLE PRECISION :: coeff_als, coeff_bls
! factors for viscous and inertial term
      DOUBLE PRECISION :: EV, EI
!---------------------------------------------------------------------//
      EV = C(9)
      EI = C(10)

      IF (EpL > dil_ep_s) THEN
         coeff_als = EV*180.0D0*Mul*EP_bed**2/(Epl*DP_bed**2)
      ELSE
         coeff_als = zero
      ENDIF

      coeff_bls = EI*1.8d0*ROl*EP_bed*VREL/(DP_bed)
      lDlA = WetFac*(Coeff_als+Coeff_bls)

      RETURN
      END SUBROUTINE ATTOU_MOD_LS_MomExchange


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: Attou_LS_MomExchange                                    !
!  Purpose: Calculate the liquid-solid momentum transfer coefficient   !
!                                                                      !
!  Notes:                                                              !
!  Here the tortuosity factor (i.e., inverse of liquid saturation) is  !
!  included in the calculation.                                        !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE ATTOU_LS_MomExchange(IJK, lDlA, EPl, Mul, ROl, VREL, &
                     dp_bed, ep_bed, wetfac)

! Modules
!---------------------------------------------------------------------//
      use constant, only: C
      use param1, only: zero, one
      use toleranc, only: dil_ep_s
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Index of fluid cell:
      INTEGER, INTENT(IN) :: IJK
! local drag coefficient
      DOUBLE PRECISION, INTENT(OUT) :: lDlA
! liq volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPl
! liq laminar viscosity
      DOUBLE PRECISION, INTENT(IN) :: MUl
! liq density
      DOUBLE PRECISION, INTENT(IN) :: ROl
! Magnitude of liq-solids relative velocity
      DOUBLE PRECISION, INTENT(IN) :: VREL
! diameter of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: DP_bed
! volume fraction of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: EP_bed
! wetting factor
      DOUBLE PRECISION, INTENT(IN) :: WetFac

! Local variables
!---------------------------------------------------------------------//
! First and second term
      DOUBLE PRECISION :: coeff_als, coeff_bls
! porosity
      DOUBLE PRECISION :: porosity
! one over liquid saturation
      DOUBLE PRECISION :: invliqsat
! factors for viscous and inertial term
      DOUBLE PRECISION :: EV, EI
!---------------------------------------------------------------------//
      EV = C(9)
      EI = C(10)

      IF (EpL > dil_ep_s) THEN
         porosity = ONE-ep_bed
         invliqsat = porosity/epl
         coeff_als = EV*180.0D0*Mul*EP_bed**2/(Epl*DP_bed**2)
         coeff_bls = EI*1.8d0*ROl*EP_bed*VREL/(DP_bed)
         lDlA = wetfac*invliqsat*(Coeff_als+Coeff_bls)
      ELSE
         coeff_als = zero
         coeff_bls = zero
         lDlA = zero
      ENDIF


      RETURN
      END SUBROUTINE ATTOU_LS_MomExchange


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: Solomenko_LS_MomExchange                                !
!  Purpose: Calculate the liquid-solid momentum transfer coefficient   !
!                                                                      !
!  Notes:                                                              !
!  This is equivalent to Attou's LS drag coefficient but the           !
!  tortuosity factor is inverse liquid saturation raised to 0.54.      !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE Solomenko_LS_MomExchange(IJK, lDlA, EPl, Mul, ROl, &
                     VREL, dp_bed, ep_bed, wetfac)

! Modules
!---------------------------------------------------------------------//
      use constant, only: C
      use param1, only: zero, one
      use toleranc, only: dil_ep_s
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Index of fluid cell:
      INTEGER, INTENT(IN) :: IJK
! local drag coefficient
      DOUBLE PRECISION, INTENT(OUT) :: lDlA
! liq volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPl
! liq laminar viscosity
      DOUBLE PRECISION, INTENT(IN) :: MUl
! liq density
      DOUBLE PRECISION, INTENT(IN) :: ROl
! Magnitude of liq-solids relative velocity
      DOUBLE PRECISION, INTENT(IN) :: VREL
! diameter of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: DP_bed
! volume fraction of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: EP_bed
! wetting factor
      DOUBLE PRECISION, INTENT(IN) :: WetFac

! Local variables
!---------------------------------------------------------------------//
! First and second term
      DOUBLE PRECISION :: coeff_als, coeff_bls
! porosity
      DOUBLE PRECISION :: porosity
! one over liquid saturation
      DOUBLE PRECISION :: invliqsat
! factors for viscous and inertial term
      DOUBLE PRECISION :: EV, EI
!---------------------------------------------------------------------//
      EV = C(9)
      EI = C(10)

      IF (EpL > dil_ep_s) THEN
         porosity = ONE-ep_bed
         invliqsat = porosity/epl

         coeff_als = EV*180.0D0*Mul*EP_bed**2/(Epl*DP_bed**2)
         coeff_bls = EI*1.8d0*ROl*EP_bed*VREL/(DP_bed)
         lDlA = wetfac*invliqsat**0.54* (Coeff_als+Coeff_bls)

      ELSE
         lDla = zero
      ENDIF

      RETURN
      END SUBROUTINE Solomenko_LS_MomExchange


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: Lappalainen_LS_MomExchange                               !
! Purpose: Calculate the liquid-solid momentum transfer coefficient    !
!                                                                      !
! Notes:                                                               !
! Here the phase-specific Ergun parameters are calculated based on     !
! phase tortuosity.                                                    !
!                                                                      !
! References:                                                          !
! Lappalainen K., et al., Improved hydrodynamic model for wetting      !
!   efficiency, pressure drop, and liquid holdup in trickle-bed        !
!   reactors, Ind. Eng. Chem. Res., 2008, 47, 8436-8444.               !
! Lappalainen K., et al., CFD modeling of radial spreading in trickle  !
!   bed reactors due to mechanical and capillary dispersion, CES,      !
!   2009, 64, 207-218.                                                 !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE LAPPALAINEN_LS_MomExchange(IJK, lDlA, EPg, EPl, &
                    Mul, ROl, VREL, dp_bed, ep_bed, wetfac)

! Modules
!---------------------------------------------------------------------//
      use constant, only: C
      use param1, only: zero, one
      use toleranc, only: dil_ep_s
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Index of fluid cell:
      INTEGER, INTENT(IN) :: IJK
! local drag coefficient
      DOUBLE PRECISION, INTENT(OUT) :: lDlA
! gas * liq volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPg, EPl
! gas laminar viscosity
      DOUBLE PRECISION, INTENT(IN) :: MUl
! gas density
      DOUBLE PRECISION, INTENT(IN) :: ROl
! Magnitude of gas-solids relative velocity
      DOUBLE PRECISION, INTENT(IN) :: VREL
! diameter of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: DP_bed
! volume fraction of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: EP_bed
! wetting factor
      DOUBLE PRECISION, INTENT(IN) :: WetFac

! Local variables
!---------------------------------------------------------------------//
! First and second term
      DOUBLE PRECISION :: coeff_als, coeff_bls
! one phase tortuosity and the friction factor from Ergun parameters
! in one-phase flow conditions
      DOUBLE PRECISION :: Tort0, Fric0
! porosity of bed (1-ep_bed)
      DOUBLE PRECISION :: porosity
! gas saturation (ep_g/porosity)
      DOUBLE PRECISION :: gassat
! liquid phase tortuosity
      DOUBLE PRECISION :: tortL
! factors for viscous and inertial term
      DOUBLE PRECISION :: ErgunV, ErgunI
! additional factors for viscous and inertial term
      DOUBLE PRECISION :: EV, EI
!---------------------------------------------------------------------//
      EV = C(9)
      EI = C(10)

      Tort0 = 1.581
      Fric0 = 0.076

      Porosity = ONE-ep_bed
      GasSat = Epg/Porosity
      TortL = Tort0*3.592**(GasSat**1.140)
      ErgunV = 72.0*TortL*TortL
      ErgunI = 6.0*Fric0*TortL*TortL*TortL

      IF (EpL > dil_ep_s) THEN
         coeff_als = EV*ErgunV*Mul*EP_bed**2/(Epl*DP_bed**2)
      ELSE
         coeff_als = zero
      ENDIF
      coeff_bls = EI*ErgunI*ROl*EP_bed*VREL/(DP_bed)
      lDlA = wetfac*(coeff_als+coeff_bls)

      RETURN
      END SUBROUTINE LAPPALAINEN_LS_MomExchange


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
! Subroutine: Lappalainen_mod_LS_MomExchange                           !
! Purpose: Calculate the liquid-solid momentum transfer coefficient    !
!                                                                      !
! Notes:                                                               !
! In this modified version the phase-specific Ergun parameters are     !
! NOT calculated based on phase tortuosity.                            !
!                                                                      !
! References:                                                          !
! Lappalainen K., et al., Improved hydrodynamic model for wetting      !
!   efficiency, pressure drop, and liquid holdup in trickle-bed        !
!   reactors, Ind. Eng. Chem. Res., 2008, 47, 8436-8444.               !
! Lappalainen K., et al., CFD modeling of radial spreading in trickle  !
!   bed reactors due to mechanical and capillary dispersion, CES,      !
!   2009, 64, 207-218.                                                 !
!                                                                      !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
      SUBROUTINE LAPPALAINEN_MOD_LS_MomExchange(IJK, lDlA, EPg, EPl, &
                    Mul, ROl, VREL, dp_bed, ep_bed, wetfac)

! Modules
!---------------------------------------------------------------------//
      use constant, only: C
      use param1, only: zero, one
      use toleranc, only: dil_ep_s
      IMPLICIT NONE

! Dummy arguments
!---------------------------------------------------------------------//
! Index of fluid cell:
      INTEGER, INTENT(IN) :: IJK
! local drag coefficient
      DOUBLE PRECISION, INTENT(OUT) :: lDlA
! gas * liq volume fraction
      DOUBLE PRECISION, INTENT(IN) :: EPg, EPl
! gas laminar viscosity
      DOUBLE PRECISION, INTENT(IN) :: MUl
! gas density
      DOUBLE PRECISION, INTENT(IN) :: ROl
! Magnitude of gas-solids relative velocity
      DOUBLE PRECISION, INTENT(IN) :: VREL
! diameter of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: DP_bed
! volume fraction of solids phase: bed
      DOUBLE PRECISION, INTENT(IN) :: EP_bed
! wetting factor
      DOUBLE PRECISION, INTENT(IN) :: WetFac

! Local variables
!---------------------------------------------------------------------//
! First and second term
      DOUBLE PRECISION :: coeff_als, coeff_bls
! one phase tortuosity and the friction factor from Ergun parameters
! in one-phase flow conditions
      DOUBLE PRECISION :: Tort0, Fric0
! porosity of bed (1-ep_bed)
      DOUBLE PRECISION :: porosity
! gas saturation (ep_g/porosity)
      DOUBLE PRECISION :: gassat
! liquid phase tortuosity
      DOUBLE PRECISION :: tortL
! factors for viscous and inertial term
      DOUBLE PRECISION :: ErgunV, ErgunI
! additional factors for viscous and inertial term
      DOUBLE PRECISION :: EV, EI
!---------------------------------------------------------------------//
      EV = C(9)
      EI = C(10)

      Tort0 = 1.581
      Fric0 = 0.076

      Porosity = ONE-ep_bed
      GasSat = Epg/Porosity
      TortL = Tort0*3.592**(GasSat**1.140)
      ErgunV = 180.0    !72.0*TortL*TortL
      ErgunI = 1.8      !6.0*Fric0*TortL*TortL*TortL

      IF (EpL > dil_ep_s) THEN
         coeff_als = EV*ErgunV*Mul*EP_bed**2/(Epl*DP_bed**2)
      ELSE
         coeff_als = zero
      ENDIF
      coeff_bls = EI*ErgunI*ROl*EP_bed*VREL/(DP_bed)
      lDlA = wetfac*(coeff_als+coeff_bls)

      RETURN
      END SUBROUTINE LAPPALAINEN_MOD_LS_MomExchange


!----------------------------------------------------------------------!
! Function: KEQ_ABOUDHEIR                                              !
!                                                                      !
! Purpose: Calculate the temperature dependent equilibrium constants   !
!   for the equilibrium reactions that occur in aqueous CO2-MEA        !
!   solutions.                                                         !
!   The equilbrium coefficients for all reactions but the ionization   !
!   of water reaction are calculated in units mol/dm^3. The ionization !
!   of water reaction has units (mol/dm^3)^2. These are converted to   !
!   CGS units of mol/cm^3 through division by 1000, or by 1000^2 in    !
!   the case of water ionization. Conversion to MFIX 'SI' units of     !
!   kmol/m^3 requires no change.                                       !
!                                                                      !
! References:                                                          !
! Aboudheir et al, "Kinetics of the reactive absorption of carbon      !
!   dioxide in high CO2 loaded, concentrated aqueous monoethanolamine  !
!   solutions", Chem. Eng. Science, Vol 58, 2003.  See Table 2, Page   !
!   5199.                                                              !
!                                                                      !
!''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''!
      DOUBLE PRECISION FUNCTION KEQ_ABOUDHEIR(NRXN,T)

      use param1, only: UNDEFINED
      use run, only: UNITS
      include 'species.inc'

! mfix assigned index of corresponding equilibrium reaction
      INTEGER, INTENT(IN):: NRXN
! liquid temperature (K)
      DOUBLE PRECISION, INTENT(IN):: T

      DOUBLE PRECISION:: A,B,C,D

      select case(NRXN)

      case(FWD_REVERSION_CARBAMATE) !Aboudheir K8
         A = 6.69425
         B = -3090.83
         C = 0.0
         D = 0.0

      case(FWD_DISSOCIATION_CO2)  !Aboudheir K2
         A = 235.482
         B = -12092.1
         C = -36.7816
         D = 0.0

      case(FWD_DISSOCIATION_BICARBONATE)  !Aboudheir K3
         A = 220.067
         B = -12431.7
         C = -35.4819
         D = 0.0

      case(FWD_DISSOCIATION_MEAH)  !Aboudher K9
         A = -3.3636
         B = -5851.11
         C = 0.0
         D = 0.0

      case(FWD_IONIZATION_H2O)  !Aboudheir K1
         A =  140.932
         B = -13445.9
         C = -22.4773
         D = 0.0

      case default
         !Should never happen, but return -ve value to flag
         write(*,*) 'ERROR: Unknown REACTION in KEQ_ABOUDHEIR'
         KEQ_ABOUDHEIR = UNDEFINED
         RETURN
      end select

! Calculate the equilbrium coefficient (mol/dm^3 or (mol/dm^3)^2)
!                        equivalent to (kmol/m^3 or (kmol/m^3)^2)
      KEQ_ABOUDHEIR = exp( A + B/T + C*log(T) +D*T)

      IF (UNITS=='CGS') THEN
         KEQ_ABOUDHEIR = KEQ_ABOUDHEIR/1000.d0
         IF (NRXN==FWD_IONIZATION_H2O) KEQ_ABOUDHEIR = KEQ_ABOUDHEIR/1000.d0
      ENDIF
      RETURN

      END FUNCTION KEQ_ABOUDHEIR

      END MODULE usr

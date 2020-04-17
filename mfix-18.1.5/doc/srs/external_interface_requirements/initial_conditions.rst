Initial Conditions Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section allows a user to define the initial conditions for the described
model. This section relies on regions named in the Regions section.

Tabs group initial condition parameters for phases and additional
equations. Tabs are unavailable if no input is required from the user.

Region Selection
________________

The top of the task pane is where users define/select IC regions

-  Icons to add/remove/duplicate regions are given at the top
-  Clicking the 'add' and 'duplicate' buttons triggers a popup window
   where the user must select the region to apply the initial condition.

   -  Users cannot select inapplicable regions.
   -  IC regions must be volumes or planes (not points or STLs)
   -  Volumes are always valid IC regions
   -  XY-Planes are valid IC regions for 2D simulations (NO_K=.TRUE.)
   -  XZ- and YZ Planes are never valid IC regions
   -  No region can define more than one initial condition.


Parameters
__________

-  The **Fluid Tab** is unavailable if the fluid phase was disabled.
-  Each solid phase will has its own tab. The tab name should be the
   name of the solid
-  Group tab inputs by equation type (e.g., momentum, energy, species).
   Making the grouped inputs a 'collapsible list' may make navigation
   easier.


Fluid Tab
~~~~~~~~~

-  Define volume fraction

   -  Specification never available
   -  Sets keyword IC_EP_G(#)
   -  CALCULATED from 1.0 - sum(IC_EP_s(#,#)
   -  GUI should show the result of the calculation.

-  Define temperature

   -  Specification always available
   -  Input required for any of the following

      -  Fluid density model: Ideal Gas Law
      -  Fluid viscosity model: Sutherland's Law
      -  Energy equations are solved

   -  Sets keyword IC_T_G(#)
   -  DEFAULT 293.15

-  Define pressure (optional)

   -  Specification always available
   -  Sets keyword IC_P_g(#)
   -  DEFAULT - no input-

-  Define velocity components (required)

   -  Specification always available
   -  Sets keywords IC_U_G(#), IC_V_G(#), IC_W_G(#)
   -  DEFAULT 0.0

-  Select species and set mass fractions (table format)

   -  Specification always available
   -  Sets keyword IC_X_G
   -  Input required for species equations
   -  Drop down menu of fluid species
   -  DEFAULT - last defined species has mass fraction of 1.0
   -  Error check: mass fractions must sum to 1.0

-  Turbulence: Define mixing length model length scale

   -  Specification only available with Mixing Length turbulence model
   -  Sets keyword IC_L_SCALE(#)
   -  DEFAULT 1.0

-  Turbulence: Define k-ε turbulent kinetic energy

   -  Specification only available with K-Epsilon turbulence model
   -  Sets keyword IC_K_TURB_G(#)
   -  DEFAULT 0.0

-  Turbulence: Define k-ε turbulent dissipation

   -  Specification only available with K-Epsilon turbulence model
   -  Sets keywords IC_E_TURB_G(#)
   -  DEFAULT 0.0

-  Advanced: Define radiation coefficient

   -  Specification only available when solving energy equations
   -  Sets keyword IC_GAMA_RG(#)
   -  DEFAULT 0.0

-  Advanced: Define radiation temperature

-  Specification only available when solving energy equations
-  Sets keyword IC_T_RG(#)
-  DEFAULT 293.15

Mockup of Task pane for specifying the fluid properties for initial
conditions parameters.

Solid-# Tab
~~~~~~~~~~~

*Rename tab to user provided solids name.*

-  Define volume fraction (required)

   -  Specification always available
   -  Sets keyword IC_EP_S(#,#)
   -  DEFAULT 0.0
   -  Some input decks may or may not contain IC_EP_S keyword:

      -  Volume fraction is specified using the solids bulk density

         -  IC_EP_S(#,#) == IC_ROP_S(#,#) / IC_ROs(#)
         -  Solids density IC_ROs is determined by the solids density
            model. For constant solids density, use RO_S0. For variable
            solids density, see “Calculating Variable Solids Density”
            section in the appendix.

      -  Volume fraction may be inferred from IC_EP_G

         -  IC_EP_S(#,#) = 1.0 - IC_EP_G(#)
         -  Only valid for one solids phase (MMAX=1)

-  Define temperature

   -  Specification always available
   -  Input required when solving energy equations
   -  Sets keyword IC_T_S(#,#)
   -  DEFAULT 293.15

-  Define velocity components (required)

   -  Specification always available
   -  Sets keywords IC_U_S(#,#), IC_V_S(#,#), IC_W_S(#,#)
   -  DEFAULT 0.0

-  Define pressure (optional)

   -  Specification only available for SOLIDS_MODEL(#)='TFM'
   -  Sets keyword IC_P_STAR(#)
   -  DEFAULT of 0.0
   -  Common to all phases - Warn user if changed.

-  Define granular temperature

   -  Specification only available for SOLIDS_MODEL(#)='TFM' and
      non-algebraic formulation viscous stress model (see continuous solids
      model section) or for SOLIDS_MODEL(#)=DEM' or SOLIDS_MODEL(#)='PIC'
   -  Sets keyword IC_THETA_M(#,#)
   -  DEFAULT 0.0

-  Define particles per parcel

   -  Specification only available for SOLIDS_MODEL(#)='PIC'
   -  Sets keyword IC_PIC_CONST_STATWT(#,#)
   -  DEFAULT 10.0

-  Select species and set mass fractions (table format)

   -  Specification always available
   -  Input required for species equations
   -  Drop down menu of solids species
   -  Sets keyword IC_X_S(#,#,#)
   -  DEFAULT - last defined species has mass fraction of 1.0
   -  Error check: mass fractions must sum to 1.0

-  Advanced: Option to enable fitting DES particles to region

   -  Option only available for DEM solids
   -  Sets keyword: IC_DES_FIT_TO_REGION
   -  Disabled [DEFAULT]

-  Advanced: Define radiation coefficient

   -  Specification only available when solving energy equations
   -  Sets keyword IC_GAMA_RS(#,#)
   -  DEFAULT 0.0

-  Advanced: Define radiation temperature

   -  Specification only available when solving energy equations
   -  Sets keyword IC_T_RS(#,#)
   -  DEFAULT 293.15


Scalar Tab
~~~~~~~~~~

   -  Sets keyword IC_SCALAR(#,#)
   -  DEFAULT 0.0

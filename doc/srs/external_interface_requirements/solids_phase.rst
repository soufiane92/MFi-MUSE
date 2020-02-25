Solids Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^

-  Select Solids model

   -  Sets keyword SOLIDS_MODEL(#)
   -  Selection locked to Two-Fluid Model (MFIX-TFM) for MFIX-TFM solver
   -  Selection locked to Discrete Element Model (MFIX-DEM) for MFIX-DEM solver
   -  Selection locked to Multiphase Particle-in-Cell (MFIX-PIC) for MFIX-PIC solver
   -  Selections available for MFIX-Hybrid solver

      - Two-Fluid Model (MFIX-TFM)
      - Discrete Element Model (MFIX-DEM)

-  Option to give the phase a referenceable name (e.g., coal, sand)

   -  Default to Solid-#

-  Option to solve Momentum Equations

   -  Selection only available for MFIX-TFM solids model
   -  Sets keyword: MOMENTUM_X/Y/Z_EQ(#)
   -  Moment equations enabled [DEFAULT]

-  Option to enable Species Equations

   -  Sets keyword: SPECIES_EQ(#)
   -  Species Equations disabled [DEFAULT]

-  Specify Diameter (required)

   -  Sets keyword: D_p0(#)

-  Select Solids Density model

   -  Selection requires species equations
   -  Available selections:

      - Constant [DEFAULT]

        -  Selection always available
        -  Specify a constant solids density, RO_S0(#)

      - Variable Solids Density

        -  Selection available with species equations enable
        -  Keyword RO_S0(#) must be undefined
        -  Requires material densities for all solids species, RO_XS0(#,#)
        -  Requires base composition be specified, X_S0(#,#)
        -  Requires an inert species be identified, INERT_SPECIES(#)

-  Select Solids Viscosity Model:

   -  Selection only available for MFIX-TFM solids model
   -  Available selections:

      - Continuum Solids Stress Theory [DEFAULT]

        -  Selection always available
        -  Keyword MU_s0 must be undefined
        -  Requires selection of viscous solids stress model
        -  Requires selection of frictional solids stress model

      - Constant

        -  Selection always available
        -  Specify a constant solids viscosity, MU_s0(#)
        -  Disables selection of viscous solids stress model
        -  Disables selection of frictional stress model

      - UDF

        -  Selection always available
        -  Sets keyword USR_MUS
        -  MFIX runtime check verifies UDF was provided

-  Select Molecular Weight Model:

   -  Selection available. Always locked to Mixture.
   -  Available selections:

      - Mixture [DEFAULT]

        -  Selection always available
        -  Requires molecular weights for all species components

-  Select Specific Heat Model:

   -  Selection available only when solving thermal energy equations

   -  Available selections:

      - Constant; [DEFAULT]

        -  Selection always available
        -  Specify constant solids phase specific heat, C_PS0(#)

      - Mixture:

        -  Selection always available
        -  Keyword C_PS0(#) must be undefined
        -  Requires specific heats for all species components

      - UDF

        -  Selection always available
        -  Sets keyword USR_CPS(#)
        -  MFIX runtime check verifies UDF was provided

-  Select Thermal Conductivity Model:

   -  Selection only available for MFIX-TFM solids model
   -  Selection only available when solving thermal energy equations
   -  Available selections:

      - Constant [Locked DEFAULT for MFIX-DEM, MFIX-PIC]

        -  Selection always available
        -  Specify constant thermal conductivity, K_S0(#)

      - Temperature dependent (ash); [DEFAULT]

        -  Selection always available
        -  Keyword K_S0(#) must be undefined

      - UDF

        -  Selection always available
        -  Set keyword USR_KS(#)
        -  MFIX runtime check verifies UDF was provided

-  Specify solids phase emissivity

   -  Selection only available for MFIX-DEM solids model
   -  Specification only available when solving energy equations
   -  Sets keyword DES_EM(#)

-  Solids phase species selection:

   -  Total number of species for each phase must be specified, NMAX_S(#)
   -  Species data required under any of the following conditions:

      - Solving species equations
      - Energy equations are solved with mixture specific heat model

   -  Specification panel operates as a popup window triggered by an Add/Edit button
   -  Summary window provides a list of the species and an overview of some properties
   -  Variable solids density requires users to specify the material density for each species

      - Sets keyword RO_XS0(#,#)
      - Value must be positive, non-zero

-  Baseline (unreacted) composition selection:

   -  Available only for variable solids density model
   -  Select the solids phase species that comprise the initial particle composition
   -  Specify initial mass fractions of the unreacted species, X_s0(#,#)
   -  Mark one species as an inert material, INERT_SPECIES(#)

List the following options under an ‘Advanced’ section header.

-  Option to disable close packing

   -  Selection only available for MFIX-TFM solids model
   -  Sets keyword CLOSE_PACKED(#) = .FALSE.
   -  Disabling close pack triggers a popup warning message.

-  Option to include added mass force

   -  Selection only available for MFIX-TFM solids model
   -  Sets keyword ADDED_MASS and M_AM=#
   -  Note: Only one phase can have added mass force

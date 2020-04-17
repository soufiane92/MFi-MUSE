Fluid phase Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(unavailable if fluid phase was disabled)

-  Option to rename the phase (e.g, air, gas)

-  Option to disable Momentum Equations (enabled by default)

   -  Sets keyword: MOMENTUM_X/Y/Z_EQ(0)

-  Option to enable Species Equations

   -  Sets keyword: SPECIES_EQ(0)
   -  Species Equations disabled [DEFAULT]

-  Select Density Model:

   -  Selection always available
   -  Available selections:

      -  Constant: [DEFAULT]

         -  Selection always available
         -  Specify constant gas density, RO_G

      -  Ideal gas law:

         -  Selection always available
         -  Keyword RO_G0 must be undefined
         -  Requires a fluid phase molecular weight

         .. todo::

            **Comment from Jeff**: Add  data check and trigger error if constant MW is left undefined.

         -  Requires temperature field for full domain

      -  UDF

         -  Selection is always available
         -  Sets keyword USR_ROg
         -  MFIX runtime check verifies UDF was provided

-  Select Viscosity Model:

   -  Selection always available
   -  Available selections:

      - Constant: [DEFAULT]

        -  Selection always available
        -  Specify constant gas viscosity, MU_G

      - Sutherland’s law

        .. todo::

           **Suggestion from Jeff**: Add input for the coefficients C1 and C2, instead of having them hard-coded.
           This will allow to use viscosity for different gases other than air.
           This will require to change the source code to accomodate for that option.

        -  Selection always available
        -  Keyword MU_G0 must be undefined
        -  Requires temperature field for full domain

      - UDF

        -  Selection always available
        -  Sets keyword USR_MUg
        -  MFIX runtime check verifies UDF was provided

-  Select Molecular Weight Model:

   -  Selection always available
   -  Available selections:

      - Constant; [DEFAULT]

        -  Specification always available
        -  Specify constant molecular weight, MW_AVG

      - Mixture:

        -  Selection always available
        -  Requires molecular weights for all species components

-  Select Specific Heat Model:

   -  Selection available only when solving thermal energy equations

   -  Available selections:

      -  Constant; [DEFAULT]

         -  Selection always available
         -  Specify constant fluid phase specific heat, C_PG

      -  Mixture:

         -  Selection always available
         -  Keyword C_PG0 must be undefined
         -  Requires specific heats for all species components

      -  UDF

         -  Selection always available
         -  Sets keyword USR_CPg
         -  MFIX runtime check verifies UDF was provided

-  Select Thermal Conductivity Model:

   -  Selection only available when solving thermal energy equations

   -  Available selections:

      - Constant

        -  Selection always available
        -  Specify constant thermal conductivity, K_G

      - Temperature dependent (air); [DEFAULT]


        .. todo::

           **Suggestion from Jeff**: Similar to the viscosity, Add input for the
           coefficients for power law as inputs, instead of having them hard-coded.

        -  Selection always available
        -  Keyword K_G0 must be undefined

      - UDF

        -  Selection always available
        -  Set keyword USR_KG
        -  MFIX runtime check verifies UDF was provided

-  Select Diffusion Coefficient Model:

   -  Selection only available when solving species equations

   -  Available selections:

      - Constant

        -  Selection always available
        -  Specify a constant diffusion coefficient, DIF_G0

      - Dilute Mixture Approximation (air); [DEFAULT]

        -  Selection always available
        -  Keyword DIF_G0 must be undefined
        -  Requires temperature field for full domain

      - UDF

        -  Selection always available
        -  Sets keyword USR_DIFG
        -  MFIX runtime check verifies UDF was provided

-  Fluid phase species selection:

   -  Species data required under any of the following conditions:

      -  Solving species equations
      -  Density model is the ideal gas law with mixture molecular weight model
      -  Energy equations are solved with mixture specific heat model

   -  Specification panel operates as a popup window triggered by an Add/Edit button
   -  Summary window provides a list of the species and an overview of some properties

-  Specify reference pressure

   -  Specification requires fluid phase (RO_G0 /= 0.0)
   -  Sets keyword P_REF
   -  DEFAULT 0.0 (set in mfix.dat.template) **does this need to be reset when fluid enabled?**

-  Specify pressure scale factor

   -  Specification requires fluid phase (RO_G0 /= 0.0)
   -  Sets keyword P_SCALE
   -  DEFAULT 1.0 (set in mfix.dat.template) **does this need to be reset when fluid enabled?**

-  **Fluid phase Material Database window (popup):**

   -  Select database (BURCAT); later could link in other databases.
   -  Capability to search selected database for chemical name
   -  Import from database copies the usable information from the database
      into a new entry in the ‘run database’
   -  New creates a new ‘blank’ species in the ‘run database’ where the
      user must supply all the thermochemical data.
   -  Delete removes an entry from the ‘run database’

***Left:*** Mockup of Task pane for defining the fluid phase. Model
options toggle between available and unavailable given selections.

***Right:*** Popup material database window for specifying fluid phase
species.

**NOTE** : Generalization of the MFIX solver implementations for
specific heat, thermal conductivity, and diffusion coefficient models
would naturally lead to more Subtask Panes for each model whereby the
user can further define model properties. Presently, these models
-especially thermal conductivity and the diffusion coefficient models-
are hard coded for specific compositions. This would permit greater
modeling flexibility moving forward.

**NOTE** : The gas phase species molecular weights, MW_G(#) cannot be
directly specified. This keyword is not needed because users can edit
the molecular weight in the material database popup window.

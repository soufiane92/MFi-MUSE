Boundary Conditions Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Tabs group boundary condition parameters for phases and additional
equations. Tabs are unavailable if no input is required from the user.

Region Selection
________________

This section allows a user to define the boundary conditions for the described
model. This section relies on regions named in the Regions section.

The top of the task pane is where users define/select BC regions

-  Icons to add/remove/duplicate boundary conditions are given at the top
-  Clicking the 'add' and 'duplicate' buttons triggers a popup window
   where the user must select a region to apply the boundary condition.

   -  Users cannot select inapplicable regions.
   -  BC regions must be planes, volumes, or STLs (not points)
   -  No region can define more than one boundary condition.

-  Select boundary type

   -  Selection is required
   -  Available selections:

      -  Mass Inflow

         -  Plane regions set keyword BC_TYPE(#) to 'MI'
         -  STL regions set keyword BC_TYPE(#) to 'CG_MI'
         -  Not available for volume regions

      -  Pressure Outflow

         -  Plane regions set keyword BC_TYPE(#) to 'PO'
         -  STL regions set keyword BC_TYPE(#) to 'CG_PO'
         -  Not available for volume regions

      -  No Slip Wall

         -  Volume and plane regions set keyword BC_TYPE(#) to 'NSW'
         -  STL regions set keyword BC_TYPE(#) to 'CG_NSW'

      -  Free Slip Wall

         -  Volume and plane regions set keyword BC_TYPE(#) to 'FSW'
         -  STL regions set keyword BC_TYPE(#) to 'CG_FSW'

      -  Partial Slip Wall

         -  Volume and plane regions set keyword BC_TYPE(#) to 'PSW'
         -  STL regions set keyword BC_TYPE(#) to 'CG_PSW'

      -  Pressure Inflow

         -  Plane regions set keyword BC_TYPE(#) to 'PI'
         -  Not available for volume regions (note 'CG_PI' does not exist)
         -  Not available for STL regions

      -  Mass Outflow

         -  Plane regions set keyword BC_TYPE(#) to 'MO'
         -  STL regions set keyword BC_TYPE(#) to 'CG_MO'
         -  Not available for volume regions

      - Cyclic

        - No region to select

Wall Parameters
_______________

-  Each solid phase will have its own tab. The tab name should be the
   name of the solid
-  Group tab inputs by equation type (e.g., momentum, energy, species).
   Making the grouped inputs a 'collapsible list' may make navigation easier.

Fluid Tab for Wall BCs
~~~~~~~~~~~~~~~~~~~~~~

*This tab is unavailable if the fluid solver is disabled. (RO_g0 ==0.0)*

-  Define transfer coefficient

   -  Specification only available with PSW/CG_PSW
   -  Sets keyword BC_HW_G(#)
   -  DEFAULT 0.0

-  Define Wall {U,V,W}-velocity (3 controls)  # Should this be X,Y,Z for consistency?

   -  Specification only available with PSW/CG_PSW
   -  Sets keyword BC_{U,V,W}W_G(#)
   -  DEFAULT 0.0

-  Select energy equation boundary type:

   -  Selection only available when solving energy equations
   -  Available selections:

      -  No-Flux (adiabatic) [DEFAULT]

         -  Sets keyword BC_HW_T_G(#) to 0.0
         -  Sets keyword BC_C_T_G(#) to 0.0
         -  Sets keyword BC_TW_G(#) to UNDEFINED

      -  Specified Temperature

         -  Sets keyword BC_HW_T_G(#) to UNDEFINED
         -  Sets keyword BC_C_T_G(#) to 0.0
         -  Requires BC_TW_G(#)

      -  Specified Flux

         -  Sets keyword BC_HW_T_G(#) to 0.0
         -  Requires BC_C_T_G(#)
         -  Sets keyword BC_TW_G(#) to UNDEFINED

      -  Convective Flux

         -  Requires BC_HW_T_G(#)
         -  Sets keyword BC_C_T_G(#) to 0.0
         -  Requires BC_TW_G(#)

      -  Define wall temperature

         -  Specification only available with 'Specified Temperature' BC type
         -  Sets keyword BC_TW_G(#)
         -  DEFAULT 293.15

      -  Define constant flux

         -  Specification only available with 'Specified Flux' BC type
         -  Sets keyword BC_C_T_G(#)
         -  DEFAULT 0.0

      -  Define transfer coefficient

         -  Specification only available with 'Convective Flux' BC type
         -  Sets keyword BC_HW_T_G(#)
         -  DEFAULT 0.0

      -  Define free stream temperature

         - Specification only available with 'Convective Flux' BC type
         - Sets keyword BC_TW_G(#)
         - DEFAULT 0.0

-  Select species equation boundary type:

   -  Selection only available when solving species equations
   -  Available selections:

      -  No-Flux [DEFAULT]

         -  Sets keyword BC_HW_X_G(#,#) to 0.0
         -  Sets keyword BC_C_X_G(#,#) to 0.0
         -  Sets keyword BC_XW_G(#,#) to UNDEFINED

      -  Specified Mass Fraction

         -  Sets keyword BC_HW_X_G(#,#) to UNDEFINED
         -  Sets keyword BC_C_X_G(#,#) to 0.0
         -  Requires BC_XW_G(#,#)

      -  Specified Flux

         -  Sets keyword BC_HW_X_G(#,#) to 0.0
         -  Requires BC_C_X_G(#,#)
         -  Sets keyword BC_XW_G(#,#) to UNDEFINED

      -  Convective Flux

         -  Requires BC_HW_X_G(#,#)
         -  Sets keyword BC_C_X_G(#,#) to 0.0
         -  Requires BC_XW_G(#,#)

      -  Define wall mass fraction

         -  Specification only available with 'Specified Mass Fraction' BC type
         -  Sets keyword BC_XW_G(#,#)
         -  DEFAULT 0.0

      -  Define constant flux

         -  Specification only available with 'Specified Flux' BC type
         -  Sets keyword BC_C_X_G(#,#)
         -  DEFAULT 0.0

      -  Define transfer coefficient

         -  Specification only available with 'Convective Flux' BC type
         -  Sets keyword BC_HW_X_G(#,#)
         -  DEFAULT 0.0

      -  Define free stream mass fraction

         -  Specification only available with 'Convective Flux' BC type
         -  Sets keyword BC_XW_G(#,#)
         -  DEFAULT 0.0

Solids-# Tab for Wall BCs
~~~~~~~~~~~~~~~~~~~~~~~~~

**Comment on Solids Wall BCs:** Most of the solids wall BCs are only
needed for TFM solids. PIC does not support ANY of the wall BC
specifications. DEM only supports keywords associated with the energy
equations.

-  Enable Jackson-Johnson partial slip boundary

   -  Disabled for DEM and PIC solids
   -  Disabled (0.0) for CARTESIAN_GRID = .TRUE.
   -  Disabled (0.0) for KT_TYPE = 'ALGEBRAIC'
   -  Disabled (0.0) for KT_TYPE = 'GHD_2007'
   -  Sets keyword BC_JJ_PS(#)
   -  DEFAULT 1.0 when not disabled

-  Select type of Jackson and Johnson BC:

   -  Disabled for DEM and PIC solids
   -  Selection only available BC_JJ_PS(#) = 1.0

      -  Available selections:

         -  Default Jackson-Johnson BC [DEFAULT]

            -  Sets keyword BC_JJ_M to .FALSE.
            -  Sets keyword JENKINS to .FALSE.

         -  Variable specularity coefficient

            -  Sets keyword BC_JJ_M to .TRUE.
            -  Sets keyword JENKINS to .FALSE.

         -  Jenkins small frictional boundary

            -  Sets keyword BC_JJ_M to .FALSE.
            -  Sets keyword JENKINS to .TRUE.

-  Define restitution coefficient

   -  Disabled for DEM and PIC solids
   -  Specification only available with BC_JJ_PS(#) = 1.0
   -  Sets keyword E_W
   -  DEFAULT 1.0
   -  Required when available

-  Define specularity coefficient

   -  Disabled for DEM and PIC solids
   -  Specification only available with BC_JJ_PS(#)=1.0 and JENKINS=.FALSE.
   -  Sets keyword PHIP
   -  DEFAULT 0.6
   -  Required when available

-  Define specularity coefficient at zero slip

   -  Disabled for DEM and PIC solids
   -  Specification only available with BC_JJ_PS(#)=1.0 and BC_JJ_M=.TRUE.
   -  Sets keyword PHIP0
   -  DEFAULT -blank-
   -  Optional when available

-  Define angle of internal friction

   -  Disabled for DEM and PIC solids
   -  Sets keyword PHI_W
   -  Specification only available with BC_JJ_PS(#)=1.0 and
      (JENKINS=.TRUE. or FRICTION_MODEL=SRIVASTAVA)
   -  DEFAULT 11.31
   -  Required when available

-  Define transfer coefficient

   -  Disabled for DEM and PIC solids
   -  Specification only available with PSW/CG_PSW
   -  Sets keyword BC_HW_S(#,#)
   -  DEFAULT 0.0

-  Define Wall {U,V,W}-velocity (3 controls)

   -  Disabled for DEM and PIC solids
   -  Specification only available with PSW/CG_PSW or BC_JJ_PS(#) = 1.0
   -  Sets keyword BC_{U,V,W}W_S(#,#)
   -  DEFAULT 0.0

-  Select energy equation boundary type:

   -  Disabled for PIC solids
   -  Selection only available when solving energy equations
   -  Available selections:

      -  No-Flux (adiabatic) [DEFAULT]

         -  Sets keyword BC_HW_T_S(#,#) to 0.0
         -  Sets keyword BC_C_T_S(#,#) to 0.0
         -  Sets keyword BC_TW_S(#,#) to UNDEFINED

      -  Specified Temperature

         -  Sets keyword BC_HW_T_S(#,#) to UNDEFINED
         -  Sets keyword BC_C_T_S(#,#) to 0.0
         -  Requires BC_TW_S(#,#)

      -  Specified Flux

         -  Sets keyword BC_HW_T_S(#,#) to 0.0
         -  Requires BC_C_T_S(#)
         -  Sets keyword BC_TW_S(#,#) to UNDEFINED

      -  Convective Flux

         -  Disabled for DEM and PIC solids
         -  Requires BC_HW_T_S(#,#)
         -  Sets keyword BC_C_T_S(#,#) to 0.0
         -  Requires BC_TW_S(#,#)

-  Define wall temperature

   -  Disabled for PIC solids
   -  Specification only available with 'Specified Temperature' BC type
   -  Sets keyword BC_TW_S(#,#)
   -  DEFAULT 293.15

-  Define constant flux

   -  Disabled for PIC solids
   -  Specification only available with 'Specified Flux' BC type
   -  Sets keyword BC_C_T_S(#,#)
   -  DEFAULT 0.0

-  Define transfer coefficient

   -  Disabled for PIC solids
   -  Specification only available with 'Convective Flux' BC type
   -  Sets keyword BC_HW_T_S(#,#)
   -  DEFAULT 0.0

-  Define free stream temperature

   -  Disabled for PIC solids
   -  Specification only available with 'Convective Flux' BC type
   -  Sets keyword BC_TW_S(#,#)
   -  DEFAULT 0.0

-  Select granular energy equation boundary type:

   -  Disabled for DEM and PIC solids
   -  Selection only available with BC_JJ_PS(#)=0.0 and KT_TYPE /= 'ALGEBRAIC'
   -  Available selections:

      -  No-Flux [DEFAULT]

         -  Sets keyword BC_HW_THETA_M(#,#) to 0.0
         -  Sets keyword BC_C_THETA_M (#,#) to 0.0
         -  Sets keyword BC_THETAW_M(#,#) to UNDEFINED

      -  Specified Temperature

         -  Sets keyword BC_HW_THETA_M(#,#) to UNDEFINED
         -  Sets keyword BC_C_THETA_M(#,#) to 0.0
         -  Requires BC_THETAW_M(#,#)

      -  Specified Flux

         -  Sets keyword BC_HW_THETA_M(#,#) to 0.0
         -  Requires BC_C_THETA_M(#)
         -  Sets keyword BC_THETAW_M(#,#) to UNDEFINED

-  Define granular temperature

   -  Disabled for DEM and PIC solids
   -  Specification only available with 'Specified Temperature' BC type
   -  Sets keyword BC_THETAW_M(#,#)
   -  DEFAULT 0.0

-  Define constant flux

   -  Disabled for DEM and PIC solids
   -  Specification only available with 'Specified Flux' BC type
   -  Sets keyword BC_C_THETA_M(#,#)
   -  DEFAULT 0.0

**When solving solids species equations:**

-  Set keyword BC_HW_X_S(#,#,#) to 0.0
-  Set keyword BC_C_X_S(#,#,#) to 0.0
-  Set keyword BC_XW_S(#,#,#) to UNDEFINED

Scalar Tab for Wall BCs
~~~~~~~~~~~~~~~~~~~~~~~

*Tab only available if scalar equations are solved (NSCALAR >0).*

-  Select scalar boundary type:

   -  Available selections:

      -  No-Flux [DEFAULT]

         -  Sets keyword BC_HW_SCALAR(#,#) to 0.0
         -  Sets keyword BC_C_SCALAR(#,#) to 0.0
         -  Sets keyword BC_SCALARW(#,#) to UNDEFINED

      -  Specified Value

         -  Sets keyword BC_HW_SCALAR(#,#) to UNDEFINED
         -  Sets keyword BC_C_SCALAR (#,#) to 0.0
         -  Requires BC_SCALARW (#,#)

      -  Specified Flux

         -  Sets keyword BC_HW_T_S(#,#) to 0.0
         -  Requires BC_C_SCALAR (#)
         -  Sets keyword BC_SCALARW (#,#) to UNDEFINED

      -  Convective Flux

         -  Requires BC_HW_T_S(#,#)
         -  Sets keyword BC_C_SCALAR (#,#) to 0.0
         -  Requires BC_SCALARW (#,#)

-  Define wall scalar

   -  Specification only available with 'Specified Temperature' BC type
   -  Sets keyword BC_SCALARW (#,#)
   -  DEFAULT 0.0

-  Define constant flux

   -  Specification only available with 'Specified Flux' BC type
   -  Sets keyword BC_C_SCALAR (#,#)
   -  DEFAULT 0.0

-  Define transfer coefficient

   -  Specification only available with 'Convective Flux' BC type
   -  Sets keyword BC_HW_SCALAR(#,#)
   -  DEFAULT 0.0

-  Define free stream temperature

   -  Specification only available with 'Convective Flux' BC type
   -  Sets keyword BC_SCALARW (#,#)
   -  DEFAULT 0.0

Inflow (MI/CG_MI/PI) Parameters
_______________________________

*Subtask Pane Tab for INFLOW type (MI, PI, CG_MI) Boundary Condition Regions*

.. todo::
   **Suggestion from Jeff**: Add option to have a transient inflow. Specify
   flow rate or velocity vs. time using a series of control points. This will
   require modification of the source code to accomodate for the new capability.
   This will replace the BC_JET_* keywords which is not very flexible.

Fluid Tab for Inflow (MI/CG_MI/PI) BCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Define volume fraction

   -  Specification always available
   -  Sets keyword BC_EP_G(#)
   -  DEFAULT 1.0 for MI and CG_MI; leave [UNDEFINED] for PI
   -  Error Check: For MI and CG_MI, BC_EP_G(#) + BC_EP_S(#,:) = 1.0
   -  Error Check: For PI -  either all are defined and sum to 1.0, or all are undefined

-  Define inflow properties

   *Mass inflow specification changes based on the BC_TYPE and Region orientation (e.g., XZ-Plane).*

   **Note**: A plane region has 2 tangential directions (lying in the plane) and one axial direction (normal to the plane).  The GUI uses 'X', 'Y', and 'Z' for axes, but the keywords use 'U', 'V', and 'W'.  So, for an XY plane, the tangential directions are U and V (corresponding to X and Y) and the axial direction is W (corresponding to Z).  For an XZ plane, the axial direction is 'Y' in the GUI display, and V in the keyword.  The abbreviated syntax {U,V,W} is used below to indicate a keyword which depends on planar orientation.

   - **For BC_TYPE='MI'**

     -  Select mass inflow specification type:

        -  Available selections:

           -  {X,Y,Z}-Axial Velocity (m/s) [DEFAULT]

              -  Sets keyword BC_{U,V,W}_G(#)
              -  DEFAULT 0.0

           -  Volumetric Flowrate (m^3/s)

              -  Sets keyword BC_VOLFLOW_G(#)
              -  DEFAULT 0.0

           -  Mass Flowrate (kg/s)

              -  Sets keyword BC_MASSFLOW_G(#)
              -  DEFAULT 0.0

     -  Define Tangential Velocities (2 controls):

        -  Define {X,Y,Z}-Axial Velocity

           -  Sets keyword BC_{U,V,W}_G(#)
           -  DEFAULT 0.0

   -  **For BC_TYPE='CG_MI'**

      -  Specify gas mass flow rate (required):

         -  Sets keyword BC_MASSFLOW_g(#)
         -  DEFAULT 0.0

      -  Specify all velocity components (3 controls) (optional):

         -  Define {X,Y,Z}-Axial Velocity

            -  Sets keyword BC_{U,V,W}_G(#)
            -  DEFAULT 0.0

   -  **For BC_TYPE='PI'**

      -  Specify all velocity components (3 controls):

      -  Define {X,Y,Z}-Axial Velocity

         -  Sets keyword BC_{U,V,W}_G(#)
         -  DEFAULT 0.0

   -  Define temperature

      -  Specification always available
      -  Input required for any of the following:

         -  Fluid density model: Ideal Gas Law
         -  Fluid viscosity model: Sutherland's Law
         -  Energy equations are solved

      -  Sets keyword BC_T_G(#)
      -  DEFAULT 293.15

   -  Define pressure

      -  Specification always available
      -  Input required when combining ideal gas law and specified mass inflow rate
      -  Input required for BC_TYPE = PI
      -  Sets keyword BC_P_G(#)
      -  DEFAULT 101.325d3

   -  Select species and set mass fractions (table format)

      -  Specification always available
      -  Input required for species equations
      -  Drop down menu of fluid species
      -  Sets keyword BC_X_G(#,#)
      -  DEFAULT -  last defined species has mass fraction of 1.0
      -  Error check: mass fractions must sum to 1.0

   -  Turbulence: Define k-ε turbulent kinetic energy

      -  Specification only available with K-Epsilon turbulence model
      -  Sets keyword BC_K_TURB_G(#)
      -  DEFAULT 0.0

   -  Turbulence: Define k-ε turbulent dissipation

      -  Specification only available with K-Epsilon turbulence model
      -  Sets keyword BC_E_TURB_G(#)
      -  DEFAULT 0.0

Solid-# Tab for Inflow (MI/CG_MI/PI) BCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Rename tab to user provided solids name.*

-  Define volume fraction

   -  Specification always available
   -  Sets keyword BC_EP_S(#,#)
   -  DEFAULT 1.0 -  (sum of previous tabs) for MI and CG_MI; leave [UNDEFINED] for PI
   -  Error Check: For MI and CG_MI, BC_EP_G(#) + BC_EP_S(#,:) = 1.0
   -  Error Check: For PI -  either all are defined and sum to 1.0, or all are undefined.
   -  Some input decks may or may not contain BC_EP_S keyword:

      -  Volume fraction is specified using the solids bulk density
      -  BC_EP_S(#,#) == BC_ROP_S(#,#) / BC_ROs(#)
      -  Solids density BC_ROs is determined by the solids density model.

         -  For constant solids density, use RO_S0.
         -  For variable solids density, see “Calculating Variable Solids Density” section.

   -  Volume fraction may be inferred from BC_EP_G

      -  BC_EP_S(#,#) = 1.0 -  BC_EP_G(#)
      -  Only valid for one solids phase (MMAX=1)

-  Define inflow properties

   *Mass inflow specification changes based on the BC_TYPE and Region orientation (e.g., XZ-Plane)*

   -  **For BC_TYPE='MI'**

      -  Select mass inflow specification type:

         -  Available selections:

            -  {X,Y,Z}-Axial Velocity (m/s) [DEFAULT]
            -  Sets keyword BC_{U,V,W}_S(#,#)
            -  DEFAULT 0.0

         -  Volumetric Flowrate (m^3/s)

            -  Sets keyword BC_VOLFLOW_S(#,#)
            -  DEFAULT 0.0

         -  Mass Flowrate (kg/s)

            -  Sets keyword BC_MASSFLOW_S(#,#)
            -  DEFAULT 0.0

      -  Define Tangential Velocities (2 controls):

         -  Define {X,Y,Z}-Axial Velocity

            -  Sets keyword BC_{U,V,W}_S(#,#)
            -  DEFAULT 0.0

   -  **For BC_TYPE='CG_MI'**

      -  Specify solids mass flow rate (required):

         -  Sets keyword BC_MASSFLOW_s(#,#)
         -  DEFAULT 0.0

      -  Specify all velocity components (3 controls) (optional):

         -  Define {X,Y,Z}-Axial Velocity

            -  Sets keyword BC_{U,V,W}_S(#,#)
            -  DEFAULT 0.0

   -  **For BC_TYPE='PI'**

      -  Specify all velocity components (3 controls):

         -  Define {X,Y,Z}-Axial Velocity

            -  Sets keyword BC_{U,V,W}_S(#,#)
            -  DEFAULT 0.0

   -  Define temperature

      -  Specification always available
      -  Input required when energy equations are solved
      -  Sets keyword BC_T_S(#,#)
      -  DEFAULT 293.15

   -  Define granular temperature at BC plane

      -  Available only for KT_TYPE /= “ALGEBRAIC"
      -  Sets keyword BC_THETA_M(#,#)
      -  DEFAULT 0.0

   -  Select species and set mass fractions (table format)

      -  Specification always available
      -  Input required for species equations
      -  Drop down menu of solid species
      -  Sets keyword BC_X_S(#,#,#)
      -  DEFAULT -  last defined species has mass fraction of 1.0
      -  Error check: mass fractions must sum to 1.0

Scalar Tab for Inflow (MI/CG_MI/PI) BCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Tab only available if scalar equations are solved (NSCALAR>0).*

-  Define initial scalar value

   -  Sets keyword BC_SCALAR(#,#)
   -  DEFAULT 0.0

Pressure Outflow (PO/CG_PO) Parameters
______________________________________

Fluid Tab for Pressure Outflow (PO/CG_PO) BCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Define pressure

   -  Specification always available
   -  Input required
   -  Sets keyword BC_P_G(#)
   -  DEFAULT 101.325d3

*The remaining inputs are optional. They do not have default values, because MFIX will calculate appropriate values if they are unspecified and 'backflow' occurs at the outlet.*

-  Define volume fraction

   -  Specification always available
   -  Sets keyword BC_EP_G(#)
   -  No DEFAULT value
   -  Error Check: If any volume fraction for the BC region is
      specified, then all volume fractions for the BC region must be
      specified and must sum to 1.0

-  Define temperature

   -  Specification always available
   -  No DEFAULT value
   -  Sets keyword BC_T_G(#)

-  Select species and set mass fractions (table format)

   -  Specification always available
   -  No DEFAULT value
   -  Sets keyword BC_X_G(#,#)

-  Error check: if specified, mass fractions must sum to 1.0

Solids-# Tab for Pressure Outflow (PO/CG_PO) BCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*All inputs are optional. They do not have default values, because MFIX will calculate appropriate values if they are unspecified and 'backflow' occurs at the outlet.*

-  Define volume fraction

   -  Specification always available
   -  Sets keyword BC_EP_S(#,#)
   -  No DEFAULT value
   -  Error Check: If any volume fraction for the BC region is
      specified, then all volume fractions for the BC region must be
      specified and must sum to 1.0

-  Define temperature

   -  Specification always available
   -  No DEFAULT value
   -  Sets keyword BC_T_S(#,#)

-  Select species and set mass fractions (table format)

   -  Specification always available
   -  No DEFAULT value
   -  Sets keyword BC_X_S(#,#,#)
   -  Error check: if specified, mass fractions must sum to 1.0

Scalar Tab for Pressure Outflow (PO/CG_PO) BCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Tab only available if scalar equations are solved (NSCALAR > 0).*

*All inputs are optional. They do not have default values, because MFIX will calculate appropriate values if they are unspecified and 'backflow' occurs at the outlet.*

-  Define scalar value

   -  Sets keyword BC_SCALAR(#,#)
   -  No DEFAULT value

Mass Outflow (MO/CG_MO) Parameters
__________________________________

**Comment on MO Volume fractions:**  The GUI should support two possible cases:

1. All volume fractions (gas and solids) for the mass outlet are
   defined and their sum equals 1.0

2. All volume fractions are undefined.

However, if the BC is defined with either a specified mass or volumetric
flow rate, then all volume fractions must be defined.

Side note: MFIX allows some strange partially-defined volume fractions
for mass outflows but these will not be supported within the GUI.



Fluid Tab for Mass Outflow (MO/CG_MO) BCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Define outflow properties

   *Mass outflow specification changes based on the BC_TYPE and Region orientation (e.g., XZ-Plane)*

   -  **For BC_TYPE='MO'**

      -  Select mass outflow specification type:

         -  Available selections:

            -  {X,Y,Z}-Axial Velocity (m/s) [DEFAULT]

               -  Sets keyword BC_{U,V,W}_G(#)
               -  DEFAULT 0.0

            -  Volumetric Flowrate (m^3/s)

               -  Sets keyword BC_VOLFLOW_G(#)
               -  DEFAULT 0.0

            -  Mass Flowrate (kg/s)

               -  Sets keyword BC_MASSFLOW_G(#)
               -  DEFAULT 0.0

      -  Define Tangential Velocities (2 controls):

         -  Define {X,Y,Z}-Axial Velocity

            -  Sets keyword BC_{U,V,W}_G(#)
            -  DEFAULT 0.0

   -  **For BC_TYPE='CG_MO'**

      -  Specify all velocity components (3 controls):

         -  Define {X,Y,Z}-Axial Velocity

            -  Sets keyword BC_{U,V,W}_G(#)
            -  DEFAULT 0.0

-  Define duration to average outflow rate.

   -  Specification always available
   -  Input required
   -  Sets keyword BC_DT_0(#)
   -  DEFAULT 0.1
   -  Error Check: Value should be positive (nonzero)
   -  BC_DT_0 specification should persist across the gas and solids tabs.
      If the user sets it in the gas phase tab, but then changes it under
      a solids tab, a warning message indicating that this value is common
      across all phases should be given.

*The remaining inputs are only required when either the mass or the volumetric flowrates are specified. They are not required if the velocities are given for the outlet.*

-  Define volume fraction

   -  Specification required for specified mass or volumetric flowrates.
   -  Input required
   -  Sets keyword BC_EP_G(#)
   -  DEFAULT value 1.0
   -  Error Check: If any volume fraction for the BC region is
      specified, then all volume fractions for the BC region must be
      specified and must sum to 1.0

-  Define temperature

   -  Specification is required when solving the energy equations
      (ENERGY_EQ == .TRUE.) or with mass or volumetric flowrates and
      (RO_G0 == UNDEFINED)
   -  DEFAULT value 293.15
   -  Sets keyword BC_T_G(#)

-  Select species and set mass fractions (table format)

   -  Specification is available with mass or volumetric
      flowrates when (R_G0 == UNDEFINED)
   -  DEFAULT value 1.0 of last defined species
   -  Sets keyword BC_X_G(#,#)
   -  Error check: if specified, mass fractions must sum to 1.0

Solids-# Tab for Mass Outflow (MO/CG_MO) BCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Define outflow properties

   *Mass outflow specification changes based on the BC_TYPE and Region orientation (e.g., XZ-Plane)*

-  **For BC_TYPE='MO'**

   -  Select mass outflow specification type:

      -  Available selections:

         -  {X,Y,Z}-Axial Velocity (m/s) [DEFAULT]

            -  Sets keyword BC_{U,V,W}_S(#,#)
            -  DEFAULT 0.0

         -  Volumetric Flowrate (m^3/s)

            -  Sets keyword BC_VOLFLOW_S(#,#)
            -  DEFAULT 0.0

         -  Mass Flowrate (kg/s)

            -  Sets keyword BC_MASSFLOW_S(#,#)
            -  DEFAULT 0.0

   -  Define Tangential Velocities (2 controls):

      -  Define {X,Y,Z}-Axial Velocity

         -  Sets keyword BC_{U,V,W}_S(#,#)
         -  DEFAULT 0.0

-  **For BC_TYPE='CG_MO'**

   -  Specify all velocity components (3 controls):

      -  Define {X,Y,Z}-Axial Velocity

         -  Sets keyword BC_{U,V,W}_S(#,#)
         -  DEFAULT 0.0

-  Define duration to average outflow rate.

   -  Specification always available
   -  Input required
   -  Sets keyword BC_DT_0(#)
   -  DEFAULT 0.1
   -  Error Check: Value should be positive (nonzero)
   -  BC_DT_0 specification should persist across the gas and solids tabs.
      If the user sets it in the gas phase tab, but then changes it under
      a solids tab, a warning message indicating that this value is common
      across all phases should be given.

Scalar Tab for Mass Outflow (MO/CG_MO) BCs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Scalar params for outflow?**


Cyclic Parameters
_________________

-  Select cyclic axis

   -  Available selections

      -  X-Axis; Cyclic YZ boundary planes

         -  Sets keyword CYCLIC_X to .TRUE.

      -  Y-Axis; Cyclic XZ boundary planes

         -  Sets keyword CYCLIC_Y to .TRUE.

      -  Z-Axis; Cyclic XY boundary planes

         -  Sets keyword CYCLIC_Z to .TRUE.

-  Enable specified pressure drop

   -  DEFAULT .FALSE.
   -  Sets keyword based on axis:

      -  X-Axis; Cyclic YZ boundary planes

         -  Sets keyword CYCLIC_X_PD to .TRUE.
         -  Required input for DELP_X
         -  DEFAULT 0.0

      -  Y-Axis; Cyclic XZ boundary planes

         -  Sets keyword CYCLIC_Y_PD to .TRUE.
         -  Required input for DELP_Y
         -  DEFAULT 0.0

      -  Z-Axis; Cyclic XY boundary planes

         -  Sets keyword CYCLIC_Z_PD to .TRUE.
         -  Required input for DELP_Z
         -  DEFAULT 0.0

   -  Error check: Only one axis can have a specified pressure drop
   -  Error check: There should not be any BCs defined on walls that are
      cyclic. (I'm not sure if this check can be easily implemented).

-  Enable specified gas mass flux

   -  Requires specified pressure drop
   -  Sets keyword FLUX_G
   -  DEFAULT 0.0
   -  Error check: Only one axis can have a specified mass flux. (This
      should not be an issue as it requires a specified pressure drop
      which can only be applied to one axis.)

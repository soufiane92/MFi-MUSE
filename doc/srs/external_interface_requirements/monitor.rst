Monitors Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^

This section defines how **monitors** are to be implemented into MFIX.


Below is a general mock-up of the Monitors pane for the GUI. This layout
is consistent with other model setup panes. A add/remove monitor feature
is located at the top left above the table listing defined monitors. The
mid and lower sections allow users to define monitor properties.

.. figure:: /../doc/srs/images/monitors.png
   :scale: 75 %
   :align: center

   Mock-up of monitor pane

Region Selection
~~~~~~~~~~~~~~~~

-  Icons to add/remove/duplicate regions are given at the top

-  Clicking the 'add' and 'duplicate' buttons triggers a popup window
   where the user must select a region.

   -  Monitor regions can be points, planes, or volumes
   -  STL regions can not be used to defined a monitor region
   -  Multiple regions can not be combined to define a monitor
   -  Invalid regions can not be selected in the menu
   -  Valid regions set the following keywords

      -  MONITOR_X_W(#)
      -  MONITOR_X_E(#)
      -  MONITOR_Y_S(#)
      -  MONITOR_Y_N(#)
      -  MONITOR_Z_B(#)
      -  MONITOR_Z_T(#)

   -  Error Checks:

      -  X_MIN <= MONITOR_X_W(#)
      -  Y_MIN <= MONITOR_Y_S(#)
      -  Z_MIN <= MONITOR_Z_T(#)
      -  MONITOR_X_E(#) <= X_MAX
      -  MONITOR_Y_N(#) <= Y_MAX
      -  MONITOR_Z_T(#) <= Z_MAX
      -  MONITOR_X_W(#) <= MONITOR_X_E(#)
      -  MONITOR_Y_S(#) <= MONITOR_Y_N(#)
      -  MONITOR_Z_B(#) <= MONITOR_Z_T(#)


General Monitor Controls
~~~~~~~~~~~~~~~~~~~~~~~~

-  Specify Monitor Name

   -  Specification is required
   -  Sets keyword MONITOR_NAME(#)
   -  DEFAULT Monitor#
   -  Error Checks:

      -  Must be unique with respect to other monitor names
      -  Must comply with OS file naming requirements
      -  Must not exceed 64 characters

-  Select Monitor Type

   -  Selection is required
   -  Available selections:

      -  Value

         -  Sets keyword MONITOR_TYPE(#) to 0
         -  Available only for point regions.

      -  Sum

         -  Sets keyword MONITOR_TYPE(#) to 1
         -  Available only for area and volume regions.

      -  Min

         -  Sets keyword MONITOR_TYPE(#) to 2
         -  Available only for area and volume regions.

      -  Max

         -  Sets keyword MONITOR_TYPE(#) to 3
         -  Available only for area and volume regions.

      -  Average

         -  Sets keyword MONITOR_TYPE(#) to 4
         -  Available only for area and volume regions.

      -  Standard Deviation

         -  Sets keyword MONITOR_TYPE(#) to 5
         -  Available only for area and volume regions.

      -  Area-Weighted Average

         -  Sets keyword MONITOR_TYPE(#) to 6
         -  Available only for area regions.

      -  Flow Rate

         -  Sets keyword MONITOR_TYPE(#) to 7
         -  Available only for area regions.

      -  Mass Flow Rate

         -  Sets keyword MONITOR_TYPE(#) to 8
         -  Available only for area regions.

      -  Mass-Weighted Average

         -  Sets keyword MONITOR_TYPE(#) to 9
         -  Available only for area regions.

      -  Volume Flow Rate

         -  Sets keyword MONITOR_TYPE(#) to 10
         -  Available only for area regions.

      -  Volume Integral

         -  Sets keyword MONITOR_TYPE(#) to 11
         -  Available only for volume regions.

      -  Volume-Weighted Average

         -  Sets keyword MONITOR_TYPE(#) to 12
         -  Available only for volume regions.

      -  Mass-Weighted Integral

         -  Sets keyword MONITOR_TYPE(#) to 13
         -  Available only for volume regions.

      -  Mass-Weighted Average

         -  Sets keyword MONITOR_TYPE(#) to 14
         -  Available only for volume regions.

-  Specify Write Interval

   -  Sets keyword MONITOR_DT(#)
   -  DEFAULT 0.05
   -  Error check:

      -  Specification is required
      -  Value must be positive, non-zero

Variable Selection Window
~~~~~~~~~~~~~~~~~~~~~~~~~

**Fluid Phase Tab**

-  Enable monitoring gas volume fraction

   -  Selection always available
   -  Sets keyword MONITOR_EP_G(#)
   -  DEFAULT value .FALSE.

-  Enable monitoring gas pressure

   -  Requires fluid solver (RO_G0 /= 0.0)
   -  Sets keyword MONITOR_P_G(#)
   -  DEFAULT value .FALSE.

-  Enable monitoring gas velocity x-component

   -  Requires fluid solver (RO_G0 /= 0.0) -
   -  Sets keyword MONITOR_U_G(#)
   -  DEFAULT value .FALSE.

-  Enable monitoring gas velocity y-component

   -  Requires fluid solver (RO_G0 /= 0.0)
   -  Sets keyword MONITOR_V_G(#)
   -  DEFAULT value .FALSE.

-  Enable monitoring gas velocity z-component

   -  Requires fluid solver (RO_G0 /= 0.0)
   -  Sets keyword MONITOR_W_G(#)
   -  DEFAULT value .FALSE.

-  Enable monitoring gas temperature

   -  Requires fluid solver (RO_G0 /= 0.0) and ENERGY_EQ = .TRUE.
   -  Sets keyword MONITOR_T_G(#)
   -  DEFAULT value .FALSE.

-  Enable monitoring gas species N ( *an entry for each defined species*)

   -  Requires defined gas phase species
   -  Use species name when generating layout (e.g., CO mass fraction)
   -  Sets keyword MONITOR_X_G(#,N)
   -  DEFAULT value .FALSE.


-  Enable monitoring turbulent kinetic energy

   -  Requires fluid solver (RO_G0 /= 0.0) and TURBULENCE_MODEL=’K_EPSILON’
   -  Sets keyword MONITOR_K_TURB_G(#)
   -  DEFAULT .FALSE.

-  Enable monitoring turbulent dissipation

   -  Requires fluid solver (RO_G0 /= 0.0) and TURBULENCE_MODEL=’K_EPSILON’
   -  Sets keyword MONITOR_E_TURB_G(#)
   -  DEFAULT .FALSE.

-  Enable monitoring reaction rates

   -  Requires nRR > 0
   -  Sets keyword MONITOR_RRATE(#, #)
   -  DEFAULT value .FALSE.

**Solids Phase Tab** *(Requires TFM Solids)*

-  Enable monitoring solids velocity x-component

   -  Requires TFM solids
   -  Sets keyword MONITOR_U_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable monitoring solids velocity y-component

   -  Requires TFM solids
   -  Sets keyword MONITOR_V_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable monitoring solids velocity z-component

   -  Requires TFM solids
   -  Sets keyword MONITOR_W_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable monitoring solids bulk density

   -  Requires TFM solids
   -  Sets keyword MONITOR_ROP_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable monitoring solids temperature

   -  Requires TFM solids and ENERGY_EQ = .TRUE.
   -  Sets keyword MONITOR_T_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable monitoring solids phase M, species N ( *an entry for each defined species*)

   -  Requires TFM solids and SPECIES_EQ(#) = .TRUE.
   -  Use species name when generating layout (e.g., Char mass fraction)
   -  Sets keyword MONITOR_X_S(#,M,N)
   -  DEFAULT value .FALSE.

-  Enable monitoring solids phase granular temperature

   -  Requires TFM solids and KT_TYPE /= “ALGEBRAIC”
   -  Sets keyword MONITOR_THETA_M(#,#)
   -  DEFAULT value .FALSE.

-  Enable monitoring solids pressure

   -  Requires TFM solids
   -  Sets keyword MONITOR_P_STAR(#)
   -  DEFAULT value .FALSE.
   -  *There is only one solids pressure for all phases.*


**Scalar Tab** *(Requires NSCALAR > 0)*

-  Enable monitoring user defined scalar

   -  Requires NSCALAR > 0
   -  Sets keyword MONITOR_SCALAR(#, #)
   -  DEFAULT value .FALSE.


Solver Integration
__________________


Definitions
~~~~~~~~~~~

DIMENSION_MONITOR
  Maximum number of monitors (100)

-  Create new variables as keywords

   -  MONITOR_X_W(:)

      -  Type: DOUBLE PRECISION array
      -  Size: DIMENSION_MONITOR
      -  Init: UNDEFINED

   -  MONITOR_X_E(:)

      -  Type: DOUBLE PRECISION array
      -  Size: DIMENSION_MONITOR
      -  Init: UNDEFINED

   -  MONITOR_Y_S(:)

      -  Type: DOUBLE PRECISION array
      -  Size: DIMENSION_MONITOR
      -  Init: UNDEFINED

   -  MONITOR_Y_N(:)

      -  Type: DOUBLE PRECISION array
      -  Size: DIMENSION_MONITOR
      -  Init: UNDEFINED

   -  MONITOR_Z_B(:)

      -  Type: DOUBLE PRECISION array
      -  Size: DIMENSION_MONITOR
      -  Init: UNDEFINED

   -  MONITOR_Z_T(:)

      -  Type: DOUBLE PRECISION array
      -  Size: DIMENSION_MONITOR
      -  Init: UNDEFINED

   -  MONITOR_NAME(:)

      -  Type: CHARACTER(len=64) array
      -  Size: DIMENSION_MONITOR
      -  Init: UNDEFINED_C

   -  MONITOR_TYPE(:)

      -  Type: INTEGER array
      -  Size: DIMENSION_MONITOR
      -  Init: UNDEFINED_I

   -  MONITOR_DT(:)

      -  Type: DOUBLE PRECISION array
      -  Size: DIMENSION_MONITOR
      -  Init: UNDEFINED

   -  MONITOR_EP_G(:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR
      -  Init: .FALSE.

   -  MONITOR_P_G(:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR
      -  Init: .FALSE.

   -  MONITOR_U_G(:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR
      -  Init: .FALSE.

   -  MONITOR_V_G(:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR
      -  Init: .FALSE.

   -  MONITOR_W_G(#)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR
      -  Init: .FALSE.

   -  MONITOR_T_G(:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR
      -  Init: .FALSE.

   -  MONITOR_X_G(:,:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR, DIM_N_G
      -  Init: .FALSE.

   -  MONITOR_T_G(:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR
      -  Init: .FALSE.

   -  MONITOR_K_TURB_G(:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR
      -  Init: .FALSE.

   -  MONITOR_E_TURB_G(:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR
      -  Init: .FALSE.

   -  MONITOR_RRATE(:,:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR, 100
      -  Init: .FALSE.

   -  MONITOR_U_S(:,:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR, DIM_M
      -  Init: .FALSE.

   -  MONITOR_V_S(:,:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR, DIM_M
      -  Init: .FALSE.

   -  MONITOR_W_S(:,:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR, DIM_M
      -  Init: .FALSE.

   -  MONITOR_ROP_S(:,:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR, DIM_M
      -  Init: .FALSE.

   -  MONITOR_T_S(:,:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR, DIM_M
      -  Init: .FALSE.

   -  MONITOR_X_S(:,:,:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR, DIM_M, DIM_N_S
      -  Init: .FALSE.

   -  MONITOR_THETA_M(:,:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR, DIM_M
      -  Init: .FALSE.

   -  MONITOR_P_STAR(:)

      -  Type: LOGICAL array
      -  Size: DIMENSION_MONITOR, DIM_M
      -  Init: .FALSE.

-  Create functions to collect monitor data.

   -  Value

      -  MONITOR_TYPE(#) == 0

   -  Sum

      -  MONITOR_TYPE(#) == 1

   -  Min

      -  MONITOR_TYPE(#) == 2

   -  Max

      -  MONITOR_TYPE(#) == 3

   -  Average

      -  MONITOR_TYPE(#) == 4

   -  Standard Deviation

      -  MONITOR_TYPE(#) == 5

   -  Area-Weighted Average

      -  MONITOR_TYPE(#) == 6

   -  Flow Rate

      -  MONITOR_TYPE(#) == 7

   -  Mass Flow Rate

      -  MONITOR_TYPE(#) == 8

   -  Mass-Weighted Average

      -  MONITOR_TYPE(#) == 9

   -  Volume Flow Rate

      -  MONITOR_TYPE(#) == 10

   -  Volume Integral

      -  MONITOR_TYPE(#) == 11

   -  Volume-Weighted Average

      -  MONITOR_TYPE(#) == 12

   -  Mass-Weighted Integral

      -  MONITOR_TYPE(#) == 13

   -  Mass-Weighted Average

      -  MONITOR_TYPE(#) == 14

-  Saving collected data:

   -  ASCII file in comma separated value (CSV) format
   -  File name = MONITOR_NAME(#).csv
   -  Header contains variable names
   -  First column is simulation time (TIME)

-  Run time error checking of monitor section.

Function Definitions
____________________


========================= ========================================
Symbol                     Description
========================= ========================================
:math:`\phi_{ijk}`        Variable value at indexed cell
:math:`\varepsilon_{ijk}` Phase **volume fraction** at indexed cell
:math:`\rho_{jk}`         Phase **density** at indexed cell
:math:`\vec{v}_{jk}`      Phase **velocity** at indexed cell
:math:`A_{ijk}`           Cross-sectional area of cell
:math:`V_{ijk}`           Volume of indexed cell
========================= ========================================

General Monitors
~~~~~~~~~~~~~~~~~~~~

-  Value

   -  **Description:** Returns the value of the field quantity in
      the selected region.

.. math:: \phi_{ijk}


-  Sum

   -  **Description:** The sum is computed by summing all values of
      the field quantity in the selected region.

.. math:: \sum_{ijk}\phi_{ijk}


-  Min

   -  **Description:** Minimum value of the field quantity in the
      selected region.

.. math:: \min_{ijk} \phi_{ijk}


-  Max

   -  **Description:** Maximum value of the field quantity in the
      selected region.

.. math:: \max_{ijk} \phi_{ijk}


-  Average

   -  **Description:** Average value of the field quantity in the selected
      region where :math:`N` is the total number of observations (cells)
      in the selected region.

.. math:: \phi_0 = \frac{\sum_{ijk} \phi_{ijk}}{N}


-  Standard Deviation

   -  **Description:** The standard deviation of the field quantity in the
      selected region where :math:`\phi_0` is the average of the variable in
      the selected region.

.. math:: \sigma_{\phi} = \sqrt{\frac{ \sum_{ijk} (\phi_{ijk}-\phi_{0})^2 }{N}}



Surface Integrals
~~~~~~~~~~~~~~~~~


-  Area

   -  **Description:** Area of selected region is computed by summing the
      areas of the facets that define the surface.

.. math:: \int dA = \sum_{ijk} \lvert A_{ijk} \rvert


-  Area-Weighted Average

   -  **Description:** The area-weighted average is computed by dividing
      the summation of the product of the selected variable and facet
      area by the total area of the region.

.. math:: \frac{\int\phi dA}{A} = \frac{\sum_{ijk}{\phi_{ijk} \lvert A_{ijk} \rvert}}{\sum_{ijk}{\lvert A_{ijk} \rvert}}


-  Flow Rate

   -  **Description:** The flow rate of a field variable through a surface
      is computed by summing the product of the phase volume fraction,
      density, the selected field variable, phase velocity normal to
      the facet :math:`v_n`, and the facet area.

.. math:: \int\varepsilon\rho\phi{v_n}dA = \sum_{ijk}\varepsilon_{ijk}\rho_{ijk}\phi_{ijk} {v}_{n,ijk} \lvert A_{ijk} \rvert


-  Mass Flow Rate

   -  **Description:** The mass flow rate through a surface is computed
      by summing the product of the phase volume fraction, density,
      phase velocity normal to the facet :math:`v_n`, and the facet
      area.

.. math:: \int\varepsilon\rho{v_n} dA = \sum_{ijk}\varepsilon_{ijk}\rho_{ijk}{v}_{n,ijk}  \lvert A_{ijk} \rvert


-  Mass-Weighted Average

   -  **Description:** **FIXME** The mass flow rate through a surface is computed
      by summing the product of the phase volume fraction, density,
      phase velocity normal to the facet, and the facet area.

.. math:: \frac{\int\varepsilon\rho\phi\lvert{v_n}dA\rvert}{\int\varepsilon\rho\lvert{v_n}dA\rvert} = \frac{\sum_{ijk}\varepsilon_{ijk}\rho_{ijk}\phi_{ijk}\lvert {v}_{n,ijk} A_{ijk} \rvert}{\sum_{ijk}\varepsilon_{ijk}\rho_{ijk} \lvert {v}_{n,ijk} A_{ijk} \rvert}


-  Volume Flow Rate

   -  **Description:** The volume flow rate through a surface is computed
      by summing the product of the phase volume fraction, phase velocity
      normal to the facet :math:`v_n`, and the facet area.

.. math:: \int\varepsilon{v_n}dA = \sum_{ijk}\varepsilon_{ijk}{v}_{n,ijk} \lvert A_{ijk} \rvert



Volume Integration
~~~~~~~~~~~~~~~~~~


-  Volume

   -  **Description:** The volume is computed by summing all of the cell
      volumes in the selected region.

.. math:: \int  dV = \sum_{ijk}{ \lvert V_{ijk}} \rvert


-  Volume Integral

   -  **Description:** The volume integral is computed by summing the product
      of the selected field variable and the cell volume.

.. math:: \int \phi dV = \sum_{ijk}{\phi_{ijk} \lvert V_{ijk}} \rvert


-  Volume-Weighted Average

   -  **Description:** The volume-weighted average is computed by dividing
      the summation of the product of the selected field variable and
      cell volume by the sum of the cell volumes.

.. math:: \frac{\int\phi dV}{V} = \frac{\sum_{ijk}{\phi_{ijk} \lvert V_{ijk} \rvert}}{\sum_{ijk}{\lvert V_{ijk} \rvert}}

-  Mass-Weighted Integral

   -  **Description:** The mass-weighted integral is computed by summing
      the product of phase volume fraction, density, selected field
      variable, and cell volume.

.. math:: \int \varepsilon\rho\phi dV = \sum_{ijk}\varepsilon_{ijk}\rho_{ijk}\phi_{ijk} \lvert V_{ijk}\rvert


-  Mass-Weighted Average

   -  **Description:** The mass-weighted average is computed by dividing the
      sum of the product of phase volume fraction, density, selected field
      variable, and cell volume by the summation of the product of the
      phase volume fraction, density, and cell volume.

.. math:: \frac{\int\phi\rho\varepsilon dV}{\int\rho\varepsilon dV} = \frac{\sum_{ijk}\phi_{ijk}\varepsilon_{ijk}\rho_{ijk} \lvert V_{ijk}\rvert}{\sum_{ijk}\varepsilon_{ijk}\rho_{ijk} \lvert V_{ijk}\rvert}



Monitors - API
______________

Monitors are specified in the input deck (mfx file). There is not
active run-time communication required between the GUI, solver, and/or
pyMFIX.

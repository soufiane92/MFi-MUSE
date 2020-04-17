Model Setup Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  .. todo::

     - Remove the check box to enable turbulence. This option is redundant.


Select MFIX solver and other conservation equations

-  Select MFIX Solver:

   -  Available selections:

      - Single phase

        -  Selection disables ‘Solids’ task pane menu
        -  Selection disables ‘Continuum Solids Model’ task pane menu
        -  Selection disables ‘Discrete Element Model’ task pane menu
        -  Selection disables ‘Particle-in-Cell’ task pane menu

      - MFIX-TFM

        -  Selection enables ‘Solids’ task pane menu
        -  Selection enables ‘Continuum Solids Model’ task pane menu

      - MFIX-DEM

        -  Selection enables ‘Solids’ task pane menu
        -  Selection enables ‘Discrete Element Model’ task pane menu

   -  Disabled selections (17.1):

      - MFIX-PIC

        -  Selection enables ‘Solids’ task pane menu
        -  Selection enables ‘Particle-in-Cell’ task pane menu

      - MFIX-Hybrid

        -  Selection enables ‘Solids’ task pane menu
        -  Selection enables ‘Continuum Solids Model’ task pane menu
        -  Selection enables ‘Discrete Element Model’ task pane menu

-  Option to disable the fluid phase

   -  Unavailable for "Single Phase" solver
   -  Disables the ‘Fluid’ task pane menu
   -  Sets keyword RO_G0 to 0.

-  Option to enable thermal energy equations

   -  This keyword should always be specified in the input deck
   -  Sets keyword ENERGY_EQ
   -  DEFAULT .FALSE.

-  Select turbulence model

   -  Selection available if fluid phase is enabled (RO_G0/=0.0)
   -  Available selections:

      -  None; [DEFAULT]

        -  Selection always available
        -  Sets keyword TURBULENCE_MODEL to NONE

      -  Mixing Length:

        -  Selection always available
        -  Sets keyword TURBULENCE_MODEL to MIXING_LENGTH
        -  Requires IC_L_SCALE for all IC regions

      -  K-Epsilon

        -  Selection always available
        -  Sets keyword TURBULENCE_MODEL to K_EPSILON
        -  Requires IC_K_TURB_G for all IC regions
        -  Requires IC_E_TURB_G for all IC regions
        -  Requires BC_K_TURB_G for inflow (MI and PI) BC regions
        -  Requires BC_E_TURB_G for inflow (MI and PI) BC regions

-  Specify maximum fluid viscosity

   -  Selection available if TURBULENCE_MODEL =/ 'NONE'
   -  Sets keyword MU_GMAX
   -  DEFAULT 1.0e3 (Pa.s)

-  Specify Gravitational acceleration

   -  Specification always available
   -  Sets keywords GRAVITY_X, GRAVITY_Y, GRAVITY_Z
   -  DEFAULT values

      -  GRAVITY_X = 0.
      -  GRAVITY_Y = -GRAVITY
      -  GRAVITY_Z = 0.

-  Specify drag model

   -  Selection requires TFM, DEM, or PIC solver
   -  Sets keyword DRAG_TYPE
   -  Available selections:

      - SYAM_OBRIEN (DEFAULT)

        -  Specify model parameter: DRAG_C

           - DEFAULT 0.8

        -  Specify model parameter: DRAG_D

           - DEFAULT 2.65

      -  BVK
      -  GIDASPOW
      -  GIDASPOW_BLEND
      -  GIDASPOW_PCF
      -  GIDASPOW_BLEND_PCF
      -  HYS

         -  Specify model parameter LAM_HYS

            - DEFAULT 1.0e-6 (meters)

      -  KOCH_HILL
      -  KOCH_HILL_PCF
      -  WEN_YU
      -  WEN_YU_PCF
      -  USER_DRAG

-  Specify heat transfer correlation (requires TFM, DEM, or PIC solver)
   *This option may be premature as MFIX is limited in heat HTCs.*

-  Specify momentum equation formulation

   -  Selection requires fluid solver
   -  Available selections:

      -  Model A [DEFAULT]
      -  Model B
      -  Jackson
      -  Ishii

-  Select sub-grid model:

   -  Selection requirements:

      -  Only available with MFIX-TFM solver
      -  DRAG_TYPE="WEN_YU"
      -  KT_TYPE="ALGEBRAIC"
      -  TURBULENCE_MODEL /= K_EPSILON
      -  BLENDING_STRESS = NONE
      -  FRICTION_MODEL /= SRIVASTAVA
      -  (There are more restrictions...)

   -  Sets keyword SUBGRID_TYPE

   -  Available selections

      - NONE (DEFAULT)
      - IGCI
      - MILIOLI

-  Specify sub-grid model filter size ratio:

   -  Specification requires SUBGRID_TYPE =/ NONE
   -  Sets keyword FILTER_SIZE_RATIO
   -  DEFAULT 2.

-  Enable sub-grid wall correction model:

   -  Specification requires SUBGRID_TYPE =/ NONE
   -  Sets keyword SUBGRID_WALL
   -  DEFAULT FALSE

-  Enable user-defined subroutines

   -  Selection always available
   -  Sets keyword CALL_USR (.TRUE./.FALSE.)
   -  DEFAULT VALUE is .FALSE.

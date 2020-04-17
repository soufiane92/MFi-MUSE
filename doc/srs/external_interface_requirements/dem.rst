Discrete Element Model Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(requires DEM solver)

- Specify clipping value of mapped void fraction
   -  Sets keyword EP_STAR

-  Enable automatic particle generation

   -  Enabled sets keyword GENER_PART_CONFIG to true
   -  Disabled enables the user to specify number of entries
      in particle input file

      -  DEFAULT 0
      -  Sets keyword PARTICLES

-  Select numerical integration method

   -  Selection always available
   -  Available selections

      -  Euler [DEFAULT]

         -  Selection always available
         -  Sets keyword DES_INTG_METHOD to ‘EULER’

      -  Adams-Bashforth

         -  Selection always available
         -  Sets keyword DES_INTG_METHOD to ‘ADAMS_BASHFORTH’

-  Selection collision model

   -  Selection always available
   -  Available selections

      -  Linear Spring-Dashpot [DEFAULT]

         -  Selection always available
         -  Sets keyword DES_COLL_MODEL to ‘LSD’

      -  Hertzian

         -  Selection always available
         -  Sets keyword DES_COLL_MODEL to ‘HERTZIAN’

-  Select gas-solids coupling scheme:

   -  Selection unavailable if fluid model is disabled
   -  Available selections:

      -  One-way Coupled

         -  Selection always available
         -  Sets keyword DES_ONEWAY_COUPLED true

      -  Fully Coupled [DEFAULT]

         -  Selection always available
         -  Sets keyword DES_ONEWAY_COUPLED false

-  Optional to enable explicitly coupled simulation

   -  Unavailable for GARG_2012 interpolation

-  Select interpolation framework:

   -  Selection always available
   -  Available selections:

      -  field-to-particle and particle-to-field [DEFAULT]

         -  Sets keyword DES_INTERP_ON to true
         -  Sets keyword DES_INTERP_MEAN_FIELDS to true

      -  field-to-particle only

         -  Sets keyword DES_INTERP_ON to true
         -  Sets keyword DES_INTERP_MEAN_FIELDS to false

      -  particle-to-field only

         -  Sets keyword DES_INTERP_ON to false
         -  Sets keyword DES_INTERP_MEAN_FIELDS to true

      -  no-interpolation

         -  Sets keyword DES_INTERP_ON to false
         -  Sets keyword DES_INTERP_MEAN_FIELDS to false

-  Select interpolation scheme:

   -  Selection available except when no-interpolation framework is selected
   -  Available selections:

      -  None [locked default for no-interpolation framework]

         -  Selection always available
         -  Sets keyword DES_INTERP_SCHEME=’NONE’

      - Garg 2012

        -  Selection not available with explicit coupling enabled
        -  Sets keyword DES_INTERP_SCHEME=’GARG_2012’

      -  Square DPVM

         -  Selection always available
         -  Requires an interpolation width, DES_INTERP_WIDTH
         -  Sets keyword DES_INTERP_SCHEME=’SQUARE_DPVM’

-  Define interpolation width (DPVM only) (required)

   -  Specification only available with SQUARE_DPVM interpolation scheme
   -  Sets keyword DES_INTERP_WIDTH

-  Option to enable diffusion of particle data

   -  Selection unavailable with GARG_2012 interpolation scheme
   -  No keyword is set by this option
   -  Enables the user to specify a diffusion width

      - Sets keyword DES_DIFFUSE_WIDTH

-  Specify friction coefficient

   -  Specification always required
   -  Sets keyword MEW (MEW_W)

-  Specify normal spring constant

   -  Only available for LSD collision model
   -  Sets keyword KN (KN_W)

-  Specify tangential spring constant factor

   -  Only available for LSD collision model
   -  Sets keyword KT_FAC (KT_W_FAC)
   -  DEFAULT 2.0/7.0

-  Specify tangential damping coefficient factor

-  Only available for LSD collision model

   -  Sets keyword DES_ETAT_FAC (DES_ETAT_W_FAC)
   -  DEFAULT 0.5

-  Specify Young’s modulus

   -  Only available for Hertzian collision model
   -  Sets keyword E_YOUNG (EW_YOUNG)

   -  Specify Poisson ratio: Only available for Hertzian collision model
   -  Sets keyword V_POISSON (VW_POISSON)

-  Specify normal restitution coefficient

   -  Specification always required
   -  Sets keyword DES_EN_INPUT (DES_EN_WALL_INPUT)
   -  Input given as an upper triangular matrix

-  Specify tangential restitution coefficient

   -  Specification available for Hertzian collision model
   -  Sets keyword DES_ET_INPUT (DES_ET_WALL_INPUT)
   -  Input given as an upper triangular matrix

-  Select cohesion model

   -  Selection always available
   -  Available selections

      -  None [DEFAULT]

         -  Selection always available
         -  Sets keyword USE_COHESION to false
         -  Sets keyword VAN_DER_WAALS to false

      -  Van der Waals

         -  Selection always available
         -  Sets keyword USE_COHESION to true
         -  Sets keyword VAN_DER_WAALS to true

-  Specify Hamaker constant

   -  Specification only available for Van der Waals cohesion model
   -  Sets keyword HAMAKER_CONSTANT (WALL_HAMAKER_CONSTANT)

-  Specify outer cutoff;

   -  Specification only available for Van der Waals cohesion model
   -  Sets keyword VDW_OUTER_CUTOFF (WALL_OUTER_CUTOFF)

-  Specify inner cutoff

   -  Specification only available for Van der Waals cohesion model
   -  Sets keyword VDW_INNER_CUTOFF (WALL_INNER_CUTOFF)

-  Specify asperities

   -  Specification only available for Van der Waals cohesion model
   -  Sets keyword ASPERITIES

Advanced
________

*List the following options under an ‘Advanced’ section header.*

-  Select Neighbor Search Method

   -  Selection always available
   -  Available selection

      -  Grid-based [DEFAULT]

         -  Selection always available
         -  Sets keyword DES_NEIGHBOR_SEARCH 4

      -  N-Square

         -  Selection always available
         -  Sets keyword DES_NEIGHBOR_SEARCH 1

-  Specify maximum steps between neighbor search

   -  Specification always available
   -  Sets keyword NEIGHBOR_SEARCH_N

-  Specify factor defining particle neighborhood

   -  Specification always available
   -  Sets keyword FACTOR_RLM

-  Specify neighborhood search radius ratio

   -  Specification always available
   -  Sets keyword NEIGHBOR_SEARCH_RAD_RATIO

-  Specify search grid partitions (optional)

   -  Specification always available
   -  Sets keyword DESGRIDSEARCH_IMAX
   -  Sets keyword DESGRIDSEARCH_JMAX
   -  Sets keyword DESGRIDSEARCH_KMAX

-  Enable user scalar tracking

   -  Selection always available
   -  Does not directly set any keywords
   -  Enables specification of number of user scalars

      -  Sets keyword DES_USR_VAR_SIZE

-  Define minimum distance for contact conduction (optional)

   -  Unavailable if not solving energy equations

-  Define fluid lens proportion constant (optional)

   -  Unavailable if not solving energy equations

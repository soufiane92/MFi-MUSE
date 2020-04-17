Numerics Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^

The numerics input is split into tabs to group similar inputs and reduce
the amount of input needed on a single pane.


   .. todo::

      **Suggestion from Jeff**: For the discretization, linear solver and
      preconditioner, add option to apply a setting to all (say apply
      superbee to all equations), and add an option to reset all to default.


Residuals Tab
_____________


-  Specify residual for continuity plus momentum equations

   -  Specification always available
   -  Sets keyword TOL_RESID
   -  DEFAULT 1.0e-3

-  Specify residual for energy equations

   -  Specification always available
   -  Sets keyword TOL_RESID_T
   -  DEFAULT 1.0e-4

-  Specify residual for species equations

   -  Specification always available
   -  Sets keyword TOL_RESID_X
   -  DEFAULT 1.0e-4

-  Specify residual for granular energy equations

   -  Specification always available
   -  Sets keyword TOL_RESID_TH
   -  DEFAULT 1.0e-4

-  Specify residual for scalar/K-Epsilon

   -  Specification always available
   -  Sets keyword TOL_RESID_SCALAR
   -  DEFAULT 1.0e-4


Discretization Tab
__________________

-  Select temporal discretization scheme

   -  Selection always available
   -  Available selections:

      -  Implicit Euler [DEFAULT]

         -  Sets keyword CN_ON to .FALSE.

      -  Crank-Nicholson

         -  Sets keyword CN_ON to .TRUE.

-  Specify spatial discretization and under relation factors (table format)

   -  Specification always available
   -  Column 1: List of equations
   -  Column 2: Select discretization scheme for equation #

      -  Available selections

         -  First-order upwind [DEFAULT for all equations]

            -  Sets keyword DISCRETIZE(#) to 0

         -  First-order upwind (dwf)

            -  Sets keyword DISCRETIZE(#) to 1

         -  Superbee

            -  Sets keyword DISCRETIZE(#) to 2

         -  SMART

            -  Sets keyword DISCRETIZE(#) to 3

         -  ULTRA-QUICK

            -  Sets keyword DISCRETIZE(#) to 4

         -  QUICKEST

            -  Sets keyword DISCRETIZE(#) to 5

         -  MUSCL

            -  Sets keyword DISCRETIZE(#) to 6

         -  van Leer

            -  Sets keyword DISCRETIZE(#) to 7

         -  minmod

            - Sets keyword DISCRETIZE(#) to 8

         -  Central

            - Sets keyword DISCRETIZE(#) to 9

-  Column 3: Specify under relation factors

   -  Specification always available
   -  Sets keyword UR_FAC for each equation #
   -  DEFAULTS are equation type specific

      -  1 - gas pressure: 0.8
      -  2 - volume fraction: 0.5
      -  3 - u-momentum: 0.5
      -  4 - v-momentum: 0.5
      -  5 - w-momentum: 0.5
      -  6 - energy: 1.0
      -  7 - species: 1.0
      -  8 - granular energy: 0.5
      -  9 - user-scalar/k-epsilon: 0.8
      -  10 - DES diffusion: 1.0

-  Enable deferred correction

   -  Selection only available if minval(discretize) > 0
   -  Sets keyword DEF_COR
   -  DEFAULT value .FALSE.

-  Enable chi-scheme correction

   -  Selection only available if the species equation spatial
      discretization is SMART or MUSCL (DISCRETIZE(7) = 3 or 6)

      -  Sets keyword CHI_SCHEME
      -  DEFAULT value .FALSE.

Linear Solver Tab
_________________

-  Specify linear solver, number of iterations, and convergence
   tolerance (table format)

   -  Specification always available
   -  Column 1: List of equations
   -  Column 2: Select linear equation solver method for equation #

      -  Available selections

      -  BiCGSTAB [DEFAULT for all equations]

         -  Sets keyword LEQ_METHOD(#) to 2

      -  GMRES

         -  Sets keyword LEQ_METHOD(#) to 3

   -  Column 3: Specify number of iterations

      -  Specification always available
      -  Sets keyword LEQ_IT for each equation #
      -  DEFAULTS are equation type specific

         -  1 - gas pressure: 20
         -  2 - volume fraction: 20
         -  3 - u-momentum: 5
         -  4 - v-momentum: 5
         -  5 - w-momentum: 5
         -  6 - energy: 15
         -  7 - species: 15
         -  8 - granular energy: 15
         -  9 - user-scalar/k-epsilon: 15
         -  10 - DES diffusion: 5

      -  Column 4: Specify convergence tolerance

         -  Specification always available
         -  Sets keyword LEQ_TOL
         -  DEFAULT 1.0E-4 for all equations

Preconditioner Tab
__________________

-  Specify linear solver, number of preconditioner and sweep direction
   (table format)

   -  Specification only available for equations using BiCGSTAB solver
   -  Column 1: List of equations
   -  Column 2: Preconditioner for equation #

      -  Available selections

         -  None

            -  Sets keyword LEQ_PC(#) to 'NONE'

         -  Line Relaxation [DEFAULT for all equations]

            -  Sets keyword LEQ_PC(#) to 'LINE'

         -  Diagonal Scaling o Sets keyword LEQ_PC(#) to 'DIAG'

   -  Column 3: Preconditioner sweep direction for equation #

      - Selection only available for equations with LINE preconditioner

      -  Available selections

         -  'Red-black sweep' [DEFAULT for all equations]

            -  Sets keyword LEQ_SWEEP(#) to 'RSRS'

         -  All sweep

            -  Sets keyword LEQ_SWEEP(#) to 'ASAS'

         -  I-sweep

            -  Sets keyword LEQ_SWEEP(#) to 'ISIS'

         -  J-sweep

            -  Sets keyword LEQ_SWEEP(#) to 'JSJS'

         -  K-sweep

            -  Sets keyword LEQ_SWEEP(#) to 'KSKS'


Advanced Tab
____________

-  Specify maximum inlet velocity factor

   -  Specification always available
   -  Sets keyword MAX_INLET_VEL_FAC
   -  DEFAULT 1.0
   -  Error check: Value greater than or equal to 1.0

-  Specify drag under relation factor

   -  Specification only available with MFIX-TFM and MFIX-Hybrid solvers
   -  Sets keyword UR_F_GS
   -  DEFAULT 1.0
   -  Error check: Value bounded between 0 and 1

-  Specify IA theory conductivity under relation factor

   -  Specification only available with KT_TYPE = 'IA_NONEP'
   -  Sets keyword UR_KTH_SML
   -  DEFAULT 1.0
   -  Error check: value bounded between 0 and 1

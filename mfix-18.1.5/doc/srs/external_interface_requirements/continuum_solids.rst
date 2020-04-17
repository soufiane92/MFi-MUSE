Continuum Solids Model Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

requires TFM solver)

-  Specify void fraction at close pack (required)

   -  Sets keyword EP_STAR

-  Select Viscous Stress Model (KTGS):

   -  Selection is unavailable for constant solids viscosity (MU_S0 defined)
   -  Available selections:

      -  Algebraic Formulation [DEFAULT]

         -  Selection always available
         -  Sets keyword KT_TYPE to ALGEBRAIC

      -  Lun et al, 1984

         -  Selection always available
         -  Sets keyword KT_TYPE to LUN_1984
         -  Requires particle-particle restitution coefficient, C_E
         -  If MMAX>1, requires interphase friction coefficient, C_F

      -  Iddir & Arastoopour, 2005

         -  Selection always available
         -  Sets keyword KT_TYPE to IA_NONEP
         -  Requires particle-particle restitution coefficient, C_E

      -  Simonin, 1996

         -  Selection only available with k-ε turbulence enabled
         -  Sets keyword KT_TYPE to SIMONIN

      -  Cao & Ahmadi, 1995

         -  Selection only available with k-ε turbulence enabled
         -  Sets keyword KT_TYPE to AHMADI

      -  Garzo and Dufty, 1999

         -  Selection only available with MMAX=1 (monodisperse)
         -  Sets keyword KT_TYPE to GD_99
         -  Requires particle-particle restitution coefficient, C_E

      -  Garzo, Tenneti, Subramaniam, Hrenya, 2012

         -  Selection only available with MMAX=1 (monodisperse)
         -  Sets keyword KT_TYPE to GTSH
         -  Requires particle-particle restitution coefficient, C_E

      -  Garzo, Hrenya and Dufty, 2007

         -  Selection not available for MMAX > 2
         -  Selection not available with added mass force
         -  Sets keyword KT_TYPE to GHD
         -  Requires WEN_YU or HYS drag model
         -  Specify coefficient of restitution; R_p (optional)

-  Select Frictional Stress Model

   -  Selection is unavailable for constant solids viscosity (MU_S0 defined)
   -  Available selections

      -  Schaeffer

         -  Selection always available
         -  Sets keyword FRICTION_MODEL to SCHAEFFER
         -  Requires angle of particle-particle friction, PHI

      -  Srivastava and Sundaresan, 2003

         -  Unavailable for Algebraic Formulation viscous stress model
         -  Sets keyword FRICTION_MODEL to SRIVASTAVA
         -  Requires angle of particle-particle friction, PHI
         -  Requires angle of particle-wall friction, PHI_W

      -  None

         -  Selection always available
         -  Sets keyword FRICTION_MODEL to NONE

-  Specify solids volume fraction at onset of friction

   -  Only available with FRICTION_MODEL=SRIVASTAVA
   -  DEFAULT 0.5
   -  Sets keyword EPS_F_MIN

-  Specify particle-particle restitution coefficient

   -  Specification available only when required
   -  Required for MMAX >=2
   -  Required for viscous stress models except GHD and algebraic formulation
   -  Sets keyword C_E

-  Specify interphase friction coefficient

   -  Specification available only when required
   -  Required for MMAX >= 2
   -  Sets keyword C_F

-  Specify angle of particle-particle friction

   -  Specification available only when required
   -  Required for FRICTION_MODEL=SCHAEFFER
   -  Required for FRICTION_MODEL=SRIVASTAVA
   -  Sets keyword PHI

List the following options under an ‘Advanced’ section header.

-  Select radial distribution function

   -  Selection always available
   -  Available selections:

      - Carnahan-Starling

        -  Only available for MMAX==1
        -  Unsets keyword RDF_TYPE

      - Lebowitz

        -  Only available for MMAX >1
        -  Sets keyword RDF_TYPE to LEBOWITZ

      - Mansoori

        -  Only available for MMAX>1
        -  Sets keyword RDF_TYPE to MANSOORI

      - Modified Lebowitz

        -  Only available for MMAX >1
        -  Sets keyword RDF_TYPE to MODIFIED_LEBOWITZ

      - Modified Mansoori

        -  Only available for MMAX>1
        -  Sets keyword RDF_TYPE to MODIFIED_MANSOORI

-  Select stress blending model

   -  Selection only available with FRICTION_MODEL=SCHAEFFER
   -  Available selections:

      - None [DEFAULT]

        -  Selection always available
        -  Sets keyword BLENDING_FUNCTION to NONE

      - Hyperbolic Tangent

        -  Selection always available
        -  Sets keyword BLENDING_FUNCTION to TANH_BLEND

      - Sigmodial

        -  Selection always available
        -  Sets keyword BLENDING_FUNCTION to SIGM_BLEND

-  Specify the segregation slope coefficient

   -  Only available for MMAX > 1 in conjunction with the following
      viscous stress models: algebraic formulation; Lun. 1984; Simonin,
      1996; Ahmadi, 1995
   -  Unavailable with (+other requirements)
   -  Sets keyword SEGREGATION_SLOPE_COEFFICIENT

-  Select maximum packing correlation

   -  Selection only available with FRICTION_MODEL=SCHAEFFER and MMAX
      >1
   -  Available selections:

      - Constant [DEFAULT]

        -  Selection always available
        -  Sets keyword YU_STANDISH to false
        -  Sets keyword FEDORS_LANDEL to false

      - Yu & Standish

        -  Selection always available
        -  Sets keyword YU_STANDISH to true
        -  Sets keyword FEDORS_LANDEL to false

      - Fedors & Landel

        -  Selection only available for MMAX = 2
        -  Sets keyword YU_STANDISH to false
        -  Sets keyword FEDORS_LANDEL to true

-  Specify excluded volume in Boyle-Massoudi stress (optional)

   -  Only available with algebraic formulation of viscous stress model
   -  Sets keyword V_EX

Mockup of Task pane for specifying the continuum solids model
parameters.

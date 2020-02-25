.. _chemical-reactions:

==================================
Setting up Chemical Reaction Cases
==================================

Chemical reactions are specified in the data file (mfix.dat) by providing species
aliases and chemical equations. Rate expressions are specified in one of two user
defined subroutines, ``usr_rates.f`` and ``usr_rates_des.f``, respectively. Heats of
reaction are automatically calculated. Optionally, users may specify constant heats of
reaction in the data file.

.. tip::

    An overview of using legacy ``rrates.f`` files is given at the end of this section.
    However, this input method is no longer supported.

Chemical Reactions Specification

There are five general steps to incorporating chemical reactions into a simulation:

1. Provide species names in the data file (SPECIES_G, SPECIES_S).
2. Assign a unique identifier (alias) to each species in the data file.
   (SPECIES_ALIAS_G and SPECIES_ALIAS_S)
3. Define chemical reaction parameters in the data file.
4. Define chemical reaction rates in ``usr_rates.f`` and/or ``usr_rates_des.f``.
5. Use build_mfixsolver to rebuild the MFIX executable.

.. note::

    Species names must appear exactly as given in the materials database (see
    the Thermochemical Properties section). Species names are typically 18
    characters, and for some species, trailing spaces are needed.

.. note::

    Reactions are limited to homogeneous or two-phase heterogeneous reactions 
    (e.g., species from three separate phases cannot be referenced by any 
    single chemical reaction).

The following explains each step in more detail.

Species Names
-------------

Provide species names in the data file (SPECIES_G, SPECIES_S).


Species Identifiers
-------------------

Each species must be assigned a unique identifier (alias).

Alias formatting restrictions:

 * Aliases must be unique.

 * Aliases are limited to 32 characters and must be a valid Fortran variable
   name (i.e., alphanumeric characters or underscore, and starting with a letter).

 * Aliases are not case sensitive.
 * Aliases cannot conflict with existing MFIX variable names (e.g., a species
   alias of MU_g will cause an error when compiling MFIX).

Reaction Parameters
-------------------

Define chemical reactions in the data file using species aliases.

Each reaction is identified by a reaction construct, and a reaction block is used to
group reaction constructs in the data file. A reaction construct has the format,
rxn_name{…}, where rxn_name is a unique identifier for the reaction. Reaction
identifiers are limited to 32 characters and must follow Fortran variable naming
convention.

Reaction input format:

MFIX processes chemical reaction data differently than other input in
the data file. A reaction block indicates the start and end of the reaction
input. A reaction construct groups a single reaction’s input parameters/.
There are two reaction block types:

 * @(RXNS)…@(END) – indicates continuum phase chemical reactions (all TFM gas
   and solids phase reactions and DEM homogeneous gas phase reactions).

 * @(DES_RXNS)…@(DES_END) – indicates heterogeneous DEM chemical reactions
   (particle/gas).

.. note::

    A data file can only contain one reaction block of each type, whereas a
    reaction block must contain one or more reaction constructs.

The following keywords are available within a reaction construct.

 * **CHEM_EQ**
   The chemical equation, contained in quotes
 * **DH**
   Heat of reaction
 * **fracDH(Phase)**
   Fractional heat of reaction to assign to the indicated phase

Reaction construct formatting notes:

 * Chemical reactions are always specified as irreversible with reactants on the
   left and products on the right. (CHEM_EQ = "Reactants --> Products")
 * An arrow or equals sign can be used to distinguish reactants from products.
   (Reactants --> Products or Reactants = Products)
 * Reversible reactions are specified as two irreversible reactions.
   (see below example, Athermal, gas phase, reversible reaction)
 * Chemical equations may span several lines by including an ampersand (&) at
   the end of the line. As the example below illustrates, each line of the
   chemical equation is contained in quotation marks and the ampersand is
   located to the right of the second quotation mark.

   .. code-block:: none

    @(RXNS)                       !  Begin reaction block
    CH4_Combustion {              !  Reaction 1 construct
    chem_eq = "CH4 + 2O2 --> " &  !  Chemical Reaction Line 1
    "CO2 + 2H2O"                  !  Chemical Reaction Line 2
    }                             !  End reaction 1 construct
    @(END)


 * Chemical equations are limited to 512 characters.
 * Chemical equations can be bound within single or double quotes.
   CHEM_EQ = ‘Reactants = Products’ or "Reactants = Products")
 * Catalytic reactions should contain a species from the catalyst phase in the
   chemical equation with a coefficient of zero. This insures the proper
   assignment of the heat of reaction.
   (CHEM_EQ = ‘A + 0.Cat -->3.0*R’ where Cat is a catalyst phase
   species)
 * Catalyst phase species can be listed as a product, reactant, or both.

Several examples illustrating the data file input (steps 2 and 3) are provided below.
Within the data input file comments are preceded with an exclamation mark (!).

Example: Methane Combustion
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::

  \text{CH}_4 (g) + 2\text{O}_2 → \text{CO}_2 (g) + 2\text{H}_2 \text{O}(g)

.. note::

   Heat of reaction is automatically calculated (default).

.. code-block:: none

    NMAX_g = 4                       ! No. of gas phase species
    Species_g(1) = "CH4 ANHARMONIC " ! Methane
    Species_g(2) = "O2"              ! Oxygen
    Species_g(3) = "CO2"             ! Carbon dioxide
    Species_g(4) = "H2O"             ! Water Vapor

    Species_Alias_g(1)  = "CH4"  ! Methane
    Species_Alias_g(2)  = "O2"   ! Oxygen
    Species_Alias_g(3)  = "CO2"  ! Carbon dioxide
    Species_Alias_g(4)  = "H2O"  ! Water Vapor

    @(RXNS)                              ! Begin reaction block
    CH4_Combustion {                     ! Reaction 1 construct
    chem_eq = "CH4 + 2O2 --> CO2 + 2H2O" ! Chemical Reaction Eq
    }                                    ! End reaction 1 construct
    @(END)                               ! End reaction block

Example: Athermal, gas phase, reversible reaction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::

  \text{A}(g) ↔ \text{R}(g)

Notes:

 * Species database names and aliases are defined on single lines.
 * The forward and backward reactions are defined separately.
 * The heats of reaction are defined as zero (athermal) and explicitly assigned to
   the gas phase.

.. code-block:: none

    NMAX_g = 2
    Species_g(1) = "A" "R"
    Species_Alias_g(1) = "A" "R"

    ! No. of gas phase species
    ! Database names
    ! Species aliases

    @(RXNS)      !  Begin reaction block
    fwd_AtoR {   !  Reaction 1 construct
    chem_eq =    !  Chemical Reaction Eq
    DH = 0.0     !  (cal/moles-reacted)
    fracDH(0)    !  Gas phase HoR
    }            !  End reaction 1 construct
    rvs_AtoR {   !  Reaction 2 construct
    chem_eq =    !  Chemical Reaction Eq
    DH = 0.0     !  (cal/moles-reacted)
    fracDH(0)    !  Gas phase HoR
    }            !  End reaction 2 construct
    @(END)       !  End reaction block

    "A --> R"
    = 1.0

    "R --> A"
    = 1.0

Example: Char combustion
~~~~~~~~~~~~~~~~~~~~~~~~

.. math::

  \text{C}(s) + 0.5 \text{O}_2 (g) → \text{CO}(g)

Notes:

 * Species database names and aliases are defined on single lines.
 * The heat of reaction is defined.
 * The gas phase receives 20% of the heat of reaction.
 * Solids phase 1 receives 80% of the heat of reaction.

.. code-block:: none

    NMAX_g = 2

    ! No. gas phase species

    Species_g(1) = "O2" "CO"
    Species_Alias_g(1) = "O2" "CO"

    ! Database names
    ! Species aliases

    NMAX_s(1) = 2
    Species_s(1,1) = "C(GR) REF ELEMENT"
    Species_s(1,2) = "Coal Ash"

    ! No. solids phase species
    ! Fixed Carbon (graphite)
    ! Coal Ash

    Species_Alias_s(1,1) = "C" "Ash"

    ! Fixed Carbon and Coal Ash

    @(RXNS)                       ! Begin reaction block
    Char_Combustion {             ! Reaction 1 construct
    chem_eq = "C + 0.5O2 --> CO"  ! Chemical Reaction Eq
    DH = -52834.0                 ! (cal/moles-reacted)
    fracDH(0) = 0.2               ! HoR assigned to gas phase
    fracDH(1) = 0.8               ! HoR assigned to s. phase 1
    }                             ! End reaction 1 construct
    @(END)                        ! End reaction block

Example: Compound DEM reaction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CO combustion:

.. math::

  \text{CO}(g) + 0.5\text{O}_2 (g) → \text{CO}_2 (g)

CO2 gasification:

.. math::

  \text{C}(s) + \text{CO}_2 (g) → 2\text{CO}(g)

Char combustion:

.. math::

  \text{C}(s) + 0.5\text{O}_2 (g) → \text{CO}(g)

Notes:

 * Gas phase species names and aliases are defined on the same line.
 * Heats of reaction for all reactions are calculated automatically.
 * A TFM reaction block is used for the gas phase homogeneous reaction.
 * A DEM reaction block is used for gas/solids reactions.
 * Reaction constructs are given in one line.

.. code-block:: none

    ! Gas phase species data
    NMAX_g = 3
    Species_g(1) = "O2"
    Species_Alias_g(1) = "O2"
    Species_g(2) = "CO"
    Species_Alias_g(2) = "CO"
    Species_g(3) = "CO2"
    Species_Alias_g(3) = "CO2"
    ! DES solids phase species data
    NMAX_s(1) = 2
    Species_s(1,1) = "C(GR) REF ELEMENT"
    Species_s(1,2) = "Coal Ash"
    Species_Alias_s(1,1) = "C"
    Species_Alias_s(1,2) = "Ash"
    ! Homogeneous gas phase reactions
    @(RXNS)
    CO_Combustion { chem_eq = "CO + 0.5O2 --> CO2" }
    @(END)
    ! DES Reaction block
    @(DES_RXNS)
    CO2_Gasification { chem_eq = "2.0C + O2 --> 2CO" }
    Char_Combustion { chem_eq = "C + CO2 --> 2CO" }
    @(DES_END)

Additional comments:

* Coal Ash is not a species included in the thermochemical database and
  would require that the properties be given in the data file (see Section 8.14
  Thermochemical properties).
* One-line reaction constructs are only possible when the heat of reaction is
  automatically calculated (i.e., the chemical equation is the only input
  parameter).

Reaction Rates
--------------

Define chemical reaction rates in user defined function (UDF) files.

 * A reaction rate should be given in either ``usr_rates.f`` or ``usr_rates_des.f``
   for each reaction listed in the data file.
 * All TFM gas and solids phase reactions as well as homogeneous gas phase
   reactions for DEM simulations are to be included in ``usr_rates.f``. Reaction
   rates defined in ``usr_rates.f`` must have units of reacted moles per time per
   volume (i.e., moles/sec/cm3 for CGS units and kmoles/sec/m3 for SI units).
 * All discrete phase heterogeneous (particle/gas) reactions are to be included in
   ``usr_rates_des.f`` located in the des subfolder. Reaction rates defined in
   ``usr_rates_des.f`` must have units of reacted moles per time (i.e., moles/sec).

.. note::

   Formation and consumption rates are automatically calculated for each
   species from the reaction rate and chemical equation.

The rate in terms of reacted moles is related to the rates of formation and
consumption through the stoichiometric coefficients. For example, consider
homogeneous gas phase reaction of methane combustion:

.. math::

  \text{CH}_4 + 2\text{O}_2 → \text{CO}_2 + 2\text{H}_2 O

The rate in terms of reacted moles is related to the rates of formation and
consumption as

.. math::

  rate &= \frac{-r_{CH_4}}{1} (\frac{kmol_{CH_4} / (s \cdot m^3)}{mol_{CH_4}})
       = \frac{-r_{O_2}}{2} (\frac{kmol_{O_2} / (s \cdot m^3)}{mol_{O_2}}) \\
       &= \frac{r_{CO_2}}{1} (\frac{kmol_{CO_2} / (s \cdot m^3)}{mol_{CO_2}})
       = \frac{r_{H_{2}O}}{2} (\frac{kmol_{H_{2}O} / (s \cdot m^3)}{mol_{H_{2}O}})

where :math:`-r_{CH_4}` and :math:`-r_{O_2}` are the rates of consumption of methane and oxygen, and
:math:`r_{CO_2}` and :math:`r_{H_{2}O}` are the rates of formation of carbon dioxide and water vapor,
respectively.

Each reaction rate is assigned to the variable ``RATES(rxn_name)``, where
``rxn_name`` is the reaction identifier used in the reaction construct. To minimize input
errors when specifying reaction rates, species aliases (``SPECIES_ALIAS``) defined in
the data file should be used in lieu of the associated species index.

For example, if oxygen is defined as gas phase species 2 with an alias of ``O2``, (e.g.,
``SPECIES_ALIAS_g(2)="O2"``), when accessing gas phase species data for
oxygen (e.g., molecular weight; MW_g), "O2" should be used and not the integer
index 2, (e.g, ``MW_g(O2)``).

Examples illustrating components of the UDF (step 4) are provided below.

Example: Methane Combustion with UDF
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::

  \text{CH}_4 (g) + 2\text{O}_2 → \text{CO}_2 (g) + 2\text{H}_2 \text{O}(g)

Notes:

• Species database names and alias are defined on the same line.
• The fluid cell index (``IJK``) is passed as a dummy argument.
• Global field variables are referenced (RO_g, X_g, T_g, and EP_g )
• Species aliases (O2 and CH4) are used instead of the species indices.
• Reaction identifier (CH4_Combustion) is used in the rates array.
• Reaction rate is stored for post processing (see below).

.. code-block:: none

    ###  mfix.dat:
    NMAX_g = 4
    Species_g(1) = "CH4 ANHARMONIC"
    Species_g(2) = "O2"
    Species_g(3) = "CO2"
    Species_g(4) = "H2O"

    Species_Alias_g(1) = "CH4"
    Species_Alias_g(2) = "O2"
    Species_Alias_g(3) = "CO2"
    Species_Alias_g(4) = "H2O"

    @(RXNS)
    CH4_Combustion { chem_eq = "CH4 + 2O2 --> CO2 + 2H2O" }
    @(END)

.. code-block:: fortran

    ### usr_rates.f:
    SUBROUTINE USR_RATES(IJK, RATES)

    DOUBLE PRECISION, INTENT(IN) :: IJK ! Fluid Cell Index
    DOUBLE PRECISION, INTENT(OUT) :: RATES(:) ! Reaction Rates
    DOUBLE PRECISION c_02 ! Oxygen concentration (mol/cm^3)
    DOUBLE PRECISION c_CH4 ! Methane concentration (mol/cm^3)

    ! Calculate species concentrations:
    c_O2 = (RO_g(IJK) * X_g(IJK,O2))/MW_g(O2)
    c_CH4 = (RO_g(IJK) * X_g(IJK,CH4))/MW_g(CH4)

    ! Methane Combustion
    !-----------------------------------------------------------------//
    RATES(CH4_Combustion) = 6.7d12 * exp(-2.4358d4/T_g(IJK)) * &
    EP_g(IJK) * (c_O2**1.3) * (c_CH4**0.2)

    ! Store the reaction rate for output/post processing.
    IF(CH4_Combustion <= NRR) &
    ReactionRates(IJK, CH4_Combustion) = RATES(CH4_Combustion)

    END SUBROUTINE USR_RATES

Example: Athermal, gas phase, reversible reaction with UDF
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::

  A(g) ↔ R(g)

Notes:
 * Species database names and alias are defined on the same line.
 * The fluid cell index (``IJK``) is passed as a dummy argument.
 * Global field variables are referenced (RO_g, X_g, T_g, and EP_g )

.. code-block:: none

    ### mfix.dat:
    NMAX_g = 2                     ! No. of gas phase species
    Species_g(1) = "A" "R"         ! Database names
    Species_Alias_g(1) = "A" "R"   ! Species Aliases

    @(RXNS)    !  Begin reaction block
    fwd_AtoR { !  Reaction 1 construct
    chem_eq =  !  Chemical Reaction Eq
    DH = 0.0   !  (cal/moles-reacted)
    fracDH(0)  !  Gas phase HoR
    }          !  End reaction 1 construct
    rvs_AtoR { !  Reaction 2 construct
    chem_eq =  !  Chemical Reaction Eq
    DH = 0.0   !  (cal/moles-reacted)
    fracDH(0)  !  Gas phase HoR
    }          !  End reaction 2 construct
    @(END)     !  End reaction block

    "A --> R"
    = 1.0

    "R --> A"
    = 1.0

    usr_rates.f:
    SUBROUTINE USR_RATES(IJK, RATES)
    DOUBLE PRECISION, INTENT(IN) :: IJK

    ! Fluid Cell Index
    DOUBLE PRECISION, INTENT(OUT) :: RATES(:) ! Reaction Rates
    DOUBLE PRECISION c_A ! species A concentration (mol/cm^3)
    DOUBLE PRECISION c_R ! species R concentration (mol/cm^3)

    ! Calculate species concentrations:
    c_A = (RO_g(IJK) * X_g(IJK,A))/MW_g(A)
    c_R = (RO_g(IJK) * X_g(IJK,R))/MW_g(R)

    ! Forward Reaction: A --> R (reacted moles/sec.cm^3)
    !-------------------------------------------------------//
    RATES(fwd_AtoR) = 1.2d17 * exp(-5.837d3/T_g(IJK)) * &
    EP_g(IJK) * c_A

    ! Reverse Reaction: R --> A (reacted moles/sec.cm^3)
    !-------------------------------------------------------//
    RATES(rvs_AtoR) = 2.5d41 * exp(-1.4897d4/T_g(IJK)) * &
    EP_g(IJK) * c_R

    END SUBROUTINE USR_RATES

Example: Char combustion with UDF
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::

  \text{C}(s) + 0.5\text{O}_2 (g) → \text{CO}(g)

Notes:

 * The fluid cell index (``IJK``) is passed as a dummy argument.
 * Algebraic expressions for the rate limiting steps are omitted for brevity.

.. code-block:: none

    ###mfix.dat: see step 3.
    ###usr_rates.f:
    SUBROUTINE USR_RATES(IJK, RATES)
    DOUBLE PRECISION, INTENT(IN) :: IJK
    ! Fluid Cell Index
    DOUBLE PRECISION, INTENT(OUT) :: RATES(:) ! Reaction Rates
    ⋮
    ! Rate limiting steps:
    DOUBLE PRECISION k_f ! film diffusion (cm/sec)
    DOUBLE PRECISION k_a ! ash layer diffusions (cm/sec)
    DOUBLE PRECISION k_s ! chemical kinetics (cm/sec)
    DOUBLE PRECISION k_eff ! effective rate (cm/sec)

    ! Total surface area of solids phase 1 in IJK
    DOUBLE PRECISION Sa ! (cm^2/cm^3)

    ! C + 0.5O2 --> CO (reacted moles/sec.cm^3)
    !-------------------------------------------------------//
    ! Verify that solids are present
    IF(.NOT.COMPARE(EP_g(IJK),ONE)) THEN
      ! Calculate film diffusion rate
      k_f = < film diffusion rate expression >
      ! (cm/sec)
      ! Calculate ash diffusion rate
      k_a = < ash diffusion rate expression >
      ! (cm/sec)
      ! Calculate kinetic rate
      k_s = < kinetic rate expression >
      ! (cm/sec)
      ! Effective rate (cm/sec)
      k_eff = ONE/(ONE/k_a + ONE/k_f + ONE/k_s)
      ! Calculate total surface area of solids phase 1
      Sa = 6.0 * EP_s(IJK,1) / D_p0(1)
      ! Calculate the reaction rate.
      RATES(Char_Combustion) = 2.0 *(Sa * k_eff * Conc(O2))
    ELSE
      ! No solids --> No reaction
      RATES(Char_Combustion) = ZERO
    ENDIF

    END SUBROUTINE USR_RATES

See legacy_tutorials/SpoutedBedCombustor for details on a similar simulation
setup.

Example: DES droplet evaporation with UDF
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. math::

  \text{H}_2 \text{O}(l) → \text{H}_2 \text{O}(g)

Notes:

 * Various algebraic expressions in the sample UDF are omitted for brevity.
 * The global particle index (``NP``), phase index (``pM``), and fluid cell index (``IJK``)
   are passed as dummy arguments.

.. code-block:: none

    ### mfix.dat:
    NMAX_g = 2                          ! No. of gas phase species
    Species_g(1) = "Air" "H2O"          ! Database names
    Species_Alias_g(1) = "Air" "Vapor"  ! Species Aliases

    NMAX_s(1) = 1                    ! No. of solids phase species
    Species_s(1,1) = "H2O(L)"        ! Database names
    Species_Alias_s(1,1) = "Liquid"  ! Species Aliases

    @(DES_RXNS)
    Evap { Liquid --> Vapor }
    @(DES_END)

    ###usr_rates_des.f:
    SUBROUTINE USR_RATES_DES(NP, pM, IJK, DES_RATES)
    DOUBLE PRECISION, INTENT(IN) :: NP  ! Global particle index
    DOUBLE PRECISION, INTENT(IN) :: pM  ! Particle solid phase
    DOUBLE PRECISION, INTENT(IN) :: IJK ! Fluid Cell Index
    DOUBLE PRECISION, INTENT(OUT) :: DES_RATES(:) ! Reaction Rates
    !-------------------------------------------------------//
    ! Calculate the concentration gradient (mole/cm^3)
    Cmg_H2O = < expression for calculating gradient >

    IF(Cmg_H2O > ZERO) THEN
      ! Calculate mass transfer coefficient (cm/sec)
      H2O_xfr = < mass transfer coeff calculation >
      ! Calculate droplet surface area (cm^3)
      Sa = Pi * 4.0d0 * (DES_RADIUS(NP)**2)
      ! Calculate the mass transfer rate (moles/sec)
      DES_RATES(Evap) = Sa * H2O_xfr * Cmg_H2O
    ENDIF

    ! Store the reaction rate for post processing.
    IF(Evap <= NRR) ReactionRates(Evap) = &
      ReactionRates(IJK, Evap) + DES_RATES(Evap)

    END SUBROUTINE USR_RATES_DES

See legacy_tests/dem-tests/evaporation for additional details.

Build Custom Solver
-------------------

Use ``build_mfixsolver`` to rebuild the MFIX executable.

See :ref:`customsolver` for detailed instructions on building the custom MFIX solver.

Rebuilding mfixsolver is required after making any of the following modifications:

 * Changing the number, order, or alias of any species in the data file.
 * Changing the number, order, or name of any chemical reaction in the data
   file.
 * Changing the chemical reaction rates in either ``usr_rates.f`` or
   ``usr_rates_des.f``.

.. note::

    ``build_mfixsolver`` preprocesses the data file to generate the ``species.inc`` file
    which is included within the ``usr_rates.f`` and ``usr_rates_des.f`` files
    as code. Therefore changes in the data file may result in the executable
    being out of date.

Extra Notes
-----------

Below is additional reaction information.

Write out reaction rates to SPx file:

1. In the data file, mfix.dat, set NRR to the desired number of reaction rates
to be written out to the file ``*.SPA``. This number is typically less than or equal
to the total number of reactions.
2. In a reaction UDF (``usr_rates.f`` or ``usr_rates_des.f``) assign the
desired reaction information to the variable ReactionRates.
ReactionRates is a two-dimensional array. The first index references the
fluid cell, IJK, while the second index ranges from 1 to NRR.

.. note::

    If the second index exceeds NRR, a run time error can result from over
    indexing the array. Using logical checks can eliminate potential errors!

Two of the above examples illustrate using the ReactionRates variable:

1. Methane Combustion: The calculated reaction rate is directly stored and a
   logical check is used to prevent over indexing of the ReactionRates array.
2. DES droplet evaporation: The calculated reaction rate is added to the storage
   array. Adding the calculated data to the storage variable is needed in DES
   since several discrete particles may exist in a single fluid cell. Again, a logical
   check is preformed to prevent over indexing the array.

Use an existing (legacy) ``rrates.f`` file:

The legacy rrates.f file should be copied to the run directory. Additionally, the
following keywords should be specified in the data file:

  * :ref:`USE_RRATES`
  * :ref:`SPECIES_NAME`
  * :ref:`NMAX`

.. note::

    Legacy species keywords, NMAX(m) and SPECIES_NAME(n), are required
    when using a legacy rrates.f file. Current species keywords NMAX_g,
    NMAX_s, SPECIES_g, and SPECIES_s cannot be used.

.. note::

    The only modification needed for a legacy mfix.dat and rrates.f file
    combination is the inclusion of USE_RRATES=.TRUE. in the data file.
    An example of legacy file usage: legacy_tutorials/reactor1b

Additional remarks:

 * Building with chemical reaction support requires that the data file, mfix.dat, be
   present in the run directory as the species aliases and reaction identifiers are
   needed to construct a species.inc file.
 * Species aliases and reaction identifiers must be unique. The build performs a
   cursory check on the supplied data and exits if non unique entries are identified.
 * If any species alias or reaction identifier conflicts with an existing global variable
   in MFIX, an error will be reported and the code will fail to compile.

Stiff Chemistry Solver
----------------------

A stiff chemistry solver has been fully integrated into MFIX. This approach
first solves the convection/diffusion equations without chemical reaction source
terms. A coupled set of ODEs is then directly integrated to impose chemical
reactions effects. This approach may decrease simulation time by permitting
larger time steps within the convection/diffusion model. However, the stiff
chemistry solver may increase simulation time, especially if reactions are not
stiff. Reactions are specified using the same approach outlined in the
chemical reactions section.

The stiff chemistry solver is invoked by specifying the keywords :ref:`STIFF_CHEMISTRY` and :ref:`STIFF_CHEM_MAX_STEPS`

.. note::

    The stiff chemistry solver does not support legacy rrates.f files

.. note::

    The stiff chemistry solver is not available with DES simulations.

Additional remarks:

 * Variables governing ODE convergence criteria are specified as parameters in
   ``stiff_chem_mod.f`` found in the ``model/chem`` directory. Additional information
   on these parameters and their usage is available in ``model/ODEPACK.F``.
 * Running your simulation in debug mode is recommended for the first time. This will catch some
   common programmatic errors in the ``usr_rates.f`` file. Additionally, the stiff
   chemistry solver checks for NaNs calculated in the ``usr_rates.f`` file.
 * The tutorial located in ``tutorials/Silane_Pyrolysis`` shows how to use
   the stiff chemistry solver.

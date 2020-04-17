Thermochemical Properties
=========================

The directory ``model/thermochemical`` contains the database of Burcat and
Ruscic (2005) and routines for reading the database. With linkage to this
database the users need not manually enter data for molecular weight, specific
heat, and heats of reactions. Instead the users only need to enter the names of
the species (keyword SPECIES_g and SPECIES_s) in the data file. If such
information is already provided in either the data file or a BURCAT.THR file in
the run directory then MFIX will not reference the database. That is, MFIX reads
the necessary thermo-chemical data from files in the following order:

1. ``mfix.dat``
2. ``BURCAT.THR`` file in the run directory
3. ``model/thermochemical/BURCAT.THR``

The species names are case sensitive and should match the names in
``BURCAT.THR`` exactly; alternatively aliases can be defined for common species,
such as O2, in read_therm.f. See ``tests/thermo`` for a sample case that
accesses the database. The format of ``BURCAT.THR`` file resembles CHEMKIN
format, but with several notable differences. To include thermochemical data in
the mfix.dat file then this information must start below a line that starts with
THERMO DATA.

Example dataset from BURCAT.THR with notations:

.. figure:: /media/burcat.png

Each entry in the database starts with a unique CAS identifier (74-82-8) for the
species, followed by several lines of comments highlighted in green. The data
section starts with the species name in columns 1-18 (CH4 RRHO). Common species
names may be followed by strings (RRHO) that identify the method used to
determine the coefficients. Additional information follows the species name. The
numbers toward the end of the line are the temperature limits (200.000 6000.000)
in degrees Kelvin where the property calculation is valid and the molecular
weight (16.04246). Unlike CHEMKIN the common temperature for the high and low
temperature branches are not recorded; it is always 1000 K. The next three lines
give the fourteen coefficients (seven coefficients each for the high and low
temperature branches) and the formation enthalpy at 298 K (which is also not
included in CHEMKIN format). All the coefficients and the enthalpy of formation
are normalized with the gas constant R (cal/mol/K). The low temperature
coefficients (:math:`a_L`) should be used for temperatures in the range Tlow to 1000K
and the high temperature coefficients (:math:`a_H`) should be used for temperatures in
the range 1000K to Thigh. The coefficients are stored in a fixed format (E15.0)
as follows:

.. table:: Coefficients

  ============= ============= ============= ============= ==========================
  :math:`a^1_H` :math:`a^2_H` :math:`a^3_H` :math:`a^4_H` :math:`a^4_H`
  :math:`a^6_H` :math:`a^7_H` :math:`a^1_L` :math:`a^2_L` :math:`a^3_L`
  :math:`a^4_L` :math:`a^4_L` :math:`a^4_L` :math:`a^4_L` :math:`\Delta H^{\circ}_f`
  ============= ============= ============= ============= ==========================


where :math:`\Delta H^{\circ}_f` is the formation enthalpy at 298K.


The normalized specific heat is given by

.. math::

  \frac{C_p}{R} = a_1 + a_2 T + a_3 T^2 + a_4 T^3 + a_5 T^4 .

Additional database comments:

 * A number of species in the database have a lower temperature limit of 300K which
   is 2 degrees above the reference temperature (298 K) used for formation enthalpy
   calculation. For those species MFIX relaxes the lower limit for Cp calculations
   to 298 K to enable heat of reaction calculation. see read_database.f

 * The database reader is set up such that the database is read only if necessary.

 * For additional details see the Burcat and Ruscic (2005) report located in the
   thermo-chemical subdirectory, ``model/thermochemical/intro.pdf``. If you
   include thermochemical properties in mfix.dat all keywords defined below the
   line that starts with THERMO DATA will be ignored!

.. _user-defined-functions:

======================
User-Defined Functions
======================

User-Defined Functions (UDFs) are source files in the same directory as the
project file that define code that is called by specific locations in the main
MFiX code. UDFs are used to change the behavior of MFiX or for writing
custom output files. Using UDFs is recommended instead of using post_mfix.

Users may modify any ``*.f`` or ``*.inc`` file under the MFIX model directory. To modify
a file, first copy it from the model directory into the run directory. All
modifications should be made to the file in the run directory. For example, list
the contents of the ``adiabaticFlame`` test case located in the ``legacy_tests``
directory:

.. code-block:: shell

    > cd tutorials/Silane_Pyrolysis
    > ls
    SP2D.mfx    usr0.f      usr1.f      usr_mod.f   usr_rates.f

The ``SP2D.mfx`` file is the project file, and the ``usr*.f`` files are the UDFs used by this project.
simulation. Running ``build_mfixsolver``:

.. code-block:: shell

    > build_mfixsolver
    ...long output...
    > ls
    Makefile    build       mfixsolver  usr.mod     usr0.f      usr1.d      usr1.o      usr_mod.f   usr_rates.d usr_rates.o
    SP2D.mfx    lib         species.inc usr0.d      usr0.o      usr1.f      usr_mod.d   usr_mod.o   usr_rates.f

Each UDF has a corresponding dependency (``.d``) file and object (``.o``) file.

If a project has a ``usr_rates.f`` UDF, the ``species.inc`` file is generated from
the project file during the reaction preprocessing step of the build.


The ``.d`` and ``.o`` files are intermediate dependency and object files that are
created and linked with the executable, mfix.

The following is a list of MFIX files that are usually modified to include user
defined scalars, user defined drag models, and chemical reactions, physical
properties, and source terms for the governing equations:

+-----------------------+-------------------------------------------------------------+
| File Name:            |   Usage Description                                         |
+-----------------------+-------------------------------------------------------------+
| ``scalar_prop.f``     |  Properties and source terms in scalar transport equations  |
+-----------------------+-------------------------------------------------------------+
|  ``usr_drag.f``       |  User-defined drag function                                 |
+-----------------------+-------------------------------------------------------------+
|  ``usr_rates.f``      | Chemical reaction rates                                     |
+-----------------------+-------------------------------------------------------------+
|  ``usr_rates_des.f``  | DES chemical reaction rates                                 |
+-----------------------+-------------------------------------------------------------+
|  ``usr_properties.f`` | Physical properties (density, specific heat, etc.)          |
+-----------------------+-------------------------------------------------------------+
|  ``usr_sources.f``    | Governing equation source terms                             |
+-----------------------+-------------------------------------------------------------+

The following routines are used for writing user-defined output:

+------------------+-------------------------------------------------------------+
| File Name:       |  Usage Description                                          |
+------------------+-------------------------------------------------------------+
| ``write_usr0.f`` | Called once at the start of a run. This file is             |
|                  |     used for opening user-defined output files.             |
+------------------+-------------------------------------------------------------+
| ``write_usr1.f`` | Called at intervals defined by USR_DT                       |
+------------------+-------------------------------------------------------------+

To activate the calls to the following three routines, set ``call_usr = .TRUE.`` in the input file
``mfix.dat``:

+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| File Name:              | Usage Description                                                                                                             |
+=========================+===============================================================================================================================+
| ``usr0.f``              | Subroutine that is called once every run, just before the time-loop begins.                                                   |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| ``usr1.f``              | Subroutine that is called once every timestep.                                                                                |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| ``usr2.f``              | Subroutine that is called once every iteration.                                                                               |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| ``usr3.f``              | Subroutine that is called once every run, after the time-loop ends.                                                           |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| ``usrnlst.inc``         | List of user-defined keywords. These may be used to enter data through the input data file mfix.dat.                          |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| ``usr_init_namelist.f`` | Initialize user-defined keywords.                                                                                             |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| ``usr_mod.f``           | User-defined module. Include ``USE usr`` to use userdefined variables in this module.                                         |
|                         | If allocatable arrays are defined in this module, allocate them in usr0.f. [1]                                                |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| ``usr0_des.f``          | Subroutine called before entering the DES time loop. [1]                                                                      |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| ``usr1_des.f``          | Subroutine called every DEM timestep after calculating DES source terms but before source terms are applied to the particles. |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| ``usr2_des.f``          | Subroutine called every DES timestep after source terms are applied to the particles.                                         |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+
| ``usr3_des.f``          | Subroutine that is called after completing the DES time loop.                                                                 |
+-------------------------+-------------------------------------------------------------------------------------------------------------------------------+

User-defined subroutines call structure:

.. figure:: /media/udf_callgraph.png
   :align: center
   :alt: UDF call graph

   Call Graph for UDF Subroutines

DES User-defined subroutines call structure:

.. figure:: /media/udf_des_callgraph.png
   :align: center
   :alt: UDF DES call graph

   Call Graph for UDF DES Subroutines

The ``USR_`` keywords do not have any explicit behavior. They are only basic
input mechanisms to interact with user-defined functions. For example,
specifying ``USR_X_w`` does not result in the associated ``I`` index, ``USR_I_W``
being calculated. It is upon the user to ensure that all user-hooks are fully
specified.

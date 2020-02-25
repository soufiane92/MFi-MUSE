.. _running_mfix_on_joule:

=====================
Running MFiX on Joule
=====================

This section is dedicated to MFiX users who want to use MFiX on NETL's HPC system Joule.
Specific modules have been created on Joule so that regular installation is not required.
Both usages through the GUI and from source are covered in the following sections.

.. _launch_gui_on_joule:

-----------------
Launching the GUI
-----------------

1.  Load the MFiX 18.1 module:

    .. code-block:: bash

        > module load mfix/18.1

2.  Optional: The ``mfix/18.1`` module loads modules ``gnu/6.4.0`` and
    ``openmpi/1.10.2_gnu6.4``. To build a custom solver with a different
    compiler, load that compiler module after ``mfix/18.1``:

    .. code-block:: bash

        > module load mfix/18.1
        > module load intel/2017.2.050
        > module load intelmpi/5.1.3.210

    .. note::
      * For GFortran, use versions 4.8 to 6.4
      * For Intel, use module ``intel/2017.2.050``

3)  Launch the GUI:

    .. code-block:: bash

        > vglrun mfix



---------------------------
Building Solver from Source
---------------------------

In this section, it is assumed the user has advanced knowledge on
how to setup an MFiX model through a text file (.mfx extension)
and run the solver on Joule. This section only covers how to build
the solver.

1)  Obtain the source code from the `MFiX download page <https://mfix.netl.doe.gov/mfix/download-mfix>`__

    Navigate to the location where you saved the tarball, and extract it.
    Here, we assume the tarball was saved into the user's home directory:

    .. code-block:: bash

        > tar xf mfix-18.1.0.tar.gz
        > cd mfix-18.1.0
        > pwd
        /home/user/mfix-18.1.0

    For the rest of this document the example directory ``/home/user/mfix-18.1.0``
    will be referred to as ``$MFIX_SRC``.

2)  Create a project

    Create a project directory.

    Save and edit the following file(s) in the project directory:

      - ``.mfx`` file (required)
      - ``geometry.stl`` (if needed)
      - ``particle_input.dat`` (if needed)
      - UDFs (if needed)

    As an example, we will copy an existing tutorial:

    .. code-block:: bash

        > cp ..   # go back to home directory
        > cp -r $MFIX_SRC/tutorials/Silane_Pyrolysis .
        > cd Silane_Pyrolysis
        > ls
        /

3)  Load cmake and Fortran compilers for serial and dmp compilation (if needed):

    To use gnu compiler:

    .. code-block:: bash

        > module load vgl cmake/3.11.0 boost/1.66.0 gnu/6.4.0 openmpi/1.10.7_gnu6.4 git/2.16.2


    To use the Intel Fortran compiler:

    .. code-block:: bash

        > module load vgl cmake/3.11.0 boost/1.66.0 gnu/6.4.0 openmpi/1.10.7_gnu6.4 git/2.16.2 intel/2016.3.067 intelmpi/5.1.3.210

    .. note::
      * For GFortran, use versions 4.8 to 6.4
      * For Intel, use module ``intel/2017.2.050``

4)  Build the solver

    This section is similar to the :ref:`batch_solver` section.

    Building the solver requires to first run ``cmake`` to configure MFiX and generate a Makefile, then run ``make`` to build the MFiX solver.

    Running ``cmake`` will generate a ``CMakeCache.txt`` file in the directory it is

    When running ``cmake``, the path to the MFiX source tree needs to be specified. It can be an
    absolute or relative path.

.. note::
   Running ``cmake`` will generate a ``CMakeCache.txt`` file in the directory it is invoked from.
   When changing configuration, it is recommended to delete the ``CMakeCache.txt`` before configuring the next build.

----------------------
Example Configurations
----------------------

Below are a few examples with gnu compiler (assuming gnu modules in step 3 above are loaded):

*   Build the optimized serial solver (short form):

    .. code-block:: bash

      > cmake $MFIX_SRC    # by default, the serial optimized solver is build
                            # when no flags are specified
                            # and there is no CMakeCache.txt
      > make -j

*   Build the optimized serial solver (long form):

    .. code-block:: bash

      > cmake $MFIX_SRC -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_Fortran_FLAGS="-O2 -g"
      > make -j

*   Build the serial solver in debug mode (short form):

    .. code-block:: bash

      > cmake $MFIX_SRC -DCMAKE_BUILD_TYPE=Debug
      > make -j

*   Build the serial solver in debug mode (long form):

    .. code-block:: bash

      > cmake $MFIX_SRC -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_Fortran_FLAGS="-O0 -g -Wall -fcheck=all"
      > make -j


*   Build the optimized solver with DMP (short form):

    .. code-block:: bash

      > cmake $MFIX_SRC -DENABLE_MPI=1 -DMPI_Fortran_COMPILER=mpifort
      > make -j

*   Build the optimized solver with DMP (long form):

    .. code-block:: bash

      > cmake $MFIX_SRC -DENABLE_MPI=1 -DMPI_Fortran_COMPILER=mpifort -DCMAKE_Fortran_FLAGS="-O2 -g"
      > make -j


Below are a few examples with the Intel Fortran compiler (assuming intel modules
in step 3 above are loaded):

*   Build the optimized serial solver (short form):

    .. code-block:: bash

      > cmake $MFIX_SRC    # by default, the serial optimized solver is built
                            # when no flags are specified
                            # and there is no CMakeCache.txt
      > make -j

*   Build the optimized serial solver (long form):

    .. code-block:: bash

      > cmake $MFIX_SRC -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_Fortran_FLAGS="-O2 -g"
      > make -j

*   Build the serial solver in debug mode (short form):

    .. code-block:: bash

      > cmake $MFIX_SRC -DCMAKE_BUILD_TYPE=Debug
      > make -j

*   Build the serial solver in debug mode (long form):

    .. code-block:: bash

      > cmake $MFIX_SRC -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_Fortran_FLAGS="-O0 -g"
      > make -j


*   Build the optimized solver with DMP (short form):

    .. code-block:: bash

      > cmake $MFIX_SRC -DENABLE_MPI=1 -DMPI_Fortran_COMPILER=mpiifort
      > make -j

*   Build the optimized solver with DMP (long form):

    .. code-block:: bash

      > cmake $MFIX_SRC -DENABLE_MPI=1 -DMPI_Fortran_COMPILER=mpiifort -DCMAKE_Fortran_FLAGS="-O2 -g"
      > make -j

.. note::
   For DMP support, defining ``-DENABLE_MPI=1`` is required. Defining
   ``CMAKE_Fortran_COMPILER`` as your MPI wrapper (e.g. ``mpif90``,  ``mpifort``, ``mpiifort``) is recommended,
   but not strictly required. CMake should automatically detect MPI include
   files and libraries for your default compiler, but specifying
   ``CMAKE_Fortran_COMPILER`` is better to ensure you are using the exact
   compiler you intend to use.

.. note::

   CMake uses configurations values from both ``CMakeCache.txt`` and from
   command line arguments. Command line arguments take precedence over
   ``CMakeCache.txt``, but ``CMakeCache.txt`` is used if nothing is specified on
   the command line. For instance, if you run ``cmake ..
   -DCMAKE_Fortran_FLAGS="-fcheck=all"``, and then run ``cmake ..`` again (in
   the same build directory), ``-fcheck=all`` will still be used (because it is
   still in ``CMakeCache.txt``). If this is not what you want, either edit
   ``CMakeCache.txt``, or just delete ``CMakeCache.txt`` and run ``cmake`` again
   with different options.

.. _developers:

==================================
Build from Source (for Developers)
==================================

.. _building:

---------------------
Obtaining MFiX Source
---------------------

This appendix describes how developers build MFiX packages for
distribution. Most users will not need to do this.

Building from source requires the MFiX source tarball, which can be obtained on
the `MFiX download page <https://mfix.netl.doe.gov/mfix/download-mfix>`__.
Extract the tarball and go into the top level source directory:

.. code-block:: bash

    > tar xf mfix-18.1.0.tar.gz
    > cd mfix-18.1.0
    > pwd
    /home/user/mfix-18.1.0

---------------------------
Building Solver from Source
---------------------------

You can build the :ref:`batch_solver` from source, without the MFiX conda
package installed. This is how MFiX was built in version 2016 and earlier.

For prerequisites, see :ref:`install-build-dependencies`.

Run ``cmake`` to configure MFiX and generate a Makefile, then run ``make`` to build the MFiX solver.

Running ``cmake`` will generate a ``CMakeCache.txt`` file in the directory it is
invoked from. Please include ``CMakeCache.txt`` when emailing the mailing list for
support with build issues.

^^^^^^^^^^^
Configuring
^^^^^^^^^^^

The default configuration for MFiX is serial with optimizations and debug symbols (standard optimization; for GFortran uses flags ``-g -O2``):

.. code-block:: bash

  > pwd
  /home/user/mfix-18.1.0   # actual working directory will vary
  > mkdir release-build
  > cd release-build
  > cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo   # .. is the relative path to the top-level MFiX source directory
  > cmake .. # same thing (RelWithDebInfo is the default CMAKE_BUILD_TYPE)

.. note::
   Using an "out of source" build is recommended. Create a new directory, ``cd``
   to that directory, and run ``cmake <path>`` where ``<path>`` is the top-level
   MFiX source directory (containing ``CMakeLists.txt``). Out of source builds
   are convenient for having different solvers built with the different
   configurations. The build directory can be whatever you want;
   ``release-build`` and ``debug-build`` used here are just examples.

To configure the MFiX solver in serial with debug mode (no optimization; for GFortran uses flags ``-g -O0``):

.. code-block:: bash

   > mkdir debug-build
   > cd debug-build
   > cmake .. -DCMAKE_BUILD_TYPE=Debug

If ``mpifort`` is your MPI compiler wrapper command, then to configure the MFiX solver with DMP with optimizations and debug symbols:

.. code-block:: bash

   > mkdir release-build
   > cd release-build
   > cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo -DENABLE_MPI=1 -DMPI_Fortran_COMPILER=mpifort
   > cmake .. # same thing (RelWithDebInfo is the default CMAKE_BUILD_TYPE)

If ``mpifort`` is your MPI compiler wrapper command, then to configure the MFiX solver with DMP with debug mode:

.. code-block:: bash

   > mkdir debug-build
   > cd debug-build
   > cmake .. -DCMAKE_BUILD_TYPE=Debug -DENABLE_MPI=1 -DMPI_Fortran_COMPILER=mpifort

To specify the Fortran compiler, define CMAKE_Fortran_COMPILER:

.. code-block:: bash

   > cmake .. -DCMAKE_Fortran_COMPILER=ifort  # for example

To add specific Fortran compiler flags, define CMAKE_Fortran_FLAGS:

.. code-block:: bash

   > cmake .. -DCMAKE_Fortran_FLAGS="-O0 -g -Wall -fcheck=all"  # for example

To configure with SMP support:

.. code-block:: bash

   > cmake .. -ENABLE_OpenMP=1

Options can be combined in a single command:

.. code-block:: bash

   > cmake .. -ENABLE_OpenMP=1 -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_Fortran_FLAGS="-O0 -g -Wall -fcheck=all"


.. note::
   For DMP support, defining ``-DENABLE_MPI=1`` is required. Defining
   ``CMAKE_Fortran_COMPILER`` as your MPI wrapper (``mpifort``) is recommended,
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

^^^^^^^^
Building
^^^^^^^^

After configuring, build with make. For further details see ``man make``.

.. code-block:: bash

   > make
   > make -j        # for parallel build jobs
   > make VERBOSE=1 # to view the full command lines used for compiling and linking


At the end of a successful build, the MFiX solver will be located in the current directory.
The solver is called ``mfixsolver`` on Linux.

----------------------
Building Conda package
----------------------

See https://conda.io/docs for documentation on Conda.

To build the conda package of MFiX, you will need to install
:ref:`install-build-dependencies` for your platform.

Install ``conda-build``:

   .. code-block:: bash

       > conda install conda-build

The conda package recipes are under ``build-aux/conda``. Refer to the
`conda-build documentation
<https://conda.io/docs/user-guide/tasks/build-packages/recipe.html>`_ for
details.

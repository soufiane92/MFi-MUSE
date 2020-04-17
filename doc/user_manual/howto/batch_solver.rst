.. _batch_solver:

============
Batch Solver
============

The "MFiX Batch Solver" refers to the command line version of MFiX as in
MFiX 2016-1 and earlier versions, to contrast it with the "MFiX Interactive
Solver". The batch solver binary is not installed in the MFiX conda package; it
needs to be compiled before being used (as in previous versions of MFiX).

Using the Batch Solver is appropriate when you do not want to use the
interactive features with the GUI. You cannot pause, unpause, stop, or monitor
status from the batch solver.

To build the solver without interactive support, run:

.. code-block:: shell

  > build_mfixsolver --batch
  > ls
  build mfixsolver

This command will build an executable ``mfixsolver`` in the current directory.

The ``build`` directory can be deleted, and the custom ``mfixsolver`` will still
run. However, leaving the ``build`` directory will greatly speed up rebuilds if
you want to edit the UDFs and run ``build_mfixsolver`` again.

Configuration options
---------------------

Arguments may be passed to ``build_mfixsolver`` to specify various options: compiler,
compiler flags, SMP/DMP support, and other options. All configuration options can
be displayed with:

.. code-block:: shell

  > build_mfixsolver --help

usage: build_mfixsolver [--batch] [--dmp] [--smp] [-k] [-j]
                        [FC=<Fortran compiler>]
                        [FCFLAGS=<Fortran flags>]

The most common arguments are given in the following table.

+----------------------+------------------------------------------------+
| Argument             | Description                                    |
+======================+================================================+
| ``FC=<compiler>``    | Specify the Fortran compiler command           |
+----------------------+------------------------------------------------+
| ``FCFLAGS=<flags>``  | Specify Fortran compiler flags                 |
+----------------------+------------------------------------------------+
| ``--dmp``            | Enable distributed memory support (MPI)        |
+----------------------+------------------------------------------------+
| ``--smp``            | Enable shared memory parallel support (OpenMP) |
+----------------------+------------------------------------------------+

The Fortran compiler is specified by passing the ``FC`` argument to
``build_mfixsolver``. If the ``FC`` argument is not specified, the script will
search the environment for a Fortran compiler (usually ``gfortran``).

.. _compiler-list:

.. table:: compiler value for FC

  +------------+----------------------------------------------------+
  | Compiler   | Description                                        |
  +============+====================================================+
  | gfortran   | GNU Fortran compiler (serial/smp)                  |
  +------------+----------------------------------------------------+
  | ifort      | Intel Fortran compiler (serial/smp)                |
  +------------+----------------------------------------------------+
  | mpif90     | Generic MPI Fortran wrapper (dmp/combined smp-dmp) |
  +------------+----------------------------------------------------+
  | mpifort    | Generic MPI Fortran wrapper (dmp/combined smp-dmp) |
  +------------+----------------------------------------------------+
  | mpiifort   | Intel MPI Fortran wrapper (dmp/combined smp-dmp).  |
  |            | Older versions commonly use the mpif90 command.    |
  +------------+----------------------------------------------------+

.. note::

   When building with the Intel Fortran compiler and DMP support, you also need
   to set ``FC`` or ``MPI_Fortran_COMPILER`` to the MPI compiler wrapper:

   .. code-block:: bash

      > build_mfixsolver --batch --dmp -DMPI_Fortran_COMPILER=mpiifort
      > build_mfixsolver --batch --dmp FC=mpiifort     # or equivalent


Compiler flags can be specified by passing the ``FCFLAGS`` argument to
``build_mfixsolver``. If the ``FCFLAGS`` argument is not specified, the compiler
defaults are used.

.. table:: Compiler Flags for gfortran and Intel Fortran

   +---------------------+---------------------+--------------------------------------------------------------------------------+
   |  GNU Fortran        |  Intel Fortran      | Description                                                                    |
   +---------------------+---------------------+--------------------------------------------------------------------------------+
   |  ``-g``             |  ``-g``             | Produce debugging information                                                  |
   +---------------------+---------------------+--------------------------------------------------------------------------------+
   |  ``-O0``, ``-O1``,  |  ``-O0``, ``-O1``,  | Optimization level (refer to the compiler documentation for details on level)  |
   |  ``-O2``, ``-O3``,  |  ``-O2``, ``-O3``,  | The following options are commonly used:                                       |
   |                     |                     |                                                                                |
   |                     |                     |        - ``-O0`` for debugging                                                 |
   |                     |                     |        - ``-O2`` for production                                                |
   +---------------------+---------------------+--------------------------------------------------------------------------------+
   |  ``-fcheck=all``    |  ``-check all``     | Generates runtime checks useful for debugging                                  |
   +---------------------+---------------------+--------------------------------------------------------------------------------+
   |  ``-fbacktrace``    |  ``-backtrace``     | Print a full stack trace when a runtime error occurs                           |
   +---------------------+---------------------+--------------------------------------------------------------------------------+
   |  ``-g``             |  ``-g``             | Generates complete debugging information                                       |
   +---------------------+---------------------+--------------------------------------------------------------------------------+

Running ``build_mfixsolver`` will display the flags specified with ``FCFLAGS``,
and other compiler flags that are always defined by the build scripts:

.. code-block:: bash

    > build_mfixsolver --batch FCFLAGS="-Wall"
    ...
    -- MFIX build settings summary:
    --    Build type        = RelWithDebInfo
    --    Fortran compiler  = /usr/bin/gfortran
    --    Fortran flags     = -Wall -cpp -ffree-line-length-0 -fconvert=big-endian -fPIC

The ``build_mfixsolver`` script will test specified Fortran compiler with any
specified flags (covered next). If the compiler is not found or if the compiler
doesn't work with the specified flags, no Makefile will be generated and an
error message will be printed. When seeking help on the mailing list for build
issues, include any error messages.


Make options
------------

``build_mfixsolver`` will pass the options ``-j`` and ``-k`` on to ``make``, that is to say

.. code-block:: shell

   > build_mfixsolver --batch -j

To build with multiple jobs on multicore systems, in order to speed up the build process.


Building
--------

.. code-block:: shell

   To build the MFiX batch solver, run ``build_mfixsolver`` with the ``--batch`` option.

    > cd tutorials/Silane_Pyrolysis/
    > ls
    SP2D.mfx  usr0.f  usr1.f  usr_mod.f  usr_rates.f
    > build_mfixsolver --batch
    ...build output...
    > ls  *
    build mfixsolver SP2D.mfx  usr0.f  usr0.o usr1.f  usr1.o  usr_mod.f  usr_mod.o usr_rates.f usr_rates.o

   Not the absence of a ``lib`` directory. The ``lib`` directory has the Python
   module for the interactive solver, and therefore is not present for a batch
   solver. Also, ``mfixsolver`` is a binary executable (not a wrapper script).


Running
-------

.. code-block:: shell

    > ./mfixsolver -f DES_FB1.mfx

.. include:: /user_manual/icons.rst

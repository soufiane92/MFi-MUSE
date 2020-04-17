.. _customsolver:

=========================
Custom Interactive Solver
=========================

For some cases, you may want to use a custom mfixsolver. For instance, when
running cases with UDFs (User Defined Functions, which are defined in Fortran
source files in the project directory), it is necessary to build a custom
solver. A custom solver is an ``mfixsolver`` executable built in the project
directory, specific to that project. This includes projects with :ref:`chemical
reactions <chemical-reactions>`, since those have UDFs.

.. note::
   Among the tutorials listed on the New Menu, only the Silane_Pyrolysis case
   has UDFs and requires running with a custom solver to run correctly.


Custom Solver From GUI
----------------------

Press the |build| (Build) button will display the Build Dialog box. Select the
compilation options, then press the "Build Solver" button. It may take a few
minutes to compile. See :ref:`build-dialog` for further details about
compilation options. The "Build Command" of the build dialog will show the
equivalent command for :ref:`build-custom-solver`.

After building the custom solver, run a simulation by pressing |play| (Run) to
display the :ref:`run-dialog`. Select the mfixsolver file you have just built.

You can pause, unpause, stop, or get info from the solver.


.. _build-custom-solver:

Custom Solver From Command Line
-------------------------------

For example, the ``Silane_Pyrolysis`` tutorial has UDFs and will not run with the
default solver.

.. code-block:: shell

    > cd tutorials/Silane_Pyrolysis/
    > ls
    SP2D.mfx  usr0.f  usr1.f  usr_mod.f  usr_rates.f

To build a custom solver for ``Silane_Pyrolysis``:

.. code-block:: shell

    > build_mfixsolver
    ...build output...
    > ls  *
    build/ lib/ mfixsolver SP2D.mfx  usr0.f  usr0.o usr1.f  usr_mod.f  usr_rates.f

The ``build_mfixsolver`` command creates a wrapper script
``mfixsolver``, that runs the case-specific MFiX solver (which is
installed in the ``lib`` directory).

The ``build`` directory can be deleted, and the custom ``mfixsolver``
will still run. However, leaving the ``build`` directory will greatly
speed up rebuilds if you want to edit the UDFs and run
``build_mfixsolver`` again.

After building the solver, on Linux or Mac run the custom solver with:

.. code-block:: shell

    ~/projectdir> ./mfixsolver -f SP2D.mfx

On Windows, run the custom solver in the project directory with:

.. code-block:: shell

    C:\projectdir > mfixsolver -f SP2D.mfx


C++ Implementation of Interactive Solver
----------------------------------------

The interactive solver in MFiX 17.x is implemented in Fortran and Python. The
``mfixsolver`` executable is actually a Python script that runs the MFiX solver
as a Python extension module.

In MFiX 18.x there is an additional implementation of the interactive solver,
written in Fortran and C++ using the `Crow <https://github.com/ipkn/crow>`_ web
framework, for which the ``mfixsolver`` is a binary executable, like the
:ref:`batch_solver`. Both the Python and C++ interactive solvers are intended to
function the same at runtime, but depending on your platform, environment,
compiler, and compilation options, one might be more suitable than the other.

For instance, building an interactive solver with the Intel Fortran Compiler
``ifort`` only works with the C++ solver (not with the Python interactive
solver).

The C++ interactive solver is only available if "Enable Developer Tools" is
checked in :ref:`Settings <settings>`.

To build the C++ interactive solver from the GUI, check "Build solver with crow
interactive support" on the Build dialog box.

To build the C++ interactive solver from the command line, run
``build_mfixsolver`` with the ``--crow`` option.

.. code-block:: shell

    > cd tutorials/Silane_Pyrolysis/
    > build_mfixsolver --crow
    ...build output...
    > ls  *
    build/ mfixsolver SP2D.mfx  usr0.f  usr1.f  usr_mod.f  usr_rates.f

.. include:: /user_manual/icons.rst

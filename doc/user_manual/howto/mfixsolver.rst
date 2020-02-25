.. _mfixsolver:

==========================
Default Interactive Solver
==========================

The default solver does not need to be compiled, because the binary executable
is installed with MFiX. The default ``mfixsolver`` will be in PATH in MFiX is
installed as in the :ref:`setup-guide`.

Running in GUI
--------------

Start the MFiX GUI from the Anaconda Prompt (Windows) or ``bash`` (Mac or Linux):

.. code-block:: shell

    # Windows
    (mfix-18.1.0-win64) C:\> mfix

    # Linux
    (mfix-18.1.0-linux64) > mfix

    # Mac
    (mfix-18.1.0-osx64) > mfix

When running a simulation, press |play| (run) and select the default mfixsolver.

This will use the mfixsolver Python library installed with the package. You can
pause, unpause, stop, or view status from the solver.

Running From Command Line
-------------------------

If you have a project file ``<RUN_NAME>.mfx`` saved in a project directory named
``<RUN_NAME>``, the following command runs the default solver:

.. code-block:: shell

    > cd <RUN_NAME>
    > mfixsolver -f <RUN_NAME>.mfx

For example, to run the 3D fluidized bed tutorial, with the project file
``FB3D.mfx`` created in the GUI and saved in a directory ``FB3D``:

.. code-block:: shell

    > cd FB3D
    > mfixsolver -f FB3D.mfx

To show additional options with the solver, run:

.. code-block:: shell

    > mfixsolver --help

.. include:: /user_manual/icons.rst

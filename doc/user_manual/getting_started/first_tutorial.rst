.. _user-guide:

==============
First Tutorial
==============

This section illustrates how to launch the GUI, open a project, run it, and
visualize the results.

This section assumes MFiX is already installed. To install MFiX, see the
:ref:`setup-guide`.

Everything in this section applies to all platforms (Linux, macOS, Windows)
unless otherwise noted.

Starting the MFiX GUI
---------------------

On Windows, open the Anaconda Prompt and activate the `mfix environment <create-environment>`

On Linux or macOS, and activate the `mfix environment <create-environment>` from ``bash``.

.. code-block:: shell

    # Windows
    (mfix-18.1.0-win64) C:\> mfix

    # Linux
    (mfix-18.1.0-linux64) > mfix

    # Mac
    (mfix-18.1.0-osx64) > mfix

If this is the first time opening the GUI, the File menu will automatically
open. Otherwise, the previous project will automatically open.

Creating a project
------------------

-  Click |new| (:ref:`new-project`)

A list of project templates is displayed.

The templates are tagged with the following icons. (Some templates have more
than one icon).

.. include:: /user_manual/template_icons.rst

.. note::
   The blank template is the default, but it won't run without customization. It
   requires a pressure outflow, among other things.

-  Filter the templates by de-selecting the |single| (single phase), and
   |chemistry| (chemistry) icons
-  Double-click on ``Hopper_DEM_3D`` to create a new project
-  Enter a project name (or keep the existing name) and browse to a location for
   the new project.
-  Click Ok

A new project directory will be created in the selected directory, with the name
being the project name. Here, a DEM Hopper simulation setup has been loaded and
is ready to run. You should see the model geometry in the "model" window
(top-right).

Running the Solver
------------------

-  Click the |play| (Start) button to open the :ref:`run-dialog`.

-  Click Ok to use the default mfixsolver: ``[default]/mfixsolver``

The solver will begin to run, with output displayed in the
:ref:`terminal-window`. There is a progress bar along the bottom of the screen
where it says `MFiX running: elapsed time`.

Pausing/Unpausing
-----------------

-  Click the |pause| (pause) button to pause the solver.

``MFiX paused`` will be displayed in the :ref:`terminal-window`. The solver
process still exists, but is waiting for you to unpause it.

While the solver is paused, you can change the project. For instance, on the
:ref:`run-pane` Pane you could change ``stop time`` to another value to have the
solver run for a different amount of time.

-  Click the |play| (run) button to unpause the solver and continue running.


Stopping/Resuming
-----------------

-  Click the |stop| (stop) button to stop the solver.

``MFiX stopped`` will be displayed in the :ref:`terminal-window`. The solver
process has ended, with the output files still in the project directory.

You can then resume the solver by:

-  Clicking the |play| (run) button to open the :ref:`run-dialog`.

-  Check ``Restart`` and select ``Resume`` from the drop down list.

-  Click Ok to use the default mfixsolver.

The solver process will start and continue from the point at which the output
files were last written.

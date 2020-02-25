System Features
===============

Guided Simulation Setup
-----------------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

High priority: Specify an MFiX simulation through an intuitive user interface (UI). This
feature should guide users in constructing a simulation by enabling/disabling
features depending on selected options. For example, users should not be able to
specify a kinetic theory model for non-Eulerian solids.

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Users will be guided through a logical process for selecting model parameters.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

See section 3.

Internal Console
----------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

High priority: Create an interactive console that allows user manipulation of the model such
as:

-  Changing keywords
-  Programmatically create BCs/ICs/PSs

As well as display relevant information including:

-  Errors
-  MFiX process connection info

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

When the user enters commands, process those commands and update the UI/model
accordingly. Display relevant information from UI algorithms.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

    1. Display a console
    2. Allow user input
    3. Display information

3D Graphics - Visualization
---------------------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

High priority: Create a primitive 3D graphics environment to help visualize the simulation
setup including:

-  Displaying geometry

-  Displaying the grid
-  Displaying defined regions

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

    As the user sets up the model, display relevant 3D objects to show geometry and regions.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

      4. Display the geometry (stl, primitives)
      5. Display Rectilinear grid overlaid with the geometry
      6. Display meshed geometry
      7. Display regions (point, plane, volume)

3D Graphics – Interaction
-------------------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

    Medium priority: Provide user interactivity with the 3D environment for changing the size,
    rotation, and position of objects such as the geometry, grid, and regions.

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The user will use mouse and keyboard inputs to change objects in the 3D space

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

        1. Change the size, position, and rotation of geometry objects
        2. Move grid control points
        3. Change the size, position, and rotation of region objects
        4. Select individual cells/triangles

Geometric preprocessing
-----------------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

    High priority: Create a project (``.mfx``) file through the GUI.

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

    As the user works through the interface, key, arguments, and values will be
    generated and stored in a data structure. When the user “saves” the project,
    the project file will be generated.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

        1. Collect and store correct keywords, arguments, and values
        2. Write the collected keywords

Installer
---------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

    High priority: Create a simple installation procedure each targeted platform.

    The conda package manager will be used to facilitate deployment to each
    targeted platform. The term "conda environment" refers to either an Anaconda
    or Miniconda distribution of Python.

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The user will run a single command to install MFiX and all dependencies in a
    conda environment.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

        1. Installation on Windows
        2. Installation on Linux (ubunutu, opensuse, fedora, ...)
        3. Installation on OSX

Creating Project File
---------------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

    High priority: Create a project (``.mfx``) file through the GUI.

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

    As the user works through the interface, key, arguments, and values will be generated and
    stored in a data structure. When the user “saves” the project, the project file will be
    generated.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

        1. Collect and store correct keywords, arguments, and values
        2. Write the collected keywords, arguments, and values to a text file with extension ``.mfix``

Building MFiX
-------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

    Medium priority: Build MFiX

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The user will select a button or menu item to build an MFiX executable from
    source code distributed with MFiX.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

    This capability is optional, because the binary distribution of MFiX
    includes a prebuilt MFiX executable for when the user does not have a
    compiler installed. Building MFiX from the GUI is useful when UDFs are
    present, or when rebuilding for different configurations: SMP, DMP,
    different Fortran compilers and MPI implementations.

        1. Build on Linux
        2. Build on OSX
        3. Build using CMake
        4. Build using SMP
        5. Build using DMP

Running MFiX
------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

High priority: Start MFiX

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The user will select a menu item to launch MFiX locally, submit it to a batch queue, or
connect to an MFiX simulation that is already running. The MFiX-UI will then display the
progress of the job and various statistics.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

    1. Remove existing run files
    2. Check to that an MFiX solver is available
    3. Launch job locally
    4. Pause a running job
    5. Resume a job
    6. Update timestep
    7. Write restart (RES) files
    8. Terminate a job
    9. Reinitialize with an updated project file
    10. View a job’s current simulation time, current timestep, residuals for current
        iteration of the current timestep, and estimated walltime to finish

Queue Support
-------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

High priority: Allow for submitting/monitoring/canceling of jobs submitted to a queue
manager.

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The user can select to submit a job to s specific queue.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

    1. Flexible interface for users to specify queue parameters
    2. Submit job to a queue
    3. Monitor job status (waiting/running/canceled/ended)
    4. Remove job from queue
    5. Monitor queue availability and automatically move queued jobs to other queues

External Programs
-----------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

    Medium Priority: After running MFiX, provide a button to open simulation results in
    Paraview.

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The user will select a menu item to launch MFiX simulation results in Paraview. Ideally, this
    will include opening the graphics stack and applying a state file.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

        1. Launch Paraview by pressing a button in the UI
        2. Open the current simulation’s graphics stack in Paraview
        3. Set a state file to setup the visualization in Paraview

Plugins
-------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

    Medium Priority: Provide a structure for user supplied plugins

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Users will be able to create plugins for the UI to handle:

-  queue submissions
-  widgets
-  Post processing

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

        1. Automatically look for plugins
        2. expose the plugin in the UI

Nodeworks Interaction
---------------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

    High Priority: import the nodeworks library and display under the “Nodeworks” mode.

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The user will select the “Nodeworks” mode where the UI will change to show the nodeworks
    interface. Run and autorun buttons will be exposed.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

        1. Import pyqtnode and imbed in the UI
        2. expose hooks for pyqtnode to run MFiX simulations

Species Database
----------------

Description and Priority
^^^^^^^^^^^^^^^^^^^^^^^^

    High Priority: Include a large collection of thermodynamic species data that a user can
    “import” into the model.

    Database links: http://webbook.nist.gov/chemistry/,
    https://www.grc.nasa.gov/WWW/CEAWeb/ceaThermoBuild.htm

Stimulus/Response Sequences
^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Where needed, a user can search and import gas, liquid, and solids species from a database.

Functional Requirements
^^^^^^^^^^^^^^^^^^^^^^^

        1. Include a thermodynamic data base with the UI
        2. Import species data from that data base

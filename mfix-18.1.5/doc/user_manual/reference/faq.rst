.. _faq:

Frequently Asked Questions
==========================

This document explains how to perform common tasks with MFiX.

How do I ask question or send feedback?
---------------------------------------

Please send specific questions and feedback regarding
the GUI to `mfix-help@mfix.netl.doe.gov <mailto:mfix-help@mfix.netl.doe.gov>`__.
Please include your version info in all communications (File Menu --> About and
press the "Copy Version Info" button).
Other technical questions regarding MFiX can be sent to
`mfix-help@mfix.netl.doe.gov <mailto:mfix-help@mfix.netl.doe.gov>`__.


How do I change the look & feel of the GUI?
-------------------------------------------

Go to the File Menu --> settings and change the style from the pull down menu.


How do I Troubleshoot VTK issues?
---------------------------------

The ``vtk`` package in the ``anaconda`` channel requires OpenGL API version 2.1.
If you system or graphics card does not support this, there is an ``vtk``
package in the ``mfix`` channel that is built for OpenGL 1.1:

.. code-block:: shell

    (mfix-18.1) > conda install vtk -c mwm126     # try this if VTK is not working on your system

If VTK still isn't working on your system, report your issue to the :ref:`mailing-list`.

How do I build the C++ solver on CentOS 7?
------------------------------------------

If you get linking errors when building with ``--crow`` on Linux, try installing
Boost from source with your system compiler. The `Spack package manager
<http://spack.io>`_ is a convenient way of installing software (such as Boost)
built from source as an ordinary user without root access (common in HPC cluster
environments).

Since Spack installs packages from source, a system compiler and other build
dependencies are required. The following command will install Spack build
dependencies (given root access):

.. code-block:: bash

    > yum install git gcc make    # CentOS 7

Spack installs software under the Github repo it is cloned from, and uses
`Environment Modules <http://modules.sourceforge.net>`_ to load installed
software packages.

.. code-block:: bash

    > cd $HOME   # or whichever directory you prefer
    > git clone https://github.com/spack/spack.git
    > ./spack/bin/spack bootstrap
    > source ./spack/share/spack/setup-env.sh
    > spack install boost
    > spack load boost

Now that ``boost`` module is loaded, try :ref:`building the
custom solver <build-custom-solver>` with Crow support again:

.. code-block:: bash

    > build_mfixsolver --crow

There may be a warning message if the spack boost version is newer than your
cmake version, but you can ignore that (or update CMake if you prefer)

This has been tested on CentOS 7, but may be
applicable for using recent Boost versions on older Linux distributions.

How do I Install MFiX to a system not connected to the Internet (offline install)?
----------------------------------------------------------------------------------

This is possible, but not convenient. You should first practice installing MFiX
on an online computer to become familiar with ``conda`` and ``MFiX``.
Familiarity with the command line is recommended.

You will require an online system that is the **same platform** as the offline
system you want to install MFiX to.

Install MFiX to online system
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. Download `Miniconda <https://conda.io/miniconda.html>`_ for your platform (64-bit Python 3.6)
#. Install miniconda an environment: ``bash Miniconda3-latest-Linux-x86_64.sh -b -p ~/miniconda3``
#. Create an environment: ``~/miniconda3/bin/conda create -n mini``
#. Activate the environment: ``source ~/miniconda3/bin/activate mini``
#. :ref:`install-mfix` in the environment: ``~/miniconda3/bin/conda install mfix -c <URL>``
#. Make a tarball of the package directory: ``tar cf ~/pkgs.tar.bz2 -C ~/miniconda3/pkgs/ .``

Copy and install files to offline system
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#. Copy ``Miniconda3-latest-Linux-x86_64.sh`` and ``pkgs.tar.bz2`` to the offline computer (using USB, ISO, or other media). ``pkgs.tar.bz2`` may be about 2Gb in size.
#. Install Miniconda with downloaded file: ``bash Miniconda3-latest-Linux-x86_64.sh -b -p ~/miniconda3``
#. Extract package tarball: ``tar xf ~/pkgs.tar.bz2 -C ~/miniconda3/pkgs``
#. Install MFiX: ``~/miniconda3/bin/conda install --offline mfix``

How do I Build the custom solver in parallel?
---------------------------------------------

The custom solver can be build in parallel by checking the "Build solver in
parallel" check box. This will pass the `-j` option to the `make` command and
significantly decrease the time to build the solver.


Play/Pause/Reinit
-----------------

While the simulation is running, it can be paused by pressing the |Pause button| button.
A few settings can be modified, such as the stop time. Settings that cannot be changed
(for example, the grid spacing) will be disabled while the simulation is paused.
To resume the simulation, save the new settings by pressing the |Save button| button
and press the |Start button| button.

Restart a simulation from current time
--------------------------------------

Once a simulation has run to completion, it can be restarted from the current simulation time.
Increase the stop time in the "Run" pane, press the |Save button| button,
and press the |Start button| button. In the "Run solver" dialog box, check the "Restart"
check box and select "Resume". Then press the "Run" button.

Restart a simulation from the beginning
---------------------------------------

Once a simulation has run to completion, or if you pressed the |Stop button| button,
the simulation can be restarted from the beginning. This is typically needed if the simulation
settings need to be changed and the current results are not needed anymore.
First press the |Rest button| to delete the current results. Next change the simulation
settings as needed (if any). Press the |Save button| button, and press the |Start button| button.


What system of units does MFiX use?
-----------------------------------

Simulations can be setup using the International System of Units (SI). The
centimeter-gram-second system (CGS) is no longer supported.

+-----------------------+------------------------------------+
|  Property             |   MFIX SI unit                     |
+=======================+====================================+
|  length, position     |  meter :math:`(m)`                 |
+-----------------------+------------------------------------+
|  mass                 |  kilogram :math:`(kg)`             |
+-----------------------+------------------------------------+
|  time                 | second :math:`(s)`                 |
+-----------------------+------------------------------------+
|  thermal temperature  |  Kelvin :math:`(K)`                |
+-----------------------+------------------------------------+
|  energy†              |  Joule :math:`(J)`                 |
+-----------------------+------------------------------------+
|  amount of substance‡ | kilomole :math:`(kmol)`            |
+-----------------------+------------------------------------+
|  force                |Newton (:math:`N = kg·m·s^{-2}`)    |
+-----------------------+------------------------------------+
|  pressure             | Pascal (:math:`Pa = N·m^{-2}`)     |
+-----------------------+------------------------------------+
|  dynamic viscosity    | :math:`Pa·s`                       |
+-----------------------+------------------------------------+
|  kinematic viscosity  | :math:`m^2·s^{-1}`                 |
+-----------------------+------------------------------------+
|  gas constant         |  :math:`J·K^{-1}·kmol^{-1}`        |
+-----------------------+------------------------------------+
|  enthalpy             |  :math:`J`                         |
+-----------------------+------------------------------------+
|  specific heat        |  :math:`J·kg^{-1}·K^{-1}`          |
+-----------------------+------------------------------------+
|  thermal conductivity |  :math:`J·m^{-1}·K^{-1}·s`         |
+-----------------------+------------------------------------+

† The SI unit for energy is the Joule. This is reflected in MFIX through the gas
constant. However, entries in the Burcat database are always
specified in terms of calories regardless of the simulation units. MFIX converts
the entries to joules after reading the database when SI units are used.

‡ The SI unit for the amount of a substance is the mole (mol). These units are
needed when specifying reaction rates:

• amount per time per volume for Eulerian model reactions
• amount per time for Lagrangian model reactions

What do I do if a run does not converge?
----------------------------------------


Initial non-convergence:

Ensure that the initial conditions are physically realistic. If in the initial
time step, the run displays NaN (Not-a-Number) for any residual, reduce the
initial time step. If time step reductions do not help, recheck the problem
setup.

Holding the time step constant (``DT_FAC=1``) and ignoring the stalling of
iterations (``DETECT_STALL=.FALSE.``) may help in overcoming initial nonconvergence.
Often a better initial condition will aid convergence. For example, using a
hydrostatic rather than a uniform pressure distribution as the initial condition
will aid convergence in fluidizedbed simulations.

If there are computational regions where the solids tend to compact (i.e.,
solids volume fraction less than EP_star), convergence may be improved by
reducing ``UR_FAC(2)`` below the default value of 0.5.

Convergence is often difficult with higher order discretization methods. First
order upwinding may be used to overcome initial transients and then the higher
order method may be turned on. Also, higher-order methods such as van Leer and
minmod give faster convergence than methods such as superbee and ULTRA-QUICK.


How to provide initial particle positions in particle_input.dat file?
---------------------------------------------------------------------

It is possible to provide initial particles position and velocity rather
than using the automatic particle generation mechanism with Initial Conditions
regions. The file ``particle_input.dat`` is a text file that stores each particle
position, radius, density, and velocity. For 2D geometries, it contains 6
columns: x , y, radius, density, u, v. In 3D, it contains 8 columns:
x , y, z, radius, density, u, v, w. The number of lines correspond to the
number of particles read, and should match the "Data file particle count"
defined in the Solids  > DEM tab. The check box "Enable automatic particle
generation" must be left unchecked.

Below is an example of a 2D ``particle_input.dat`` (only 4 particles shown):

.. code-block:: shell

    0.2000D-03  0.2000D-03  0.5000D-04  0.1800D+04  0.0000D+00  0.0000D+00
    0.6000D-03  0.2000D-03  0.5000D-04  0.1800D+04  0.0000D+00  0.0000D+00
    0.1000D-02  0.2000D-03  0.5000D-04  0.1800D+04  0.0000D+00  0.0000D+00
    0.1400D-02  0.2000D-03  0.5000D-04  0.1800D+04  0.0000D+00  0.0000D+00




Below is an example of a 3D ``particle_input.dat`` (only 4 particles shown):

.. code-block:: shell

    0.2000D-03  0.2000D-03  0.1000D-01  0.5000D-04  0.1800D+04  0.0000D+00  0.0000D+00  0.0000D+00
    0.6000D-03  0.2000D-03  0.1000D-01  0.5000D-04  0.1800D+04  0.0000D+00  0.0000D+00  0.0000D+00
    0.1000D-02  0.2000D-03  0.1000D-01  0.5000D-04  0.1800D+04  0.0000D+00  0.0000D+00  0.0000D+00
    0.1400D-02  0.2000D-03  0.1000D-01  0.5000D-04  0.1800D+04  0.0000D+00  0.0000D+00  0.0000D+00


Currently, the ``particle_input.dat`` file must be edited outside of the GUI
and saved in the project directory. Please see the FluidBed_DES tutorial for
an example.

.. include:: /user_manual/icons.rst

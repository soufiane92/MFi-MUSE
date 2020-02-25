===================
Overall Description
===================

Product Perspective
-------------------

This product is an evolution of the current MFIX-GUI application. The
original product (v0.1) is currently available to the MFIX user base and
works as a mediator between users and the model deck file (``.mfx``)
while offering moderate runtime analysis (e.g., plotting residuals) and
build environment support. The expanding model capabilities of the MFIX
software suite, growing need for runtime control and interaction of
simulations, and model optimization necessitates an immersive user
interface.

Product Functions
-----------------

-  Fully specify an MFIX simulation through an easy to use user
   interface (UI)
- Define run parameters, geometry, initial/boundary
   conditions, material properties
- Basic in situ ‘sanity checks’ on
   specified input
-  Instantaneous control/interaction of a running MFIX simulation o
   Launch simulations from the UI, locally or via a queuing system o
   Connect to running simulations that were launched in batch mode o
   Patch field variables with user specified value
-  Basic in situ data collection/analysis
- Average field variable
   across point/line/plane/volume
- Plot field variable across
   line/plane

User Classes and Characteristics
--------------------------------

General CFD practitioners: This user class is comprised of traditional
CFD users interested primarily in setting up and running single- and
multi-phase flow simulations. This class may make use well
supported/defined UDF hooks however; substantial interest in code
development is not expected.

General developers: This user class is comprised of researches at
Universities and other National Labs. This class engages in moderate
levels of code development, primarily to investigate specific physical
models. This class needs the ability to readily build and rebuild MFIX
executables from locally modified source code. This user is not likely to
modify the MFIX-UI or contribute to general code maintenance.

MFIX developers: This user class is comprised of the core MFIX
development team and NETL support collaborators. This class will
regularly modify the MFIX solver source code for debugging and model
development. This class requires access to the MFIX deck file for
toggling new features. This user must work tightly with the MFIX-UI team
to ensure features added to the solver are available through the
MFIX-UI.

Operating Environment
---------------------

The user interface shall work on the following operating systems:

-  Linux (Ubuntu, Fedora, OpenSUSE)
-  Windows 7, 8.1, 10
-  OS X (Mac)

The interface shall connect and interact with running models on Linux
based systems. Joule, the NETL Supercomputer, is the primary target of
this feature. However, extension and testing on other HPC systems will
be investigated as access and resources permit.

Design and Implementation Constraints
-------------------------------------

Python will be used to implement the MFIX-UI. The MFIX executable
compiled with Python/Fortran bindings provides access to Python
libraries for networking. Anaconda Python Distribution from Continuum
Analytics provides dependencies across all operating environments as
described in 0.

-  Primary Language: Python
   - Target versions: 2.7 and 3.
   - This includes all “python core” libraries such as os, etc.

-  Third party Python libraries (open source)
   - GUI library: Qt

     - Supported wrappers: PyQt4, PyQt5, PySide

   - 3D graphics library: VTK
   - Array library: numpy
   - Fortran/Python interface generator: F2PY
   - Web framework library: Flask

User Documentation
------------------

Text based documentation generated using Sphinx, covering the following
areas:

-  Disclaimer
-  Introduction
-  Installation
-  Features
-  Tutorials
-  Developer Guide
   - Contributing
   - Style Guide
   - Tests
   - Building Binaries
-  Help
-  Release Notes
-  Test Harness Report
-  API
   - Automatically pulled from docstrings in the code

Video based screen cast tutorials will also be developed and published
on YouTube through the NETL YouTube Channel.

Assumptions and Dependencies
----------------------------

This plans assumes continued support for the Anaconda Python
Distribution and that it will contain the following dependencies:

-  PyQt4, PyQt5, or PySide
-  VTK
-  Numpy
-  F2PY
-  Flask

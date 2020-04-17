Job submission/Interactive Control
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section describes the interface for running a job and how to
interact with the running solver.

When the MFIX solver is launched the following actions are taken: 1)
Initializes MPI for DMP enabled solvers. 2) Initializes OpenMP threads
for SMP enabled solvers. 3) Processes the case file (\*.mfx) 4) Check
validity of project settings a. Check and setup base background mesh b.
Setup MPI domain decomposition c. Preform sanity checks on user inputs
d. Set flags and loop structures for background mesh e. Invoke Cartesian
grid cut-cell preprocessing f. Allocate data arrays 5) Process RES
(restart) files a. Save current state for new runs b. Read RES files for
restarts 6) Setup initial field states prior to time march

yeStep 4 is where the majority of failures will be caught when starting
the MFIX solver. The solver should catch the error and report it back to
the GUI so the user may correct the issue. To facilitate

Nodeworks Mode
^^^^^^^^^^^^^^

Nodeworks mode utilizes nodework's graphical programing interface and
provides a collection of MFIX specific blocks for creating, running, and
post processing MFIX simulations. This is where design of experiments,
uncertainty quantification, and optimization can be performed.

Developer Mode
^^^^^^^^^^^^^^

Developer mode allows users to edit user-defined functions (UDFs) and
the MFIX-Solver source code. Developer mode also contains tools to help
develop, maintain, and characterize performance of the MFIX-Solver
source code.

Figure 3 : General layout of the “Nodeworks” mode.

The layout will consist of a tree on the left side showing the structure
and files that compose the MFIX-Solver. There will also be a search bar
that can either search the files names or search through the files and
show were that specific search term exists in the files it has been
found in.

Screenshot of a neat tool in spyder for searching through code
repositories with regular expressions. Indicates files and lines in
files where the search has been found.

A light-weight text editor should be included, providing FORTRAN syntax
highlighting, tabbed completion, and inline error checking (Note: could
use spyder’s editor, however it seems to be sluggish and “heavy”
sometimes, especially on the SBEUC).

Hardware Interfaces
^^^^^^^^^^^^^^^^^^^

XBOX Kinetic control of the interface with hand motions ☺.

Software Interfaces
^^^^^^^^^^^^^^^^^^^

-  Paraview: The MFIX-UI should be able to open Paraview and ideally
   open a graphics file stack from the current simulation directory with
   a state file.
-  C3M: Open C3M (available only on Windows)

Communications Interfaces
^^^^^^^^^^^^^^^^^^^^^^^^^

-  Interaction with a running simulation o The MFIX-UI will communicate
   with a running MFIX simulation over a user-specified port using a
   RESTful HTTP interface. The interface will be language neutral, using
   the JSON format for data exchange. o The status of a running MFIX
   simulation will be monitored by the MFIX-UI over HTTP.
-  Interaction with the MFS Website o Read/display news o Import
   particle properties from the MFS particle database o Download
   updates?

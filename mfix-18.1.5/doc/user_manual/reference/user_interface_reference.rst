.. _user-interface-reference:

=============
GUI Reference
=============

Main Toolbar
============

The Main toolbar located at the top of the GUI window.

+-----------------------+--------------------------------------------+
| Icon                  | Description                                |
+=======================+============================================+
| |File menu|           | :ref:`file-menu`                           |
+-----------------------+--------------------------------------------+
| |save|                |     Save button saves the project file.    |
+-----------------------+--------------------------------------------+
| |start|               |  Start button                              |
+-----------------------+--------------------------------------------+
| |pause|               |  Pause button                              |
+-----------------------+--------------------------------------------+
| |stop|                |  Stop button                               |
+-----------------------+--------------------------------------------+
| |reset|               |  Reset button                              |
+-----------------------+--------------------------------------------+
| |Build button|        | :ref:`build-dialog`                        |
+-----------------------+--------------------------------------------+
| |Parameters button|   | :ref:`parameters`                          |
+-----------------------+--------------------------------------------+

Solver Controls:

+-----------------+-------------------------------------------------------+
|                 |          Solver State is....                          |
|                 +---------------------------+---------------------------+
|                 | ...Running and....        | ...Stopped with...        |
|                 +-------------+-------------+-------------+-------------+
|                 | ...Unpaused.| ...Paused.  | no RES.     |    RES.     |
+=================+=============+=============+=============+=============+
| |start|  Start  |   Disabled  |  Unpause    | Start       | Resume      |
+-----------------+-------------+-------------+-------------+-------------+
| |pause|  Pause  |   Pause     |                      Disabled           |
+-----------------+-------------+-------------+-------------+-------------+
| |stop|   Stop   |         Stops the solver  |        Disabled           |
+-----------------+-------------+-------------+-------------+-------------+
| |reset|  Reset  |        Disabled                         | Clear RES   |
+-----------------+-------------+-------------+-------------+-------------+


If no MFiX job is currently running, the Start button will open the run
dialog where settings for job can be changed. The simulation can be
started from this dialog. If an MFiX job is running, you can pause it
with the pause button and un-pause it with the start button. You can
stop a running MFiX job with the stop button. A stopped job leaves
restart (\*.RES) and output files to resume the simulation. Starting a
job by pressing the Start button with \*.RES files present will resume
the job where it stopped. The Reset button will allow the option to
delete the \*.RES files and output files from the project directory.
This will allow the simulation to start from the beginning the next time the
Start button is pressed.

.. _run-dialog:

Run dialog
~~~~~~~~~~

The run dialog is for entering options on starting the solver.

Different solvers can be selected.

If a custom solver is compiled with SMP or DMP support, the following options may be available:

-  Threads (number of threads used for OpenMP)
-  NODESI (number divisions in X direction)
-  NODESJ (number divisions in Y direction)
-  NODESK (number divisions in Z direction)

:math:`NODESI \times NODESJ \times NODESK = n` where :math:`n` is the
number of MPI processes running. If not using MPI,
:math:`NODESI=NODESJ=NODESK=1`.

The GUI supports running both locally as well as submitting to a queue.

.. note::
   SMP and DMP options are disabled for the default solver.
   You will need to build a custom solver with DMP or SMP flags
   to enable these options.

Run Locally
^^^^^^^^^^^

To run locally, select a solver
from the drop-down list or click the browse button to specify a solver that
is not in the list. Usually the default installed solver should be sufficient.
If running a case with UDFs, you need to first build a case-specific MFiX as
described in the :ref:`setup-guide`. You may want to build your own solver for
other reasons, such as specifying various compiler flags to optimize the
executable for your specific hardware.

Make sure the "Submit to queue" check-box is unchecked, and
click "Run" in the Run dialog to start the solver.

Submit to Queue
^^^^^^^^^^^^^^^

To submit a job to a queue (submit to queuing system, e.g. Grid Engine,
PBS, SLURM), select the "Submit to Queue" tab. Select an executable from
the drop-down list or click the browse button to specify an executable
that is not in the list. Next, select a template from the drop-down list
or click the browse button to choose a template that is not in the
drop-down list. Based on the selected template, the widgets in the
"queue options" section will update automatically. Once the options are
specified, click "submit" in the run dialog to submit the job to the
queue.

Custom queue scripts are supported. The format for this script is
described in the `Queue Templates <#queue-templates>`__ section.

.. _build-dialog:

Build Dialog
~~~~~~~~~~~~

The build dialog allows for building custom solvers in project run directories.
This is used to build :ref:`customsolver`.

There is a combo box to select the Fortran compiler command. It will be
populated with any :ref:`known Fortran compilers <compiler-list>` in ``PATH``.
Alternatively, type in the command for the compiler.

There is a field to type in the flags for the Fortran compiler. See your
compiler manual for details, such as the `GNU Fortran Manual
<https://gcc.gnu.org/onlinedocs/gfortran/Option-Summary.html>`_.

There are checkboxes for building with SMP or DMP support. Note that DMP support
require both checking the DMP box, and selecting an MPI compiler (such as
`mpifort`). The "Build solver in parallel" checkbox will run ``make`` with
multiple jobs. DMP and SMP are not available on Windows.

If "Enable Developer Tools" is checked in :ref:`settings`, then there will be a
checkbox for building with the C++ version of the interactive solver.

"Build Command" displays the command line used for building the solver with
the selected options.

Reset Dialog
~~~~~~~~~~~~

The Reset dialog allows for optional deleting of output files from the
run directory. These files include:

+-------------+---------------------------------------------------------------------+
| File Type   | Wild-card match                                                     |
+=============+=====================================================================+
| Restart     | \*.RES                                                              |
+-------------+---------------------------------------------------------------------+
| SPx         | \*.SP?                                                              |
+-------------+---------------------------------------------------------------------+
| VTK         | \*.vtp, \*.vtu, \*.pid                                              |
+-------------+---------------------------------------------------------------------+
| Other       | \*.OUT, \*.pid, \*.error, \*.e[0-9]\*, \*.pe[0-9]\*, \*.po[0-9]\*   |
+-------------+---------------------------------------------------------------------+

\*.RES and \*.SPx files have to be removed from the run directory before
a simulation can be played from the beginning. It is recommended to
remove VTK files from the run directory as well because they will be
over-written.

.. note::
    If there are restart files present in the run directory, some widgets will
    be disabled because certain model parameters can not be edited during a
    resume, or restart state.

.. _parameters:

Parameter Dialog
~~~~~~~~~~~~~~~~

The parameter dialog allows users to create parameters, or variables,
that can then be referenced in widgets that support the use of these
parameters. This functionality allows user to create relationships among
various inputs and change the values of multiple items by changing the
value of a single parameter. In many respects, this is a similar feature
that is present in most commercial CAD packages.

There are six special parameters; ``xmin``, ``xmax``, ``ymin``, ``ymax``,
``zmin``, and ``zmax`` that reference the simulation domain extents, entered on
the geometry pane. These parameters make it easy to setup regions that
automatically adjust to the simulation domain if the extents change.

.. _file-menu:

File menu
=========

The file menu allows for opening, creating, copying, and exporting
projects as well as viewing project meta data, changing settings, and
viewing available help documentation, including this document.

Project Info
~~~~~~~~~~~~

Displays metadata about the current project file.

+-----------------------------+----------------------------------------------------------+
| Label                       | Description                                              |
+=============================+==========================================================+
| Project Version             | version number incremented each time project is saved    |
+-----------------------------+----------------------------------------------------------+
| Created with MFiX Version   | version of MFiX that project was created with            |
+-----------------------------+----------------------------------------------------------+
| Author                      | user-name that created the project                       |
+-----------------------------+----------------------------------------------------------+
| Modified By                 | list of user-names that edited the project               |
+-----------------------------+----------------------------------------------------------+
| Last Modified               | date the project was last modified                       |
+-----------------------------+----------------------------------------------------------+
| Created                     | date the project was created                             |
+-----------------------------+----------------------------------------------------------+
| Notes                       | area where the user can record notes about the project   |
+-----------------------------+----------------------------------------------------------+

.. _new-project:

New
~~~

Create a new project file from a template file. When a template is
selected, the GUI will ask for a project name and parent directory to
save the project to. A new directory with the project name will be
created in the parent directory. A .mfx file will be created in that
directory by copying the contents of the selected template file. Any
other files, such as \*.stl files, will also be copied if present. The
list of templates can be filtered by selecting one or more of the
following model types:

.. include:: /user_manual/template_icons.rst

Open
~~~~

Open an existing project. You can import mfix.dat files from previous
releases of MFiX, but the GUI will save them as a new filename with a
\*.mfx extension. The GUI also performs a number of conversions,
including converting old keywords to new keywords and conversion from
CGS units to SI units. Any user files are not converted. It is strongly
suggested that the project be checked after this conversion.

Save
~~~~

Save the current project.

Save As
~~~~~~~

Save the current project to a new directory and/or as a new filename.
The project is then opened in that location and with that filename.

Export Project
~~~~~~~~~~~~~~

Export the current project to a new directory and/or as a new filename,
but keep the original project opened.

.. _settings:

Settings
~~~~~~~~

Change settings that affect how the GUI acts.

+----------------------+-----------------------------------------------------+
| Option               | Description                                         |
+======================+=====================================================+
| Style                | change the application style                        |
+----------------------+-----------------------------------------------------+
| Enable animations    | enable or disable animated widgets                  |
+----------------------+-----------------------------------------------------+
| Animation Speed      | set the speed at which animations occur             |
+----------------------+-----------------------------------------------------+
| Enable Developer     | hide/show widgets that are mainly for developers of |
| Tools                | the GUI                                             |
+----------------------+-----------------------------------------------------+

Help
~~~~

Show available documentation (including this document) and tutorials, both text
based and videos (if connected to the Internet) demonstrating features of the
GUI.

About
~~~~~

Display the version of MFiX and the versions of the various
dependencies that the GUI is using. If issues are discovered while using the
GUI, it is recommended that any reports include this version information which
can be easily copied by pressing the "copy version info" button.

Quit
~~~~

Exit MFiX. Will ask for confirmation if project is unsaved or if a job
is running.

Model Panes
===========

Each pane in the main window allows editing of different options for an MFiX
project. The Panes are ordered in a logical progression: Model defines the
overall solver type (TFM, DEM, PIC). Geometry and Mesh define the overall domain
space of the solver. Regions are geometrical surfaces and volumes within the
overall domain. Fluid and Solids specify details on the phases the solver deals
with. Initial Conditions (ICs), Boundary Conditions (BCs), Point Sources (PSs),
and Internal Surfaces (ISs) depend on Regions, Fluid, and Solids, so they come
next. Chemistry provides options for reacting cases, and Numerics specifies
details on the Eulerian solver. Output specifies how output data is written.
(Monitors is not yet implemented). Run specifies how long to run the solver for.
(Post is not yet implemented).

.. note::
    Selections on panes may enable or disable widgets based on what input
    parameters are required or available.

Model Setup
~~~~~~~~~~~

The Model pane is used to specify overall options for the project.
Depending on what is selected, other panes may be enabled or disabled.
Options that are specified on this pane include:

-  Solver (Single Phase, Two-Fluid Model, DEM, PIC, Hybrid)
-  Option to disable the fluid phase
-  Option to enable thermal energy equations
-  Option to enable turbulence, if the fluid phase is enabled
-  Gravity in the x, y, and z directions
-  Drag Model including parameters for the selected drag model

Other advanced options that can be selected include:

-  Momentum formulation (Model a, Model B, Jackson, or Ishii)
-  Sub-grid model (only available with TFM, Wen-Yu drag model, etc...)
-  Sub-gird filter size
-  Sub-grid wall correction

Geometry
~~~~~~~~

The Geometry pane allows the specification of the model geometry. This
includes specifying the domain extents (xmin, xmax, ymin, ymax, zmin,
zmax) and 2D/3D selection. If there is complex geometry, the "Auto-size"
button can automatically set the extents to encompass the geometry.

The geometry section provides tools for adding, applying filters, using
automated wizards to create and copy geometry, remove, copy, and perform
Boolean operations on the geometry. All the geometry operations and
visualizations are performed using the `Visualization Toolkit
(VTK) <https://www.vtk.org/>`__'s methods and functions.

Geometry toolbar icons:

+----------------+-----------------------------------------------------+
| Icon           | Description                                         |
+================+=====================================================+
| |geometry|     | Add geometry to model                               |
+----------------+-----------------------------------------------------+
| |filter|       | Modify selected geometry with filter                |
+----------------+-----------------------------------------------------+
| |wizard|       | Add geometry from wizard                            |
+----------------+-----------------------------------------------------+
| |remove|       | Remove the selected geometry                        |
+----------------+-----------------------------------------------------+
| |copy|         | Add duplicate of the selected geometry (copy/paste) |
+----------------+-----------------------------------------------------+
| |union|        | Add union of two or more selected geometries        |
+----------------+-----------------------------------------------------+
| |intersect|    | Add intersection of two or more selected geometries |
+----------------+-----------------------------------------------------+
| |difference|   | Add difference of two or more selected geometries   |
+----------------+-----------------------------------------------------+

In the geometry tree, the geometry object is displayed with the following icons:

+----------------+-----------------+
| Icon           | Geometry Type   |
+================+=================+
| |geometry|     | polydata        |
+----------------+-----------------+
| |function|     | implicit        |
+----------------+-----------------+
| |filter|       | filter          |
+----------------+-----------------+
| |union|        | union           |
+----------------+-----------------+
| |intersect|    | intersect       |
+----------------+-----------------+
| |difference|   | difference      |
+----------------+-----------------+

Adding Geometry
^^^^^^^^^^^^^^^

Geometry can be added by pressing the |geometry| (Geometry) icon and selecting
the geometry to add from the drop-down menu.

The following geometric objects can be added:

- STL files (select a \*.stl file from a file dialog)
- Implicit (Quadric) functions (sphere, box, cylinder, cone, quadric, superquadric)
- Primitives (sphere, box, cylinder, cone)
- Parametrics (torus, boy, conic spiral, etc.)


Applying Filters
^^^^^^^^^^^^^^^^

Filters can be applied to selected geometry by selecting a filter from the
filter menu. The filter options can be edited in the parameter section. The
following filters are included:

+----------------------------+----------------------------------------------+------------------------------+
|Filter                      |Description                                   |vtk class                     |
+============================+==============================================+==============================+
|sample implicit             |converts an implicit function to polydata     |vtkSampleFunction             |
+----------------------------+----------------------------------------------+------------------------------+
|transform                   |rotate, scale, translate polydata             |vtkTransformPolyDataFilter    |
+----------------------------+----------------------------------------------+------------------------------+
|clean                       |merge duplicate points and remove unused      |vtkCleanPolyData              |
|                            |points and degenerate cells                   |                              |
+----------------------------+----------------------------------------------+------------------------------+
|fill holes                  |fill holes                                    |vtkFillHolesFilter            |
+----------------------------+----------------------------------------------+------------------------------+
|triangle                    |make sure all polys are triangles             |vtkTriangleFilter             |
+----------------------------+----------------------------------------------+------------------------------+
|decimate                    |reduce the number of triangles                |vtkDecimatePro                |
+----------------------------+----------------------------------------------+------------------------------+
|quadric decimation          |reduce the number of triangles                |vtkQuadricDecimation          |
+----------------------------+----------------------------------------------+------------------------------+
|quadric clustering          |reduce the number of triangles                |vtkQuadricClustering          |
+----------------------------+----------------------------------------------+------------------------------+
|linear subdivision          |subdivide based on a linear scheme            |vtkLinearSubdivisionFilter    |
+----------------------------+----------------------------------------------+------------------------------+
|loop subdivision            |subdivide based on the Loop scheme            |vtkLoopSubdivisionFilter      |
+----------------------------+----------------------------------------------+------------------------------+
|butterfly subdivision       |subdivide based on 8-point butterfly scheme   |vtkButterflySubdivisionFilter |
+----------------------------+----------------------------------------------+------------------------------+
|smooth                      |move points based on Laplacian smoothing      |vtkSmoothPolyDataFilter       |
+----------------------------+----------------------------------------------+------------------------------+
|windowed sinc               |move points based on a windowed sinc function |vtkWindowedSincPolyDataFilter |
|                            |interpolation kernel                          |                              |
+----------------------------+----------------------------------------------+------------------------------+
|reverse sense               |reverse order and/or normals of triangles     |vtkReverseSense               |
+----------------------------+----------------------------------------------+------------------------------+

Refer to the `VTK website <https://www.vtk.org/documentation>`__ for details.

Wizards
^^^^^^^

Four wizards have been included to perform routine tasks when setting up
multiphase flow projects. These wizards allow for creating cyclones,
reactors, and hoppers. The distributed wizard can also be used to
distribute one geometry inside another geometry with random, cubic, or
body centered cubic positions. Random rotations can also be applied with
the wizard.

|cyclone wizard| |hopper wizard| |reactor wizard|

Remove & Copy
^^^^^^^^^^^^^

The |remove| (Remove) button removes the selected geometry. If a geometry is
grouped as part of a :ref:`boolean-operation`, removing it is disabled until the
Boolean Operation is removed first.

The |copy| (Duplicate) button will create a copy of the selected geometry. If
multiple geometries are selected, duplication is disabled.

.. _boolean-operation:

Boolean Operation
^^^^^^^^^^^^^^^^^

There are two types of geometric objects supported in the GUI: implicit
functions (quadrics), and objects defined by polydata (everything else: STL
files, primitives, parametrics, wizard geometry). Boolean operations cannot be
performed between polydata and implicit geometry types; the implicit function
needs to be converted to polydata by using the ``sample implicit`` filter.
Converting the implicit function also needs to be done in order for the GUI to
export a STL file that the mfixsolver can use.

Boolean operations can be performed with geometry objects of the same
type (implicit, polydata). Boolean operations can not be performed
between polydata and implicit geometry types. The implicit object needs
to be first converted to a polydata object using the sample implicit
filter.

.. note::

   Boolean operation between two polydata objects is supported by MFiX, but for
   complex objects the VTK library may crash.

Mesh
~~~~

The Mesh pane has two tabs:

    - :ref:`background-mesh` for the specification of the background mesh
    - :ref:`mesher` for changing cut-cell tolerances

.. _background-mesh:

Background Mesh
^^^^^^^^^^^^^^^

The Background tab is for specifying the mesh used by the Eulerian solver. A
uniform mesh can be specified by entering the number of cells in the x, y, and z
directions. The resulting Cell Size is computed from the geometry and the number
of cells (Cell Size is not editable directly). The resulting mesh will be
visible in the model setup view. The visibility of the mesh can be toggled with
the |visibility| (Visibility) button at the top of the model setup view.

Control points can be added by pressing the |add| button. Once a control point
has been added, the position, number of cells, stretch, first and last
parameters can be changed. A control point can be split evenly by
``right-click`` on the control point to be split and selecting split. This
operation will create a new control point at the midpoint between the previous
control point and the selected control point, dividing the cells evenly between
the two. Control points can be removed by pressing the |remove| button.

The stretch parameter is a value that will apply a non-uniform grid
spacing to the cells. The value is defined as
:math:`{Last Width} \over {First Width}`. a value larger than 1
stretches the grid as x increases, while a value smaller than one
compresses the grid as x increases. A value of 1 will keep the spacing
uniform.

The first and last values allow the specification of the first and last widths,
the other cell widths adjust accordingly. If a negative value is specified in
the column "First", the last width from the previous grid segment is copied. If
a negative value is specified in the column "Last", the first width from the
next segment is copied.

.. _mesher:

Mesher
^^^^^^

Meshing of complex geometry is performed dynamically by the solver at runtime.
The Mesher tab exposes options to adjust the cut-cell mesher. These options
include:

+----------------------+-------------------------------------------------------------------------+
|Option                |Description                                                              |
+======================+=========================================================================+
|External flow         |select internal or external flow. Note: this depends on which way the    |
|                      |normals are pointing on the STL file. If they are pointing out of the    |
|                      |geometry, then the text will be correct.                                 |
+----------------------+-------------------------------------------------------------------------+
|Small cell tolerance  |tolerance to detect, and remove small cells                              |
+----------------------+-------------------------------------------------------------------------+
|Small area tolerance  |tolerance to detect, and remove cells with small faces                   |
+----------------------+-------------------------------------------------------------------------+
|Merge tolerance       |tolerance used to merge duplicate nodes                                  |
+----------------------+-------------------------------------------------------------------------+
|Snap tolerance        |tolerance to move an intersection point to an existing cell corner       |
+----------------------+-------------------------------------------------------------------------+
|Allocation Factor     |factor used in allocation of cut-cell arrays                             |
+----------------------+-------------------------------------------------------------------------+
|Maximum iterations    |maximum number of iterations used to find intersection points            |
+----------------------+-------------------------------------------------------------------------+
|Intersection tolerance|tolerance used to find intersection of background mesh and STL triangles |
+----------------------+-------------------------------------------------------------------------+
|Facet angle tolerance |ignore STL facets that have an angle less than this tolerance            |
+----------------------+-------------------------------------------------------------------------+
|Dot product tolerance |tolerance used to determine if a point lies in a facet                   |
+----------------------+-------------------------------------------------------------------------+
|Max facets per cell   |maximum number of facets allowed in a cell                               |
+----------------------+-------------------------------------------------------------------------+

Regions
~~~~~~~

The Regions pane defines spatial regions (points, lines, planes, boxes, or STLs)
of the geometry that are used for:

- `Initial Conditions <#initial-conditions>`__
- `Boundary Conditions <#boundary-conditions>`__
- `Point Sources <#point-sources>`__
- `Internal Surfaces <#internal-surfaces>`__
- `Outputs <#outputs>`__

The following buttons are at the top of the Regions pane:

+-------------------+-----------------------------------------------------+
| Icon              | Description                                         |
+===================+=====================================================+
| |add|             | create a new region                                 |
+-------------------+-----------------------------------------------------+
| |remove|          | delete the selected region                          |
+-------------------+-----------------------------------------------------+
| |copy|            | duplicate the selected region                       |
+-------------------+-----------------------------------------------------+
| |all region|      | create a region the encompasses the entire domain   |
+-------------------+-----------------------------------------------------+
| |left region|     | create a region on the left side of the domain      |
+-------------------+-----------------------------------------------------+
| |right region|    | create a region on the right side of the domain     |
+-------------------+-----------------------------------------------------+
| |top region|      | create a region on the top side of the domain       |
+-------------------+-----------------------------------------------------+
| |bottom region|   | create a region on the bottom side of the domain    |
+-------------------+-----------------------------------------------------+
| |front region|    | create a region on the front side of the domain     |
+-------------------+-----------------------------------------------------+
| |back region|     | create a region on the back side of the domain      |
+-------------------+-----------------------------------------------------+

The |add| (Add) button creates a new region. The region will be created with a
generic name such as ``R_1``; it is strongly recommended to give rename it to
something more descriptive when referring to it later. The Color button will
change the color of the region in the model setup view.

The From and To fields for the X, Y, and Z axes define the extents of the
region. These widgets take special parameters, ``min`` and ``max``, that
reference the minimum and maximum values of the domain, as specified in the
`geometry section <#geometry>`__. These values will get automatically updated
when the extents in the geometry section are updated. The region type will be
inferred from the specified extents.

If the region needs to be a collection of triangles from the STL file,
select the Select Facets (STL) check-box. The selection shape can be
changed between a box and an ellipsoid. Triangles that fall on the edge of
the shape can be sliced by selecting the Slice Facets check-box. The
triangles can be further filtered by the normal direction be specifying
a vector and a deviation angle around that vector.

.. note::
    If a region is referenced by an item in the ``Used By`` column, the region
    can not be deleted and its type (STL vs non-STL) cannot be changed.

Fluid
~~~~~

The fluid pane is used to define the physical properties of the fluid phase.
This includes adding fluid phase species which can either be imported from the
BURCAT_ thermodynamic database or created. The GUI will generate the correct
thermo data format.

.. _BURCAT: http://garfield.chem.elte.hu/Burcat/burcat.html

Solids
~~~~~~

The solids phase is used to define the physical properties of each solid(s)
phase. Names can be given to each solid. This name will be used throughout the
GUI to reference that solid phase (i.e. when defining initial conditions,
boundary conditions, etc.). The solid phase model (TFM, DEM, or PIC) can also be
specified here however the selectable solid phase model depends on the solver
selected on the "Model" pane.

The selected solver will also enable/disable the TFM, DEM, and PIC sub-panes
where various options for those solids models can be accessed.

.. _initial-conditions:

Initial Conditions (ICs)
~~~~~~~~~~~~~~~~~~~~~~~~

The initial conditions pane is used to define initial conditions for
a selected Region (defined on each Region pane) for each phase (defined on
Fluid or Solids panes). Initial conditions are required to cover the entire
domain and provide an initial guess that the solver will use. The closer values
such as volume fraction, temperature, pressure, and velocity are to a pseudo
steady state, that faster these initial conditions will get "washed out" from
the domain.

To create a new initial condition, press the |add| button which will bring up
the Region Selection dialog. Select a region to associate with the new initial
condition and press OK. Regions that are not volumes or are already used by
another initial condition will not be selectable.

Once the region has been created, values can be edited. Sub-panes will be
created dynamically based on model parameters as well as the number of solid
phases.

.. note::
    Initial condition regions may overlap. When an overlap occurs, the
    initial condition with the higher IC number will be used at that location.

.. _boundary-conditions:

Boundary Conditions (BCs)
~~~~~~~~~~~~~~~~~~~~~~~~~

The boundary conditions pane is used to define boundary conditions for
a selected Region (defined on each Region pane) for each phase (defined on
Fluid or Solids panes). This is where inflow, outflow, pressure, walls, and
cyclic boundary conditions are created.

To create a new boundary condition, press the |add| button which will bring up
the Region Selection dialog. Next, select a boundary type from the combo box.
Regions will be enabled/disabled based on the selected boundary type and whether
or not the region is already used by a boundary condition. Boundary conditions
must be surfaces, either planes or collections of facets (STL). Pressing OK will
create the boundary condition.

The sub-panes are created dynamically for each boundary condition based on the
boundary condition type as well as other model parameters including the number
of solid species.

.. note::
    Two boundary surfaces must not intersect.

.. _point-sources:

Point Sources (PSs)
~~~~~~~~~~~~~~~~~~~

Point sources (PS) are used in place of mass inlets where either the geometry
and/or grid resolution prohibit proper boundary condition specification. For
example, a point source may be used to model an injector with dimensions smaller
than the grid. Point sources may be defined within a single computational cell,
along a plane, or as a volume of computational cells.

Point sources introduce mass directly into a computational cell unlike a
boundary condition which specifies flow along a cell face. One consequence of
this implementation is that point sources are subjected to convection/diffusion
forces and may not travel parallel to the specified directional preference.
Directional preference is specified with a velocity vector (i.e., PS_U_g,
PS_V_g, etc.), however, directional preference is not required.

To create a new point source, press the |add| button which will bring up
the Region Selection dialog. Select a region to associate with the new point
source and press OK.

The sub-panes are created dynamically for each point source based on the model
parameters including the number of solid species.

.. _internal-surfaces:

Internal Surfaces (ISs)
~~~~~~~~~~~~~~~~~~~~~~~

Internal surfaces allow to create a zero-thickness walls that are normal to one
of the coordinate directions and coincide with one of the faces of the scalar control volume.

Two types of internal surfaces are available: Impermeable, which acts as a free-slip wall for
both gas and solids phases, and Semi-Impermeable, which allows only the gas phase to pass through
the internal surface.

To create a new internal surface, press the |add| button which will bring up
the Region Selection dialog. Select a region to associate with the new Internal Surface.
Select the type of Internal surface and press OK. For Semi-Impermeable surfaces, the fluid permeability
and Internal Resistance Coefficient must be provided as well.

Typically, the region associated with an Internal Surface will be a plane. To specify a large number of
internal surfaces in a region, a 3D region may be selected. In this case, a prefix (X\_, Y\_, or Z\_) is added to the
Internal Surface type to indicate the direction of the internal surfaces; e.g., X_IMPERMEABLE specifies
impermeable internal surfaces parallel to the X coordinate.

Internal surfaces act as free-slip walls in stress computations. This default condition
cannot be changed.

Chemistry
~~~~~~~~~

Setting up a project with chemical reactions is a multi-step process and requires coding the reactions
rates in ``usr_rates.f`` for TFM or ``usr_rates_des.f`` for DEM. The solver needs to be built to take the reaction rates
into account. A brief overview is outlined below. It is recommended to study the Silane pyrolysis example to get familiar with the process.

-  Check the "Enable user-defined subroutine" check-box in the "Model Setup" pane.

-  Check the "Enable Species equations" check-box  and define species in the "Fluid" and "Solids" panes.

-  In the "Chemistry" pane, define chemical equations by pressing the |add| button.
   Define the reaction equation name, and add reactants and products with the |add| button.
   Select the phase and species from the dropdown list, and enter the stoichiometric coefficient.
   Repeat until all products and reactants are defined, and press OK to validate. Specific heats of reactions are
   computed automatically, but this setting can be manually overwritten if needed by checking the "Specify Heat of Reaction" check-box,
   entering the heat of reaction and assigning the gas and solids phases fractions.

-  (Optional) turn on the stiff chemistry solver in the "Chemistry" pane by checking the "Enable stiff chemistry solver" check-box.

-  Edit and save the ``usr_rates.f`` or ``usr_rates_des.f`` UDF, and any other UDF needed for the reaction rates calculation.

-  Build the custom solver by pressing the |Build button| button.

Numerics
~~~~~~~~
The Numerics panes defines various numerical parameters, and are grouped in several sub panes:

-  Residuals: Defines convergence criteria for each type of equation, as well as the maximum number of iterations and residual normalization options.

-  Discretization: Defines temporal, spatial discretization schemes and relaxation factors for each equation

-  Linear Solver: Defines the Linear Equation solver, tolerance and maximum number of iterations for each equation

-  Preconditioner: Defines the preconditioner options for each equation

-  Advanced: Defines less common parameters, such as the maximum Inlet velocity factor, drag and IA theory under relaxation factors and fourth order interpolation scheme

Outputs
~~~~~~~

A variety of output formats can be written by the solver including
restart files (``*.RES``), VTK files (``*.pvd``, ``*.vtp``, ``*.vtu``), legacy Single
Precision files (``*.SP?``), and netCDF (``*.nc``) files. The VTK files
can be read by and displayed in the GUI. Most of these files can be viewed with
the post-processing programs Paraview_ and Visit_.

The Basic sub-pane is where the restart file write frequency as well as the
VTK, SPx, and netCDF outputs can be enabled. Once these are enabled, the
specific sub-pane will be enabled, allowing for specific control over these
outputs.

The VTK output has the most flexibility. Selected particle data or cell data
variables can be exported at a specific region and write frequency. To create a
new VTK output, press the |add| button which will bring up the Region Selection
dialog. Next, select an output type (particle or cell), select a region, and
press OK. The file pastern name, write frequency, and variables can be selected
for the newly created VTK output.

.. include:: /user_manual/output_format_table.rst

Monitors
~~~~~~~~

Planned.

.. _run-pane:

Run
~~~

The Run pane is used to define parameters related to how long the solver
runs, time-step controls, as well as options to cleanly terminate a
solver after a certain amount of time. These options are particularly useful
when running in a queue environment because they allow the solver to cleanly
exit before the job gets killed by the queuing system. This minimizes the
chance of corrupting output files if the process is killed while writing a file.

Visualization window
====================

The visualization window provides a collection 3D views and 2D plots for
visualizing the model setup and model outputs. New windows, or tabs, can
be created by pressing the |add| button. Once the tab has been added,
the type of view can be selected. Tabs can be closed by pressing the
|close| button located on the tabs.

Model
~~~~~

The Model tab is always present and cannot be closed. This 3D view shows
the setup of the project including the background mesh, geometry, and
regions.

The scene can be manipulated with the mouse and the toolbar buttons:

+-----------------+-------------------------------------------------------+
| Icon            | Description                                           |
+=================+=======================================================+
| |overscan|      | Reset view, make all items visible                    |
+-----------------+-------------------------------------------------------+
| |xy|            | Change to XY view                                     |
+-----------------+-------------------------------------------------------+
| |xz|            | Change to XZ view                                     |
+-----------------+-------------------------------------------------------+
| |yz|            | Change to YZ View                                     |
+-----------------+-------------------------------------------------------+
| |perspective|   | Toggle between perspective and parallel projections   |
+-----------------+-------------------------------------------------------+
| |camera|        | Save an image of the current view                     |
+-----------------+-------------------------------------------------------+
| |visibility|    | Change the visibility and properties of actors        |
+-----------------+-------------------------------------------------------+

The visibility menu allows for the manipulation of how the objects in
the scene represented including:

-  changing the visibility with the |visibility| button
-  changing the representation (wire, solids, edges, and points)
-  changing the color
-  changing the transparency of the objects

Plot Tab(s)
~~~~~~~~~~~

The plot tab(s) can be used to graph live statistics about the
solver as it is running. Values such as the time step (dt), number
of iterations (nit), and the simulated time (time) can be plotted. The
plot can be manipulated with the mouse, and a ``right-click`` menu
provides access to options that can be changed as well as export options
for saving an image of the plot or exporting the data to a text file.

.. note::
    Plotting only available when solver is running

.. _vtk-tabs:

VTK Tab(s)
~~~~~~~~~~

The VTK tab provides a way to quickly view results from the solver. For more
complex visualization of solver data, Paraview_ or Visit_ is recommended.

.. _Paraview: https://www.paraview.org/
.. _Visit: https://wci.llnl.gov/simulation/computer-codes/visit/

The tab automatically sets-up VTK pipelines for reading and showing \*.vtu and
\*.vtp files.

.. note::
    The directory is automatically searched every second for
    \*.pvd files. If a \*.pvd file is found, the GUI will read and show
    the new VTK file if the |play| button is pressed.

Results can be "played" as well as manipulated using the
following toolbar:

+-----------------+----------------------------------------------------------------------+
| Icon            | Description                                                          |
+=================+======================================================================+
| |overscan|      | Reset view, make all items visible                                   |
+-----------------+----------------------------------------------------------------------+
| |xy|            | Change to XY view                                                    |
+-----------------+----------------------------------------------------------------------+
| |xz|            | Change to XZ view                                                    |
+-----------------+----------------------------------------------------------------------+
| |yz|            | Change to YZ View                                                    |
+-----------------+----------------------------------------------------------------------+
| |perspective|   | Toggle between perspective and parallel projections                  |
+-----------------+----------------------------------------------------------------------+
| |camera|        | Save an image of the current view                                    |
+-----------------+----------------------------------------------------------------------+
| |visibility|    | Change the visibility and properties of actors                       |
+-----------------+----------------------------------------------------------------------+
| |first|         | Go to the first frame                                                |
+-----------------+----------------------------------------------------------------------+
| |back|          | Go back one frame                                                    |
+-----------------+----------------------------------------------------------------------+
| |play|          | Play available frames, starting at the current frame                 |
+-----------------+----------------------------------------------------------------------+
| |next|          | Go to the next frame                                                 |
+-----------------+----------------------------------------------------------------------+
| |last|          | Go to the last frame                                                 |
+-----------------+----------------------------------------------------------------------+
| |speed|         | Change the playback speed, or the amount of time in between frames   |
+-----------------+----------------------------------------------------------------------+

The visibility menu allows for the manipulation of how the objects in
the scene represented including:

-  visibility
-  the data array used to color
-  color bars
-  transparency

Further options for the points can be adjusted by clicking the |more|
button next to the label including:

-  Maximum number of particles to be displayed
-  The mapper (sprites requires VTK version 7.0+)
-  The glyph object

.. _terminal-window:

Terminal window
===============

The terminal window displays the output of the solver job that would
be displayed when running the solver on the command line. Error messages
and warnings, from both the GUI and a running solver, are displayed
and colored in red.

Informational messages from the GUI unrelated to the solver are colored
in blue.

Mode bar
========

The Mode bar allows switching the GUI between various modes including:

-  Modeler, used to setup a project
-  Nodeworks, future feature to support creation, management, post
   processing, and optimization of projects.

A status bar is also present, showing the current status of the GUI or a
running solver, including a progress bar showing the current
progress of the solver and elapsed time.


.. include:: queue.rst

.. _3D_fluidbed_tutorial:

Three Dimensional Fluid Bed
---------------------------

This tutorial shows how to create a three dimensional fluidized bed
simulation using the two fluid model and the discrete element model (DEM). The
model setup is:

+------------------+--------------------------------------------+
| Property         | Value                                      |
+==================+============================================+
| geometry         | 10 cm diameter x 40 cm                     |
+------------------+--------------------------------------------+
| mesh             | 20 x 60 x 20                               |
+------------------+--------------------------------------------+
| solid diameter   | 200 microns (:math:`200 \times 10^{-6}` m) |
+------------------+--------------------------------------------+
| solid density    | 2500 kg/m\ :sup:`2`                        |
+------------------+--------------------------------------------+
| gas velocity     | 0.25 m/s                                   |
+------------------+--------------------------------------------+
| temperature      | 298 K                                      |
+------------------+--------------------------------------------+
| pressure         | 101325 Pa                                  |
+------------------+--------------------------------------------+

Create a new project
^^^^^^^^^^^^^^^^^^^^

-  (:numref:`fig_new_project`): On the file menu click on the |newfolder| button
-  Create a new project by double-clicking on "Blank" template.
-  Enter a project name and browse to a location for the new project.

.. _fig_new_project:

.. figure:: /media/gui_new_project.png
           :width: 8cm
           :alt: create project

Select model parameters
^^^^^^^^^^^^^^^^^^^^^^^

(:numref:`fig_model_param`): On the ``Model`` pane:

-  Enter a descriptive text in the
   ``Description`` field
-  Select "Two-Fluid Model (MFiX-TFM)" in the ``Solver`` combo-box.

.. _fig_model_param:

.. figure:: /media/gui_3d_tfm_model.png
           :width: 8cm
           :alt: select model parameters

Enter the geometry
^^^^^^^^^^^^^^^^^^
On the ``Geometry`` pane:

-  Create the cylindrical geometry by pressing the ``Add Geometry`` button
   -> ``primitives`` -> ``cylinder``

.. figure:: /media/gui_3d_tfm_add_cylinder.png
           :width: 8cm
           :alt: add cylinder

-  Enter ``40/100`` meters for the cylinder height
-  Enter ``10/2/100`` meters for the cylinder diameter
-  Enter ``30`` for the cylinder resolution
-  Press the autosize button to fit the domain extents to the geometry
-  Extend the height of the cylinder by adding ``0.1`` meters. This will hang
   the stl file outside of the domain, allowing for a sharp and clean cut.

.. figure:: /media/gui_3d_tfm_add_geo.png
            :width: 8cm
            :alt: enter cylinder input

Enter the mesh
^^^^^^^^^^^^^^
On the ``Mesh`` pane:

-  On the ``Background`` sub-pane

  -  Enter ``20`` for the x cell value
  -  Enter ``60`` for the y cell value
  -  Enter ``20`` for the z cell value

.. note::
    This is a fairly coarse grid for a TFM simulation. After completing this
    tutorial, try increasing the grid resolution to better resolve the bubbles.

.. figure:: /media/gui_3d_tfm_add_mesh.png
            :width: 8cm
            :alt: enter mesh

-  On the ``Mesher`` sub-pane

  -  Uncheck ``External Flow``
  -  Enter a value of ``40`` in the ``max facets per cell`` field

Create regions for initial and boundary condition specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Select the ``Regions`` pane. By default, a region that covers the entire domain
is already defined. This is typically used to initialize the flow field and
visualize the results.

-  click the |all_region| button to create a new region to be used for the bed
   initial condition.
-  Enter a name for the region in the ``Name`` field ("bed")
-  Change the color by pressing the ``Color`` button
-  Enter ``0`` in the ``To Y`` field

.. figure:: /media/gui_3d_tfm_add_region1.png
            :width: 8cm
            :alt: create region 1

-  Click the |bottom_region| button to create a new region to be used by the gas
   inlet boundary condition.
-  Enter a name for the region in the ``Name`` field ("inlet")

.. figure:: /media/gui_3d_tfm_add_region2.png
            :width: 8cm
            :alt: create region 2

-  Click the |top_region| button to create a new region to be used by the
   pressure outlet boundary condition.
-  Enter a name for the region in the ``Name`` field ("outlet")

.. figure:: /media/gui_3d_tfm_add_region3.png
            :width: 8cm
            :alt: create region 3

-  Click the |all_region| button to create a new region to be used to select the
   walls.
-  Enter a name for the region in the ``Name`` field ("walls")
-  Check the ``Select Facets`` checkbox
-  Enter ``ymin-0.1`` in the ``From Y`` field
-  Enter ``ymax+0.1`` in the ``To Y`` field
-  Check the ``Slice Facets`` checkbox

All the facets around the outside of the cylinder should now be selected. The
top and bottom cylinder facets should not be selected. This allows for the
standard boundary conditions to be applied.

.. figure:: /media/gui_3d_tfm_add_region4.png
            :width: 8cm
            :alt: create region 4

-  Click the |left_region| button to create a new region to be used to save a
   slice of cells at the center of the domain.
-  Enter a name for the region in the ``Name`` field ("slice")
-  Enter ``0`` in the ``From X`` and ``To X`` fields

.. figure:: /media/gui_3d_tfm_add_region5.png
            :width: 8cm
            :alt: create region 5

Create a solid
^^^^^^^^^^^^^^

On the ``Solids`` pane:

-  Click the |add| button to create a new solid
-  Enter a descriptive name in the ``Name`` field ("glass beads")
-  Keep the model as "Two-Fluid Model (MFiX-TFM)")
-  Enter the particle diameter of ``200e-6`` m in the ``Diameter`` field
-  Enter the particle density of ``2500`` kg/m\ :sup:`2` in the ``Density`` field

.. figure:: /media/gui_3d_tfm_solids.png
            :width: 8cm
            :alt: create a solid

Create Initial Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Initial Conditions`` pane:

-  Select the already populated "Background IC" from the region list.
   This will initialize the entire flow field with air.
-  Enter ``101325`` Pa in the ``Pressure (optional)`` field

-  Create a new Initial Condition by pressing the |add| button
-  Select the region created previously for the bed Initial Condition
   ("bed" region) and click the ``OK`` button.
-  Select the solid (named previously as "glass beads") sub-pane and
   enter a volume fraction of ``0.4`` in the ``Volume Fraction`` field.
   This will fill the bottom half of the domain with glass beads.

.. figure:: /media/gui_3d_tfm_ics.png
            :width: 8cm
            :alt: initial conditions

Create Boundary Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Boundary Conditions`` pane:

-  Create a new Boundary condition by clicking the |add| button
-  On the ``Select Region`` dialog, select "Mass Inflow" from the
   ``Boundary type`` combo-box
-  Select the "inlet" region and click ``OK``
-  On the ``Fluid`` sub-pane, enter a velocity in the ``Y-axial velocity``
   field of ``0.25`` m/s

-  Create another Boundary condition by clicking the |add| button
-  On the ``Select Region`` dialog, select "Pressure Outflow" from the
   ``Boundary type`` combo-box
-  Select the "outlet" region and click ``OK``

.. note::
    The default pressure is already set to 101325 Pa, no changes
    need to be made to the outlet boundary condition.

-  Create another Boundary condition by clicking the |add| button
-  On the ``Select Region`` dialog, select "No Slip Wall" from the
   ``Boundary type`` combo-box
-  Select the "wall" region and click ``OK``


Change numeric parameters
^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Numeric`` pane, ``Residuals`` sub-pane:

-  Enter ``0`` in the ``Fluid Normalization`` field.

Select output options
^^^^^^^^^^^^^^^^^^^^^

On the ``Output`` pane:

-  On the ``Basic`` sub-pane, check the
   ``Write VTK output files (VTU/VTP)`` checkbox
-  Select the ``VTK`` sub-pane
-  Create a new output by clicking the |add| button
-  Select the "Background IC" region from the list to save all the cell data
-  Click ``OK`` to create the output
-  Enter a base name for the ``*.vtu`` files in the ``Filename base``
   field
-  Change the ``Write interval`` to ``0.1`` seconds
-  Select the ``Volume fraction``, ``Pressure``, and ``Velocity vector``
   check-boxes on the ``Fluid`` sub-sub-pane

-  Create another output by clicking the |add| button
-  Select the "Slice" region from the list to save all the cell data
-  Click ``OK`` to create the output
-  Enter a base name for the ``*.vtu`` files in the ``Filename base`` field
-  Change the ``Write interval`` to ``0.1`` seconds
-  Select the ``Volume fraction``, ``Pressure``, and ``Velocity vector``
   check-boxes on the ``Fluid`` sub-sub-pane

.. figure:: /media/gui_3d_tfm_output.png
            :width: 8cm
            :alt: initial conditions

Change run parameters
^^^^^^^^^^^^^^^^^^^^^

On the ``Run`` pane:

-  Change the ``Time step`` to ``1e-3`` seconds
-  Change the ``Maximum time step`` to ``1e-2`` seconds

.. figure:: /media/gui_tfm_2d_run.png
            :width: 8cm
            :alt: new boundary condition

Run the project
^^^^^^^^^^^^^^^

-  Save project by clicking |save| button
-  Run the project by clicking the |play| button
-  On the ``Run`` dialog, select the default executable from the list
-  Click the ``Run`` button to actually start the simulation

View results
^^^^^^^^^^^^

Results can be viewed, and plotted, while the simulation is running.

-  Create a new visualization tab by pressing the |add| in the upper
   right hand corner.
-  Select an item to view, such as plotting the time step (dt) or click
   the ``VTK`` button to view the vtk output files.

.. figure:: /media/gui_tfm_2d_new_output.png
            :width: 8cm
            :alt: new boundary condition


Convert this project into a 3D DEM simulation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-  On the ``Model`` pane, change the solver to ``MFiX-DEM``
-  On the ``Mesh`` pane, coarsen the grid to `15`, `45`, and `15` cells in the
   in the x, y, and z direction, respectfully.

.. note::
    The grid resolution needs to be coarser because we are drastically
    increasing the particle diameter below. The fluid grid cell size has to be
    bigger than the particle size.

-  On the ``Solids`` pane, change that particle diameter to ``5.0000e-03`` to
   get a more reasonable particle count for tutorial purposes.
-  On the ``Solids`` pane, ``DEM`` sub-pane, check the ``Enable automatic
   particle generation``
-  On the ``Boundary Conditions`` pane, change the ``inlet`` ``Y-axial
   velocity`` to ``0.6`` m/s
-  On the ``Output`` pane

   -  Delete the ``all`` or ``Background_IC`` output
   -  Create a new output, change the ``Output type`` to ``Particle data`` and
      select the ``Background_IC`` region.
   -  Change the write frequency to ``0.01``
   -  Select the ``Diameter`` and ``Translational Velocity`` data

- Run the simulation

.. figure:: /media/gui_3d_dem_results.png
            :width: 8cm
            :alt: new boundary condition

.. include:: /user_manual/icons.rst

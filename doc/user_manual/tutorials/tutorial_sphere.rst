3D Single phase flow over a sphere
----------------------------------

This tutorial shows how to create a three dimensional single phase flow over a
sphere.

+------------------+-----------------------------------+
| Property         | Value                             |
+==================+===================================+
| geometry         | 60 cm 20 cm x 20 cm               |
+------------------+-----------------------------------+
| mesh             | 30 x 10 x 10                      |
+------------------+-----------------------------------+
| gas velocity     | 1 m/s                             |
+------------------+-----------------------------------+
| temperature      | 298 K                             |
+------------------+-----------------------------------+
| pressure         | 101325 Pa                         |
+------------------+-----------------------------------+

Create a new project
^^^^^^^^^^^^^^^^^^^^

-  On the file menu click on the |newfolder| button
-  Create a new project by double-clicking on "Blank" template.
-  Enter a project name and browse to a location for the new project.

.. note::
    A new project directory will be created in the location
    directory, with the name being the project name.

.. figure:: /media/gui_new_project.png
            :width: 8cm
            :alt: create project

Select model parameters
^^^^^^^^^^^^^^^^^^^^^^^
On the ``Model`` pane:

-  Enter a descriptive text in the
   ``Description`` field
-  Select "Single Phase" in the ``Solver`` combo-box.

.. figure:: /media/gui_sphere_model.png
            :width: 8cm
            :alt: select model parameters

Enter the geometry
^^^^^^^^^^^^^^^^^^

On the ``Geometry`` pane enter the domain extents:

-  ``60/100`` meters for the maximum x value
-  ``20/100`` meters for the maximum y value
-  ``20/100`` meters for the maximum z value

Next, add a sphere by clicking the |geometry| button -> primitives -> sphere.
This adds a sphere constructed of triangles (STL) to the project.

.. figure:: /media/gui_sphere_add_geometry.png
            :width: 8cm
            :alt: enter geometry

Change the position and radius of the sphere so that it is located in the domain
by entering the following:

-  ``10/100`` for the center X position
-  ``10/100`` for the center Y position
-  ``10/100`` for the center Z position

Change the radius of the sphere by entering:

- ``5/100`` for the radius.

.. figure:: /media/gui_sphere_geometry.png
            :width: 8cm
            :alt: enter geometry

Enter the mesh
^^^^^^^^^^^^^^

On the ``Mesh`` pane, ``Background`` sub-pane:

-  Enter ``30`` for the x cell value
-  Enter ``10`` for the y cell value
-  Enter ``10`` for the z cell value

.. figure:: /media/gui_sphere_mesh.png
            :width: 8cm
            :alt: enter mesh

On the ``Mesh`` pane, ``Mesher`` sub-pane:

-  Enter ``30`` in the ``max facets per cell`` to allow more facets in a single
   cell. Generally, the default value is too small.

Create regions for initial and boundary condition specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Select the ``Regions`` pane. By default, a region that covers the
entire domain is already defined.

A region for the sphere is needed to apply a wall boundary condition to:

-  click the |all_region| button to create a region that encompasses the entire
   domain
-  change the name of the region to a descriptive ``name`` such as "sphere"
-  Check the ``Select Facets (STL)`` check-box to turn the region into a STL
   region. The facets of the sphere should now be selected.

.. figure:: /media/gui_sphere_region1.png
            :width: 8cm
            :alt: create region 1

Create a region to apply a mass inflow boundary condition to:

-  Click the |left_region|
-  Enter a name for the region in the ``Name`` field ("inlet")

Create a region to apply a pressure outlet boundary condition to:

-  Click the |right_region|
-  Enter a name for the region in the ``Name`` field ("outlet")

Finally, create a slice through the center to use as a vtk output region:

-  Click the |front_region|
-  Enter a name for the region in the ``Name`` field ("slice")
-  Enter ``zmax/2`` in both the ``From Z`` and ``To Z`` fields to move the region
   to the center of the domain

.. figure:: /media/gui_sphere_region2.png
            :width: 8cm
            :alt: create region 2


Create Initial Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^

-  Select the ``Initial Conditions`` pane
-  Select the already populated "Background IC" from the region list.
   This will initialize the entire flow field with air.
-  Enter ``101325`` Pa in the ``Pressure (optional)`` field

Create Boundary Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^

Select the ``Boundary Conditions`` pane and create a wall boundary condition for
the sphere by:

-  clicking the |add| button
-  On the ``Select Region`` dialog, select "No Slip Wall" from the
   ``Boundary type`` combo-box
-  Select the "sphere" region and click ``OK``

Add a mass inlet boundary condition by:

-  clicking the |add| button
-  On the ``Select Region`` dialog, select "Mass Inflow" from the
   ``Boundary type`` combo-box
-  Select the "inlet" region and click ``OK``
-  On the "Fluid" sub-pane, enter a velocity in the ``X-axial velocity``
   field of ``1.0`` m/s

Finally, create a pressure outlet boundary condition by:

-  clicking the |add| button
-  On the ``Select Region`` dialog, select "Pressure Outflow" from the
   ``Boundary type`` combo-box
-  Select the "outlet" region and click ``OK``

.. note::
    The default pressure is already set to 101325 Pa, no changes
    need to be made to the outlet boundary condition.


Select output options
^^^^^^^^^^^^^^^^^^^^^

On the ``Output`` pane:

-  On the ``Basic`` sub-pane, check the
   ``Write VTK output files (VTU/VTP)`` checkbox

.. figure:: /media/gui_tfm_2d_output.png
            :width: 8cm
            :alt: new boundary condition

-  Select the ``VTK`` sub-pane
-  Create a new output by clicking the |add| button
-  Select the "slice" region from the list to save cell data at a slice through
   the domain
-  Click ``OK`` to create the output
-  Change the ``Write interval`` to "0.01" seconds
-  Select the ``Pressure``, ``Velocity vector``, ``Velocity x-component``,
   ``Velocity y-component``, and ``Velocity z-component``
   check-boxes on the ``Fluid`` sub-sub-pane

Run the project
^^^^^^^^^^^^^^^^

-  Save project by clicking |save| button
-  Run the project by clicking the |play| button
-  On the ``Run`` dialog, select the executable
-  Click the ``Run`` button to actually start the simulation

.. figure:: /media/gui_run_dialog.png
            :width: 8cm
            :alt: new boundary condition

View results
^^^^^^^^^^^^

Results can be viewed, and plotted, while the simulation is running.

-  Create a new visualization tab by pressing the |add| in the upper
   right hand corner.
-  Select an item to view, such as plotting the time step (dt) or click
   the ``VTK`` button to view the vtk output files.

.. figure:: /media/gui_sphere_results.png
            :width: 8cm
            :alt: results

.. include:: /user_manual/icons.rst

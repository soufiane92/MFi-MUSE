3D DEM Hopper
-------------

.. raw:: html

    <iframe width="560" height="315"
    src="https://www.youtube.com/embed/kEiPs-phndg" frameborder="0"
    allowfullscreen></iframe>

This tutorial shows how to create a three dimensional granular flow DEM
simulation. The model setup is:

+------------------+-----------------------------------+
| Property         | Value                             |
+==================+===================================+
| geometry         | 5 cm diameter hopper              |
+------------------+-----------------------------------+
| mesh             | 10 x 25 x 10                      |
+------------------+-----------------------------------+
| solid diameter   | 0.003 m                           |
+------------------+-----------------------------------+
| solid density    | 2500 kg/m\ :sup:`2`               |
+------------------+-----------------------------------+
| gas velocity     | NA                                |
+------------------+-----------------------------------+
| temperature      | 298 K                             |
+------------------+-----------------------------------+
| pressure         | NA                                |
+------------------+-----------------------------------+

Create a new project
^^^^^^^^^^^^^^^^^^^^

-  On the file menu click on the |newfolder| button
-  Create a new project by double-clicking on "Blank" template.
-  Enter a project name and browse to a location for the new project.

.. figure:: /media/gui_new_project.png
            :width: 8cm
            :alt: create project

Select model parameters
^^^^^^^^^^^^^^^^^^^^^^^

-  On the ``Model`` pane, enter a descriptive text in the
   ``Description`` field
-  Select "Discrete Element Model (MFiX-DEM)" in the ``Solver`` combo-box.
-  Check the ``Disable Fluid Solver (Pure Granular Flow)`` check-box.

.. figure:: /media/gui_hopper_model.png
            :width: 8cm
            :alt: select model parameters

Enter the geometry
^^^^^^^^^^^^^^^^^^

On the ``Geometry`` pane:

-  Select the |wand| wizard menu and select the ``hopper`` wizard
-  Select ``apply`` to build the hopper stl file
-  Press the ``Autosize`` button to fit the domain extents to the geometry
-  Press the |overscan| Reset View icon on the top-left corner of the Model window

.. figure:: /media/gui_hopper_wizard.png
            :width: 8cm
            :alt: hopper wizard

Enter the mesh
^^^^^^^^^^^^^^

On the ``Mesh`` pane, ``Background`` sub-pane:

-  Enter ``10`` for the x cell value
-  Enter ``25`` for the y cell value
-  Enter ``10`` for the z cell value

.. figure:: /media/gui_hopper_mesh.png
            :width: 8cm
            :alt: enter mesh

On the ``Mesh`` pane, ``Mesher`` sub-pane:

-  Uncheck the "External Flow" check-box
-  Enter ``1.0`` in the ``Allocation factor`` field
-  Enter ``0.0`` in the ``Facet Angle Tolerance``
-  Enter ``30`` in the ``max facets per cell`` to allow more facets in a single
   cell. Generally, the default value is too small.

Create regions for initial and boundary condition specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Select the ``Regions`` pane. By default, a region that covers the
entire domain is already defined.

A region for the hopper walls (STL) is needed to apply a wall boundary condition
to:

-  click the |all_region| button to create a region that encompasses the entire
   domain
-  change the name of the region to a descriptive ``name`` such as "walls"
-  Check the ``Select Facets (STL)`` check-box to turn the region into a STL
   region. The facets of the hopper should now be selected.

In order to allow the particles to leave the domain through the bottom, the
facets at the bottom of the hopper need to be deselected. There are three ways
to accomplish this:

1.  Change the ``Y Min`` domain extent value on the geometry pane by a small
    amount so that those facets fall outside the simulation domain (change the
    value to ``-0.0974``).
2.  Change the ``Y From`` region extent value on the regions pane for this STL
    region by a small amount so that those facets fall outside the region
    (change the value to ``ymin+0.0001``)
3.  Or, use the ``Filter facets based on normals`` option on the region pane.

For this example, use option 3 to filter the facets that have normals pointed in
the ``Y`` direction by:

-  Check the ``Filter facets based on normals`` check-box
-  Enter a vector pointed in the positive ``Y`` direction by entering ``0``,
   ``1``, and ``0`` in the x, y, z fields, respectively
-  Check the ``Include equilibrant vector`` to include facets with normals in
   the opposite direction of the vector (-x, -y, -z)
-  Enter an angle of ``10`` in the ``angle`` field to include facets with
   normals within 10 degrees of the filter vector
-  Check the ``Invert Selection`` check box to de-select facets that fall along
   the filter vector and select all the other facets

All the facets, except for the facets at the top and bottom, of the hopper
should now be selected.

.. figure:: /media/gui_hopper_region_stl.png
            :width: 8cm
            :alt: create region 1

Create a region to apply a pressure outlet boundary condition to allow
particles to leave the domain:

-  Click the |bottom_region|
-  Enter a name for the region in the ``Name`` field ("outlet")

Finally, a region to initialize the solids to:

-  Click the |all_region|
-  Enter a name for the region in the ``Name`` field ("solids")
-  Enter ``ymin/3`` in the ``From Y`` field
-  Enter ``0`` in the ``To Y`` field

Create a solid
^^^^^^^^^^^^^^

On the ``Solids`` pane

-  Click the |add| button to create a new solid
-  Enter a descriptive name in the ``Name`` field ("solids")
-  Keep the model as "Discrete Element Model (MFiX-DEM)")
-  Enter the particle diameter of ``0.003`` m in the ``Diameter`` field
-  Enter the particle density of ``2500`` kg/m\ :sup:`2` in the ``Density`` field

In the ``DEM`` sub-pane, check ``Enable automatic particle generation`` check-box

Create Initial Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``ICs`` (Initial Conditions) pane

-  Click the |add| button to create a new initial condition
-  On the ``Select Region`` dialog, select the "solids" region and click ``OK``
-  On the ``solids`` sub-pane, enter ``0.4`` in the ``Volume fraction`` field

Create Boundary Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^

Select the ``BCs`` (Boundary Conditions) pane and create a wall boundary condition for
the hopper by:

-  clicking the |add| button
-  On the ``Select Region`` dialog, select "No Slip Wall" from the
   ``Boundary type`` combo-box
-  Select the "walls" region and click ``OK``

Finally, create a pressure outlet boundary condition by:

-  clicking the |add| button
-  On the ``Select Region`` dialog, select "Pressure Outflow" from the
   ``Boundary type`` combo-box
-  Select the "outlet" region and click ``OK``


Select output options
^^^^^^^^^^^^^^^^^^^^^

-  Select the ``Output`` pane
-  On the ``Basic`` sub-pane, check the
   ``Write VTK output files (VTU/VTP)`` checkbox

.. figure:: /media/gui_tfm_2d_output.png
            :width: 8cm
            :alt: new boundary condition

-  Select the ``VTK`` sub-pane
-  Create a new output by clicking the |add| button
-  On the "select region" dialog, select "particle data" from the
   ``output type`` combobox
-  Select the "Background IC" region from the list to save particle data over
   the entire domain
-  Click ``OK`` to create the output
-  Change the ``Write interval`` to ``0.01`` seconds
-  Select the ``Diameter`` and  ``Translational velocity`` check-boxes

Run the project
^^^^^^^^^^^^^^^

-  Save project by clicking |save| button
-  Run the project by clicking the |play| button
-  On the ``Run`` dialog, select the default solver
-  Click the ``Run`` button to actually start the simulation

.. figure:: /media/gui_run_dialog.png
            :width: 8cm
            :alt: run

View results
^^^^^^^^^^^^

Results can be viewed, and plotted, while the simulation is running.

-  Create a new visualization tab by pressing the |add| in the upper
   right hand corner.
-  Select an item to view, such as plotting the time step (dt) or click
   the ``VTK`` button to view the vtk output files.

.. figure:: /media/gui_hopper_results.png
            :width: 8cm
            :alt: results

.. include:: /user_manual/icons.rst

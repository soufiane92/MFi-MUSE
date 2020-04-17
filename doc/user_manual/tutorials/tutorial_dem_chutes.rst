.. _dem_chutes_tutorial:

DEM Granular Flow Chutes
------------------------

This tutorial shows how to create a three dimensional pure granular flow
simulation of a series of cylindrical chutes. The model setup is:

+------------------+--------------------------------------------+
| Property         | Value                                      |
+==================+============================================+
| geometry         | 1.75 m  x 0.94 m x 0.2 m                   |
+------------------+--------------------------------------------+
| mesh             | 20 x 20 x 10                               |
+------------------+--------------------------------------------+
| solid diameter   | 12.5 mm (0.0125 m)                         |
+------------------+--------------------------------------------+
| solid density    | 2500 kg/m\ :sup:`2`                        |
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

-  Enter a descriptive text in the ``Description`` field
-  Select "Discrete Element Model (MFiX-DEM)" in the ``Solver`` combo-box.
-  Check the ``Disable Fluid Solver`` checkbox.

.. _fig_model_param:

.. figure:: /media/gui_dem_chutes_model.png
           :width: 8cm
           :alt: select model parameters

Enter the geometry
^^^^^^^^^^^^^^^^^^
On the ``Geometry`` pane:

-  (:numref:`fig_add_cylinder`): Create the cylindrical geometry by pressing |geometry|
   -> ``primitives`` -> ``cylinder``

   -  Enter ``0.1`` meters for the cylinder radius
   -  Enter ``30`` for the cylinder resolution

.. _fig_add_cylinder:

.. figure:: /media/gui_dem_chutes_geo1.png
           :width: 8cm
           :alt: add cylinder

-  Create another cylindrical geometry by pressing |geometry|
   -> ``primitives`` -> ``cylinder``

   -  Enter ``1.2`` meters for the cylinder height
   -  Enter ``0.08`` meters for the cylinder radius
   -  Enter ``30`` for the cylinder resolution

-  (:numref:`fig_difference`): Subtract the smaller radius cylinder from the larger radius cylinder by

   -  Selecting the larger radius cylinder (named ``cylinder``)
   -  While holding ``ctrl`` select the smaller radius cylinder (named
      ``cylinder1``)
   -  Press the |difference| button

.. _fig_difference:

.. figure:: /media/gui_dem_chutes_diff1.png
           :width: 8cm
           :alt: apply difference

-  Slice the tube in half by:

   -  Add a box: |geometry| -> ``primitives`` -> ``box``

     -  Change the ``Center X`` value to ``0.5`` m
     -  Change the ``Y Length`` to ``1.2`` m

   -  Select the tube (named ``difference``)
   -  While holding ``ctrl`` select the box (named ``box``)
   -  Press the |difference| button

-  Rotate the chute by:

   -  Select the chute (named ``difference1``)
   -  (:numref:`fig_add_transform`): Add a filter: |filter| -> ``transform``
   -  Enter a value of ``0.8`` m in the ``Translate Y`` field.
   -  Enter a value of ``70`` in the ``Rotation Z`` field.

.. _fig_add_transform:

.. figure:: /media/gui_dem_chutes_filter1.png
           :width: 8cm
           :alt: add filter

-  Copy the chute by pressing the |copy| button with the chute selected (named
   ``transform``)

   -  Enter a value of ``0.8`` in the ``Translate X`` field
   -  Enter a value of ``0.6`` in the ``Translate Y`` field
   -  Enter a value of ``120`` in the ``Rotation Z`` field

-  Copy the chute again by pressing the |copy| button with the chute selected
   (named ``transform1``)

   -  Enter a value of ``0.0`` in the ``Translate X`` field
   -  Enter a value of ``0.3`` in the ``Translate Y`` field
   -  Enter a value of ``70`` in the ``Rotation Z`` field

-  Press the `autosize` button to fit the domain extents to the geometry

.. figure:: /media/gui_dem_chutes_geo_final.png
            :width: 8cm
            :alt: enter cylinder input

Enter the mesh
^^^^^^^^^^^^^^
On the ``Mesh`` pane:

-  On the ``Background`` sub-pane

  -  Enter ``20`` for the x cell value
  -  Enter ``20`` for the y cell value
  -  Enter ``10`` for the z cell value

-  On the ``Mesher`` sub-pane

  -  Enter a value of ``100`` in the ``max facets per cell`` field

Create regions for initial and boundary condition specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Select the ``Regions`` pane.

-  Create a region to be used for the wall boundary condition

   -  click the |all_region| button to create a new region
   -  Enter a name for the region in the ``Name`` field ("walls")
   -  Change the color by pressing the ``Color`` button
   -  Check the ``Select Facets (STL)`` checkbox

-  Create a region to be used as the mass inflow boundary condition

   -  Click the |top_region| button to create a new region
   -  Enter a name for the region in the ``Name`` field ("inlet")
   -  Enter a value of ``-0.4`` m in the ``From X`` field
   -  Enter a value of ``-0.2`` m in the ``To X`` field

Create a solid
^^^^^^^^^^^^^^

On the ``Solids`` pane:

-  Click the |add| button to create a new solid
-  Enter a descriptive name in the ``Name`` field ("glass beads")
-  Enter the particle diameter of ``1/80`` m in the ``Diameter`` field
-  Enter the particle density of ``2500`` kg/m\ :sup:`2` in the ``Density`` field

On the ``DEM`` sub pane:

-  check the ``Enable automatic particle generation`` checkbox
   and keep defaults values for all other settings.

Create Initial Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Initial Conditions`` pane leave the default initial condition.

Create Boundary Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Boundary Conditions`` pane:

-  Create a new Boundary condition by clicking the |add| button
-  On the ``Select Region`` dialog, select "Mass Inflow" from the
   ``Boundary type`` combo-box
-  Select the "inlet" region and click ``OK``
-  On the ``glass beads`` sub-pane:

   -  Enter a value of ``0.1`` in the ``volume fraction`` field.
   -  Enter a velocity in the ``Y-axial velocity``
      field of ``-0.1`` m/s

-  Create another Boundary condition by clicking the |add| button
-  On the ``Select Region`` dialog, select "No Slip Wall" from the
   ``Boundary type`` combo-box
-  Select the "walls" region and click ``OK``

Select output options
^^^^^^^^^^^^^^^^^^^^^

On the ``Output`` pane:

-  On the ``Basic`` sub-pane, check the
   ``Write VTK output files (VTU/VTP)`` check box
-  Select the ``VTK`` sub-pane
-  Create a new output by clicking the |add| button
-  Select ``Particle Data`` in the ``Output type`` combo box.
-  Select the "Background IC" region from the list to save all the cell data
-  Click ``OK`` to create the output
-  Enter a base name for the ``*.vtu`` files in the ``Filename base``
   field
-  Change the ``Write interval`` to ``0.1`` seconds
-  Select the ``Diameter`` and ``Translational Velocity`` check boxes.

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

.. figure:: /media/gui_dem_chutes_results.png
            :width: 8cm
            :alt: new boundary condition

.. include:: /user_manual/icons.rst

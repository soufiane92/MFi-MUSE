Two Dimensional Fluid Bed, Discrete Element Model (DEM)
-------------------------------------------------------

This tutorial shows how to create a two dimensional fluidized bed
simulation using the Discrete Element Model. The model setup is:

+------------------+----------------------------------------------+
| Property         | Value                                        |
+==================+==============================================+
| geometry         | 5 cm x 10 cm x 0.2 cm                        |
+------------------+----------------------------------------------+
| mesh             | 20 x 40 x 1                                  |
+------------------+----------------------------------------------+
| solid diameter   | 1000 microns (:math:`1000 \times 10^{-6}` m) |
+------------------+----------------------------------------------+
| solid density    | 2500 kg/m\ :sup:`2`                          |
+------------------+----------------------------------------------+
| gas velocity     | 3.0 m/s                                      |
+------------------+----------------------------------------------+
| temperature      | 298 K                                        |
+------------------+----------------------------------------------+
| pressure         | 101325 Pa                                    |
+------------------+----------------------------------------------+

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
-  Select "Discrete Element Model (MFiX-DEM)" in the ``Solver``
   combo-box.

.. figure:: /media/gui_dem_2d_model.png
            :width: 8cm
            :alt: model parameters

Enter the geometry
^^^^^^^^^^^^^^^^^^

On the ``Geometry`` pane:

-  Enter ``5/100`` meters for the maximum x value
-  Enter ``10/100`` meters for the maximum y value
-  Enter ``2/1000`` meters for the maximum z value

.. figure:: /media/gui_dem_2d_geometry.png
            :width: 8cm
            :alt: enter geometry

Enter the mesh
^^^^^^^^^^^^^^

On the ``Mesh`` pane, ``Background`` sub-pane:

-  Enter ``20`` for the x cell value
-  Enter ``40`` for the y cell value
-  Enter ``1`` for the z cell value

.. note::
    Since there is only one cell in the Z direction, this model is
    effectively as 2D simulation.

.. figure:: /media/gui_dem_2d_mesh.png
            :width: 8cm
            :alt: enter mesh

Create regions for initial and boundary condition specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Regions`` pane:

-  click the |all_region| button to create a new region that covers the entire
   domain to be used for the bed initial condition.
-  Enter a name for the region in the ``Name`` field ("bed")
-  Change the ``To Y`` field to be "ymax/2"
-  Click the |bottom_region| button to create a new region to be used by the gas
   inlet boundary condition.
-  Enter a name for the region in the ``Name`` field ("inlet")
-  Click the |top_region| button to create a new region to be used by pressure
   outlet boundary condition.
-  Enter a name for the region in the ``Name`` field ("outlet")

.. figure:: /media/gui_tfm_2d_region3.png
            :width: 8cm
            :alt: create regions

Create a solid
^^^^^^^^^^^^^^

On the ``Solids`` pane, ``Materials`` sub-pane:

-  Click the |add| button to create a new solid
-  Enter a descriptive name in the ``Name`` field ("solids")
-  Change the solids model to "Discrete Element Model (MFiX-DEM)")
-  Enter the particle diameter of ``0.001`` m in the ``Diameter`` field
-  Enter the particle density of ``2500`` kg/m\ :sup:`2` in the ``Density`` field

.. figure:: /media/gui_dem_2d_solids.png
            :width: 8cm
            :alt: new boundary condition

-  Select the ``Solids`` pane, ``DEM`` sub-pane
-  Check the ``Enable automatic particle generation`` checkbox, so that
   the bed Initial Condition, defined later, will be filled with solids

.. figure:: /media/gui_dem_2d_solids2.png
            :width: 8cm
            :alt: new boundary condition

Create Initial Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Initial Conditions`` pane:

-  Select the already populated "Background IC" from the region list.
   This will initialize the entire flow field with air.
-  Enter ``101325`` Pa in the ``Pressure (optional)`` field

.. figure:: /media/gui_tfm_2d_ics1.png
            :width: 8cm
            :alt: new boundary condition

-  Create a new Initial Condition by pressing the |add| button
-  Select the region created previously for the bed Initial Condition
   ("bed" region) and click the ``OK`` button.

.. figure:: /media/gui_tfm_2d_newic.png
            :width: 8cm
            :alt: new boundary condition

-  Select the solid (named previously as "solid") sub-pane and enter a
   volume fraction of ``0.4`` in the ``Volume Fraction`` field. This will
   fill the bottom half of the domain with solids.

.. figure:: /media/gui_dem_2d_ics2.png
            :width: 8cm
            :alt: new boundary condition

Create Boundary Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Boundary Conditions`` pane:

-  Create a new Boundary condition by clicking the |add| button
-  On the ``Select Region`` dialog, select "Mass Inflow" from the
   ``Boundary type`` combo-box
-  Select the "inlet" region and click ``OK``

.. figure:: /media/gui_tfm_2d_newbc1.png
            :width: 8cm
            :alt: new boundary condition

-  On the "Fluid" sub-pane, enter a velocity in the ``Y-axial velocity``
   field of "3" m/s

.. figure:: /media/gui_tfm_2d_bcs1.png
            :width: 8cm
            :alt: new boundary condition

-  Create another Boundary condition by clicking the |add| button
-  On the ``Select Region`` dialog, select "Pressure Outflow" from the
   ``Boundary type`` combo-box
-  Select the "outlet" region and click ``OK``

.. note::
    The default pressure is already set to 101325 Pa, no changes
    need to be made to the outlet boundary condition.

 .. figure:: /media/gui_tfm_2d_newbc2.png
             :width: 8cm
             :alt: new boundary condition

.. note::
    By default, boundaries that are left undefined (here the left,
    right, front, and back planes) will behave as No-Slip walls.

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
-  Select "Particle Data" from the 'Output type' drop-down.
-  Select the "Background IC" region from the list to save all the
   particle data
-  Click ``OK`` to create the output

.. figure:: /media/gui_dem_2d_newoutput.png
            :width: 8cm
            :alt: new boundary condition

-  Enter a base name for the ``*.vtu`` files in the ``Filename base``
   field ("particles")
-  Change the ``Write interval`` to ``0.1`` seconds
-  Select the ``Diameter`` and ``Translational Velocity`` check-boxes

.. figure:: /media/gui_dem_2d_output_vtk.png
            :width: 8cm
            :alt: new boundary condition

Run the project
^^^^^^^^^^^^^^^

-  Save project by clicking |save| button
-  Run the project by clicking the |play| button
-  On the ``Run`` dialog, select the executable from the combo-box
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
-  On the ``VTK`` results tab, the visibility and representation of the
   ``*.vtk`` files can be controlled with the ``Visibility`` menu.

.. figure:: /media/gui_vtk_visible_dialog.png
            :width: 8cm
            :alt: new boundary condition

-  Change frames with the |first|, |back|, |next|, and |last| buttons
-  Click the |play| button to play the available vtk files.
-  Change the playback speed with the |speed| button

.. figure:: /media/gui_dem_2d_vtk_view.png
            :width: 8cm
            :alt: vtk view

.. include:: /user_manual/icons.rst

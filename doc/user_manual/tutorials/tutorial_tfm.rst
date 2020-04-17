.. _2DFB_TFM_tutorial:

Two Dimensional Fluid Bed, Two Fluid Model (TFM)
------------------------------------------------

.. raw:: html

    <iframe width="560" height="315"
    src="https://www.youtube.com/embed/rZgdGH2pkx4" frameborder="0"
    allowfullscreen></iframe>

This tutorial shows how to create a two dimensional fluidized bed
simulation using the two fluid model. The model setup is:

+------------------+--------------------------------------------+
| Property         | Value                                      |
+==================+============================================+
| geometry         | 10 cm x 30 cm                              |
+------------------+--------------------------------------------+
| mesh             | 20 x 60                                    |
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

.. note::
    A new project directory will be created in the location
    directory, with the name being the project name.

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

.. figure:: /media/gui_tfm_2d_model.png
           :width: 8cm
           :alt: select model parameters

Enter the geometry
^^^^^^^^^^^^^^^^^^
On the ``Geometry`` pane:

-  Select the ``2 Dimensional`` checkbox
-  Enter ``10/100`` meters for the maximum x value
-  Enter ``30/100`` meters for the maximum y value

.. note::
    We could have entered 0.1 and 0.3 to define the domain extents, but this
    example shows that simple mathematical expressions are allowed.

.. figure:: /media/gui_tfm_2d_geometry.png
            :width: 8cm
            :alt: enter geometry

Enter the mesh
^^^^^^^^^^^^^^
On the ``Mesh`` pane:

-  ``Background`` sub-pane
-  Enter ``20`` for the x cell value
-  Enter ``60`` for the y cell value

.. figure:: /media/gui_tfm_2d_mesh.png
            :width: 8cm
            :alt: enter mesh

Create regions for initial and boundary condition specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Select the ``Regions`` pane. By default, a region that covers the entire domain
is already defined. This is typically used to initialize the flow field and
visualize the results.

-  click the |add| button to create a new region to be used for the bed
   initial condition.
-  Enter a name for the region in the ``Name`` field ("bed")
-  Change the color by pressing the ``Color`` button
-  Enter ``xmin`` or ``min`` in the ``From X`` field
-  Enter ``xmax`` or ``max`` in the ``To X`` field
-  Enter ``ymin`` or ``min`` in the ``From Y`` field
-  Enter ``ymax/2`` or ``max/2`` in the ``To Y`` field
-  Enter ``zmin`` or ``min`` in the ``From Z`` field
-  Enter ``zmax`` or ``max`` in the ``To Z`` field

.. note::
    Here we could have entered numerical values for the coordinates, but it
    is recommended to use parameters (xmin, xmax etc.) when possible. These
    parameters will update automatically if the "Domain Extents" change.

.. figure:: /media/gui_tfm_2d_region1.png
            :width: 8cm
            :alt: create region 1

-  Click the |bottom_region| button to create a new region with the ``From`` and
   ``To`` fields already filled out for a region at the bottom of the
   domain, to be used by the gas inlet boundary condition. ``From Y``
   should equal ``To Y``, defining an XZ-plane.
-  Enter a name for the region in the ``Name`` field ("inlet")

.. figure:: /media/gui_tfm_2d_region2.png
            :width: 8cm
            :alt: create region 2

-  Click the |top_region| button to create a new region with the ``From`` and
   ``To`` fields already filled out for a region at the top of the
   domain, to be used by the pressure outlet boundary condition.
   ``From Y`` should equal ``To Y``, defining an XZ-plane.
-  Enter a name for the region in the ``Name`` field ("outlet")

.. figure:: /media/gui_tfm_2d_region3.png
            :width: 8cm
            :alt: create region 3

Create a solid
^^^^^^^^^^^^^^

On the ``Solids`` pane:

-  Click the |add| button to create a new solid
-  Enter a descriptive name in the ``Name`` field ("glass beads")
-  Keep the model as "Two-Fluid Model (MFiX-TFM)")
-  Enter the particle diameter of ``200e-6`` m in the ``Diameter`` field
-  Enter the particle density of ``2500`` kg/m\ :sup:`2` in the ``Density`` field

.. figure:: /media/gui_tfm_2d_solids.png
            :width: 8cm
            :alt: create a solid

Create Initial Conditions
^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Initial Conditions`` pane:

-  Select the already populated "Background IC" from the region list.
   This will initialize the entire flow field with air.
-  Enter ``101325`` Pa in the ``Pressure (optional)`` field

.. figure:: /media/gui_tfm_2d_ics1.png
            :width: 8cm
            :alt: initial condition pane

-  Create a new Initial Condition by pressing the |add| button
-  Select the region created previously for the bed Initial Condition
   ("bed" region) and click the ``OK`` button.

.. figure:: /media/gui_tfm_2d_newic.png
            :width: 8cm
            :alt: create new initial condition

-  Select the solid (named previously as "glass beads") sub-pane and
   enter a volume fraction of ``0.4`` in the ``Volume Fraction`` field.
   This will fill the bottom half of the domain with glass beads.

.. figure:: /media/gui_tfm_2d_ics2.png
            :width: 8cm
            :alt: initial condition pane2

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
   field of "0.25" m/s

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
    By default, boundaries that are left undefined (here the left
    and right planes) will behave as No-Slip walls.

Change numeric parameters
^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Numeric`` pane, ``Residuals`` sub-pane:

-  Enter ``0`` in the ``Fluid Normalization`` field.

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
-  Select the "Background IC" region from the list to save all the cell
   data
-  Click ``OK`` to create the output

.. figure:: /media/gui_tfm_2d_newoutput.png
            :width: 8cm
            :alt: new boundary condition

-  Enter a base name for the ``*.vtu`` files in the ``Filename base``
   field
-  Change the ``Write interval`` to ``0.1`` seconds
-  Select the ``Volume fraction``, ``Pressure``, and ``Velocity vector``
   check-boxes on the ``Fluid`` sub-sub-pane

.. figure:: /media/gui_tfm_2d_output_vtk.png
            :width: 8cm
            :alt: new boundary condition

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

.. figure:: /media/gui_tfm_2d_new_output.png
            :width: 8cm
            :alt: new boundary condition

-  On the ``VTK`` results tab, the visibility and representation of the
   ``*.vtk`` files can be controlled with the ``Visibility`` menu.

.. figure:: /media/gui_vtk_visible_dialog.png
            :width: 8cm
            :alt: new boundary condition

-  Change frames with the |first|, |back|, |next|, and |last| buttons
-  Click the |play| button to play the available vtk files.
-  Change the playback speed with the |speed| button

.. figure:: /media/gui_tfm_2d_vtk_view.png
            :width: 8cm
            :alt: new boundary condition

Increase grid resolution
^^^^^^^^^^^^^^^^^^^^^^^^

By increasing the grid resolution, the bubbles will be better resolved.

-  On the ``Mesh`` pane, change the ``X Cells`` to 80 and the ``Y Cells`` to 180
-  On the ``Output`` pane, ``VTK`` sub-pane, change the write interval to
   ``0.01``

.. include:: /user_manual/icons.rst

Post Processing
===============

MFIX can generate output data in several formats for visualization and analysis.
The command line tool ``postmfix`` is distributed with MFIX. In addition, the
MFiX output file format is supported by the open source GUI tools `ParaView
<https://www.paraview.org>`_ (version 4.1 or later) and `VisIt
<https://wci.llnl.gov/simulation/computer-codes/visit/>`_.

.. include:: /user_manual/output_format_table.rst

Notes:

1. pvd files only contain information linking the respective .vtu and .vtp files to time- step information.
2. <VTK_REGION >_###.vtp files are binary files. The <RUN_NAME >_DES_###.vtp are ASCII files.


Running Paraview
~~~~~~~~~~~~~~~~

This walk through demonstrates how to use ParaView to visualize the results of
the FluidBed_DES tutorial. It is assumed that the FluidBed_DES tutorial was
successfully run with default settings and that ParaView is installed on your
system. First the tutorial demonstrates how to visualize the fluid field; then
the DEM particles are added using spherical glyphs.

.. role:: u

.. _fig_paraview-fileopen:

.. figure:: /media/paraview_1.png
   :figwidth: 8cm
   :width: 8cm
   :alt: paraview visualization

   Click on File icon |file|, or :u:`F`\ ile > Open

Fluid field results for a standard structured mesh are displayed in ParaView by
loading the .RES file. To open the RES file, click on File/Open
(:numref:`fig_paraview-fileopen`).

Once the file is loaded, you need to click on the green "Apply" icon |apply| to load
all of the variables.

When prompted, select the ``DES_FB1.RES`` file (:numref:`fig_paraview-loadres`).
Do NOT load the DES restart file (``DES_FB1_DES.RES``). This binary file is not
supported by any visualization software. It only contains restart data and will
likely cause ParaView to crash if loaded.

.. _fig_paraview-loadres:

.. figure:: /media/paraview_2.png
   :figwidth: 8cm
   :width: 8cm
   :alt: paraview visualization

   Load ``DES_FB1.RES`` (do NOT load ``DES_FB1_DES.RES``)

Newer versions of ParaView require that you select the appropriate reader.
Choose to open the data with the 'MFIX Unstructured Grid Files'.
(:numref:`fig_paraview-unstructured`)

.. _fig_paraview-unstructured:

.. figure:: /media/paraview_3.png
   :figwidth: 8cm
   :width: 8cm
   :alt: paraview visualization

   Select "MFIX Unstructured Grid Files"

ParaView typically displays the gas phase volume fraction once the data is
loaded. It may be necessary to rescale the data range as the initial range may
only be suitable for displaying the initial conditions. This can be done by
clicking the |rescale| icon.

.. _fig_paraview-pvd:

.. figure:: /media/paraview_4.png
   :figwidth: 8cm
   :width: 8cm
   :alt: paraview visualization

   Open DES_FB1_DES.pvd


DEM and PIC particle simulation data is loaded into ParaView by opening the
``.pvd`` file. (:numref:`fig_paraview-pvd`)

Again, click on the green "Apply" icon |apply| to load all of the variables.
(:numref:`fig_paraview-load`)

.. _fig_paraview-load:

.. figure:: /media/paraview_5.jpg
   :figwidth: 8cm
   :width: 8cm
   :alt: paraview visualization

   Load all variables

Particles are shown by applying a glyph to the
dataset. To apply a glyph filter, either:

 - :u:`F`\ ilters > Alphabetical > Glyph
 - or click the |glyph| icon.

Commonly, particle data is represented using:

 1. `Sphere` glyph type and
 2. Scalar scale mode with a scale factor of one.

Note that these screenshots may appear differently depending on your version of
ParaView.

.. |apply| image:: /media/paraview_apply_icon.jpg
                   :width: 1cm

.. |file| image:: /media/paraview_file_icon.png
.. |glyph| image:: /media/paraview_glyph_icon.jpg
.. |rescale| image:: /media/paraview_rescale_icon.jpg

Running postmfix
~~~~~~~~~~~~~~~~

The ``postmfix`` command is used for reading the binary MFIX ``.SPx`` output
files and outputting data in text files. The following walk through demonstrates
how to run ``postmfix`` on the results from the FluidBed_DES tutorial with the
default settings. The ``postmfix`` executable is built by running
``build_mfixsolver`` with the argument ``postmfix``:

.. code-block:: shell

    > build_mfixsolver postmfix

After building, run the executable and enter the run name:

.. code-block:: shell

    > ./postmfix
    Enter the RUN_NAME to post_process > DES_FB1 ? DES_FB1<Enter>

A simple menu of options is presented. Type `1` to examine/print data and press Enter.

.. code-block:: shell

    *************************************************
    0 - Exit POST_MFIX
    1 - Examine/print data
    2 - Write .RES from data in .SPx files
    3 - Write .RES for a new grid, using old data
    4 - Calculate miscellaneous quantities
    5 - Print out variables
    6 - Call user defined subroutine USR_POST
    7 - Write a new SPx file with selected records
    8 - Write new SPx files with time averaged data
    9 - Perform ORNL calculations
    10 - run scavenger code
    *********************************** **************
    Enter menu selection > 1

    Interactive data retrieval program. Type ? any time for help,
    or press RETURN to select default values shown in parenthesis.

Type F to use the default data precision and press Enter.

.. code-block:: shell

  Write output using user - supplied precision? (T/F) F ? F<Enter>

Enter a time range for data extraction and analysis. The default simulation has
a simulation length of one second, so enter a range from 0.1 seconds to 0.9
seconds. The next prompt asks if the data should be time averaged. Press Enter
to skip the averaging.

.. code-block:: shell

    Time: (0.000, 0.000) > 0.1, 0.9
    Time average ? (N) > ? < Enter >

Enter the variable of interest. The default is the gas phase volume fraction,
EP_G. A complete list of possible entries is given by typing `?` and
Enter. Press Enter to select the gas phase volume fraction.

.. code-block:: shell

  Variable: (EP_g) > ? < Enter >

Next enter the spatial range to extract the data. This requires an understanding
of the I/J/K values for your simulation. Basic geometric information for the
simulation is provided in the .OUT file. For this example, we will take a
vertical slice from the approximate center of the 2D domain.

.. code-block:: shell

    I range: (1, 1) > 8 , 8 ? 8,8< Enter >
    J range: (1, 1) >2,40 ? 2,40< Enter >
    Average or sum over J? (N) > ? < Enter >
    K range: (1, 1) > ? < Enter >

Specify where to output the data. Press Enter to select the default ``*``, printing
 the data to the terminal. Alternatively , specify a file name to save the data.

.. code-block:: shell

    File: (*) > ? <Enter> prints to screen
    (filename<Enter> saves to filename)
    X = 6.5000
    Z = - 0.20000
    Time = 0.10075
    Y      EP_g
    1.0000 0.43971
    3.0000 0.45994
    ....etc....

Return to the original time prompt to continue data extraction or analysis. Type
`q` and Enter to return to the main menu. From the main menu, type `0` and Enter
to exit .

.. code-block:: shell

    Time: (1.000, 1.000) > q ? q< Enter > to quit
    < main menu is displayed again >
    Enter menu selection > 0 ? 0< Enter > to exit

.. include:: /user_manual/icons.rst

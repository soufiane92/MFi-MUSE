File Menu
^^^^^^^^^

The file menu is composed of a *Navigation pane* and a *Task pane*. The
Navigation pane is located on the left with a static layout. Contents of
the Task pane are based on what is selected in the Navigation pane.

-  Project Info

   -  Description: Provides general project specific information.
   -  Task pane layout:

      -  Header Text: *Project Info*
      -  Displayed fields

         -  Project Version Number
         -  MFIX version that generated the project (mfx) file
         -  Project creator
         -  Date created
         -  Last project author (last modifier)
         -  Date last modified
         -  Project Notes (user notes)

   -  Notes: This is the default screen displayed when entering the
      File Menu using the hamburger icon on an existing project.

-  New

   -  Description: Display template selection for new projects
   -  Task pane layout:

      -  Header Text: *New Project*
      -  Allow display as list or tiles
      -  Provide secondary navigation pane with filter options:

         -  Solver: Single, TFM, DEM, PIC, Hybrid
         -  Cut-cell
         -  Chemistry
      -  Display tutorial cases

         -  Icon
         -  Tutorial description
   -  Notes: This is the default Task pane when the GUI is
      opened without an existing project file.

-  Open

   -  Description: Display list of recent projects and option to
      search for a file locally.
   -  Task pane layout:

      -  Header Text: *Open Project*
      -  Allow display as list or tiles
      -  Display recently opened cases

         -  Icon (if available)
         -  File location
         -  Description (if available)

-  Save

   -  Description: Save current project. Presently, the button is
      only accessible when the project has changes that have not
      been saved.
   -  Task pane layout: *None*

-  Save As

   -  Description: Save current project under a different name. The project is
      then opened in that location and with that filename.
   -  Task pane layout: *None*
   -  Trigger navigation popup

-  Export

   -  Description: Save the current project to a new directory and/or
      as a new filename, but keep the original project opened.
   -  Task pane layout: *None*
   -  Trigger navigation popup



**Navigation divider**

.. todo::

   **Comment from Jeff**: I don't understand what this Default setting is about.
   Will there be a button to reset values to these defaults?

-  Defaults

   -  Description: Location where global defaults are set and displayed. Each model
      has its own sub-pane similar to the Solids task pane.

   -  Header: *Model Defaults*

   -  Fluid sub-pane layout:

      -  Constant density

         -  Keyword: RO_G0
         -  Default: 1.2005
         -  Unit: kg/m^3
      -  Constant viscosity

         -  Keyword: MU_G0
         -  Default: 1.8d-5
         -  Unit: kg/(m.s)
      -  Molecular weight

         -  Keyword: MW_AVG
         -  Default: 29.0
         -  Unit: kg/kmol
      -  Temperature

         -  Keywords: IC_T_g, BC_T_g, PS_T_g
         -  Default: 293.15
         -  Unit: K
      -  Species (drop down)

         -  Keywords: IC_X_g, BC_X_g, PS_X_g
         -  Available selections

            -  First species
            -  Last species
         - Action: Sets the mass fraction of the first (last) species
           to 1.0 and all others to zero.
      -  Specific Heat

         -  Keywords: C_PG0
         -  Default: 1005.0
         -  Unit: J/(kg.K)
      -  Thermal conductivity

         -  Keywords: K_G0
         -  Default: 0.0257
         -  Unit: W/(m.K)
      -  Diffusivity

         -  Keywords: DIF_G0
         -  Default: 1.0e-5
         -  Unit: m^2/s

   -  Solids Material sub-pane layout:

      -  Diameter

         -  Keyword: D_p0
         -  Default: 0.001
         -  Unit: m
      -  Constant Density

         -  Keyword: RO_S0
         -  Default: 1000.0
         -  Unit: kg/m^3
      -  Temperature

         -  Keywords: IC_T_s, BC_T_s, PS_T_s
         -  Default: 293.15
         -  Unit: K
      -  Species (drop down)

         -  Keywords: IC_X_s, BC_X_s, PS_X_s
         -  Available selections

            -  First species
            -  Last species
         - Action: Sets the mass fraction of the first (last) species
           to 1.0 and all others to zero.
      -  Constant viscosity

         -  Keyword: MU_S0
         -  Default: 1.0e-5
         -  Unit: Pa.s
      -  Specific Heat

         -  Keywords: C_PS0
         -  Default: 830.0
         -  Unit: J/(kg.K)
      -  Thermal conductivity

         -  Keywords: K_G0
         -  Default: 2.0
         -  Unit: W/(m.K)
      -  Emissivity

         -  Keywords: DES_EM
         -  Default: 0.0
         -  Unit: (1)

-  Settings

   -  Description: Location to change GUI display and operation settings.

-  Help

   -  Description: Provide links to online and local documentation.
   -  Task pane layout:

      -  Link to local, distributed documentation
      -  Link to online (mfix website) documentation

      -  Tutorials:

         -  Text: Link to tutorial in local documentation
         -  Video: Link to YouTube tutorial (if available)

-  About

   -  Description: Collect and display GUI and library information.
   -  Task pane layout:

      -  Display generic MFIX disclaimer.
      -  MFIX version
      -  Python version
      -  Qt wrapper
      -  Qt version
      -  qtpy version
      -  Numpy version
      -  VTK version

   -  Provide button to save version information to plain text file.
   -  Show/link to MFIX install location
   -  Show/link to MFIX solver source code location
   -  Show/link to default MFIX solver location

-  Quit

   -  Description: Close/Exit MFIX
   -  Popup dialog to confirm
   -  Popup dialog to save before exiting if not saved

Output Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^

The output input is split into tabs.

Basic (tab)
___________

-  Specify Restart/checkpoint write interval

   -  Specification always available (required)
   -  Sets keyword RES_DT
   -  DEFAULT 1.0

-  Specify the number of backup copies

   .. todo::

      **Question from Jeff**: Is there a way to keep track of the
      time associated with a restart file?

      One suggestion we got at the workshop was to add an option to
      pick which restart file to use when restarting a simulation and
      several restart files are available.

      **JM Response** There are a number of ways we could do this.
      A very simple approach would be to tag the RES file with the
      current simulation time (RUN_NAME.RES-TIME).
      Another approach that same line is to use the current time
      step number. This is a bit nicer in that we are dealing with
      integers rather than floats, but the information is a bit
      less useful to the user.

   -  Specification always available
   -  Sets keyword RES_BACKUPS
   -  DEFAULT 0
   -  Error check: value is greater than or equal to 0

-  Specify the backup interval

   -  Specification only available if RES_BACKUPS > 0
   -  Sets keyword RES_BACKUP_DT
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Enable VTK output

   -  Specification always available
   -  Sets keyword WRITE_VTK_FILES
   -  DEFAULT .FALSE.
   -  Enables VTK tab

-  Enable time-dependent VTK files

   -  Specification only if WRITE_VTK_FILES = .TRUE.
   -  Sets keyword TIME_DEPENDENT_FILENAME
   -  DEFAULT value .TRUE.

-  Specify VTK Directory

   -  Specification only if WRITE_VTK_FILES = .TRUE.
   -  Sets keyword VTU_DIR
   -  No default (empty string)

-  Write binary **S** ingle **P** recision files (SPx)

   -  No keyword association.
   -  Enables SPx tab
   -  Backwards compatibility: Enabled if any SPx time values are
      specified

-  Enable NetCDF output files

   -  Not available when SPx output is enabled
   -  No keyword association.
   -  Enables NetCDF tab

SPx (tab)
_________

Note: Technically, MFIX will now permit a user to mix-and-match the SPx
output files meaning that some can be written and others not. However,
this is likely to break the ParaView reader. Therefore, if the “Write
binary SPx” checkbox is enabled, output is required for all SPx files.
Otherwise, all should remain unspecified to skip writing the SPx files.

-  Write interval for gas volume fraction

   -  Sets keyword SPX_DT(1)
   -  DEFAULT 1.0
   -  Required if SPx data is enabled.
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for gas and solids pressure

   -  Sets keyword SPX_DT(2)
   -  Required if SPx data is enabled.
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for gas velocity

   -  Sets keyword SPX_DT(3)
   -  Required if SPx data is enabled.
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for solids velocity

   -  Required if SPx data is enabled.
   -  Sets keyword SPX_DT(4)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for solids bulk density

   -  Sets keyword SPX_DT(5)
   -  Required if SPx data is enabled.
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for gas and solids temperature

   -  Sets keyword SPX_DT(6)
   -  Required if SPx data is enabled and solving any energy equations.
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for gas and solids mass fractions

   -  Sets keyword SPX_DT(7)
   -  Required if SPx data is enabled and solving any species equations.
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for granular temperature

   -  Sets keyword SPX_DT(8)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for user defined scalars

   -  Sets keyword SPX_DT(9)
   -  Required if SPx data is enabled and solving any user defined scalar equations
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for reaction rates

   -  Sets keyword SPX_DT(10)
   -  Required if SPx data is enabled and NRR > 0 (see below)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT


-  Write interval for turbulence quantities

   -  Sets keyword SPX_DT(11)
   -  Required if SPx data is enabled and TURBULENCE_MODEL = “K_EPSILON”
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write ASCII particle data

   -  Selection only available if DEM or PIC solids
   -  Sets keyword PRINT_DES_DATA
   -  DEFAULT .TRUE.

-  Specify VTP Directory

   -  Specification only if PRINT_DES_DATA = .TRUE.
   -  Sets keyword VTP_DIR
   -  No default (empty string)

   -  Select particle data format

      -  Selection only available if DEM or PIC solids and PRINT_DES_DATA = .TRUE.
      -  Sets keyword DES_OUTPUT_TYPE
      -  Available Selections

         -  ParaView - VTK/.vtp [DEFAULT]
         -  Tecplot - .dat

VTK (tab)
_________

Icons and table similar to IC/BC/PS/IS for adding VTK regions. This
section requires WRITE_VTK_FILES = .TRUE.

-  Icons to add/remove/duplicate regions are given at the top
-  Clicking the 'add' and 'duplicate' buttons triggers a popup window
   where the user must select a VTK region.

   -  Users cannot select inapplicable regions.
   -  VTK regions can be points, planes, or volumes (not STLs)
   -  Regions can define multiple VTK regions.
-  Select Output type

   - Selection is required

   -  Available selections:

      -  Cell data

         -  Selection always available
         -  Set keyword VTK_DATA(#) to 'C'

      -  Particle data

         -  Selection only available with DEM or PIC solids
         -  Sets keyword VTK_DATA(#) to 'P'

VTK Region settings
~~~~~~~~~~~~~~~~~~~

There is a need for some hand waving here. Many ``.mfx`` files may use a
different specification for VTK input. There will need to be a way of
catching the ‘old’ format and converting it to this input style.

These settings define the
vtk file name, write interval and allow to generate slices.

-  Specify filename base

   -  Specification is required.
   -  Sets keyword VTK_FILEBASE(#)
   -  DEFAULT region name

-  Specify write interval

   -  Specification is required
   -  Sets keyword VTK_DT(#)
   -  DEFAULT 1.0 (must write)

-  Specify region x-axis slices

   -  Specification always available
   -  Sets keyword VTK_NXS(#)
   -  DEFAULT 0

-  Specify region y-axis slices

   -  Specification always available
   -  Sets keyword VTK_NYS(#)
   -  DEFAULT 0

-  Specify region z-axis slices

   -  Specification always available
   -  Sets keyword VTK_NZS(#)
   -  DEFAULT 0

-  Specify slice tolerance

   -  Specification always available
   -  Sets keyword VTK_SLICE_TOL(#)

-  Check box: "Only save data in cut cells"

   -  Specification only available for cell data
   -  Sets keyword VTK_CUTCELL_ONLY(#) = .TRUE.

Cell data sub-pane
~~~~~~~~~~~~~~~~~~

Fluid Phase (tab)
~~~~~~~~~~~~~~~~~

-  Enable writing gas volume fraction

   -  Selection always available
   -  Sets keyword VTK_EP_G(#)
   -  DEFAULT value .FALSE.

-  Enable writing gas pressure

   -  Requires fluid solver (RO_G0 /= 0.0)
   -  Sets keyword VTK_P_G(#)
   -  DEFAULT value .FALSE.

-  Enable writing solids pressure

   -  Requires TFM solids
   -  Sets keyword VTK_P_STAR
   -  DEFAULT value .FALSE.

-  Enable writing gas velocity vector

   -  Requires fluid solver (RO_G0 /= 0.0)
   -  Sets keyword VTK_VEL_G(#)
   -  DEFAULT value .FALSE.

-  Enable writing gas velocity x-component

   -  Requires fluid solver (RO_G0 /= 0.0) -
   -  Sets keyword VTK_U_G(#)
   -  DEFAULT value .FALSE.

-  Enable writing gas velocity y-component

   -  Requires fluid solver (RO_G0 /= 0.0)
   -  Sets keyword VTK_V_G(#)
   -  DEFAULT value .FALSE.

-  Enable writing gas velocity z-component

   -  Requires fluid solver (RO_G0 /= 0.0)
   -  Sets keyword VTK_W_G(#)
   -  DEFAULT value .FALSE.

-  Enable writing gas temperature

   -  Requires fluid solver (RO_G0 /= 0.0) and ENERGY_EQ = .TRUE.
   -  Sets keyword VTK_T_G(#)
   -  DEFAULT value .FALSE.

-  Enable writing gas species N ( **an entry for each defined species**)

   -  Requires defined gas phase species
   -  Sets keyword VTK_X_G(#,N)
   -  DEFAULT value .FALSE.

-  Enable writing gas temperature

   -  Requires fluid solver (RO_G0 /= 0.0) and ENERGY_EQ = .TRUE.
   -  Sets keyword VTK_T_G(#)
   -  DEFAULT value .FALSE.

-  Enable writing gas species N

   -  Requires fluid solver (RO_G0 /= 0.0) and SPECIES_EQ(0) = .TRUE.
   -  Sets keyword VTK_X_G(#,N)
   -  DEFAULT value .FALSE.

-  Enable writing turbulent kinetic energy

   -  Requires fluid solver (RO_G0 /= 0.0) and TURBULENCE_MODEL=’K_EPSILON’
   -  Sets keyword VTK_K_TURB_G(#)
   -  DEFAULT .FALSE.

-  Enable writing turbulent dissipation

   -  Requires fluid solver (RO_G0 /= 0.0) and TURBULENCE_MODEL=’K_EPSILON’
   -  Sets keyword VTK_E_TURB_G(#)
   -  DEFAULT .FALSE.


Solids Phase (tab)
~~~~~~~~~~~~~~~~~~

-  Enable writing solids velocity vector

   -  Requires TFM solids
   -  Sets keyword VTK_VEL_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable writing solids velocity x-component

   -  Requires TFM solids
   -  Sets keyword VTK_U_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable writing solids velocity y-component

   -  Requires TFM solids
   -  Sets keyword VTK_V_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable writing solids velocity z-component

   -  Requires TFM solids
   -  Sets keyword VTK_W_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable writing solids bulk density

   -  Requires TFM solids
   -  Sets keyword VTK_ROP_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable writing solids temperature

   -  Requires TFM solids and ENERGY_EQ = .TRUE.
   -  Sets keyword VTK_T_S(#,#)
   -  DEFAULT value .FALSE.

-  Enable writing solids phase M, species N

   -  Requires TFM solids and SPECIES_EQ(#) = .TRUE.
   -  Sets keyword VTK_X_S(#,M,N)
   -  DEFAULT value .FALSE.

-  Enable writing solids phase granular temperature

   -  Requires TFM solids and KT_TYPE /= “ALGEBRAIC”
   -  Sets keyword VTK_THETA_M(#,#)
   -  DEFAULT value .FALSE.

Scalar (tab)
~~~~~~~~~~~~

-  Enable writing user defined scalar

   -  Requires NSCALAR > 0
   -  Sets keyword VTK_SCALAR(#)
   -  DEFAULT value .FALSE.

Reactions (tab)
~~~~~~~~~~~~~~~

-  Enable writing user defined reaction rates

   -  Requires NRR > 0
   -  Sets keyword VTK_rrate(#,r), where r goes from 1 to nRR
   -  DEFAULT value .FALSE.
   -  Sets keyword VTK_RRate_label(#,r) where r goes from 1 to nRR
   -  DEFAULT value is '' (blank character)
   -  Set the text box for the VTK_RRate_label on the same row as the checkbox

Other (tab)
~~~~~~~~~~~

-  Enable writing vorticity
   -  Requires fluid solver (RO_G0 /= 0.0)
   -  Sets keyword VTK_VORTICITY (#)
   -  Sets keyword VTK_LAMBDA_2(#)
   -  DEFAULT value .FALSE.

-  Enable writing partition

   -  Sets keyword VTK_PARTITION(#)
   -  DEFAULT value .FALSE.

-  Enable writing boundary ID

   -  Sets keyword VTK_BC_ID(#)
   -  DEFAULT value .FALSE.

-  Enable writing wall distance

   -  Sets keyword VTK_DWALL(#)
   -  DEFAULT value .FALSE.

-  Enable writing cell index

   -  Sets keyword VTK_IJK(#)
   -  DEFAULT value .FALSE.


Particle data sub-pane
~~~~~~~~~~~~~~~~~~~~~~

There is a need for some hand waving here. Many ``.mfx`` files may use a
different specification for VTK input. There will need to be a way of
catching the ‘old’ format and converting it to this input style.

-  Specify selection mode

   -  Sets keyword VTK_SELECT_MODE(#)
   -  DEFAULT 'C'
   -  Valid values

      - 'C' (center inside region)
      - 'P' (entire particle inside region)
      - 'I' (particle intersects region)

-  Enable writing particle diameter

   -  Requires DEM or PIC solids
   -  Sets keyword VTK_PART_DIAMETER(#)
   -  DEFAULT value .FALSE.

-  Enable writing particle translational velocity

   -  Requires DEM or PIC solids
   -  Sets keyword VTK_PART_VEL(#)
   -  DEFAULT value .FALSE.

-  Enable writing particle rotational velocity

   -  Requires DEM or PIC solids
   -  Sets keyword VTK_PART_ANGULAR_VEL(#)
   -  DEFAULT value .FALSE.

-  Enable writing particle orientation

   -  Requires DEM or PIC solids
   -  Sets keyword VTK_PART_ORIENTATION(#)
   -  DEFAULT value .FALSE.

-  Enable writing particle temperature

   -  Requires DEM or PIC solids and ENERGY_EQ=.TRUE.
   -  Sets keyword VTK_PART_TEMP(#)
   -  DEFAULT value .FALSE.

-  Enable writing particle density

   -  Requires DEM or PIC solids
   -  Sets keyword VTK_PART_DENSITY(#)
   -  DEFAULT value .FALSE.

-  Enable writing particle cohesive force

   -  Requires DEM or PIC solids and USE_COHESION=.TRUE.
   -  Sets keyword VTK_PART_COHESION(#)
   -  DEFAULT value .FALSE.

-  Enable writing particle user variable

   -  Requires DEM or PIC solids and DES_USR_VAR > 0
   -  Sets keyword VTK_PART_USR_VAR(#,#)
   -  DEFAULT value .FALSE.

-  Enable writing particle species composition

   -  Requires DEM or PIC solids and any SPECIES_EQ=.TRUE.
   -  Sets keyword VTK_PART_X_S(#,#)
   -   Note, VTK_PART_X_S(#,N) where N ranges from 1 to max(nmax_s)
   -   DEFAULT value .FALSE.

-  Enable writing particle MPI rank

   -  Requires DEM or PIC solids
   -  Sets keyword VTK_PART_RANK(#)
   -  DEFAULT value .FALSE.

-  Enable writing particle global ID

   -  Requires DEM or PIC solids
   -  Sets keyword VTK_PART_ID(#)
   -  DEFAULT value .FALSE.

-  Enable writing data only for particle belonging to the given phase(s)

   -  Requires DEM or PIC solids
   -  Sets keyword VTK_PART_PHASE(#,M) = .TRUE., one check box per DEM phase M
   -  Display the solids phase name rather than the phase index
   -  DEFAULT value .TRUE.


NetCDF (tab)
____________

Note: NetCDF support ‘piggy-backs’ off of the SPx keywords. The output
time values are specified via SPX_DT while NetCDF output is triggered
by a BWRITE_NETCDF flag. To make this less opaque to users, both SPx
and netCDF output cannot be enabled at the same time.

-  Write interval for gas volume fraction

   -  Sets keyword BWRITE_NETCDF(1) = .TRUE.
   -  Sets keyword SPX_DT(1)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for gas and solids pressure

   -  Sets keyword BWRITE_NETCDF(2) = .TRUE.
   -  Sets keyword BWRITE_NETCDF(3) = .TRUE.
   -  Sets keyword SPX_DT(2)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for gas velocity

   -  Sets keyword BWRITE_NETCDF(4) = .TRUE.
   -  Sets keyword SPX_DT(3)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for solids velocity

   -  Sets keyword BWRITE_NETCDF(5) = .TRUE.
   -  Sets keyword SPX_DT(4)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for solids bulk density

   -  Sets keyword BWRITE_NETCDF(6) = .TRUE.
   -  Sets keyword SPX_DT(5)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for gas and solids temperature

   -  Only available when solving energy equations
   -  Sets keyword BWRITE_NETCDF(7) = .TRUE.
   -  Sets keyword BWRITE_NETCDF(8) = .TRUE.
   -  Sets keyword SPX_DT(6)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for gas and solids mass fractions

   -  Only available when solving species equations
   -  Sets keyword BWRITE_NETCDF(9) = .TRUE.
   -  Sets keyword BWRITE_NETCDF(10) = .TRUE.
   -  Sets keyword SPX_DT(7)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for granular temperature

   -  Only available when KT_TYPE =/ ‘ALGEBRAIC’
   -  Sets keyword BWRITE_NETCDF(11) = .TRUE.
   -  Sets keyword SPX_DT(8)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for user defined scalars

   -  Only available when solving any user defined scalar equations
   -  Sets keyword BWRITE_NETCDF(12) = .TRUE.
   -  Sets keyword SPX_DT(9)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Write interval for reaction rates

   -  Only available if NRR > 0 (see below)
   -  Sets keyword BWRITE_NETCDF(13) = .TRUE.
   -  Sets keyword SPX_DT(10)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT

-  Number of reaction rates to write

   -  Specification always available
   -  Sets keyword NRR
   -  DEFAULT 0
   -  Error check: value must be greater than or equal to 0

-  Write interval for turbulence quantities

   -  Only available if TURBULENCE_MODEL = “K_EPSILON”
   -  Sets keyword BWRITE_NETCDF(14) = .TRUE.
   -  Sets keyword SPX_DT(11)
   -  DEFAULT 1.0
   -  Error check: value must be greater than or equal to RES_DT


**This is the same particle section as the SPx section.**


-  Write ASCII particle data

   -  Selection only available if DEM or PIC solids
   -  Sets keyword PRINT_DES_DATA
   -  DEFAULT .TRUE.

-  Specify VTP Directory

   -  Specification only if PRINT_DES_DATA = .TRUE.
   -  Sets keyword VTP_DIR
   -  No default (empty string)

-  Select particle data format

   -  Selection only available if DEM or PIC solids and PRINT_DES_DATA = .TRUE.
   
LOG (tab)
___________

(May need to adjust the order of the keywords based on how it looks in the GUI)

-  Specify DMP log (All ranks write error messages)

   -  Specification always available
   -  Sets keyword enable_dmp_log
   -  DEFAULT .FALSE.   

-  Specify Display residuals

   -  Specification always available
   -  Sets keyword full_log
   -  DEFAULT .TRUE.   
   
-  Specify residuals grouping

   -  Specification available if full_log is .TRUE.
   -  Sets keyword group_resid
   -  DEFAULT .FALSE.      
   
-  Specify Residuals to display

   -  Specification available if group_resid is .FALSE.
   -  Display a list of variables :
      - P0	 	Gas pressure
      - PM	 	Solids phase M pressure
      - R0	 	Gas density
      - RM	 	Solids phase M density
      - U0	 	Gas phase U-velocity
      - V0	 	Gas phase V-velocity
      - W0	 	Gas phase W-velocity
      - UM	 	Solids phase M U-velocity
      - VM	 	Solids phase M V-velocity
      - WM	 	Solids phase M W-velocity
      - T0	 	Gas temperature
      - TM	 	Solids phase M temperature
      - X0NN 	Gas phase species NN mass fraction
      - XMNN 	Solids phase M species NN mass fraction
      - K0	 	K-Epsilon model residuals
   -  Sets keyword resid_string by building an array of at most 8 values among the above
   -  DEFAULT RESID_STRING(1:6) = 'P0' , 'P1', 'U0', 'V0', 'U1', 'V1'
     
   
-  Specify number of time steps between .LOG file update

   -  Specification always available
   -  Sets keyword nlog
   -  DEFAULT is 25 
   
-  Specify interval at which .OUT file is updated

   -  Specification always available
   -  Sets keyword out_dt
   -  DEFAULT is UNDEFINED
   
-  Specify Write text dashboard

   -  Specification always available
   -  Sets keyword write_dashboard
   -  DEFAULT is .FALSE.
   
-  Specify Report negative density

   -  Specification always available
   -  Sets keyword report_neg_density
   -  DEFAULT is .FALSE.


-  Specify Report solid inventory

   -  Specification always available
   -  Sets keyword report_solid_inventory
   -  DEFAULT is .FALSE. 
   -  Sets keyword report_solid_inventory_dt
   -  DEFAULT is 0.1
   -  Sets keyword breakdown_solid_inventory_by_phase
   -  DEFAULT is .FALSE.   
   -  Check box to save solids inventory vs. time to csv file. Default is .FALSE.
   -  Text input for the file name
   -  Radio button for "When file exists:" 3 options: Overwrite, Append, Increment, Default is Overwrite
      
      
      This will require GUI to do the following: As values are received from the solver, 
      write them in the csv file (first column is time, other colums are solids inventory for each phase if needed).
      Say the file is "Sm.csv". When starting the simulation, if there is no "Sm.csv", the GUI will create it.
      If there is already a "Sm.csv", Overwrite will just overwrite the file (content is lost), Append will keep 
      the existing file and add values at the bottom, Increment will keep the "Sm.csv" and create a new "Sm_1.csv"
      where new data will be written.
   
-  Specify Save standard output to file

   -  Specification always available
   -  Check box to save stdout to text file. 
   -  Default is .FALSE.
   -  Text input for the file name
   -  Radio button for "When file exists:" 3 options: Overwrite, Append, Increment, Default is Overwrite

-  Specify Save DT to file

   -  Specification always available
   -  Check box to save time step (DT) vs. time to csv file. 
   -  Default is .FALSE.
   -  Text input for the file name
   -  Radio button for "When file exists:" 3 options: Overwrite, Append, Increment, Default is Overwrite

-  Specify Save NIT to file

   -  Specification always available
   -  Check box to save NIT vs. time to csv file. 
   -  Default is .FALSE.
   -  Text input for the file name
   -  Radio button for "When file exists:" 3 options: Overwrite, Append, Increment, Default is Overwrite
   
-  Specify Save Residuals to file

   -  Specification always available
   -  Check box to save Residuals vs. iteration to csv file. 
   -  Default is .FALSE.
   -  Text input for the file name
   -  Radio button for "When file exists:" 3 options: Overwrite, Append, Increment, Default is Overwrite


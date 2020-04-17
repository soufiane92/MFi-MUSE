Internal Surfaces Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section allows a user to define internal surfaces for the described model.
This section relies on regions named in the Regions section.

Region Selection
________________


The top of the task pane is where users define/select IS regions

-  Icons to add/remove/duplicate boundary conditions are given at the top
-  Clicking the 'add' and 'duplicate' buttons triggers a popup window
   where the user must select a region to apply the internal surface.

   -  Users cannot select inapplicable regions.
   -  IS regions can be planes or volumes (not points or STLs)
   -  No region can define more than one internal surface.

-  Select internal surface type

   -  Selection is required
   -  Available selections:

      -  Impermeable [DEFAULT]

         -  Selection only available for plane regions
         -  Set keyword IS_TYPE(#) to 'IMPERMEABLE'

      -  X-Axis Impermeable

         -  Selection only available for volume regions
         -  Sets keyword IS_TYPE(#) to 'X_IMPERMEABLE'

      -  Y-Axis Impermeable

         -  Selection only available for volume regions
         -  Sets keyword IS_TYPE(#) to 'Y_IMPERMEABLE'

      -  Z-Axis Impermeable

         -  Selection only available for volume regions
         -  Sets keyword IS_TYPE(#) to 'Z_IMPERMEABLE'

      -  Semi-permeable

         -  Selection only available for plane regions
         -  Sets keyword IS_TYPE(#) to 'SEMIPERMEABLE'

      -  X-Axis semi-permeable

         -  Selection only available for volume regions
         -  Sets keyword IS_TYPE(#) to 'X_SEMIPERMEABLE'

      -  Y-Axis semi-permeable

         -  Selection only available for volume regions
         -  Sets keyword IS_TYPE(#) to 'Y_SEMIPERMEABLE'

      -  Z-Axis semi-permeable

         -  Selection only available for volume regions
         -  Sets keyword IS_TYPE(#) to 'Z_SEMIPERMEABLE'


Parameters
__________

*Input is only needed for semi-permeable surfaces.*

-  Gas permeability:

   -  Specification only available for semipermeable regions
   -  DEFAULT value 1.0d32
   -  Sets keyword IS_PC(#,1)

-  Internal resistance coefficient:

   -  Specification only available for semipermeable regions
   -  DEFAULT value 0.0
   -  Sets keyword IS_PC(#,2)

-  Solids velocity through surface:

   -  Specification only available for semipermeable regions
   -  DEFAULT value 0.0
   -  Sets keyword IS_VEL_s(#,PHASE)
   -  There should be a line for each solids phase.
      Use the user provided solids name.

Point Source Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section allows a user to define point sources for the described model. This
section relies on regions named in the Regions section.

Tabs group point source parameters for phases. Tabs are unavailable if
no input is required from the user.

Region Selection
________________


The top of the task pane is where users define/select PS regions

-  Icons to add/remove/duplicate boundary conditions are given at the top
-  Clicking the 'add' and 'duplicate' buttons triggers a popup window
   where the user must select a region to apply the point source.

   -  Users cannot select inapplicable regions.
   -  PS regions can be points, planes, or volumes (not STLs)
   -  No region can define more than one point source.

-  Fluid tab - Unavailable if the fluid phase was disabled.
-  Each solid phase will have its own tab. The tab name should be the
   name of the solid


Parameters
__________


Fluid Tab
~~~~~~~~~

-  Define mass flowrate:

   -  Select mass inflow specification type:
   -  Sets keyword PS_MASSFLOW_G(#)
   -  DEFAULT 0.0

-  Define temperature

   -  Specification only available when solving energy equations
   -  DEFAULT value 293.15
   -  Sets keyword PS_T_G(#)

-  Select species and set mass fractions (table format)

   -  Specification only available when solving species equations
   -  DEFAULT value 1.0 of last defined species
   -  Sets keyword PS_X_G(#,#)
   -  Error check: if specified, mass fractions must sum to one

-  Define X-axial velocity:

   -  Specification always available
   -  DEFAULT value 0.0
   -  Sets keyword PS_U_G(#)

-  Define Y-axial velocity:

   -  Specification always available
   -  DEFAULT value 0.0
   -  Sets keyword PS_V_G(#)

-  Define Z-axial velocity:

   -  Specification always available
   -  DEFAULT value 0.0
   -  Sets keyword PS_W_G(#)

Solids-# Tab
~~~~~~~~~~~~

*At this time, only TFM solids can be defined with point sources. At some point in the future, this could be extended to PIC solids, but never DEM.*

-  Define mass flowrate:

   -  Select mass inflow specification type:
   -  Sets keyword PS_MASSFLOW_S(#,#)
   -  DEFAULT 0.0

-  Define temperature

   -  Specification only available when solving energy equations
   -  DEFAULT value 293.15
   -  Sets keyword PS_T_S(#,#)

-  Select species and set mass fractions (table format)

   -  Specification only available when solving species equations
   -  DEFAULT value 1.0 of last defined species
   -  Sets keyword PS_X_S(#,#,#)
   -  Error check: if specified, mass fractions must sum to one

-  Define X-axial velocity:

   -  Specification always available
   -  DEFAULT value 0.0
   -  Sets keyword PS_U_S(#,#)

-  Define Y-axial velocity:

   -  Specification always available
   -  DEFAULT value 0.0
   -  Sets keyword PS_V_S(#,#)

-  Define Z-axial velocity:

   -  Specification always available
   -  DEFAULT value 0.0
   -  Sets keyword PS_W_S(#,#)

Scalar Tab
~~~~~~~~~~

*Point sources have not been implemented for general scalars.*

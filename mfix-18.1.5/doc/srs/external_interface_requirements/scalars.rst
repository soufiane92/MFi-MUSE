Scalars phase Task Pane Window
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This section defines how **scalars** are to be implemented into MFIX GUI. An
example layout of the Scalar Pane is provided in :numref:`srs_scalars`.
The layout follows the solids model pane were a table summarizes previously
defined scalars and add/delete buttons allow scalars to be added and removed.
The table should list the user-defined scalar name, the convecting phase, and
the scalar index.

.. _srs_scalars:
.. figure:: /../doc/srs/images/scalars.png
   :scale: 75 %
   :align: center

   Mock-up of Scalar pane

Add/Delete Scalars
__________________

Add/Delete scalar

-  Increments/decrements integer keyword :code:`NScalar` by one
-  NScalar has a default value of zero

The MFIX solver assumes scalar array inputs are contiguous. As a result, if
one or more scalar equations is removed or the order of the scalars is
changed, then all scalar input arrays must be updated. For example, consider
three scalar that are convected with a different phase.

.. code-block:: fortran

   NScalar = 3
   Phase4Scalar(1) = 0
   Phase4Scalar(2) = 1
   Phase4Scalar(3) = 2

If a user deletes the second scalar, then array values assigned to the
third scalar must be shifted down.

.. code-block:: fortran

   NScalar = 2
   Phase4Scalar(1) = 0
   Phase4Scalar(2) = 2

Array inputs for scalars include the following:

.. code-block:: fortran

   Phase4Scalar(1:NScalar)

   IC_Scalar(:, 1:NScalar)

   BC_HW_Scalar(:, 1:NScalar)
   BC_ScalarW(:, 1:NScalar)
   BC_C_Scalar(:, 1:NScalar)
   BC_Scalar(:, 1:NScalar)

   VTK_Scalar(:,1:NScalar)
   MONITOR_Scalar(:,1:NScalar)


Scalar properties
_________________


-  Option to name the scalar (e.g, tracer, scalar1, bob).

   -  This name is local to the GUI and not provided to the solver.
   -  By default, scalars should be named "Scalar ID" where ID is the
      index of the scalar.

-  Drop-down menu to select the convecting phase.

   -  Sets keyword PHASE4SCALAR(ID) = PHASE where PHASE is the
      phase index.

      -  PHASE = 0 for the fluid
      -  PHASE = solids phase index for TFM solids

   -  The fluid phase should only be available when the fluid solver
      is enabled (RO_g0 /= 0).
   -  TFM solids phases should be listed when defined.
   -  DEM and PIC solids phases should never be listed.


Housekeeping
____________

-  The scalars were already implemented though options available in the
   fluid and TFM solids panes. These options should be removed.
-  The user-defined scalar names could be added to IC/BC menus were
   scalar inputs are defined.

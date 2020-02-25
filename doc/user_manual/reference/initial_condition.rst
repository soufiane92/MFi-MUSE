Initial Condition
=================

Each initial condition (IC) is specified over a rectangular region (or
pie-shaped for cylindrical coordinates) that corresponds to the scalar numerical
grid. These are 3D regions: ``X_w X_e``, ``Y_s Y_n``, and ``Z_t Z_b``. The
region is defined by the constant coordinates of each of the six faces, which
may be specified as the physical coordinates or the cell indices. The physical
coordinates are easier to specify than the cell indices. If cell sizes are not
small enough to resolve a region specified using physical coordinates, MFIX will
indicate this problem with an error message.

In cylindrical coordinates, when the theta direction crosses the 0 value, split
that region into two regions: e.g., Split a region spanning 1.9 pi to 0.1 pi as
1.9 pi to 2 pi and 0 to 0.1 pi.

Initial condition regions may overlap. When an overlap occurs, MFIX uses the
conditions specified for the higher IC number.

.. include:: /user_manual/reference/initial_condition_gen.rst

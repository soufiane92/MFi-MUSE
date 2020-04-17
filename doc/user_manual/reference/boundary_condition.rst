Boundary Conditions
===================

Boundary conditions (BC) are specified over flow planes or 2D surfaces that are
normal to one of the coordinate directions and coincide with a face of the
scalar control-volume. The values for one of the three pairs of coordinates are
equal. The surface is defined by the constant coordinates of each of the four
edges, which can be specified with physical coordinates or cell indices, and the
two equal values for the direction normal to the face, which can only be
specified with physical coordinates. If cell sizes are not small enough to
resolve a surface specified using physical coordinates, MFIX will indicate this
problem with an error message.

A flow plane must have a wall cell (or an outside boundary) on one side and a
flow cell on the other side. The BC section is also used to specify obstacles in
the flow domain. Obstacles are 3D regions, just as for the IC regions: ``X_w
X_e, Y_s Y_n, and Z_t Z_b``. By default the outside boundary is initialized as
no-slip walls. For cylindrical coordinates the axis is initialized as a
free-slip wall.

Two boundary surfaces must not intersect. Two obstacle regions may intersect.

.. include:: /user_manual/reference/boundary_condition_gen.rst

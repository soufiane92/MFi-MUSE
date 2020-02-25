Point Source
============

Point sources (PS) are used in place of mass inlets where either the geometry
and/or grid resolution prohibit proper boundary condition specification. For
example, a point source may be used to model an injector with dimensions smaller
than the grid. Point sources may be defined within a single computational cell,
along a plane, or as a volume of computational cells.

Point sources introduce mass directly into a computational cell unlike a
boundary condition which specifies flow along a cell face. One consequence of
this implementation is that point sources are subjected to convection/diffusion
forces and may not travel parallel to the specified directional preference.
Directional preference is specified with a velocity vector (i.e., ``PS_U_g``,
``PS_V_g``, etc.), however, directional preference is not required.

Examples showing how to setup point sources can be found in: ``legacy_tutorials/point_source_spiral``

.. include:: /user_manual/reference/point_source_gen.rst

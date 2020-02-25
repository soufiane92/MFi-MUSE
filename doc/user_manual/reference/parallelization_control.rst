Parallelization Control
=======================

The parallel performance depends on several things and one has to evaluate
different options before choosing the right strategy for the problem at hand.
For example if the J direction is the strongest coupled direction, the
preconditioning for the linear solver will be poor if there is decomposition in
that direction. However, since decomposing in all the directions reduces the
processor grid surface area, the communication cost will be less for the same
computational grid. The preconditioners are chosen with the keyword ``LEQ_PC``.
In addition to LINE relaxation, one can choose the "DIAG" or "NONE"
preconditioner that reduces inter-processor communications but would increase
the number of linear equation solver iterations. The DIAG and NONE choices for
preconditioners may be appropriate for all equations except the continuity (or
pressure and volume fraction correction) equations. The parallel performance is
greatly dependent on the choices stated here, and some trial an error may be
required to determine the right combination of decomposition direction and the
choice of preconditioners to get the best performance in production runs.

``NODESI * NODESJ * NODESK`` must be the same as the number of processors
specified using ``mpirun`` (or equivalent command). Otherwise the code will
return with an error.

.. include:: /user_manual/reference/parallelization_control_gen.rst

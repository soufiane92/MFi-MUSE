Other Nonfunctional Requirements
================================

Performance Requirements
------------------------

The UI should not “hang” during operations that require significant
amounts of time (> 1s) to complete. Example actions that could take
significant time include:

-  Reading ``.stl`` files
-  Sending interaction requests to a running MFiX simulation
-  Receiving data from a running MFiX simulation
-  Compiling MFiX

Safety Requirements
-------------------

If MFiX removes any files from the user’s system, a prompt must be
displayed asking the user for confirmation.

Security Requirements
---------------------

User names and passwords for access to the MFS (mfix.netl.doe.gov)
website should be stored (encrypted) securely.

Software Quality Attributes
---------------------------

The software must be written in a modular and flexible fashion, allowing
for maintainability, reusability, and robustness. Documentation,
“docstrings”, should be included with function, class, and method
definitions. All functions, classes, and methods should be
unit-testable. Unit tests should be written for every function, class,
and method as the functions, classes, and methods are developed, **NOT
AS AN AFTERTHOUGHT**.

Code Style Guide
----------------

All Python code should follow the standard Python style guide. If your
editor supports it, please enable pylint checking.

Business Rules
--------------

Other Requirements
==================

Appendix A: Glossary..................................................................................................................
--------------------------------------------------------------------------------------------------------------------------------------

Appendix B: Analysis Models
---------------------------

Appendix C: To Be Determined List
---------------------------------

Appendix D: UI Resources
^^^^^^^^^^^^^^^^^^^^^^^^

**6.1.1 Other CFD UI’s**

SimFlow
^^^^^^^

HELYX-OS
^^^^^^^^

SimScale
^^^^^^^^

DICEhub
^^^^^^^

Appendix E: Calculating Variable Solids Density
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Calculating the solids density is done in the context an initial
condition (IC) region, but it is the same calculations hold for boundary
conditions (BC_EP_S/BC_ROP_S)

::

    1) Calculate the baseline density, RO_S0(M) = 1.0/sumk(X_S0(M,K)/RO_XS0(M,K))
    2) Calculate the solids density:
       a. Let I=INERT_SPECIES(M)
       b. IC_RO_S(N,M) = RO_S0(M) * X_S0(M,I)/IC_X_S(N,M,I)
    3) Use solids density to derive volume fraction:
       IC_EP_S(N,M) = IC_ROP_S(N,M) / IC_RO_S(N,M)


N is the index of the IC, M is a phase index, and I and K are
species indices.

Note that RO_S0 and IC_RO_S are not keywords and should not be set
in the mfix file.

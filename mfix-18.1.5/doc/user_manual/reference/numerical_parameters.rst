Numerical Parameters
====================

This section contains keywords for defining numerical parameters. Keywords
related to solving the governing equations (e.g., ``LEQ_IT``, ``DISCRETIZE``,
``UR_FAC``, etc.) are dimensioned for the ten types of equations:

===== ===============================================
Index  Equation Type
===== ===============================================
1      Gas Pressure
2      Solids Volume Fraction
3      Gas and Solids U Momentum Equation
4      Gas and Solids V Momentum Equation
5      Gas and Solids W Momentum Equation
6      Gas and Solids Energy Equations (Temperature)
7      Gas and Solids Species Mass Fractions
8      Granular Temperature
9      User-Defined Scalar and K-Epsilon Equation
10     DES Diffusion Equation
===== ===============================================

For example, ``LEQ_IT(3) = 10``, specifies to use 10 iterations within the
linear equation solver for the U Momentum Equations (of type 3).

.. include:: /user_manual/reference/numerical_parameters_gen.rst

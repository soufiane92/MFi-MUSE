# Test cases for MFIX Fluid solver

## Test case overview

| Test  | Description                                        |
| ----  | -------------------------------------------------- |
| MMS01 | Single phase, 2D, sinusoidal functions             |
| MMS02 | Two phase, 3D, curl based, fixed vol fraction      |
| MMS03 | Two phase, 3D, curl based, variable vol fraction   |
| MMS04 | Single phase, 3D curl based for No-slip wall BC    |
| MMS05 | Single phase, 3D curl based for Free-slip wall BC  |
| MMS06 | Single phase, 3D curl based for Pressure outflow BC|


## Feature/model test overview
|              | MMS01 | MMS02 | MMS03 | MMS04 | MMS05 | MMS06 |
| ------------ | :---: | :---: | :---: | :---: | :---: | :---: |
| Test Freq.   |   M   |   M   |   M   |   M   |   M   |   M   |
| Dimension    |  2D   |  2D   |  3D   |  3D   |  3D   |  3D   |
| Multiphase   |       |   X   |   X   |       |       |   X   |
| Continuity   |   X   |   X   |       |       |       |       |
| Momentum     |   X   |   X   |   X   |       |       |       |
| Them Energy  |       |   X   |   X   |       |       |       |
| Species Mass |       |       |       |       |       |       |
| Granular Eng |       |       |       |       |       |       |
| Turbulence   |       |       |       |       |       |       |
| BCs Tested   |       |       |       |  NSW  |  FSW  |  PO   |
| SMP Parallel |       |       |       |       |       |       |
| DMP Parallel |       |       |       |       |       |       |



--------------------------------------------------------------------

## Notice
Neither the United States Government nor any agency thereof, nor any
of their employees, makes any warranty, expressed or implied, or
assumes any legal liability or responsibility for the accuracy,
completeness, or usefulness of any information, apparatus, product,
or process disclosed or represents that its use would not infringe
privately owned rights.

* MFIX is provided without any user support for applications in the
  user's immediate organization. It should not be redistributed in
  whole or in part.

* The use of MFIX is to be acknowledged in any published paper based
  on computations using this software by citing the MFIX theory
  manual. Some of the submodels are being developed by researchers
  outside of NETL. The use of such submodels is to be acknowledged
  by citing the appropriate papers of the developers of the submodels.

* The authors would appreciate receiving any reports of bugs or other
  difficulties with the software, enhancements to the software, and
  accounts of practical applications of this software.

# Disclaimer
This report was prepared as an account of work sponsored by an agency
of the United States Government. Neither the United States Government
nor any agency thereof, nor any of their employees, makes any
warranty, express or implied, or assumes any legal liability or
responsibility for the accuracy, completeness, or usefulness of any
information, apparatus, product, or process disclosed, or represents
that its use would not infringe privately owned rights. Reference
herein to any specific commercial product, process, or service by
trade name, trademark, manufacturer, or otherwise does not
necessarily constitute or imply its endorsement, recommendation, or
favoring by the United States Government or any agency thereof. The
views and opinions of authors expressed herein do not necessarily
state or reflect those of the United States Government or any
agency thereof.

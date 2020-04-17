# Test cases for MFIX Fluid solver

## Test case overview

| Test  | Description                                        |
| ----  | -------------------------------------------------- |
| FLD01 | Steady, 2D plane Poiseuille flow                   |
| FLD02 | Steady, 1D heat conduction                         |
| FLD03 | Steady, 2D lid-driven cavity flow                  |
| FLD04 | Steady, 2D Gresho vortex problem                   |
| FLD05 | Steady, 2D Couette flow                            |
| FLD06 | Multicomponent species transport                   |
| FLD07 | Turbulent flow in a channel                        |
| FLD08 | Turbulent flow in a pipe                           |
| FLD09 | Turbulent round jet                                |


## Feature/model test overview
|              | FLD01 | FLD02 | FLD03 | FLD04 | FLD05 | FLD06 | FLD07 | FLD08 | FLD08 |
| ------------ | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| Test freq    |   C   |   C   |   C   |   C   |   C   |   C   |   C   |   C   |   C   |
| Ref data set |   A   |   A   |   P   |   A   |   A   |   A   |   A   |   A   |   A   |
| Dimension    |  2D   |  1D   |  2D   |  2D   |  2D   |  2D   |  2D   |  2D   |  2D   |
| Momentum     |   X   |       |   X   |   X   |   X   |   X   |   X   |   X   |   X   |
| Them Energy  |       |   X   |       |       |       |       |       |       |       |
| Species Mass |       |       |       |       |       |   X   |       |       |       |
| Turbulence   |       |       |       |       |       |       |   X   |   X   |   X   |
| SMP Parallel |       |       |       |       |       |       |       |       |       |
| DMP Parallel |       |       |   X   |       |       |       |       |       |       |



## Spatial discretization test overview
| Name         | FLD01 | FLD02 | FLD03 | FLD04 | FLD05 | FLD06 | FLD07 | FLD08 | FLD09 |
| ------------ | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| FOUP         |       |   X   |       |   X   |       |       |       |       |       |
| FOUP w/DWF   |       |       |       |       |       |       |       |       |       |
| Superbee     |   X   |       |   X   |   X   |   X   |       |   X   |   X   |   X   |
| SMART        |       |       |       |   X   |       |   X   |       |       |       |
| ULTRA-QUICK  |       |       |       |       |       |       |       |       |       |
| QUICKEST     |       |       |       |   X   |       |       |       |       |       |
| MUSCL        |       |       |       |   X   |       |       |       |       |       |
| van Leer     |       |       |       |   X   |       |       |       |       |       |
| Minmod       |       |       |       |   X   |       |       |       |       |       |
| Central      |       |       |       |   X   |       |       |       |       |       |



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

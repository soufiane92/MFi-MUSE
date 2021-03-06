.. table:: Output Format Table

    +----------------------------+-------------+------------+---------------+-----------+--------+
    | Filename                   | File Type   | Fluid Data | Particle Data | Paraview  | Visit  |
    +============================+=============+============+===============+===========+========+
    | ``<RUN_NAME>.RES``         | MFiX Binary | Yes        | No            | Yes       | Yes    |
    +----------------------------+-------------+------------+---------------+-----------+--------+
    | ``<RUN_NAME>_###_DES.RES`` | MFiX Binary | No         | Yes           | No        | No     |
    +----------------------------+-------------+------------+---------------+-----------+--------+
    | ``<RUN_NAME>_###.nc``      | NetCDF      | Yes        | No            | Yes       | Yes    |
    +----------------------------+-------------+------------+---------------+-----------+--------+
    | ``<RUN_NAME>.pvd``         | VTK         | Yes        | No            | Yes       | No     |
    +----------------------------+-------------+------------+---------------+-----------+--------+
    | ``<RUN_NAME>_###.vtu``     | VTK         | Yes        | No            | Yes       | Yes    |
    +----------------------------+-------------+------------+---------------+-----------+--------+
    | ``<VTK_REGION>.pvd``       | VTK         | Yes        | No            | Yes       | No     |
    +----------------------------+-------------+------------+---------------+-----------+--------+
    | ``<VTK_REGION>_###.vtu``   | VTK         | Yes        | No            | Yes       | Yes    |
    +----------------------------+-------------+------------+---------------+-----------+--------+
    | ``<RUN_NAME>_DES.pvd``     | VTK         | No         | Yes           | Yes       | No     |
    +----------------------------+-------------+------------+---------------+-----------+--------+
    | ``<RUN_NAME>_DES_###.vtp`` | VTK         | No         | Yes           | Yes       | Yes    |
    +----------------------------+-------------+------------+---------------+-----------+--------+
    | ``<VTK_REGION>.pvd``       | VTK         | No         | Yes           | Yes       | No     |
    +----------------------------+-------------+------------+---------------+-----------+--------+
    | ``<VTK_REGION>_###.vtp``   | VTK         | No         | Yes           | Yes       | Yes    |
    +----------------------------+-------------+------------+---------------+-----------+--------+

set( SERIAL_TESTS
  cg_channel
  )

set( DMP_TESTS
  3dfb
  cyclone
  cylinder
  cg_hourglass
  msh
  naca4412
  polygons
  spoutedbed1
  spoutedbed2
  stl
  wavy
  )

set (TEST_PATH
  legacy_tutorials/Cartesian_grid_tutorials
  )

include(MFIXTest)

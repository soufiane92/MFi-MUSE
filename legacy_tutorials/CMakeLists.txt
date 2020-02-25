set( DMP_TESTS
  3dCfb
  3dFluidbed
  DEM_Wall_HT
  fluidBed1
  fluidBed1_dmp_test
  fluidBed2
  FluidBed2D
  Fully_Developed_With_AhmadiModel
  Fully_Developed_With_SimoninModel
  Gidaspow_fbed_With_SimoninModel
  hourglass
  LiquidFluidBed
  point_source_spiral
  reactor1a
  reactor1b
  silane_pyrolysis
  SpoutedBedCombustor
  usr_drag
  variable_density
  vtk_regions
  )

set( SERIAL_TESTS
  channel
  IATheory
  )

set (TEST_PATH
  legacy_tutorials
  )

include(MFIXTest)

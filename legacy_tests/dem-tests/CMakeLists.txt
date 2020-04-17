set( DMP_TESTS
  circle-advection
  condensation
  contact-conduction
  convection
  evaporation
  homogeneous-cooling-system
  particle-fluid-particle-conduction
  particle-vortex
  poly_dem_inlet_outlet
  radiation
  reactive-chemistry
  restart
  sphere-advection
  )

set (TEST_PATH
  legacy_tests/dem-tests
  )

include(MFIXTest)

set( DMP_TESTS
  2Dfluidbed_VDW_high_coh
  Added_Mass
  Added_Mass_CartesianGridsCase
  Added_Mass_LiftForce
  adiabaticFlame
  cohesionVdwAsperities
  cond_conv
  conduction2
  cyclic-shear
  dmp_test
  dmp_test_cyclic
  dqmom
  driven_cavity_cohesion_VDW
  drying
  fluidBed1_cn
  FluidBed_DES_3D
  granular_shear
  hopper
  HYS_drag_tutorial
  ozone
  packed_bed
  periodic-flow-fixed-mass-flux
  phase_change
  rotary-drum
  segregation
  settling-tank
  simple-2phase
  solid-body-rotation
  ssfGHD
  thermo
  # tsuji  #  FIXME:  DT < DT_MIN at TIME 0.14602E-03 out of TSTOP 0.5E-3
  )

set( SERIAL_TESTS
  chi-scheme
  conduction
  developed_pipe_flow
  developed_pipe_flow1
  developed_slit_flow
  fluidBed1SI
  # fluidBed1SIConstVisc  # FIXME: File "AUTOTEST/POST_epgAxial.dat" differs from file "POST_epgAxial.dat"
  turbulent_pipe_flow
  )

set (TEST_PATH
  legacy_tests
  )

set (
  rank_legacy_tests_FluidBed_DES_3D
  4
  )

set (
  rank_legacy_tests_dmp_test
  4
  )

set (
  rank_legacy_tests_dmp_test_cyclic
  4
  )

include(MFIXTest)

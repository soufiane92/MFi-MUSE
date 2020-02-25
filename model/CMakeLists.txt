set(F77_sources
  ODEPACK.F)

set(F90_sources
  BLAS.f
  DGTSV.f
  GhdTheory/adjust_eps_ghd.f
  GhdTheory/bulk_viscosity.f
  GhdTheory/calc_d_ghd.f
  GhdTheory/calc_external_forces.f
  GhdTheory/calc_nflux.f
  GhdTheory/chi_ij_GHD.f
  GhdTheory/cooling_rate.f
  GhdTheory/cooling_rate_tc.f
  GhdTheory/dufour_coeff.f
  GhdTheory/ghd.f
  GhdTheory/ghdmassflux.f
  GhdTheory/ghdtheory_mod.f
  GhdTheory/mass_mobility.f
  GhdTheory/ordinary_diff.f
  GhdTheory/partial_elim_ghd.f
  GhdTheory/pressure.f
  GhdTheory/shear_viscosity.f
  GhdTheory/source_ghd_granular_energy.f
  GhdTheory/thermal_conductivity.f
  GhdTheory/thermal_diffusivity.f
  GhdTheory/thermal_mobility.f
  GhdTheory/transport_coeff_ghd.f
  accum_resid.f
  adjust_a_u_g.f
  adjust_a_u_s.f
  adjust_a_v_g.f
  adjust_a_v_s.f
  adjust_a_w_g.f
  adjust_a_w_s.f
  adjust_eps.f
  adjust_leq.f
  adjust_theta.f
  allocate_arrays.f
  ambm_mod.f
  b_m_p_star.f
  bc_mod.f
  bc_phi.f
  bc_theta.f
  bodyforce_mod.f
  cal_d.f
  calc_cell.f
  calc_coeff.f
  calc_d_e.f
  calc_d_n.f
  calc_d_t.f
  calc_dif_g.f
  calc_dif_s.f
  calc_e.f
  calc_gama.f
  calc_grbdry.f
  calc_h.f
  calc_k_cp.f
  calc_k_g.f
  calc_k_s.f
  calc_mflux.f
  calc_mu_g.f
  calc_mu_s.f
  calc_outflow.f
  calc_p_star.f
  calc_resid.f
  calc_s_ddot_s.f
  calc_trd_g.f
  calc_trd_s.f
  calc_u_friction.f
  calc_vol_fr.f
  cartesian_grid/CG_set_bc0.f
  cartesian_grid/CG_set_outflow.f
  cartesian_grid/CG_source_u_g.f
  cartesian_grid/CG_source_u_s.f
  cartesian_grid/CG_source_v_g.f
  cartesian_grid/CG_source_v_s.f
  cartesian_grid/CG_source_w_g.f
  cartesian_grid/CG_source_w_s.f
  cartesian_grid/allocate_cut_cell_arrays.f
  cartesian_grid/allocate_dummy_cut_cell_arrays.f
  cartesian_grid/calc_vort_out.f
  cartesian_grid/cartesian_grid_init_namelist.f
  cartesian_grid/check_data_cartesian.f
  cartesian_grid/cut_cell_preprocessing.f
  cartesian_grid/cutcell_mod.f
  cartesian_grid/dashboard_mod.f
  cartesian_grid/deallocate_cut_cell_arrays.f
  cartesian_grid/define_quadrics.f
  cartesian_grid/dmp_cartesian.f
  cartesian_grid/eval_usr_fct.f
  cartesian_grid/get_alpha.f
  cartesian_grid/get_connectivity.f
  cartesian_grid/get_cut_cell_flags.f
  cartesian_grid/get_cut_cell_volume_area.f
  cartesian_grid/get_delh.f
  cartesian_grid/get_master.f
  cartesian_grid/get_poly_data.f
  cartesian_grid/get_stl_data.f
  cartesian_grid/polygon_mod.f
  cartesian_grid/progress_bar_mod.f
  cartesian_grid/quadric_mod.f
  cartesian_grid/set_Odxyz.f
  cartesian_grid/stl_mod.f
  cartesian_grid/update_dashboard.f
  cartesian_grid/vtk_mod.f
  cartesian_grid/vtk_out.f
  cartesian_grid/write_progress_bar.f
  cdist_mod.f
  check_ab_m.f
  check_bqend.f
  check_convergence.f
  check_data/check_axis.f
  check_data/check_bc_dem.f
  check_data/check_bc_geometry.f
  check_data/check_bc_inflow.f
  check_data/check_bc_outflow.f
  check_data/check_bc_pic.f
  check_data/check_bc_walls.f
  check_data/check_boundary_conditions.f
  check_data/check_chemical_rxns.f
  check_data/check_dmp_prereqs.f
  check_data/check_gas_phase.f
  check_data/check_geometry.f
  check_data/check_geometry_prereqs.f
  check_data/check_ic_common_discrete.f
  check_data/check_ic_dem.f
  check_data/check_ic_mppic.f
  check_data/check_initial_conditions.f
  check_data/check_internal_surfaces.f
  check_data/check_numerics.f
  check_data/check_odepack_stiff_chem.f
  check_data/check_output_control.f
  check_data/check_point_sources.f
  check_data/check_run_control.f
  check_data/check_solids_common_all.f
  check_data/check_solids_common_discrete.f
  check_data/check_solids_continuum.f
  check_data/check_solids_dem.f
  check_data/check_solids_model_prereqs.f
  check_data/check_solids_mppic.f
  check_data/check_solids_phases.f
  check_data_20.f
  check_data_30.f
  check_mod.f
  check_plane.f
  chem/stiff_chem_dbg_mod.f
  chem/stiff_chem_maps_mod.f
  chem/stiff_chem_mod.f
  chem/stiff_chem_rrates.f
  chem/stiff_chem_stats_mod.f
  chischeme_mod.f
  cn_extrapol.f
  coeff_mod.f
  constant_mod.f
  conv_dif_phi.f
  conv_dif_u_g.f
  conv_dif_u_s.f
  conv_dif_v_g.f
  conv_dif_v_s.f
  conv_dif_w_g.f
  conv_dif_w_s.f
  conv_pp_g.f
  conv_rop.f
  conv_rop_g.f
  conv_rop_s.f
  conv_source_epp.f
  copy_a.f
  corner.f
  correct_0.f
  correct_1.f
  dbg_mod.f
  deprecated.f
  des/calc_collision_wall_mod.f
  des/calc_des_2fluid.f
  des/calc_drag_des.f
  des/calc_epg_des.f
  des/calc_force_dem.f
  des/calc_gamma_des.f
  des/calc_grad_des.f
  des/calc_interp_weights.f
  des/calc_pg_grad.f
  des/calc_rrates_des.f
  des/calc_thermo_des.f
  des/cfassign.f
  des/cffctowall.f
  des/cfnewvalues.f
  des/cfrelvel.f
  des/cfslide.f
  des/cfupdateold.f
  des/cfwallcontact.f
  des/cfwallposvel.f
  des/check_cell_movement.f
  des/comp_mean_fields.f
  des/comp_mean_fields0.f
  des/comp_mean_fields1.f
  des/derived_types_mod.f
  des/des_allocate_mod.f
  des/des_bc_mod.f
  des/des_cluster_mod.f
  des/des_functions.f
  des/des_granular_temperature.f
  des/des_init_arrays.f
  des/des_init_namelist.f
  des/des_physical_prop.f
  des/des_reaction_model.f
  des/des_rxns_mod.f
  des/des_thermo_cond_mod.f
  des/des_thermo_conv.f
  des/des_thermo_mod.f
  des/des_thermo_newvalues.f
  des/des_thermo_rad.f
  des/des_time_march.f
  des/desgrid_mod.f
  des/desmpi_mod.f
  des/desmpi_wrapper_mod.f
  des/dif_phi_bc_des.f
  des/dif_phi_des.f
  des/dif_phi_source_des.f
  des/diffuse_mean_fields.f
  des/discretelement_mod.f
  des/drag_gp_des.f
  des/drag_gs_des0.f
  des/drag_gs_des1.f
  des/drag_ss_dem_noninterp.f
  des/gas_drag.f
  des/generate_particles_mod.f
  des/init_settling_dem.f
  des/interpolation_mod.f
  des/layout_mi_dem.f
  des/list.f
  des/make_arrays_des.f
  des/mass_inflow_dem.f
  des/mass_outflow_dem.f
  des/mpi_comm_des_mod.f
  des/mpi_funs_des_mod.f
  des/mpi_init_des_mod.f
  des/mpi_node_des_mod.f
  des/mpi_pack_des_mod.f
  des/mpi_unpack_des_mod.f
  des/neighbour.f
  des/nsquare.f
  des/particle_filter_mod.f
  des/particles_in_cell.f
  des/pic/apply_wall_bc_pic.f
  des/pic/calc_dtpic.f
  des/pic/calc_ps_grad_pic.f
  des/pic/calc_ps_pic.f
  des/pic/integrate_time_pic.f
  des/pic/interpolate_pic.f
  des/pic/mass_inflow_pic.f
  des/pic/mass_outflow_pic.f
  des/pic/mfix_pic_mod.f
  des/pic/pic_bc_mod.f
  des/pic/pic_routines.f
  des/pic/pic_time_march.f
  des/pic/report_stats_pic.f
  des/pic/set_bc_pic.f
  des/pic/set_bc_pic_mi.f
  des/pic/set_bc_pic_mo.f
  des/randomno_mod.f
  des/read_particle_input.f
  des/read_res0_des.f
  des/read_res1_des_mod.f
  des/resize.f
  des/rxns_gs_des1.f
  des/sendrecvnode_mod.f
  des/set_bc_dem.f
  des/set_bc_dem_mi.f
  des/set_bc_dem_mo.f
  des/set_filter_des.f
  des/set_geometry_des.f
  des/set_ic_dem.f
  des/set_phase_index.f
  des/solid_drag.f
  des/stl_dbg_des_mod.f
  des/stl_functions_des_mod.f
  des/stl_preproc_des_mod.f
  des/usr0_des.f
  des/usr1_des.f
  des/usr2_des.f
  des/usr3_des.f
  des/usr4_des.f
  des/usr_rates_des.f
  des/vtp_mod.f
  des/write_des_data.f
  des/write_res0_des.f
  des/write_res1_des_mod.f
  dgtsl.f
  dif_u_is.f
  dif_v_is.f
  dif_w_is.f
  discretization_mod.f
  display_resid.f
  dmp_modules/compar_mod.f
  dmp_modules/dbg_util_mod.f
  dmp_modules/debug_mod.f
  dmp_modules/gridmap_mod.f
  dmp_modules/mpi_utility_mod.f
  dmp_modules/parallel_mpi_mod.f
  dmp_modules/sendrecv_mod.f
  dqmom/gaussj.f
  dqmom/odeint.f
  dqmom/rkck.f
  dqmom/rkqs.f
  dqmom/source_population_eq.f
  dqmom/usr_dqmom.f
  drag_gs.f
  drag_mod.f
  drag_ss.f
  energy_mod.f
  eos_mod.f
  error_manager_mod.f
  exchange.f
  exit.f
  fldvar_mod.f
  flow_to_vel.f
  fun_avg_mod.f
  functions_mod.f
  funits_mod.f
  geometry_mod.f
  get_bc_area.f
  get_eq.f
  get_flow_bc.f
  get_hloss.f
  get_is.f
  get_philoss.f
  get_ps.f
  get_smass.f
  get_stats.f
  get_walls_bc.f
  ic_mod.f
  in_binary_512_mod.f
  in_binary_512i_mod.f
  indices_mod.f
  init_ab_m.f
  init_fvars.f
  init_namelist.f
  init_resid.f
  is_mod.f
  iterate.f
  k_epsilon_prop.f
  kintheory_mod.f
  kintheory_u_s.f
  kintheory_v_s.f
  kintheory_w_s.f
  leq_bicgs.f
  leq_bicgst.f
  leq_cg.f
  leq_gmres.f
  leq_sor.f
  leqsol_mod.f
  location_check.f
  machine_mod.f
  main.f
  make_upper_case.f
  mark_phase_4_cor.f
  mfix.f
  mfix_netcdf_mod.f
  mflux_mod.f
  mms_mod.f
  mod_bc_i.f
  mod_bc_j.f
  mod_bc_k.f
  monitors/area_average.f90
  monitors/average.f90
  monitors/functions.f90
  monitors/minmax.f90
  monitors/monitor_mod.f90
  monitors/volume_average.f90
  monitors/write_monitor.f90
  open_file.f
  open_files.f
  out_array.f
  out_array_c.f
  out_array_k.f
  out_array_kc.f
  out_bin_512.f
  out_bin_512i.f
  out_bin_512r.f
  out_bin_r.f
  output_manager.f
  output_mod.f
  parallel_mod.f
  param1_mod.f
  param_mod.f
  parse_line.f
  parse_mod.f
  parse_resid_string.f
  parse_rxn.f
  partial_elim.f
  pause.f
  pgcor_mod.f
  physical_prop.f
  physprop_mod.f
  ps_mod.f
  pscor_mod.f
  qmomk/qmom_kinetic_equation_mod.f
  qmomk/qmomk_allocate_arrays.f
  qmomk/qmomk_bc_mod.f
  qmomk/qmomk_collision_mod.f
  qmomk/qmomk_fluxes_mod.f
  qmomk/qmomk_gas_drag.f
  qmomk/qmomk_init_bc.f
  qmomk/qmomk_init_namelist.f
  qmomk/qmomk_initial_conditions.f
  qmomk/qmomk_make_arrays.f
  qmomk/qmomk_parameters_mod.f
  qmomk/qmomk_quadrature_mod.f
  qmomk/qmomk_read_restart.f
  qmomk/qmomk_set_bc.f
  qmomk/qmomk_time_march.f
  qmomk/qmomk_tools_mod.f
  qmomk/qmomk_write_restart.f
  radial_vel_correction.f
  rdf_mod.f
  read_database.f
  read_namelist.f
  read_res1.f
  reinitialize.f
  remove_comment.f
  reset_new.f
  residual_mod.f
  residual_pub_mod.f
  rrates.f
  rrates0.f
  rrates_init.f
  run_mod.f
  rxn_com_mod.f
  rxns_mod.f
  scalar_prop.f
  scalars_mod.f
  scales_mod.f
  set_bc0.f
  set_bc1.f
  set_bc_flow.f
  set_constprop.f
  set_flags.f
  set_fluidbed_p.f
  set_geometry.f
  set_geometry1.f
  set_ic.f
  set_icbc_flags.f
  set_increments.f
  set_index1.f
  set_index1a.f
  set_max2.f
  set_mw_mix_g.f
  set_outflow.f
  set_param.f
  set_ps.f
  set_ro_g.f
  set_ro_s.f
  set_wall_bc.f
  shift_dxyz.f
  solids_pressure_mod.f
  solve_continuity.f
  solve_energy_eq.f
  solve_epp.f
  solve_granular_energy.f
  solve_k_epsilon_eq.f
  solve_lin_eq.f
  solve_pp_g.f
  solve_scalar_eq.f
  solve_species_eq.f
  solve_vel_star.f
  source_granular_energy.f
  source_phi.f
  source_pp_g.f
  source_rop_g.f
  source_rop_s.f
  source_u_g.f
  source_u_s.f
  source_v_g.f
  source_v_s.f
  source_w_g.f
  source_w_s.f
  tau_g_mod.f
  tau_s_mod.f
  tau_u_g.f
  tau_u_s.f
  tau_v_g.f
  tau_v_s.f
  tau_w_g.f
  tau_w_s.f
  test_lin_eq.f
  thermochemical/get_values.f
  thermochemical/read_thermochemical_mod.f
  time_cpu_mod.f
  time_step.f
  toleranc_mod.f
  trace_mod.f
  transport_prop.f
  turb_mod.f
  undef_2_0.f
  under_relax.f
  update_old.f
  ur_facs_mod.f
  usr0.f
  usr1.f
  usr2.f
  usr3.f
  usr_drag.f
  usr_init_namelist.f
  usr_mod.f
  usr_prop_mod.f
  usr_properties.f
  usr_rates.f
  usr_read_namelist.f
  usr_sources.f
  usr_src_mod.f
  usr_write_out0.f
  usr_write_out1.f
  utilities_mod.f
  vavg_u_g.f
  vavg_u_s.f
  vavg_v_g.f
  vavg_v_s.f
  vavg_w_g.f
  vavg_w_s.f
  vf_gs_x.f
  vf_gs_y.f
  vf_gs_z.f
  visc_g_mod.f
  visc_s_mod.f
  vshear_mod.f
  vtc_scalar.f
  write_ab_m.f
  write_ab_m_var.f
  write_error.f
  write_header.f
  write_out0.f
  write_out1.f
  write_out3.f
  write_res0.f
  write_res1.f
  write_spx0.f
  write_spx1.f
  write_table.f
  write_usr0.f
  write_usr1.f
  xerbla.f
  xsi_mod.f
  zero_norm_vel.f)

if(ENABLE_CROW)
  set(F90_sources
    ${F90_sources}
    crow/crowmfix_wrapper.f
  )
endif()

set_source_files_properties(${F90_sources} PROPERTIES Fortran_FORMAT "FREE")
set_source_files_properties(${F77_sources} PROPERTIES Fortran_FORMAT "FIXED")
set_source_files_properties(${F77_sources} PROPERTIES COMPILE_FLAGS "-w")

add_library( mfixcore STATIC ${F90_sources} ${F77_sources} )

if(APPLE)
  set(CMAKE_C_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
  set(CMAKE_CXX_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
  set(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
endif()

target_include_directories(mfixcore PUBLIC include)
target_include_directories(mfixcore PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/mod)
set_target_properties(mfixcore PROPERTIES Fortran_MODULE_DIRECTORY mod)

# set preprocessor
target_compile_options(mfixcore PUBLIC $<$<STREQUAL:${CMAKE_Fortran_COMPILER_ID},GNU>:-cpp>)
target_compile_options(mfixcore PUBLIC $<$<STREQUAL:${CMAKE_Fortran_COMPILER_ID},Flang>:-Mpreprocess>)
target_compile_options(mfixcore PUBLIC $<$<STREQUAL:${CMAKE_Fortran_COMPILER_ID},Intel>:-cpp>)
target_compile_options(mfixcore PUBLIC $<$<STREQUAL:${CMAKE_Fortran_COMPILER_ID},PGI>:-Mpreprocess>)

# set bigendian
set(INTEL_ENDIAN -convert big_endian -assume byterecl)
set(PGI_ENDIAN -Mextend -byteswapio)
set(FLANG_ENDIAN -Mextend -byteswapio)
set(GNU_ENDIAN -fconvert=big-endian)
target_compile_options(mfixcore PUBLIC $<$<STREQUAL:${CMAKE_Fortran_COMPILER_ID},GNU>:${GNU_ENDIAN}>)
target_compile_options(mfixcore PUBLIC $<$<STREQUAL:${CMAKE_Fortran_COMPILER_ID},Flang>:${FLANG_ENDIAN}>)
target_compile_options(mfixcore PUBLIC $<$<STREQUAL:${CMAKE_Fortran_COMPILER_ID},Intel>:${INTEL_ENDIAN}>)
target_compile_options(mfixcore PUBLIC $<$<STREQUAL:${CMAKE_Fortran_COMPILER_ID},PGI>:${PGI_ENDIAN}>)

# remove line length limit for gfortran
target_compile_options(mfixcore PUBLIC $<$<STREQUAL:${CMAKE_Fortran_COMPILER_ID},GNU>:-ffree-line-length-0>)

if(MINGW)
  target_compile_definitions( mfixcore PUBLIC MINGW )
endif()

if(SPECIES_INC)
  target_compile_definitions( mfixcore PUBLIC SPECIES_INC=\"${SPECIES_INC}\" )
endif()

#
# Pymfix
#
if(ENABLE_PYMFIX)
  target_compile_definitions(mfixcore PUBLIC PYMFIX)
  set_property(TARGET mfixcore PROPERTY POSITION_INDEPENDENT_CODE ON)
endif()

#
# Crow
#
if(ENABLE_CROW)
  target_compile_definitions(mfixcore PUBLIC CROW PYMFIX)
endif()

# Define BURCAT.THR as path to Burcat database file
target_compile_definitions( mfixcore PUBLIC
  BURCAT_THR=\"${PROJECT_SOURCE_DIR}/model/thermochemical/BURCAT.THR\" )

#
# MPI
#

if(ENABLE_MPI)

  if(NOT MPI_Fortran_COMPILER)
    message(FATAL_ERROR "Setting ENABLE_MPI requires specifying MPI_Fortran_COMPILER as the MPI compiler wrapper ${MPI_Fortran_COMPILER}")
  elseif("gfortran" STREQUAL MPI_Fortran_COMPILER)
    message(WARNING "ENABLE_MPI is enabled but MPI_Fortran_COMPILER is set to gfortran\nMPI_Fortran_COMPILER should be set to MPI wrapper (such as mpifort)")
  else()
    message(STATUS "Using MPI_Fortran_COMPILER: ${MPI_Fortran_COMPILER}")
  endif()

  find_package(MPI REQUIRED)
  target_compile_options( mfixcore PUBLIC ${MPI_Fortran_COMPILE_FLAGS} )
  target_compile_definitions( mfixcore PUBLIC MPI )
  include_directories(${MPI_Fortran_INCLUDE_PATH})
  target_link_libraries(mfixcore ${MPI_Fortran_LIBRARIES})
endif()

#
# OpenMP
#

if(ENABLE_OpenMP)
  find_package(OpenMP REQUIRED)
  target_compile_options( mfixcore PUBLIC ${OpenMP_Fortran_FLAGS} )
  target_link_libraries( mfixcore PUBLIC ${OpenMP_Fortran_LIBRARIES} )
endif()

#
# NetCDF
#
if(ENABLE_NETCDF)
  target_compile_definitions( mfixcore PUBLIC NETCDF )

  find_package(netCDF REQUIRED)
  target_include_directories(mfixcore PUBLIC ${netCDF_INCLUDE_DIR})
  find_library(NETCDFF_LIBRARY
    NAMES netcdff
    HINTS ${netCDF_LIB_DIR})
  target_link_libraries(mfixcore ${NETCDFF_LIBRARY})
endif()

find_package(Git)
set( VERSION_FILE ${CMAKE_SOURCE_DIR}/.tarball-version )

target_compile_definitions(mfixcore PUBLIC MFIX_VERSION=${MFIX_VERSION})

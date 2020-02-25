set(POST_sources
  any_more_data.f
  calc_cell2.f
  calc_corr_01.f
  calc_corr_type_1.f
  calc_distance.f
  calc_dpody.f
  calc_ep_g.f
  calc_quantities.f
  calc_ro_g.f
  calc_vol.f
  correl_mod.f
  deallocate_arrays.f
  des/write_des_data.f
  examine_data.f
  f_init_data.f
  file_handle.f
  finit.f
  flow_gx.f
  flow_gy.f
  flow_gz.f
  flow_sx.f
  flow_sy.f
  flow_sz.f
  functions_mod.f
  gas_flux.f
  get_file_name.f
  get_file_status.f
  get_index.f
  get_location.f
  get_mu_s.f
  get_same_time.f
  get_selection.f
  get_substr.f
  granular_qty.f
  header_main.f
  ik_avg.f
  ik_avg_out.f
  interp_res.f
  main_f.f
  ornl_corr.f
  ornl_corr_c.f
  ornl_filt.f
  ornl_filt_c.f
  ornl_ft.f
  ornl_ft_c.f
  ornl_header.f
  ornl_pca.f
  ornl_stats.f
  ornl_stats_c.f
  ornl_sym.f
  ornl_util.f
  ornl_zone.f
  out_from_res.f
  out_from_spx.f
  out_spec_time.f
  out_time.f
  paralleldata_mod.f
  post3d_mod.f
  post_precision_mod.f
  print_out.f
  read_res0.f
  read_spx0.f
  read_spx1.f
  res_from_spx.f
  seek_time.f
  select_spx_rec.f
  set_dollar.f
  set_read_spx.f
  sol_flux.f
  strcmp.f
  streqs.f
  time_avg.f
  usr_input.f
  usr_input_mod.f
  usr_post.f
  usr_write_out1.f)

set_source_files_properties(${POST_sources} PROPERTIES Fortran_FORMAT "FREE")

add_executable(postmfix EXCLUDE_FROM_ALL ${POST_sources})

set_target_properties(postmfix
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
)

# Inherit compilation options from mfixcore
target_link_libraries( postmfix mfixcore )
target_compile_definitions(mfixsolver PUBLIC $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_DEFINITIONS>)
target_compile_options(mfixsolver PUBLIC $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_OPTIONS>)
target_include_directories(mfixsolver PUBLIC $<TARGET_PROPERTY:mfixcore,INTERFACE_INCLUDE_DIRECTORIES>)

if(ENABLE_MPI)
  target_link_libraries(postmfix ${MPI_Fortran_LIBRARIES})
endif()

if(ENABLE_NETCDF)
  target_link_libraries(postmfix ${NETCDF_F90_LIBRARIES})
endif()

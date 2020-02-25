MODULE monitor

  Use param1
! Maximum number of solids phases.
  use param, only: DIM_M
! Maximum number of gas phase species
  use param, only: DIM_N_g
! Maximum number of solids phase species
  use param, only: DIM_N_s
! Maximum number of scalar equations
  use param, only: DIM_Scalar
! Maximum number of DEM solids phase species
  use param, only: DIMENSION_N_S

  integer, parameter :: dimension_monitor = 100
  integer, parameter :: monitor_nrrmax = 100

  character(len=256) :: monitor_name(dimension_monitor)
  integer :: monitor_type(dimension_monitor)
  double precision :: monitor_dt(dimension_monitor)

  logical :: monitor_defined (dimension_monitor)
  double precision :: monitor_time(dimension_monitor)
  integer :: monitor_var_count(dimension_monitor)

! Spatial location of monitors
  double precision :: monitor_x_w(dimension_monitor)
  double precision :: monitor_x_e(dimension_monitor)
  double precision :: monitor_y_s(dimension_monitor)
  double precision :: monitor_y_n(dimension_monitor)
  double precision :: monitor_z_b(dimension_monitor)
  double precision :: monitor_z_t(dimension_monitor)

! Fluid phase variables
  logical :: monitor_ep_g(dimension_monitor)
  logical :: monitor_ro_g(dimension_monitor)
  logical :: monitor_p_g(dimension_monitor)
  logical :: monitor_u_g(dimension_monitor)
  logical :: monitor_v_g(dimension_monitor)
  logical :: monitor_w_g(dimension_monitor)
  logical :: monitor_t_g(dimension_monitor)
  logical :: monitor_x_g(dimension_monitor, dim_n_g)

  logical :: monitor_k_turb_g(dimension_monitor)
  logical :: monitor_e_turb_g(dimension_monitor)

! Eulerian solids variables
  logical :: monitor_ep_s (dimension_monitor, dim_m)
  logical :: monitor_rop_s(dimension_monitor, dim_m)
  logical :: monitor_ro_s(dimension_monitor, dim_m)
  logical :: monitor_p_star(dimension_monitor)
  logical :: monitor_u_s(dimension_monitor, dim_m)
  logical :: monitor_v_s(dimension_monitor, dim_m)
  logical :: monitor_w_s(dimension_monitor, dim_m)
  logical :: monitor_t_s(dimension_monitor, dim_m)
  logical :: monitor_x_s(dimension_monitor, dim_m, dim_n_s)
  logical :: monitor_theta_m(dimension_monitor, dim_m)

  logical :: monitor_scalar(dimension_monitor, dim_scalar)

  logical :: monitor_rrate(dimension_monitor,monitor_nrrmax)


! MONITOR Particle selection: 'C': center, 'P': entire particle, 'I': particle intersect
  character(len=1) :: monitor_select_mode(dimension_monitor)

! Lagrangian solids (not yet supported)
  logical :: monitor_part_phase(dimension_monitor, dim_m)
  logical :: monitor_part_diameter(dimension_monitor)
  logical :: monitor_part_vel(dimension_monitor)
  logical :: monitor_part_angular_vel(dimension_monitor)
  logical :: monitor_part_orientation(dimension_monitor)
  logical :: monitor_part_usr_var(dimension_monitor,3)
  logical :: monitor_part_temp(dimension_monitor)
  logical :: monitor_part_x_s(dimension_monitor,100)
  logical :: monitor_part_cohesion(dimension_monitor)
  logical :: monitor_part_id(dimension_monitor)

end module monitor

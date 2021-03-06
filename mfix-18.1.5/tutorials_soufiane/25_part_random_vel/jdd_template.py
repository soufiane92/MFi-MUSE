Template="""
########################################################################
#                                                                      #
# Author:  Sreekanth Pannala                            Date: 01-2010  #
# Modified: J. Musser                                   Date: 05-2014  #
#                                                                      #
#  Description:                                                        #
#  Freely falling particles under gravity that collide with a wall.    #
#  This is an extension of the regular freely-falling particle test    #
#  case to run parallel.                                               #
#                                                                      #
#  References:                                                         #
#  R. Garg, J. Galvin, T. Li, and S. Pannala, Documentation of open-   #
#     source MFIX-DEM software for gas-solids flows," from URL:        #
#     https://mfix.netl.doe.gov/documentation/dem_doc_2012-1.pdf,      #
#                                                                      #
########################################################################

  RUN_NAME = 'TEST_PARALLELISATION'
  DESCRIPTION = 'TEST PARALLELISATION'


#_______________________________________________________________________
# RUN CONTROL SECTION

  RUN_TYPE = 'NEW'
  UNITS = 'SI'

  TIME =  0.0
  TSTOP = in_tstop

  DT =     1.0d-2
  DT_MAX = 1.0d-2

  ENERGY_EQ = .F.

  SPECIES_EQ(0:1) =  .F.  .F.

  MOMENTUM_X_EQ(0:1) = .F.  .F.
  MOMENTUM_Y_EQ(0:1) = .F.  .F.
  MOMENTUM_Z_EQ(0:1) = .F.  .F.

  CALL_USR = .T.

#_______________________________________________________________________
# NUMERICAL SECTION



#_______________________________________________________________________
# GEOMETRY SECTION

  COORDINATES = 'CARTESIAN'

  !XLENGTH  = 1.0   !IMAX = 6
  !YLENGTH  = 1.0   !JMAX = 6
  !ZLENGTH  = 1.0   !KMAX = 1

  x_min = in_xmin
  x_max = in_xmax
  imax = in_imax

  y_min = in_ymin
  y_max = in_ymax
  jmax = in_jmax

  !z_min = 0.0
  !z_max = 1.0
  !kmax = 6

  cartesian_grid = .FALSE.


  CYCLIC_X = .TRUE.
  CYCLIC_Y = .TRUE.

#_______________________________________________________________________
# MATERIAL SECTION

! Gas Section
!---------------------------------------------------------------------//

  RO_g0 = 0.0
  MU_g0 = 0.2


! PARTICLE SECTION
!---------------------------------------------------------------------//
  MMAX = 1
  !ep_star = 0.42

  !GRAVITY_X = 0
  GRAVITY_Y = 0
  !GRAVITY_Z = 0

  PARTICLES = NB_PARTICLES               ! Number of particles

  !GENER_PART_CONFIG = .TRUE.
  
  !NO_K = .FALSE.
  NO_K = .TRUE. 

  FACTOR_RLM = 1.5
  
! Friction coefficients. (1)
  MEW =   0.0                  ! particle-particle
  MEW_W = 0.0                  ! particle-wall

  DES_NEIGHBOR_SEARCH =  1     ! 4 = Grid based neighbor search, 1 = Nsquare
  NEIGHBOR_SEARCH_N   =  1     ! Steps between neighbor search

  DESGRIDSEARCH_IMAX = in_imax
  DESGRIDSEARCH_JMAX = in_jmax
  DESGRIDSEARCH_KMAX = 2

  DES_EN_INPUT = 0.9
  DES_EN_WALL_INPUT = 0.9
  DES_INTG_METHOD = 'intg_meth'
  KN = 10000
  KN_W = 10000

!......................................................................!
! The following keywords are required inputs for the simulation. They  !
! are not specified here because they are passed as run time arguments.!
!                                                                      !
! Normal collision spring constant. (N/m)                              !
! KN =   xxxxx                 ! particle-particle                     !
! KN_W = xxxxx                 ! particle-wall                         !
!                                                                      !
! Restitution coefficient. (1)                                         !
! DES_EN_INPUT =      xxxxx    ! particle-particle                     !
! DES_EN_WALL_INPUT = xxxxx    ! particle-wall                         !
!......................................................................!


 


! Solids phase 1
!---------------------------------------------------------------------//
  SOLIDS_MODEL(1) = 'DEM'

  D_p0(1) =    IN_DIAM    ! (m)
  RO_s0(1) =   2600   ! (kg/m)


  DES_USR_VAR_SIZE = var_size

#_______________________________________________________________________
# INITIAL CONDITIONS SECTION

  IC_X_w(1) =     in_xmin   ! (m)
  IC_X_e(1) =     in_xmax   ! (m)
  IC_Y_s(1) =     0.0   ! (m)
  IC_Y_n(1) =     1.0   ! (m)
  IC_Z_b(1) =     0.0   ! (m)
  IC_Z_t(1) =     1.0   ! (m)
  
  IC_EP_g(1) =    0.1   ! (1)
  !IC_EP_g(1) =    1.0 

  IC_EP_S(1,1) = 0.9
  !IC_EP_S(1,1) = 1.0

  IC_U_S(1,1) = 0.0
  IC_V_S(1,1) = 0.0
  IC_W_S(1,1) = 0.0

  IC_THETA_M(1,1) = 0.0

  IC_P_g(1) =     0.00   ! (Pa)

  IC_U_g(1) =     0.00   ! (m/sec)
  IC_V_g(1) =     0.00   ! (m/sec)
  IC_W_g(1) =     0.00   ! (m/sec)


#_______________________________________________________________________
# BOUNDARY CONDITIONS SECTION

! None: Using default walls.


#_______________________________________________________________________
# USER OUTPUT CONTROL

  USR_DT(1) = 5.0d-3
  USR_TYPE(1) = 'ASCII'
  USR_VAR(1) =  'Particle position and velocity'


#_______________________________________________________________________
# OUTPUT CONTROL SECTION

  RES_DT = 1.0d3  ! interval to update restart (.RES) file

  FULL_LOG = .F.  ! display residuals on screen
  NLOG = 250      ! time steps between updates to (.LOG) file

! Interval at which .SPX files are written
  SPX_DT(1:9) = 9*1.0d3

  GROUP_RESID = .T.

  PRINT_DES_DATA = .F.


#_______________________________________________________________________
# DMP SETUP

  write_vtk_files = .True.
  time_dependent_filename = .True.
  vtk_data(1) = 'P'
  
  vtk_filebase(1) = 'particles'
  vtk_dt(1) = 0.01
  vtk_nxs(1) = 0
  vtk_nys(1) = 0
  vtk_nzs(1) = 0
  vtk_part_diameter(1) = .True.
  vtk_part_vel(1) = .True.  
  vtk_part_rank(1) = .True.
  NODESI = CPU_I
  NODESJ = CPU_J
  NODESK = 1

  ENABLE_DMP_LOG = .F.
  CHK_BATCHQ_END = .F.
"""

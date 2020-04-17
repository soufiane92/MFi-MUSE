#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Simulation Units
Simulations can be setup using the International System of Units (SI) or the centimetergram-second system (CGS). Although the majority of units are consistent with the specified systems, there are exceptions. The following table provides the SI and CGS units employed by MFIX for various quantities.


Quantity                      MFIX SI unit              MFIX CGS unit

length, position              meter(m)                  centimeter (cm)
mass                          kilogram (kg)             gram (g)
time                          second (s)                second (s)
thermal temperature           Kelvin (K)                Kelvin (K)
energy†                       Joule (J)                 calorie (cal)
amount of substance‡          kilomole (kmol)           mole (mol)
force                         Newton (1 N = 1 kg·m·s-2) dyne (1 dyn = 1 g·cm·s-2)
pressure                      Pascal (1 Pa = 1 N·m-2)   barye (1 Ba = 1 dyn·cm-2)
dynamic viscosity             Pa·s                      poise (1 P = 1 g·cm-1·s-1)
kinematic viscosity           m2·s-1                    Stokes (1 St = 1 cm2·s-1)
gas constant                  J·K-1·kmol-1              erg·K-1·mol-1
enthalpy                      J                         cal
specific heat                 J·kg-1·K-1                cal·g-1·K-1
thermal conductivity          J·m-1·K-1·s-1             cal·cm-1·K-1·s-1

† The CGS unit for energy is the ergon (1 erg = 1 dyne·cm). This is reflected in MFIX
through the gas constant. However, all thermochemical properties related to the energy
equations are based in calories for CGS units. Entries in the Burcat database are
always specified in terms of calories regardless of the simulation units. MFIX converts
the entries to joules after reading the database when SI units are used.
‡ The SI unit for the amount of a substance is the mole (mol). These units are needed
when specifying reaction rates:
• amount per time per volume for Eulerian model reactions
• amount per time for Lagrangian model reactions
"""

import re

from mfixgui.namelistparser import getKeywordDoc


# Running this file will add to the definitions below, but will not modify or delete any
# existing definitions

cgs_to_SI = {
  'aggregation_eff':1,    # factor                 : Success-factor for aggregation.
  'alpha_max':      1,    # factor                 : Maximum acceptable value of interpolation correction factor.
  'asperities':     0.01, # cm -> m                : Mean radius of surface asperities that influence the
  'bar_resolution': 1,    # max = 100.0            : Update frequency of progress bar, expressed in percent of
  'batch_wallclock':1,    # s                      : Total wall-clock duration of the job, in seconds.
  'bc_c_scalar':    100,  # 1/cm -> 1/m            : Specified constant scalar flux, C, in diffusion boundary
  'bc_c_t_g':       100,  # K/cm -> K/m            : Specified constant gas phase heat flux, C, in diffusion
  'bc_c_t_s':       100,  # K/cm -> K/m            : Specified constant solids phase heat flux, C, in diffusion
  'bc_c_theta_m':   0.01, # cm/s^2 -> m/s^2        : Specified constant flux, C, in diffusion boundary condition:
  'bc_c_x_g':       100,  # 1/cm -> 1/m            : Specified constant gas species mass flux, C, in diffusion
  'bc_c_x_s':       100,  # 1/cm -> 1/m            : Specified constant solids species mass flux, C, in diffusion
  'bc_dt_0':        1,    # s                      : The interval at the beginning when the normal velocity at
  'bc_dt_h':        1,    # s                      : The interval when normal velocity is equal to BC_Jet_gh.
  'bc_dt_l':        1,    # s                      : The interval when normal velocity is equal to BC_JET_gL.
  'bc_e_turb_g':    0.0001,# cm^2/s^3 -> m^2/s^3   : Boundary value of Epsilon for K-Epsilon Equation.
  'bc_ep_g':        1,    # void fraction          : Void fraction at the BC plane.
  'bc_ep_s':        1,    # volume fraction        : Solids volume fraction at the BC plane.
  'bc_hw_g':        100,  # 1/cm -> 1/m            : Gas phase hw for partial slip boundary.
  'bc_hw_s':        100,  # 1/cm -> 1/m            : Solids phase hw for partial slip boundary.
  'bc_hw_scalar':   100,  # 1/cm -> 1/m            : Scalar transfer coefficient, Hw, in diffusion boundary
  'bc_hw_t_g':      100,  # 1/cm -> 1/m            : Gas phase heat transfer coefficient, Hw, in diffusion
  'bc_hw_t_s':      100,  # 1/cm -> 1/m            : Solids phase heat transfer coefficient, Hw, in diffusion
  'bc_hw_theta_m':  100,  # 1/cm -> 1/m            : Transfer coefficient, Hw, in diffusion boundary condition:
  'bc_hw_x_g':      100,  # 1/cm -> 1/m            : Gas phase species mass transfer coefficient, Hw, in
  'bc_hw_x_s':      100,  # 1/cm -> 1/m            : Solid phase species mass transfer coefficient, Hw, in
  'bc_jet_g0':      0.01, # cm/s -> m/s            : Value of normal velocity during the initial interval
  'bc_jet_gh':      0.01, # cm/s -> m/s            : Value of normal velocity during the interval BC_DT_h.
  'bc_jet_gl':      0.01, # cm/s -> m/s            : Value of normal velocity during the interval BC_DT_L.
  'bc_k_turb_g':    0.0001,# cm^2/s^2 -> m^2/s^2   : Boundary value of K for K-Epsilon Equation.
  'bc_massflow_g':  0.001,# g/s -> kg/s            : Gas mass flow rate through the boundary.
  'bc_massflow_s':  0.001,# g/s -> kg/s            : Solids mass flow rate through the boundary.
  'bc_p_g':         0.1,  # barye -> Pa            : Gas pressure at the BC plane.
  'bc_pic_mi_const_statwt':None,# UNKNOWN          : Flag to specify the constant statistical weight for
  'bc_rop_s':       1000.0,# g/cm^3 -> kg/m^3      : Bulk density of solids phase at the BC plane.
  'bc_scalar':      None, # problem dependent      : Boundary value for user-defined scalar equation.
  'bc_scalarw':     1,    # scalar                 : Specified scalar value at the wall, ScalarW, in diffusion
  'bc_t_g':         1,    # K                      : Gas phase temperature at the BC plane.
  'bc_t_s':         1,    # K                      : Solids phase temperature at the BC plane.
  'bc_theta_m':     0.0001,# cm^2/s^2 -> m^2/s^2   : Solids phase granular temperature at the BC plane.
  'bc_thetaw_m':    0.0001,# cm^2/s^2 -> m^2/s^2   : Specified wall value, THETAw_M, in diffusion boundary
  'bc_tw_g':        1,    # K                      : Specified gas phase wall temperature, Tw_g, in diffusion
  'bc_tw_s':        1,    # K                      : Specified solids phase wall temperature, Tw_s, in diffusion
  'bc_u_g':         0.01, # cm/s -> m/s            : X-component of gas velocity at the BC plane.
  'bc_u_s':         0.01, # cm/s -> m/s            : X-component of solids-phase velocity at the BC plane.
  'bc_uw_g':        0.01, # cm/s -> m/s            : Gas phase Uw for partial slip boundary.
  'bc_uw_s':        0.01, # cm/s -> m/s            : Solids phase Uw for partial slip boundary.
  'bc_v_g':         0.01, # cm/s -> m/s            : Y-component of gas velocity at the BC plane.
  'bc_v_s':         0.01, # cm/s -> m/s            : Y-component of solids-phase velocity at the BC plane.
  'bc_volflow_g':   1e-06,# cm^3/s -> m^3/s        : Gas volumetric flow rate through the boundary.
  'bc_volflow_s':   1e-06,# cm^3/s -> m^3/s        : Solids volumetric flow rate through the boundary.
  'bc_vw_g':        0.01, # cm/s -> m/s            : Gas phase Vw for partial slip boundary.
  'bc_vw_s':        0.01, # cm/s -> m/s            : Solids phase Vw for partial slip boundary.
  'bc_w_g':         0.01, # cm/s -> m/s            : Z-component of gas velocity at the BC plane.
  'bc_w_s':         0.01, # cm/s -> m/s            : Z-component of solids-phase velocity at the BC plane.
  'bc_ww_g':        0.01, # cm/s -> m/s            : Gas phase Ww for partial slip boundary.
  'bc_ww_s':        0.01, # cm/s -> m/s            : Solids phase Ww for partial slip boundary.
  'bc_x_e':         0.01, # cm -> m                : X coordinate of the east face or edge.
  'bc_x_g':         1,    # mass fraction          : Mass fraction of gas species at the BC plane.
  'bc_x_s':         1,    # mass fraction          : Mass fraction of solids species at the BC plane.
  'bc_x_w':         0.01, # cm -> m                : X coordinate of the west face or edge.
  'bc_xw_g':        1,    # mass fraction          : Specified wall gas species mass fraction, Xw, in diffusion
  'bc_xw_s':        1,    # mass fraction          : Specified solids species mass fraction at the wall, Xw, in
  'bc_y_n':         0.01, # cm -> m                : Y coordinate of the north face or edge.
  'bc_y_s':         0.01, # cm -> m                : Y coordinate of the south face or edge.
  'bc_z_b':         0.01, # cm -> m                : Z coordinate of the bottom face or edge.
  'bc_z_t':         0.01, # cm -> m                : Z coordinate of the top face or edge.
  'bend_r1':        0.01, # cm -> m                : Bend Radius 1 (used when QUADRIC_FORM = BEND*),
  'bend_r2':        0.01, # cm -> m                : Bend Radius 2 (used when QUADRIC_FORM = BEND*),
  'bend_theta1':    1,    # angle                  : Bend start angle, in degrees (used when QUADRIC_FORM =
  'bend_theta2':    1,    # angle                  : Bend end angle, in degrees (used when QUADRIC_FORM = BEND*).
  'breakage_eff':   1,    # factor                 : Success-factor for breakage.
  'c':              None, # problem dependent      : User defined constants.
  'c2c_r1':         0.01, # cm -> m                : Cylinder-cone_cylinder Radius 1 (used when QUADRIC_FORM =
  'c2c_r2':         0.01, # cm -> m                : Cylinder-cone_cylinder Radius 2 (used when QUADRIC_FORM =
  'c2c_y1':         0.01, # cm -> m                : Cylinder-cone_cylinder Y1 (used when QUADRIC_FORM = C2C*).
  'c2c_y2':         0.01, # cm -> m                : Cylinder-cone_cylinder Y2 (used when QUADRIC_FORM = C2C*).
  'c_e':            1,    # max = 1.0              : Coefficient of restitution for particle-particle collisions.
  'c_f':            1,    # friction               : Coefficient of friction between the particles of two solids
  'c_pg0':          4184, # cal/g.K -> J/kg.K      : Specified constant gas specific heat .
  'c_ps0':          4184, # cal/g.K -> J/kg.K      : Specified constant solids specific heat .
  'cfl_pic':        1,    # s                      : CFL number used to decide maximum time step size for parcels
  'cg_ur_fac':      1,    # factor                 : Under-relaxation factor used in cut cells (only CG_UR_FAC(2)
  'clip_xmax':      0.01, # cm -> m                : Upper x-limit where the quadric is defined.
  'clip_xmin':      0.01, # cm -> m                : Lower x-limit where the quadric is defined.
  'clip_ymax':      0.01, # cm -> m                : Upper y-limit where the quadric is defined.
  'clip_ymin':      0.01, # cm -> m                : Lower y-limit where the quadric is defined.
  'clip_zmax':      0.01, # cm -> m                : Upper z-limit where the quadric is defined.
  'clip_zmin':      0.01, # cm -> m                : Lower z-limit where the quadric is defined.
  'cpx':            0.01, # cm -> m                : Location of control points in x-direction.
  'cpy':            0.01, # cm -> m                : Location of control points in y-direction.
  'cpz':            0.01, # cm -> m                : Location of control points in z-direction.
  'd_p0':           0.01, # cm -> m                : Initial particle diameters .
  'delp_x':         0.1,  # barye -> Pa            : Fluid pressure drop across XLENGTH when a cyclic boundary
  'delp_y':         0.1,  # barye -> Pa            : Fluid pressure drop across YLENGTH when a cyclic boundary
  'delp_z':         0.1,  # barye -> Pa            : Fluid pressure drop across ZLENGTH when a cyclic boundary
  'des_diffuse_width':0.01,# cm -> m               : The length scale used to smooth dispersed phase averaged
  'des_em':         1.0,  # emissivity             : Emissivity of solids phase.
  'des_en_input':   1,    # factor                 : The normal restitution coefficient for inter-particle
  'des_en_wall_input':1,  # factor                 : The normal restitution coefficient for particle-wall
  'des_et_input':   1,    # max = 1.0              : Tangential restitution coefficient for inter-particle
  'des_et_wall_input':1,  # max = 1.0              : Tangential restitution coefficient for particle wall
  'des_etat_fac':   1,    # max = 1.0              : Ratio of the tangential damping factor to the normal damping
  'des_etat_w_fac': 1,    # max = 1.0              : Ratio of the tangential damping factor to the normal damping
  'des_interp_width':0.01,# cm -> m                : The length used in interpolating data to/from a particle's
  'des_min_cond_dist':0.01,# cm -> m               : Minimum separation distance between the surfaces of two
  'dif_g0':         0.0001,# cm^2/s -> m^2/s       : Specified constant gas diffusivity .
  'dif_s0':         0.0001,# cm^2/s -> m^2/s       : Specified constant solids diffusivity .
  'dil_factor_vsd': 1,    # volume fraction        : Factor to define the dilute region where the solids density
  'dil_inert_x_vsd':1,    # mass fraction          : Mass fraction of inert solids phase species in the dilute
  'dquadric':       1.0,  # coeff                  : Coefficient D in equation (1).
  'drag_c1':        1.0,  # factor                 : Quantity for calibrating Syamlal-O'Brien drag correlation
  'drag_d1':        1.0,  # factor                 : Quantity for calibrating Syamlal-O'Brien drag correlation
  'dt':             1,    # s                      : Initial time step size. If left undefined, a steady-state
  'dt_fac':         1,    # max = 1.0              : Factor for adjusting time step. * The value must be less
  'dt_max':         1,    # s                      : Maximum time step size.
  'dt_min':         1,    # s                      : Minimum time step size.
  'dx':             0.01, # cm -> m                : Cell sizes in the x (r) direction. Enter values from DX(0)
  'dy':             0.01, # cm -> m                : Cell sizes in the y-direction. Enter values from DY(0) to
  'dz':             0.01, # cm -> m                : Cell sizes in the z (theta) direction. Enter values from
  'e_w':            1,    # max = 1.0              : Coefficient of restitution for particle-wall collisions when
  'e_young':        0.1,  # barye -> Pa            : Young's modulus for the particle . Required when using the
  'e_young_actual': 0.1,  # barye -> Pa            : Actual Young's modulus for the particle . Used for computing
  'ep_s_max':       1,    # volume fraction        : Maximum solids volume fraction at packing for polydisperse
  'ep_star':        1,    # void fraction          : Packed bed void fraction. Used to calculate plastic stresses
  'eps_f_min':      1,    # solids fraction        : Minimum solids fraction above which friction sets in.  (when
  'erx':            1,    # ratio                  : Expansion ratio (last DX/first DX) in a segment
  'ery':            1,    # ratio                  : Expansion ratio (last DY/first DY) in a segment
  'erz':            1,    # ratio                  : Expansion ratio (last DZ/first DZ) in a segment
  'ew_young':       0.1,  # barye -> Pa            : Young's modulus for the wall . Required when using the
  'ew_young_actual':0.1,  # barye -> Pa            : Actual Young's modulus for the walls . Used for computing
  'fac_dim_max_cut_cell':1,# factor                : Factor used to allocate cut cell arrays (expressed as a
  'factor_rlm':     1,    # factor                 : Effectively increase the radius of a particle (multiple of
  'filter_size_ratio':1,  # ratio                  : Ratio of filter size to computational cell size.
  'first_dx':       0.01, # cm -> m                : Value of first DX in a segment (x-direction). A negative
  'first_dy':       0.01, # cm -> m                : Value of first DY in a segment (y-direction). A negative
  'first_dz':       0.01, # cm -> m                : Value of first DZ in a segment (z-direction). A negative
  'flpc':           0.01, # cm -> m                : Fluid lens proportion constant used to calculate the radius
  'flux_g':         100,  # 1/cm -> 1/m            : If a value is specified, the domain-averaged gas flux is
  'fric_exp_pic':   None, # UNKNOWN                : Beta term in the frictional stress model of Snider.
  'gravity':        0.01, # cm/s^2 -> m/s^2        : Gravitational acceleration.
  'gravity_x':      0.01, # cm/s^2 -> m/s^2        : X-component of gravitational acceleration vector.
  'gravity_y':      0.01, # cm/s^2 -> m/s^2        : Y-component of gravitational acceleration vector.
  'gravity_z':      0.01, # cm/s^2 -> m/s^2        : Z-component of gravitational acceleration vector.
  'half_angle':     1,    # angle                  : Cone half angle, expressed in degrees (used when
  'hamaker_constant':1e-07,# erg -> J              : Hamaker constant used in particle-particle cohesive
  'ic_e_turb_g':    0.0001,# cm^2/s^3 -> m^2/s^3   : Initial value of Epsilon in K-Epsilon.
  'ic_ep_g':        1,    # void fraction          : Initial void fraction in the IC region.
  'ic_ep_s':        1,    # volume fraction        : Initial solids volume fraction of solids phase in the IC
  'ic_gama_rg':     None, # UNKNOWN                : Gas phase radiation coefficient in the IC region. Modify
  'ic_gama_rs':     None, # UNKNOWN                : Solids phase radiation coefficient in the IC region.
  'ic_k_turb_g':    0.0001,# cm^2/s^2 -> m^2/s^2   : Initial value of K in K-Epsilon.
  'ic_l_scale':     0.01, # cm -> m                : Turbulence length scale in the IC region.
  'ic_p_g':         0.1,  # barye -> Pa            : Initial gas pressure in the IC region. If this quantity is
  'ic_p_star':      0.1,  # barye -> Pa            : Initial solids pressure in the IC region. Usually, this
  'ic_pic_const_statwt':None,# UNKNOWN             : Flag to specify the initial constant statistical weight for
  'ic_rop_s':       1000.0,# g/cm^3 -> kg/m^3      : Initial bulk density (rop_s = ro_s x ep_s) of solids phase
  'ic_scalar':      None, # problem depedent       : Initial value of Scalar n.
  'ic_t_g':         1,    # K                      : Initial gas phase temperature in the IC region.
  'ic_t_rg':        1,    # K                      : Gas phase radiation temperature in the IC region.
  'ic_t_rs':        1,    # K                      : Solids phase radiation temperature in the IC region.
  'ic_t_s':         1,    # K                      : Initial solids phase temperature in the IC region.
  'ic_theta_m':     0.0001,# cm^2/s^2 -> m^2/s^2   : Initial solids phase granular temperature in the IC
  'ic_u_g':         0.01, # cm/s -> m/s            : Initial x-component of gas velocity in the IC region.
  'ic_u_s':         0.01, # cm/s -> m/s            : Initial x-component of solids-phase velocity in the IC
  'ic_v_g':         0.01, # cm/s -> m/s            : Initial y-component of gas velocity in the IC region.
  'ic_v_s':         0.01, # cm/s -> m/s            : Initial y-component of solids-phase velocity in the IC
  'ic_w_g':         0.01, # cm/s -> m/s            : Initial z-component of gas velocity in the IC region.
  'ic_w_s':         0.01, # cm/s -> m/s            : Initial z-component of solids-phase velocity in the IC
  'ic_x_e':         0.01, # cm -> m                : X coordinate of the east face.
  'ic_x_g':         1,    # mass fraction          : Initial mass fraction of gas species.
  'ic_x_s':         1,    # mass fraction          : Initial mass fraction of solids species.
  'ic_x_w':         0.01, # cm -> m                : X coordinate of the west face.
  'ic_y_n':         0.01, # cm -> m                : Y coordinate of the north face.
  'ic_y_s':         0.01, # cm -> m                : Y coordinate of the south face.
  'ic_z_b':         0.01, # cm -> m                : Z coordinate of the bottom face.
  'ic_z_t':         0.01, # cm -> m                : Z coordinate of the top face.
  'is_pc':          None, # depends on index       : Internal surface parameters
  'is_vel_s':       0.01, # cm/s -> m/s            : Value of fixed solids velocity through semipermeable
  'is_x_e':         0.01, # cm -> m                : X coordinate of the east face or edge.
  'is_x_w':         0.01, # cm -> m                : X coordinate of the west face or edge.
  'is_y_n':         0.01, # cm -> m                : Y coordinate of the north face or edge
  'is_y_s':         0.01, # cm -> m                : Y coordinate of the south face or edge
  'is_z_b':         0.01, # cm -> m                : Z coordinate of the bottom face or edge
  'is_z_t':         0.01, # cm -> m                : Z coordinate of the top face or edge
  'k_g0':           418.4,# cal/s.cm.K -> J/s.m.K  : Specified constant gas conductivity .
  'k_s0':           418.4,# cal/s.cm.K -> J/s.m.K  : Specified constant solids conductivity .
  'kn':             0.001,# dyne/cm -> N/m         : Normal spring constant  for inter-particle collisions.
  'kn_w':           0.001,# dyne/cm -> N/m         : Normal spring constant  for particle-wall collisions.
  'kt_fac':         1,    # max = 1.0              : Ratio of the tangential spring constant to normal spring
  'kt_w_fac':       1,    # max = 1.0              : Ratio of the tangential spring constant to normal spring
  'lam_hys':        0.01, # cm -> m                : The lubrication cutoff distance for HYS drag model.  In
  'lambda_x':       10000.0, # per Jeff Dieteker,  : Coefficient LAMBDA_X in equation (1) ('NORMAL' form) or
  'lambda_y':       10000.0, #  see issues/231     : Coefficient LAMBDA_Y in equation (1) ('NORMAL' form) or
  'lambda_z':       10000.0, #                     : Coefficient LAMBDA_Z in equation (1) ('NORMAL' form) or
  'last_dx':        0.01, # cm -> m                : Value of last DX in a segment (x-direction). A negative
  'last_dy':        0.01, # cm -> m                : Value of last DY in a segment (y-direction). A negative
  'last_dz':        0.01, # cm -> m                : Value of last DZ in a segment (z-direction). A negative
  'leq_tol':        1,    # tolerance              : Linear Equation tolerance .
  'max_inlet_vel_fac':0.01,# cm/s -> m/s           : The code declares divergence if the velocity anywhere in the
  'mew':            1,    # max = 1.0              : Inter-particle Coulomb friction coefficient.
  'mew_w':          1,    # max = 1.0              : Particle-wall Coulomb friction coefficient.
  'mppic_coeff_en1':1,    # coefficient            : First coefficient of restitution for the frictional stress
  'mppic_coeff_en2':1,    # coefficient            : Second coefficient of restitution for the frictional stress
  'mppic_coeff_en_wall':1,# coefficient            : Normal coefficient of restitution for parcel-wall collisions
  'mppic_coeff_et_wall':1,# coefficient            : Tangential coefficient of restitution for parcel-wall
  'mu_g0':          0.1,  # g/cm.s -> kg/m.s       : Specified constant gas viscosity .
  'mu_gmax':        0.1,  # barye.s -> Pa.s        : Maximum value of the turbulent viscosity of the fluid, which
  'mu_s0':          0.1,  # barye.s -> Pa.s        : Specified constant viscosity. If any value is specified
  'mw_avg':         1,    # g/mol                  : Average molecular weight of gas . Used in calculating the
  'mw_g':           1,    # g/mol                  : Molecular weight of gas species .
  'mw_s':           1,    # g/mol                  : Molecular weight of solids phase species .
  'n_x':            1.0, # unit vector             : X-component of normal vector defining the plane (used when
  'n_y':            1.0, # unit vector             : Y-component of normal vector defining the plane (used when
  'n_z':            1.0, # unit vector             : Z-component of normal vector defining the plane (used when
  'neighbor_search_rad_ratio':1,# ratio            : Ratio of the distance (imaginary sphere radius) to particle
  'norm_g':         1,    # factor                 : Factor to normalize the gas continuity equation residual.
  'norm_s':         1,    # factor                 : Factor to normalize the solids continuity equation residual.
  'out_dt':         1,    # s                      : Interval at which standard output (.OUT) file is updated.
  'out_msh_value':  1, # UNKNOWN                : Defines value of f outside of the .msh geometry. a value of
  'out_stl_value':  1, # UNKNOWN                : Defines value of F_STL outside of the STL geometry. A value
  'p_ref':          0.1,  # barye -> Pa            : Reference pressure.
  'p_scale':        1,    # factor                 : Scale factor for pressure.
  'phi':            1,    # angle                  : Angle of internal friction (in degrees). Set this value to
  'phi_w':          1,    # angle                  : Angle of internal friction (in degrees) at walls. Set this
  'phip':           1,    # max = 1.0              : Specularity coefficient associated with particle-wall
  'phip0':          0.01, # cm/s -> m/s            : Specify the value of specularity coefficient when the
  'piece_xmax':     0.01, # cm -> m                : Upper z-limit where the quadric is defined in a piecewise
  'piece_xmin':     0.01, # cm -> m                : Lower x-limit where the quadric is defined in a piecewise
  'piece_ymax':     0.01, # cm -> m                : Upper y-limit where the quadric is defined in a piecewise
  'piece_ymin':     0.01, # cm -> m                : Lower y-limit where the quadric is defined in a piecewise
  'piece_zmax':     0.01, # cm -> m                : Upper z-limit where the quadric is defined in a piecewise
  'piece_zmin':     0.01, # cm -> m                : Lower z-limit where the quadric is defined in a piecewise
  'ps_massflow_g':  0.001,# g/s -> kg/s            : Gas mass flow rate through the point source.
  'ps_massflow_s':  0.001,# g/s -> kg/s            : Solids mass flow rate through the point source.
  'ps_t_g':         1,    # K                      : Temperature of incoming gas.
  'ps_t_s':         1,    # K                      : Temperature of incoming solids.
  'ps_u_g':         0.01, # cm/s -> m/s            : X-component of incoming gas velocity.
  'ps_u_s':         0.01, # cm/s -> m/s            : X-component of incoming solids velocity.
  'ps_v_g':         0.01, # cm/s -> m/s            : Y-component of incoming gas velocity.
  'ps_v_s':         0.01, # cm/s -> m/s            : Y-component of incoming solids velocity.
  'ps_w_g':         0.01, # cm/s -> m/s            : Z-component of incoming gas velocity.
  'ps_w_s':         0.01, # cm/s -> m/s            : Z-component of incoming solids velocity.
  'ps_x_e':         0.01, # cm -> m                : X coordinate of the east face or edge.
  'ps_x_g':         1,    # mass fraction          : Gas phase incoming species n mass fraction.
  'ps_x_s':         1,    # mass fraction          : Solids phase incoming species n mass fraction.
  'ps_x_w':         0.01, # cm -> m                : X coordinate of the west face or edge.
  'ps_y_n':         0.01, # cm -> m                : Y coordinate of the north face or edge.
  'ps_y_s':         0.01, # cm -> m                : Y coordinate of the south face or edge.
  'ps_z_b':         0.01, # cm -> m                : Z coordinate of the bottom face or edge.
  'ps_z_t':         0.01, # cm -> m                : Z coordinate of the top face or edge.
  'psfac_fric_pic': None, # UNKNOWN                : P_s term in the frictional stress model of Snider.
  'quadric_scale':  1,    # factor                 : Scaling factor, applied to all quadric geometry parameters.
  'r_p':            1,    # coefficient            : Coefficient of restitution for particle-particle collisions
  'radius':         0.01, # cm -> m                : Cylinder radius (used when QUADRIC_FORM = *_CYL_***)
  'reactor1_r1':    0.01, # cm -> m                : Reactor 1, lower cylinder radius.
  'reactor1_r2':    0.01, # cm -> m                : Reactor 1, upper cylinder radius.
  'reactor1_rr1':   0.01, # cm -> m                : Reactor 1, lower rounding radius.
  'reactor1_rr2':   0.01, # cm -> m                : Reactor 1, upper rounding radius.
  'reactor1_theta1':1,    # angle                  : Reactor 1, lower rounding angle (degrees).
  'reactor1_theta2':1,    # angle                  : Reactor 1, upper rounding angle (degrees).
  'reactor1_y1':    0.01, # cm -> m                : Reactor 1, lower conical transition between cylinders.
  'reactor1_y2':    0.01, # cm -> m                : Reactor 1, upper conical transition between cylinders.
  'reactor1_yr1':   0.01, # cm -> m                : Reactor 1, lower rounding below cylinder.
  'reactor1_yr2':   0.01, # cm -> m                : Reactor 1, upper rounding above cylinder.
  'report_mass_balance_dt':1,# s                   : Frequency to perform an overall species mass balance.
  'res_backup_dt':  1,    # s                      : Interval at which a backup copy of the restart file is
  'res_dt':         1,    # s                      : Interval at which restart (.res) file is updated.
  'ro_g0':          1000.0,# g/cm^3 -> kg/m^3      : Specified constant gas density . An equation of state -the
  'ro_s0':          1000.0,# g/cm^3 -> kg/m^3      : Specified constant solids density . Reacting flows may use
  'ro_xs0':         1000.0,# g/cm^3 -> kg/m^3      : Specified constant solids species density .
  'scale_msh':      1,    # factor                 : Scaling factor, applied to the .msh geometry. Note that
  'scale_stl':      1,    # factor                 : Scaling factor, applied to the STL geometry. Note that
  'segregation_slope_coefficient':1.0,# coeff      : Used in calculating the initial slope of segregation: see
  'spx_dt':         1,    # s                      : Interval at which .SPX files are updated.
  'stl_small_angle':1,    # angle                  : Smallest angle accepted for valid STL triangles (in
  't_x':            0.01, # cm -> m                : Translation in x-direction.
  't_y':            0.01, # cm -> m                : Translation in y-direction.
  't_z':            0.01, # cm -> m                : Translation in z-direction.
  'term_buffer':    1,    # s                      : Buffer time specified to allow MFIX to write out the files
  'theta_x':        1,    # angle                  : Rotation angle with respect to x-axis (degrees).
  'theta_y':        1,    # angle                  : Rotation angle with respect to y-axis (degrees).
  'theta_z':        1,    # angle                  : Rotation angle with respect to z-axis (degrees).
  'time':           1,    # s                      : Simulation start time. This is typically zero.
  'tol_delh':       1,    # tolerance              : Tolerance used to limit acceptable values of normal distance
  'tol_diverge':    1,    # tolerance              : Minimum residual for declaring divergence . This parameter
  'tol_f':          1,    # tolerance              : Tolerance used to find intersection of quadric surfaces or
  'tol_msh':        1,    # tolerance              : Tolerance used to find intersection of .msh file with
  'tol_poly':       1,    # tolerance              : Tolerance used to find intersection of polygon with
  'tol_resid':      1,    # tolerance              : Maximum residual at convergence (Continuity + Momentum) .
  'tol_resid_k_epsilon':1,# tolerance              : Maximum residual at convergence (K_Epsilon Model) .
  'tol_resid_scalar':1,   # tolerance              : Maximum residual at convergence (Scalar Equations) .
  'tol_resid_t':    1,    # tolerance              : Maximum residual at convergence (Energy) .
  'tol_resid_th':   1,    # tolerance              : Maximum residual at convergence (Granular Energy) .
  'tol_resid_x':    1,    # tolerance              : Maximum residual at convergence (Species Balance) .
  'tol_small_area': 1,    # tolerance              : Tolerance used to detect small faces (expressed as a
  'tol_small_cell': 1,    # tolerance              : Tolerance used to detect small cells (expressed as a
  'tol_snap':       1,    # tolerance              : Tolerance used to snap an intersection point onto an
  'tol_stl':        1,    # tolerance              : Tolerance used to find intersection of STL triangles with
  'tol_stl_dp':     1,    # tolerance              : Dot product tolerance when determining if a point lies in a
  'torus_r1':       0.01, # cm -> m                : Torus Radius 1 (used when QUADRIC_FORM = TORUS_*), R1>R2 for
  'torus_r2':       0.01, # cm -> m                : Torus Radius 2 (used when QUADRIC_FORM = TORUS_*), R1>R2 for
  'tstop':          1,    # s                      : Simulation stop time.
  'tx_msh':         0.01, # cm -> m                : Translation in x-direction, applied to the .msh geometry.
  'tx_stl':         0.01, # cm -> m                : Translation in x-direction, applied to the STL geometry.
  'ty_msh':         0.01, # cm -> m                : Translation in y-direction, applied to the .msh geometry.
  'ty_stl':         0.01, # cm -> m                : Translation in y-direction, applied to the STL geometry.
  'tz_msh':         0.01, # cm -> m                : Translation in z-direction, applied to the .msh geometry.
  'tz_stl':         0.01, # cm -> m                : Translation in z-direction, applied to the STL geometry.
  'ucoil_r1':       0.01, # cm -> m                : U-shaped coil Radius 1 (used when QUADRIC_FORM = UCOIL*),
  'ucoil_r2':       0.01, # cm -> m                : U-shaped coil Radius 2 (used when QUADRIC_FORM = UCOIL*),
  'ucoil_y1':       0.01, # cm -> m                : U-shaped coil ymax (used when QUADRIC_FORM = UCOIL*),
  'ucoil_y2':       0.01, # cm -> m                : U-shaped coil ymin (used when QUADRIC_FORM = UCOIL*),
  'ur_f_gs':        1,    # s                      : The implicitness calculation of the gas-solids drag
  'ur_fac':         1,    # factor                 : Under relaxation factors.
  'ur_kth_sml':     1,    # factor                 : Under relaxation factor for conductivity coefficient
  'usr_dt':         1,    # s                      : Intervals at which subroutine write_usr1 is called.
  'usr_x_e':        0.01, # cm -> m                : Udf Hook: x coordinate of the east face or edge.
  'usr_x_w':        0.01, # cm -> m                : Udf Hook: x coordinate of the west face or edge.
  'usr_y_n':        0.01, # cm -> m                : Udf Hook: y coordinate of the north face or edge.
  'usr_y_s':        0.01, # cm -> m                : Udf Hook: y coordinate of the south face or edge.
  'usr_z_b':        0.01, # cm -> m                : Udf Hook: z coordinate of the bottom face or edge.
  'usr_z_t':        0.01, # cm -> m                : Udf Hook: z coordinate of the top face or edge.
  'v_ex':           None, # UNKNOWN                : Excluded volume in Boyle-Massoudi stress.
  'v_poisson':      1,    # ratio                  : Poisson's ratio for the particle. Required when using the
  'v_poisson_actual':1,   # ratio                  : Poisson's ratio for the particle. Used for computing
  'v_sh':           0.01, # cm/s -> m/s            : Specifies the mean y velocity component at the eastern
  'vdw_inner_cutoff':0.01,# cm -> m                : Minimum separation distance below which van der Waals forces
  'vdw_outer_cutoff':0.01,# cm -> m                : Maximum separation distance above which van der Waals forces
  'vtk_dt':         1,    # s                      : Interval (expressed in seconds of simulation time) at which
  'vtk_slice_tol':  1,    # tolerance              : Tolerance to detect particle in a VTK region.
  'vtk_x_e':        0.01, # cm -> m                : East location of VTK region.
  'vtk_x_w':        0.01, # cm -> m                : West location of VTK region.
  'vtk_y_n':        0.01, # cm -> m                : North location of VTK region.
  'vtk_y_s':        0.01, # cm -> m                : South location of VTK region.
  'vtk_z_b':        0.01, # cm -> m                : Bottom location of VTK region.
  'vtk_z_t':        0.01, # cm -> m                : West location of VTK region.
  'vw_poisson':     1,    # ratio                  : Poisson's ratio for the wall. Required when using the
  'vw_poisson_actual':1,  # ratio                  : Poisson's ratio for the wall. Used for computing correction
  'wall_hamaker_constant':1e-07,# erg -> J         : Hamaker constant used in particle-wall cohesive
  'wall_vdw_inner_cutoff':0.01,# cm -> m           : Minimum separation distance below which van der Waals forces
  'wall_vdw_outer_cutoff':0.01,# cm -> m           : Maximum separation distance above which van der Waals forces
  'x_max':          0.01, # cm -> m                : Reactor upper bound in the x-direction.
  'x_min':          0.01, # cm -> m                : Reactor lower bound in the x-direction.
  'x_s0':           1,    # mass fraction          : Baseline species mass fraction. Specifically, the mass
  'xlength':        0.01, # cm -> m                : Reactor length in the x (r) direction.
  'xmin':           0.01, # cm -> m                : The inner radius in the simulation of an annular cylindrical
  'y_max':          0.01, # cm -> m                : Reactor upper bound in the y-direction.
  'y_min':          0.01, # cm -> m                : Reactor lower bound in the y-direction.
  'ylength':        0.01, # cm -> m                : Reactor length in the y-direction.
  'z_max':          0.01, # cm -> m                : Reactor upper bound in the z-direction.
  'z_min':          0.01, # cm -> m                : Reactor lower bound in the z-direction.
  'zlength':        0.01, # cm -> m                : Reactor length in the z (theta) direction.
}


def main():

    def trim(s, maxlen=60):
        pat = re.compile(r'\[[^]]*\]') # Remove anything inside []
        s = pat.sub('', s)
        while len(s) > maxlen:
            for c in (' ', '.', ','):
                i = s.rindex(c)
                if i > 0:
                    s = s[:i]
                break
            else:
                break
        return s

    doc = getKeywordDoc()

    # We're not going to worry about any non-float keywords
    dp_keys = sorted([key for key, entry in doc.items() if entry['dtype'] == 'DP'])

    pat = re.compile(r'\[(.*) in SI.*\]')

    unit_to_SI = { 'm':    ('cm', 1e-2),
                   'kg':   ('g', 1e-3),
                   'J':    ('cal', 4.184),
                   'kmol': ('mol', 1e-3),
                   'N':    ('dyne', 1e-5),
                   'Pa':   ('barye', 0.1),
                   'K':    ('K', 1),
                   's':    ('s', 1),
    }

    # Try to get units from keyword doc. Add to predefined factors above

    new_cgs_to_SI = {}
    for key in dp_keys:
        entry = doc[key]

        desc = entry['description']
        if ' o ' in desc:
            desc = desc[:desc.index(' o ')] # Strip bullets
            entry['description'] = desc # Save it
        desc_lower = desc.lower()
        SI_unit = None

        # Handle keys with bad keyword doc first - we'll ignore the doc entry
        if 'gravity' in key:
            SI_unit = 'm/s^2'

        # Anything with a maximum must be dimensionless
        r = entry.get('validrange')
        if r:
            m = r.get('max')
            if m:
                SI_unit = 'max = %s' % m

        # Now look for description in keyword doc
        if SI_unit is None:
            match = pat.search(desc)
            if match:
                SI_unit = match.group(1)
                if SI_unit.startswith('(') and SI_unit.endswith(')'):
                    SI_unit = SI_unit[1:-1]

        # If units are not explicitly documented, add units for some known types of keys

        if SI_unit is None:

            # All 'dt's are time.
            if key.endswith('_dt'):
                SI_unit = 's'
            if 'factor' in desc_lower or 'factor' in key:
                SI_unit = 'factor'
            elif 'ratio' in desc_lower or 'ratio' in key:
                SI_unit = 'ratio'
            else:
                # Anything which is a fraction is dimensionless
                for s in ('volume fraction', 'void fraction', 'mass fraction', 'solids fraction'):
                    if s in desc_lower:
                        SI_unit = s
                        break

            if SI_unit:
                pass
            # Assuming tolerances dimensionless also,
            #although some are distances, fluxes, etc...
            elif ('tolerance' in desc_lower
                  or 'tol_' in key
                  or '_tol' in key):
                SI_unit = 'tolerance'
            # all 'dt' are in seconds, but watch out for 'width'
            elif ('_dt' in key
                  or (key.startswith('dt') and key != 'dt_fac')
                  or key=='time'
                  or key=='tstop'
                  or 'time' in desc_lower
                  or 'clock' in desc_lower):
                SI_unit = 's'
            # Coefficient of friction is unitless
            elif 'coefficient of friction' in desc_lower:
                SI_unit = 'friction'
            # Flow rates
            elif 'massflow' in key:
                SI_unit = 'kg/s'
            elif 'volflow' in key:
                SI_unit = 'm^3/s'
            # Length, but avoid 'across xlength'
            elif 'length' in desc_lower and 'across' not in desc_lower:
                SI_unit = 'm'
            # Region boundaries are all meters
            elif 'coordinate' in desc_lower:
                SI_unit = 'm'
            # as are VTK region locations
            elif 'location of' in desc_lower:
                SI_unit = 'm'
            elif 'cell size' in desc_lower:
                SI_unit = 'm'
            # [xyz]max
            elif any(s in key for s in
                     ('xmax', 'ymax', 'zmax', 'x_max', 'y_max', 'z_max',
                      'xmin', 'ymin', 'zmin', 'x_min', 'y_min', 'z_min')):
                SI_unit = 'm'
            # BC velocity keys
            elif any (key.startswith(x) for x in ('bc_uw', 'bc_vw', 'bc_ww')):
                SI_unit = 'm/s'
            elif 'heat flux' in desc_lower:
                SI_unit = 'K/m' # per GUI
            elif 'mass flux' in desc_lower:
                SI_unit = '1/m' # mass fraction per meter:  d(X_g)/dn + Hw (X_g - Xw_g) = C
            elif 'scalar flux' in desc_lower:
                SI_unit = '1/m'
            elif 'scalar value' in desc_lower:
                SI_unit = 'scalar'
            elif 'transfer coefficient' in desc_lower or key in ('bc_hw_g', 'bc_hw_s'):
                SI_unit = '1/m' # 'Hw' term in d(T_g)/dn + Hw (T_g - Tw_g) = C
            elif 'granular temperature' in desc_lower or key=='bc_thetaw_m':
                SI_unit = 'm^2/s^2' # per Jordan
            elif key == 'bc_c_theta_m': # Granular energy flux
                SI_unit = 'm/s^2' # per Jordan
            # (when granular temperature is units length^2/time^2 then hw = 1/length and c = length/time^2)
            # Temperatures are all Kelvin, except for granular
            elif 'temperature' in desc_lower:
                SI_unit = 'K'
            # Velocities are m/s
            elif 'velocity' in desc_lower:
                SI_unit = 'm/s'
            # Coefficient of restitition is dimensionless
            elif 'coefficient of restitution' in desc_lower:
                SI_unit = 'coefficient'
            # Densities
            elif 'density' in desc_lower:
                SI_unit = 'kg/m^3'
            # Radii are meters
            elif 'radius' in desc_lower:
                SI_unit = 'm'
            # Distances are too
            elif 'distance' in desc_lower:
                SI_unit = 'm'
            # Angles don't need to be converted
            elif 'angle' in desc_lower:
                SI_unit = 'angle'
            # K-epsilon
            elif 'k-epsilon' in desc_lower:
                if 'value of epsilon' in desc_lower:
                    SI_unit = 'm^2/s^3' # From GUI
                elif 'value of k' in desc_lower:
                    SI_unit = 'm^2/s^2'
            elif 'hamaker' in key: # Note ,this is being converted from erg, not kcal
                SI_unit = 'J'
            elif 'viscosity' in desc_lower:
                SI_unit = 'Pa.s'
            # Pressures are Pa # do this last, due to many references to 'pressure drop' etc
            elif any(p in desc_lower for p in ('gas pressure', 'solids pressure',
                                               'fluid pressure', 'reference pressure')):
                SI_unit = 'Pa'


        if SI_unit:
            if '/' in SI_unit:
                num, denom = SI_unit.split('/')
            else:
                num, denom = SI_unit, ''
            if num.startswith('(') and num.endswith(')'):
                num = num[1:-1]
            if denom.startswith('(') and denom.endswith(')'):
                denom = denom[1:-1]
            factor = 1.0
            comment = ''
            for term in num.split('.'):
                if '^' in term:
                    u, pow = term.split('^')
                    pow = int(pow)
                else:
                    u = term
                    pow = 1
                (c,x) = unit_to_SI.get(u, (u,1))
                if pow != 1:
                    c = '%s^%s' % (c, pow)
                    factor *= x**pow
                else:
                    factor *= x
                comment = '%s.%s'%(comment,c) if comment else c
            denom_comment = ''
            if denom:
                for term in denom.split('.'):
                    if '^' in term:
                        u, pow = term.split('^')
                        pow = int(pow)
                    else:
                        u = term
                        pow = 1
                    (c,x) = unit_to_SI.get(u, (u,1))
                    if pow != 1:
                        factor /= x**pow
                        c = '%s^%s' % (c, pow)
                    else:
                        factor /= x
                    denom_comment = '%s.%s'%(denom_comment,c) if denom_comment else c
            if denom_comment:
                comment = '%s/%s' % (comment, denom_comment)
            int_factor = int(factor)
            if int_factor == factor:
                factor = int_factor
            else:
                factor = round(factor, 10)
            if factor != 1:
                comment = '%s -> %s' % (comment, SI_unit)
                comment = comment.replace('(', '')
                comment = comment.replace(')', '')
            # Special hack for hamaker constant, which is being converted from
            # erg, not kcal
            if 'hamaker' in key.lower():
                factor = 1e-07
                comment = 'erg -> J'
            new_cgs_to_SI[key] = (factor, comment)

    infile = open(__file__, 'r', encoding='utf-8', errors='replace')

    lines = [line for line in infile]

    start = lines.index('cgs_to_SI = {\n') + 1
    end = lines.index('}\n')

    head = lines[:start]
    data = lines[start:end]
    tail = lines[end:]

    outfile = open(__file__ + '.tmp', 'w', encoding='utf-8', errors='replace')

    for line in head:
        outfile.write(line)

    line_by_key = {}
    for line in data:
        key = line.split(':',1)[0].strip()[1:-1] # remove quotes
        line_by_key[key] = line

    new_data = [] # Updated 'data' section
    for key in dp_keys:
        if cgs_to_SI.get(key): # Already have an entry
            new_data.append(line_by_key[key])
            continue

        factor, comment = new_cgs_to_SI.get(key, (None, ''))
        desc = doc[key].get('description').strip()
        l = "  '%s':" % key
        l += ' '*(20-len(l))
        l += '%s,' % factor
        l += ' '*(26-len(l))
        l += '# %s' % (comment if factor else 'UNKNOWN')
        l += ' '*(50-len(l))
        l += ' : ' + trim(desc)
        l = l.rstrip()+'\n'
        new_data.append(l)

    new_data.sort()
    for line in new_data:
        outfile.write(line)

    for line in tail:
        outfile.write(line)

    print("Generated %s" % (__file__ + '.tmp'))


if (__name__ == '__main__'):
    main()

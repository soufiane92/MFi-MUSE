!
!  First, define the user-defined namelist variables as DOUBLE PRECISION,
!  INTEGER, LOGICAL, or CHARACTER.  Second, include the variable in the
!  appropriate common.  Third, add the variable name in the NAMELIST section.
!  A dummy variable is defined, so that the code can be compiled with the
!  template include file.
!
      DOUBLE PRECISION DUMMY_DPA
!
!     Double Precision variables
      COMMON / USR_DATA_DP / DUMMY_DPA
!
!     Integer variables
!      COMMON / USR_DATA_I /
!
!     Logical variables
!      COMMON / USR_DATA_L /
!
!     Character variables
!      COMMON / USR_DATA_C /
!
!
      NAMELIST / USR_INPUT_DATA / DUMMY_DPA, SOLVENT_ABSORPTION, &
         MECH_DISPERSION, SPREAD_FACTOR, ABSORPTION_CHEM_TYPE, &
         ENHANCEMENT_FACTOR, CAP_PRESS_TYPE, USR_DRAG_TYPE, &
         LAM_MU_s0, LAM_MU_g0, sa_pack, d_pack, ch_pack, &
         omega_pack, omega_l0, &
         WETAREA_TYPE, APPLY_WAF, wetareafrac0

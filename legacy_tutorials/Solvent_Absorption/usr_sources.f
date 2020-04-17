!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Subroutine: USR_SOURCES                                             C
!  Purpose: Hook for user defined source terms                         C
!                                                                      C
!  Comments:                                                           C
!  Discretized equations take form as a matrix equation Ax=b.          C
!  Terms that may be represented as a coefficient of the dependent     C
!  variable go to the center coefficient (ap) through sourcelhs        C
!  (i.e., included on the left-hand-side (lhs)).                       C
!  Terms that are a constant go to the source vector (b) through       C
!  sourcerhs (i.e., included on the right-hand-side (rhs)).            C
!                                                                      C
!  Source terms are often a function of the dependent variable. To     C
!  aid in convergence, this dependency should be acknowledged in the   C
!  equation. Incorporate the dependency of the source term on the      C
!  dependent variable through both ap and b.                           C
!                                                                      C
!  See Patankar, S. V., Numerical heat transfer and fluid flow,        C
!  Taylor and Francis, 1980, for rules and suggestions for             C
!  appropriate discretization of the source term.                      C
!                                                                      C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR_SOURCES(lEQ_NO, IJK, sourcelhs, sourcerhs, M, N)

! Modules
!-----------------------------------------------
      use constant, only: pi, gas_const, gravity
      use fldvar, only: u_g, v_g, w_g
      use fldvar, only: u_s, v_s, w_s
      use fldvar, only: ep_g, rop_g, ro_g, T_g, X_g, P_g
      use fldvar, only: ep_s, rop_s, ro_s, T_s, X_s, d_p, theta_m
      use fldvar, only: k_turb_g, e_turb_g, scalar
      use functions
      use geometry
      use indices, only: i_of, j_of, k_of
      use indices, only: im1, ip1, jm1, jp1, km1, kp1
      use param1, only: zero, one, half, undefined, undefined_i
      use physprop
      use scalars, only: phase4scalar
      use usr_src
      use toleranc, only: dil_ep_s
      use usr, only: index_liq

      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! reference equation
      INTEGER, INTENT(IN) :: lEQ_NO
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (lhs) - part of a_m matrix
! source vector (rhs) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) :: sourcelhs, sourcerhs
! Phase index
      INTEGER, INTENT(IN) :: M
! Species index OR scalar equation number
! (if applicable otherwise undefined_i)
      INTEGER, INTENT(IN) :: N

! Local variables
!-----------------------------------------------
! source terms
      DOUBLE PRECISION :: scp, scc
      DOUBLE PRECISION :: scpA, scpB, scpC
      DOUBLE PRECISION :: sccA, sccB, sccC
! index
      INTEGER :: iliquid

!-----------------------------------------------
! initialize
      sourcelhs = zero
      sourcerhs = zero
      iliquid = index_liq

      SELECT CASE(lEQ_NO)

! source for pressure correction equation: pp_g
      CASE (PRESSURE_CORRECTION)

! source for solids correctione quation:: epp
      CASE (SOLIDS_CORRECTION)

! source for gas continuity equation: rop_g
      CASE (GAS_CONTINUITY)

! source for solids continuity equation: rop_s
      CASE (SOLIDS_CONTINUITY)

! source for gas u momentum equation: U_g
      CASE (GAS_U_MOM)
! Note at this point the volume fraction at the face should exceed
! dilute flow conditions but not necessarily at cell center!
         CALL MECH_DISPERSION_UG(IJK, SCPA, SCCA)
         CALL ADD_DRAG_TERM_UG(IJK, SCPB, SCCB)
         sourcerhs = SCPA + SCPB
         sourcelhs = SCCA + SCCB

! source for solids u momentum equation: U_s
      CASE (SOLIDS_U_MOM)
         IF (M /= iliquid) RETURN
         CALL CAP_PRESSURE_U(IJK, M, SCPA, SCCA)
         CALL MECH_DISPERSION_US(IJK, M, SCPB, SCCB)
         CALL ADD_DRAG_TERM_US(IJK, M, SCPC, SCCC)
         sourcerhs = SCPA + SCPB + SCPC
         sourcelhs = SCCA + SCCB + SCCC

! source for gas v momentum equation: V_g
      CASE (GAS_V_MOM)
         CALL MECH_DISPERSION_VG(IJK, SCPA, SCCA)
         CALL ADD_DRAG_TERM_VG(IJK, SCPB, SCCB)
         sourcerhs = SCPA + SCPB
         sourcelhs = SCCA + SCCB

! source for solids v momentum equation: V_s
      CASE (SOLIDS_V_MOM)
         IF (M /= iliquid) RETURN
         CALL CAP_PRESSURE_V(IJK, M, SCPA, SCCA)
         CALL MECH_DISPERSION_VS(IJK, M, SCPB, SCCB)
         CALL ADD_DRAG_TERM_VS(IJK, M, SCPC, SCCC)
         sourcerhs = SCPA + SCPB + SCPC
         sourcelhs = SCCA + SCCB + SCCC

! source for gas w momentum equation: W_g
      CASE (GAS_W_MOM)
         CALL MECH_DISPERSION_WG(IJK, SCPA, SCCA)
         CALL ADD_DRAG_TERM_WG(IJK, SCPB, SCCB)
         sourcerhs = SCPA + SCPB
         sourcelhs = SCCA + SCCB

! source for solids w momentum equation: W_s
      CASE (SOLIDS_W_MOM)
         IF (M /= iliquid) RETURN
         CALL CAP_PRESSURE_W(IJK, M, SCPA, SCCA)
         CALL MECH_DISPERSION_WS(IJK, M, SCPB, SCCB)
         CALL ADD_DRAG_TERM_WS(IJK, M, SCPC, SCCC)
         sourcerhs = SCPA + SCPB + SCPC
         sourcelhs = SCCA + SCCB + SCCC

! source for gas temperature equation: T_g
      CASE (GAS_ENERGY)

! source for solids temperature equation: T_s
      CASE (SOLIDS_ENERGY)

! source for gas species equation: X_g
      CASE (GAS_SPECIES)

! source for solids species equation: X_s
      CASE (SOLIDS_SPECIES)

! source for granular energy equation: Theta_m
      CASE (GRAN_ENERGY)

! source for user scalar equations: scalar
      CASE (USR_SCALAR)

! source for k_epsilon turbulence equations: k_turb_g
      CASE (K_EPSILON_K)

! source for k_epsilon turbulence equations: e_turb_g
      CASE (K_EPSILON_E)

      END SELECT

      CONTAINS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Incorporate an additional source term into the gas phase   C
!  x-momentum equation that arises due to an atypical relative         C
!  velocity definition used in evaluating the drag model.              C
!                                                                      C
!  References:                                                         C
!  See drag routines in usr_mod                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE ADD_DRAG_TERM_UG(IJK, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs
      use fldvar, only: u_s
      use fldvar, only: ep_s
      use functions, only: east_of
      use fun_avg, only: avg_x
      use geometry, only: vol_u
      use indices, only: i_of
      use param1, only: zero
      use usr_src
      use usr, only: usr_drag_type_enum
      use usr, only: lappalainen_09, lappalainen_09_mod
      use usr, only: index_sol, index_liq
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION ::  porosity
      DOUBLE PRECISION :: epl_e
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KGL
      INTEGER :: isolid, iliquid
! indices
      INTEGER :: I, IJKE

!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

! set indices of phases
      isolid = index_sol
      iliquid = index_liq

      IJKE = EAST_OF(IJK)
      I = I_OF(IJK)

! bed volume fraction at i+1/2, j, k
      eps_bed = AVG_X(EP_S(IJK,isolid),EP_S(IJKE,isolid),I)
      porosity = 1.0 - eps_bed

! volume fraction at i+1/2, j, k
! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      EPL_E = AVG_X(EP_S(IJK,iliquid),EP_S(IJKE,iliquid),I)

      SELECT CASE(USR_DRAG_TYPE_ENUM)
         CASE (LAPPALAINEN_09, LAPPALAINEN_09_MOD)
! momentum transfer coefficients at i+1/2, j, k
! note the factor epl_e/porosity is necessary due to the definition of f_gs
            KGL = EPL_E/porosity*AVG_X(F_GS(IJK,iliquid),F_GS(IJKE,iliquid),I)
            SCC = -KGL*U_S(IJK,iliquid)*VOL_U(IJK)

! no center contribution to the gas phase 
            SCP = ZERO

         CASE DEFAULT
            RETURN
      END SELECT

      RETURN
      END SUBROUTINE ADD_DRAG_TERM_UG


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Incorporate an additional source term into the liquid      C
!  phase x-momentum equation that arises due to an atypical relative   C
!  definition used in evaluating the drag model.                       C
!                                                                      C
!  References:                                                         C
!  See drag routines in usr_mod                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE ADD_DRAG_TERM_US(IJK, M, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs
      use fldvar, only: ep_s
      use functions, only: east_of
      use fun_avg, only: avg_x
      use geometry, only: vol_u
      use indices, only: i_of
      use param1, only: zero
      use usr_src
      use usr, only: usr_drag_type_enum
      use usr, only: lappalainen_09, lappalainen_09_mod
      use usr, only: index_sol, index_liq
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC
! Phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION ::  porosity
      DOUBLE PRECISION :: epl_e
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KGL
      INTEGER :: isolid, iliquid
! indices
      INTEGER :: I, IJKE

!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

! set indices of phases
      isolid = index_sol
      iliquid = index_liq

      IJKE = EAST_OF(IJK)
      I = I_OF(IJK)

! bed volume fraction at i+1/2, j, k
      eps_bed = AVG_X(EP_S(IJK,isolid),EP_S(IJKE,isolid),I)
      porosity = 1.0 - eps_bed

! volume fraction at i+1/2, j, k
! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      EPL_E = AVG_X(EP_S(IJK,iliquid),EP_S(IJKE,iliquid),I)

      SELECT CASE(USR_DRAG_TYPE_ENUM)
         CASE (LAPPALAINEN_09, LAPPALAINEN_09_MOD)
! momentum transfer coefficients at i+1/2, j, k
! note the factor epl_e/porosity is necessary due to the definition of f_gs
            KGL = EPL_E/porosity*AVG_X(F_GS(IJK,iliquid),F_GS(IJKE,iliquid),I)
            SCP = -KGL*VOL_U(IJK)

! no source contribution to the liquid phase 
            SCC = ZERO

         CASE DEFAULT
            RETURN
      END SELECT

      RETURN
      END SUBROUTINE ADD_DRAG_TERM_US

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Incorporate an additional source term into the gas phase   C
!  y-momentum equation that arises due to an atypical relative         C
!  velocity definition used in evaluating the drag model.              C
!                                                                      C
!  References:                                                         C
!  See drag routines in usr_mod                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE ADD_DRAG_TERM_VG(IJK, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs
      use fldvar, only: v_s
      use fldvar, only: ep_s
      use functions, only: north_of
      use fun_avg, only: avg_y
      use geometry, only: vol_v
      use indices, only: j_of
      use param1, only: zero
      use usr_src
      use usr, only: usr_drag_type_enum
      use usr, only: lappalainen_09, lappalainen_09_mod
      use usr, only: index_sol, index_liq
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION ::  porosity
      DOUBLE PRECISION :: epl_n
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KGL
      INTEGER :: isolid, iliquid
! indices
      INTEGER :: J, IJKN

!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

! set indices of phases
      isolid = index_sol
      iliquid = index_liq

      IJKN = NORTH_OF(IJK)
      J = J_OF(IJK)

! bed volume fraction at i, j+1/2, k
      eps_bed = AVG_Y(EP_S(IJK,isolid),EP_S(IJKN,isolid),J)
      porosity = 1.0 - eps_bed

! volume fraction at i, j+1/2, k
! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      EPL_N = AVG_Y(EP_S(IJK,iliquid),EP_S(IJKN,iliquid),J)

      SELECT CASE(USR_DRAG_TYPE_ENUM)
         CASE (LAPPALAINEN_09, LAPPALAINEN_09_MOD)
! momentum transfer coefficients at i, j+1/2, k
! note the factor epl_e/porosity is necessary due to the definition of f_gs
            KGL = EPL_N/porosity*AVG_Y(F_GS(IJK,iliquid),F_GS(IJKN,iliquid),J)
            SCC = -KGL*V_S(IJK,iliquid)*VOL_V(IJK)

! no center contribution to the gas phase 
            SCP = ZERO

         CASE DEFAULT
            RETURN
      END SELECT

      RETURN
      END SUBROUTINE ADD_DRAG_TERM_VG


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Incorporate an additional source term into the liquid      C
!  phase y-momentum equation that arises due to an atypical relative   C
!  definition used in evaluating the drag model.                       C
!                                                                      C
!  References:                                                         C
!  See drag routines in usr_mod                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE ADD_DRAG_TERM_VS(IJK, M, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs
      use fldvar, only: ep_s
      use functions, only: north_of
      use fun_avg, only: avg_y
      use geometry, only: vol_v
      use indices, only: j_of
      use param1, only: zero
      use usr_src
      use usr, only: usr_drag_type_enum
      use usr, only: lappalainen_09, lappalainen_09_mod
      use usr, only: index_sol, index_liq
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC
! Phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION ::  porosity
      DOUBLE PRECISION :: epl_n
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KGL
      INTEGER :: isolid, iliquid
! indices
      INTEGER :: J, IJKN

!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

! set indices of phases
      isolid = index_sol
      iliquid = index_liq

      IJKN = NORTH_OF(IJK)
      J = J_OF(IJK)

! bed volume fraction at i, j+1/2, k
      eps_bed = AVG_Y(EP_S(IJK,isolid),EP_S(IJKN,isolid),J)
      porosity = 1.0 - eps_bed

! volume fraction at i, j+1/2, k
! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      EPL_N = AVG_Y(EP_S(IJK,iliquid),EP_S(IJKN,iliquid),J)

      SELECT CASE(USR_DRAG_TYPE_ENUM)
         CASE (LAPPALAINEN_09, LAPPALAINEN_09_MOD)
! momentum transfer coefficients at i, j+1/2, k
! note the factor epl_e/porosity is necessary due to the definition of f_gs
            KGL = EPL_N/porosity*AVG_Y(F_GS(IJK,iliquid),F_GS(IJKN,iliquid),J)
            SCP = -KGL*VOL_V(IJK)

! no source contribution to the liquid phase 
            SCC = ZERO

         CASE DEFAULT
            RETURN
      END SELECT

      RETURN
      END SUBROUTINE ADD_DRAG_TERM_VS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Incorporate an additional source term into the gas phase   C
!  z-momentum equation that arises due to an atypical relative         C
!  velocity definition used in evaluating the drag model.              C
!                                                                      C
!  References:                                                         C
!  See drag routines in usr_mod                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE ADD_DRAG_TERM_WG(IJK, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs
      use fldvar, only: w_s
      use fldvar, only: ep_s
      use functions, only: top_of
      use fun_avg, only: avg_z
      use geometry, only: vol_w
      use indices, only: k_of
      use param1, only: zero
      use usr_src
      use usr, only: usr_drag_type_enum
      use usr, only: lappalainen_09, lappalainen_09_mod
      use usr, only: index_sol, index_liq
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION ::  porosity
      DOUBLE PRECISION :: epl_t
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KGL
      INTEGER :: isolid, iliquid
! indices
      INTEGER :: K, IJKT

!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

! set indices of phases
      isolid = index_sol
      iliquid = index_liq

      IJKT = TOP_OF(IJK)
      K = K_OF(IJK)

! bed volume fraction at i, j, k+1/2
      eps_bed = AVG_Z(EP_S(IJK,isolid),EP_S(IJKT,isolid),K)
      porosity = 1.0 - eps_bed

! volume fraction at i, j, k+1/2
! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      EPL_T = AVG_Z(EP_S(IJK,iliquid),EP_S(IJKT,iliquid),K)

      SELECT CASE(USR_DRAG_TYPE_ENUM)
         CASE (LAPPALAINEN_09, LAPPALAINEN_09_MOD)
! momentum transfer coefficients at i, j, k+1/2
! note the factor epl_e/porosity is necessary due to the definition of f_gs
            KGL = EPL_T/porosity*AVG_Z(F_GS(IJK,iliquid),F_GS(IJKT,iliquid),K)
            SCC = -KGL*W_S(IJK,iliquid)*VOL_W(IJK)

! no center contribution to the gas phase 
            SCP = ZERO

         CASE DEFAULT
            RETURN
      END SELECT

      RETURN
      END SUBROUTINE ADD_DRAG_TERM_WG


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Incorporate an additional source term into the liquid      C
!  phase z-momentum equation that arises due to an atypical relative   C
!  definition used in evaluating the drag model.                       C
!                                                                      C
!  References:                                                         C
!  See drag routines in usr_mod                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE ADD_DRAG_TERM_WS(IJK, M, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs
      use fldvar, only: ep_s
      use functions, only: top_of
      use fun_avg, only: avg_z
      use geometry, only: vol_w
      use indices, only: k_of
      use param1, only: zero
      use usr_src
      use usr, only: usr_drag_type_enum
      use usr, only: lappalainen_09, lappalainen_09_mod
      use usr, only: index_sol, index_liq
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC
! Phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION ::  porosity
      DOUBLE PRECISION :: epl_t
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KGL
      INTEGER :: isolid, iliquid
! indices
      INTEGER :: K, IJKT

!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

! set indices of phases
      isolid = index_sol
      iliquid = index_liq

      IJKT = TOP_OF(IJK)
      K = K_OF(IJK)

! bed volume fraction at i, j, k+1/2
      eps_bed = AVG_Z(EP_S(IJK,isolid),EP_S(IJKT,isolid),K)
      porosity = 1.0 - eps_bed

! volume fraction at i, j, k+1/2
! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      EPL_T = AVG_Z(EP_S(IJK,iliquid),EP_S(IJKT,iliquid),K)

      SELECT CASE(USR_DRAG_TYPE_ENUM)
         CASE (LAPPALAINEN_09, LAPPALAINEN_09_MOD)
! momentum transfer coefficients at i, j, k+1/2
! note the factor epl_e/porosity is necessary due to the definition of f_gs
            KGL = EPL_T/porosity*AVG_Z(F_GS(IJK,iliquid),F_GS(IJKT,iliquid),K)
            SCP = -KGL*VOL_W(IJK)

! no source contribution to the liquid phase 
            SCC = ZERO

         CASE DEFAULT
            RETURN
      END SELECT

      RETURN
      END SUBROUTINE ADD_DRAG_TERM_WS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Implement a dispersion term to the 'liquid' phase          C
!  x-momentum equation                                                 C
!                                                                      C
!  References:                                                         C
!  Solomenko, Z., Haroun, Y., Fourati, M., Larchi, F., Boyer, C., and  C
!     Augier, F., Liquid spreading in trickle-bed reactors:            C
!     experiments and numerical simulations using eulerian-eulerian    C
!     two-fluid approach, Chemical Engineering Science, 126 (2015),    C
!     698-710. Equations 13-16.                                        C
!  Lappalainen, K., Gorshkova, El., Manninen, M., Alopaeus, V.,        C
!     Characteristics of liquid and tracer dispersion in trickle-bed   C
!     reactors: effect on CFD modeling and experimental analyses       C
!     Computers and Chemical Engineering, 35 (2011), 41-49.            C
!     Equations: 20, 21,24, 25.                                        C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE MECH_DISPERSION_US(IJK, M, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs, f_ss
      use fldvar, only: u_s
      use fldvar, only: ep_s
      use functions, only: east_of
      use functions, only: funlm
      use fun_avg, only: avg_x
      use indices, only: i_of
      use param1, only: zero
      use usr_src
      use usr, only: MECH_DISPERSION
      use usr, only: spread_factor
      use usr, only: index_sol
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC
! Phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION :: terms1, terms2, udle
      DOUBLE PRECISION :: termg1, termg2, udge
      DOUBLE PRECISION :: epl_e
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KLS, KGL
      INTEGER :: isolid
! indices
      INTEGER :: I, IJKE
! index for storing solids-solids drag coefficients in the upper
! triangle of the matrix
      INTEGER :: LM

!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

      IF (.NOT.MECH_DISPERSION) RETURN

      isolid = index_sol

! assuming this is never called for the solids phase...
      LM = FUNLM(M,isolid)

! volume fraction at i+1/2, j, k
      IJKE = EAST_OF(IJK)
      I = I_OF(IJK)
      eps_bed = AVG_X(EP_S(IJK,isolid),EP_S(IJKE,isolid),I)
      IF (eps_bed <= ZERO) RETURN  ! assuming force only acts in the packing region

      CALL Sol_DispersionVel_U(IJK,M,TERMS1,TERMS2,UDLE)
      CALL Gas_DispersionVel_U(IJK,TERMG1,TERMG2,UDGE)

      IF (TERMS1 == ZERO .AND. TERMS2 == ZERO) RETURN ! no force

! volume fraction at i+1/2, j, k
      EPL_E = AVG_X(EP_S(IJK,M),EP_S(IJKE,M),I)

! momentum transfer coefficients at i+1/2, j, k
      KLS = AVG_X(F_SS(IJK,LM),F_SS(IJKE,LM),I)
      KGL = AVG_X(F_GS(IJK,M),F_GS(IJKE,M),I)

! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      SCC = (KLS+KGL)*(spread_factor/EPL_E)*(-TERMS1+&
            ZMAX(TERMS2)*U_S(IJK,M))
      SCP = (KLS+KGL)*(spread_factor/EPL_E)*ZMAX(-TERMS2)

! include the gas dispersion term
      SCC = SCC-KGL*UDGE

      RETURN
      END SUBROUTINE MECH_DISPERSION_US

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Implement a dispersion term to the gas phase x-momentum    C
!  equation                                                            C
!                                                                      C
!  Comments:                                                           C
!  See mech_dispersion_us for details                                  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE MECH_DISPERSION_UG(IJK, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs
      use fldvar, only: u_g
      use fldvar, only: ep_g, ep_s
      use functions, only: east_of
      use fun_avg, only: avg_x
      use indices, only: i_of
      use param1, only: zero
      use usr_src
      use usr, only: MECH_DISPERSION
      use usr, only: spread_factor
      use usr, only: index_sol
      use usr, only: index_sol, index_liq
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION :: terms1, terms2, udle
      DOUBLE PRECISION :: termg1, termg2, udge
      DOUBLE PRECISION :: epg_e
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KGS, KGL
      INTEGER :: isolid, iliquid
! indices
      INTEGER :: I, IJKE
!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

      IF (.NOT.MECH_DISPERSION) RETURN

      isolid = index_sol
      iliquid = index_liq

! volume fraction at i+1/2, j, k
      IJKE = EAST_OF(IJK)
      I = I_OF(IJK)
      eps_bed = AVG_X(EP_S(IJK,isolid),EP_S(IJKE,isolid),I)
      IF (eps_bed <= ZERO) RETURN  ! assuming force only acts in the packing region

      CALL Sol_DispersionVel_U(IJK,iliquid,TERMS1,TERMS2,UDLE)
      CALL Gas_DispersionVel_U(IJK,TERMG1,TERMG2,UDGE)

      IF (TERMS1 == ZERO .AND. TERMS2 == ZERO) RETURN ! no force

! volume fraction at i+1/2, j, k
      EPG_E = AVG_X(EP_G(IJK),EP_G(IJKE),I)

! momentum transfer coefficients at i+1/2, j, k
      KGS = AVG_X(F_GS(IJK,isolid),&
                  F_GS(IJKE,isolid),I)
      KGL = AVG_X(F_GS(IJK,iliquid),&
                  F_GS(IJKE,iliquid),I)

! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      SCC = (KGS+KGL)*(spread_factor/EPG_E)*(-TERMS1+&
            ZMAX(TERMS2)*U_G(IJK))
      SCP = (KGS+KGL)*(spread_factor/EPG_E)*ZMAX(-TERMS2)

! include the liquid dispersion term
      SCC = SCC-KGL*UDLE

      RETURN
      END SUBROUTINE MECH_DISPERSION_UG


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Implement a dispersion term to the 'liquid' phase          C
!  y-momentum equation                                                 C
!                                                                      C
!  Comments:                                                           C
!  See mech_dispersion_us for details                                  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE MECH_DISPERSION_VS(IJK, M, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs, f_ss
      use fldvar, only: v_s
      use fldvar, only: ep_s
      use functions, only: north_of
      use functions, only: funlm
      use fun_avg, only: avg_y
      use indices, only: j_of
      use param1, only: zero
      use usr_src
      use usr, only: MECH_DISPERSION
      use usr, only: spread_factor
      use usr, only: index_sol
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC
! Phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION :: terms1, terms2, udln
      DOUBLE PRECISION :: termg1, termg2, udgn
      DOUBLE PRECISION :: epl_n
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KLS, KGL
      INTEGER :: isolid
! indices
      INTEGER :: J, IJKN
! index for storing solids-solids drag coefficients in the upper
! triangle of the matrix
      INTEGER :: LM

!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

      IF (.NOT.MECH_DISPERSION) RETURN

      isolid = index_sol

! assuming this is never called for the solids phase...
      LM = FUNLM(M,isolid)

! volume fraction at i, j+1/2, k
      IJKN = NORTH_OF(IJK)
      J = J_OF(IJK)
      eps_bed = AVG_Y(EP_S(IJK,isolid),EP_S(IJKN,isolid),J)
      IF (eps_bed <= ZERO) RETURN  ! assuming force only acts in the packing region

      CALL Sol_DispersionVel_V(IJK,M,TERMS1,TERMS2,UDLN)
      CALL Gas_DispersionVel_V(IJK,TERMG1,TERMG2,UDGN)

      IF (TERMS1 == ZERO .AND. TERMS2 == ZERO) RETURN ! no force

! volume fraction at i, j+1/2, k
      EPL_N = AVG_Y(EP_S(IJK,M),EP_S(IJKN,M),J)

! momentum transfer coefficients at i, j+1/2, k
      KLS = AVG_Y(F_SS(IJK,LM),F_SS(IJKN,LM),J)
      KGL = AVG_Y(F_GS(IJK,M),F_GS(IJKN,M),J)

! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      SCC = (KLS+KGL)*(spread_factor/EPL_N)*(-TERMS1+&
            ZMAX(TERMS2)*V_S(IJK,M))
      SCP = (KLS+KGL)*(spread_factor/EPL_N)*ZMAX(-TERMS2)

! include the gas dispersion term
      SCC = SCC-KGL*UDGN

      RETURN
      END SUBROUTINE MECH_DISPERSION_VS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Implement a dispersion term to the gas phase y-momentum    C
!  equation                                                            C
!                                                                      C
!  Comments:                                                           C
!  See mech_dispersion_us for details                                  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE MECH_DISPERSION_VG(IJK, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs
      use fldvar, only: v_g
      use fldvar, only: ep_g, ep_s
      use functions, only: north_of
      use fun_avg, only: avg_y
      use indices, only: j_of
      use param1, only: zero
      use usr_src
      use usr, only: MECH_DISPERSION
      use usr, only: spread_factor
      use usr, only: index_sol
      use usr, only: index_sol, index_liq
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION :: terms1, terms2, udln
      DOUBLE PRECISION :: termg1, termg2, udgn
      DOUBLE PRECISION :: epg_n
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KGS, KGL
      INTEGER :: isolid, iliquid
! indices
      INTEGER :: J, IJKN
!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

      IF (.NOT.MECH_DISPERSION) RETURN

      isolid = index_sol
      iliquid = index_liq

! volume fraction at i, j+1/2, k
      IJKN = NORTH_OF(IJK)
      J = J_OF(IJK)
      eps_bed = AVG_Y(EP_S(IJK,isolid),EP_S(IJKN,isolid),J)
      IF (eps_bed <= ZERO) RETURN  ! assuming force only acts in the packing region

      CALL Sol_DispersionVel_V(IJK,iliquid,TERMS1,TERMS2,UDLN)
      CALL Gas_DispersionVel_V(IJK,TERMG1,TERMG2,UDGN)

      IF (TERMS1 == ZERO .AND. TERMS2 == ZERO) RETURN ! no force

! volume fraction at i, j+1/2, k
      EPG_N = AVG_Y(EP_G(IJK),EP_G(IJKN),J)

! momentum transfer coefficients at i, j+1/2, k
      KGS = AVG_Y(F_GS(IJK,isolid),&
                  F_GS(IJKN,isolid),J)
      KGL = AVG_Y(F_GS(IJK,iliquid),&
                  F_GS(IJKN,iliquid),J)

! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      SCC = (KGS+KGL)*(spread_factor/EPG_N)*(-TERMS1+&
            ZMAX(TERMS2)*V_G(IJK))
      SCP = (KGS+KGL)*(spread_factor/EPG_N)*ZMAX(-TERMS2)

! include the liquid dispersion term
      SCC = SCC-KGL*UDLN

      RETURN
      END SUBROUTINE MECH_DISPERSION_VG


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Implement a dispersion term to the 'liquid' phase          C
!  z-momentum equation                                                 C
!                                                                      C
!  Comments:                                                           C
!  See mech_dispersion_us for details                                  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE MECH_DISPERSION_WS(IJK, M, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs, f_ss
      use fldvar, only: w_s
      use fldvar, only: ep_s
      use functions, only: top_of
      use functions, only: funlm
      use fun_avg, only: avg_z
      use indices, only: k_of
      use param1, only: zero
      use usr_src
      use usr, only: MECH_DISPERSION
      use usr, only: spread_factor
      use usr, only: index_sol
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC
! Phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION :: terms1, terms2, udlt
      DOUBLE PRECISION :: termg1, termg2, udgt
      DOUBLE PRECISION :: epl_t
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KLS, KGL
      INTEGER :: isolid
! indices
      INTEGER :: K, IJKT
! index for storing solids-solids drag coefficients in the upper
! triangle of the matrix
      INTEGER :: LM

!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

      IF (.NOT.MECH_DISPERSION) RETURN

      isolid = index_sol

! assuming this is never called for the solids phase...
      LM = FUNLM(M,isolid)

! volume fraction at i, j, k+1/2
      IJKT = TOP_OF(IJK)
      K = K_OF(IJK)
      eps_bed = AVG_Z(EP_S(IJK,isolid),EP_S(IJKT,isolid),K)
      IF (eps_bed <= ZERO) RETURN  ! assuming force only acts in the packing region

      CALL Sol_DispersionVel_W(IJK,M,TERMS1,TERMS2,UDLT)
      CALL Gas_DispersionVel_W(IJK,TERMG1,TERMG2,UDGT)

      IF (TERMS1 == ZERO .AND. TERMS2 == ZERO) RETURN ! no force

! volume fraction at i, j, k+1/2
      EPL_T = AVG_Z(EP_S(IJK,M),EP_S(IJKT,M),K)

! momentum transfer coefficients at i, j, k+1/2
      KLS = AVG_Z(F_SS(IJK,LM),F_SS(IJKT,LM),K)
      KGL = AVG_Z(F_GS(IJK,M),F_GS(IJKT,M),K)

! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      SCC = (KLS+KGL)*(spread_factor/EPL_T)*(-TERMS1+&
            ZMAX(TERMS2)*W_S(IJK,M))
      SCP = (KLS+KGL)*(spread_factor/EPL_T)*ZMAX(-TERMS2)

! include the gas dispersion term
      SCC = SCC-KGL*UDGT

      RETURN
      END SUBROUTINE MECH_DISPERSION_WS


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Purpose: Implement a dispersion term to the gas phase w-momentum    C
!  equation                                                            C
!                                                                      C
!  Comments:                                                           C
!  See mech_dispersion_us for details                                  C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE MECH_DISPERSION_WG(IJK, SCP, SCC)

! Modules
!-----------------------------------------------
      use drag, only: f_gs
      use fldvar, only: w_g
      use fldvar, only: ep_g, ep_s
      use functions, only: top_of
      use fun_avg, only: avg_z
      use indices, only: k_of
      use param1, only: zero
      use usr_src
      use usr, only: MECH_DISPERSION
      use usr, only: spread_factor
      use usr, only: index_sol
      use usr, only: index_sol, index_liq
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) ::  SCP, SCC

! Local variables
!-----------------------------------------------
      DOUBLE PRECISION :: terms1, terms2, udlt
      DOUBLE PRECISION :: termg1, termg2, udgt
      DOUBLE PRECISION :: epg_t
      DOUBLE PRECISION :: eps_bed
      DOUBLE PRECISION :: KGS, KGL
      INTEGER :: isolid, iliquid
! indices
      INTEGER :: K, IJKT
!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

      IF (.NOT.MECH_DISPERSION) RETURN

      isolid = index_sol
      iliquid = index_liq

! volume fraction at i, j, k+1/2
      IJKT = TOP_OF(IJK)
      K = K_OF(IJK)
      eps_bed = AVG_Z(EP_S(IJK,isolid),EP_S(IJKT,isolid),K)
      IF (eps_bed <= ZERO) RETURN  ! assuming force only acts in the packing region

      CALL Sol_DispersionVel_W(IJK,iliquid,TERMS1,TERMS2,UDLT)
      CALL Gas_DispersionVel_W(IJK,TERMG1,TERMG2,UDGT)

      IF (TERMS1 == ZERO .AND. TERMS2 == ZERO) RETURN ! no force

! volume fraction at i, j, k+1/2
      EPG_T = AVG_Z(EP_G(IJK),EP_G(IJKT),K)

! momentum transfer coefficients at i, j+1/2, k
      KGS = AVG_Z(F_GS(IJK,isolid),&
                  F_GS(IJKT,isolid),K)
      KGL = AVG_Z(F_GS(IJK,iliquid),&
                  F_GS(IJKT,iliquid),K)

! recall the volume fraction at the face (EPL) should exceed dilute
! conditions
      SCC = (KGS+KGL)*(spread_factor/EPG_T)*(-TERMS1+&
            ZMAX(TERMS2)*W_G(IJK))
      SCP = (KGS+KGL)*(spread_factor/EPG_T)*ZMAX(-TERMS2)

! include the liquid dispersion term
      SCC = SCC-KGL*UDLT

      RETURN
      END SUBROUTINE MECH_DISPERSION_WG


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE Sol_DispersionVel_U(IJK,M,TERM1,TERM2,UD_E)

! Modules
!-----------------------------------------------
      use fldvar, only: u_s, v_s, w_s, ep_s
      use fun_avg, only: avg_x, avg_y, avg_z, avg_y_n, avg_z_t
      use functions, only: ip_of, jm_of, km_of
      use functions, only: east_of, north_of, south_of
      use functions, only: top_of, bottom_of
      use geometry, only: ayz
      use geometry, only: ody, odz, ox_e, odx_e, vol_u
      use indices, only: i_of, j_of, k_of
      use indices, only: jm1, km1
      use param1, only: zero, small_number
      use usr_src
      use usr, only: spread_factor
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! first and second term defining dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: term1, term2
! dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: ud_e
! phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
! indices
      INTEGER :: I, J, K, JM, KM
      INTEGER :: IPJK, IJMK, IPJMK, IJKM, IPJKM
      INTEGER :: IJKE, IJKN, IJKS, IJKNE, IJKSE
      INTEGER :: IJKT, IJKB, IJKTE, IJKBE
! volume fraction at u-cell center
      DOUBLE PRECISION :: EP
! change in volume fraction across x
      DOUBLE PRECISION :: DEPX
! y-gradient and z-gradient in volume fraction at u-cell center
      DOUBLE PRECISION :: DEPYoy, DEPZoz
! u dot gradient volume fraction
      DOUBLE PRECISION :: Udot_gradEP
! velocity at u-cell center
      DOUBLE PRECISION :: V_E, W_E, U_E
! magnitude of velocity at u-cell center
      DOUBLE PRECISION :: UmagE
! volume fraction at u-cell faces
      DOUBLE PRECISION :: EP_NE, EP_SE, EP_TE, EP_BE
!-----------------------------------------------
      TERM1 = ZERO
      TERM2 = ZERO
      UD_E = ZERO

! determine indices
      I = I_OF(IJK)
      J = J_OF(IJK)
      K = K_OF(IJK)
      IPJK = IP_OF(IJK)
      IJMK = JM_OF(IJK)
      IPJMK = IP_OF(IJMK)
      IJKM = KM_OF(IJK)
      IPJKM = IP_OF(IJKM)

! evaluate magnitude of velocity at i+1/2, j, k
      V_E = AVG_X(AVG_Y_N(V_S(IJMK,M),V_S(IJK,M)),&
                  AVG_Y_N(V_S(IPJMK,M),V_S(IPJK,M)), I)  !i+1/2, j, k

      W_E = AVG_X(AVG_Z_T(W_S(IJKM,M),W_S(IJK,M)),&
                  AVG_Z_T(W_S(IPJKM,M),W_S(IPJK,M)),I)   !i+1/2, j, k

      U_E = U_S(IJK,M)
      UmagE = SQRT(U_E*U_E+V_E*V_E+W_E*W_E)

      IF (ABS(UmagE)<=SMALL_NUMBER) RETURN
! if this is zero, then the first term should evaluate to zero anyway
! however, to avoid division by zero in second term exit now....
! not sure that is the best strategy

! volume fraction at i+1/2, j, k
      IJKE = EAST_OF(IJK)
      EP = AVG_X(EP_S(IJK,M),EP_S(IJKE,M),I)
! x-gradient in volume fraction at i+1/2, j, k
      DEPX = (EP_S(IJKE,M)-EP_S(IJK,M))

! evaluate the first term for mechanical dispersion
! mag(u)*de/dx
      TERM1 = UmagE*DEPX*AYZ(IJK)

! evaluate y-gradient in volume fraction at i+1/2, j, k
! de/dy
      IJKNE = NORTH_OF(IJKE)
      IJKN = NORTH_OF(IJK)
      IJKSE = SOUTH_OF(IJKE)
      IJKS = SOUTH_OF(IJK)
      JM = JM1(J)
      EP_NE = AVG_X(AVG_Y(EP_S(IJK,M),EP_S(IJKN,M),J),&
                    AVG_Y(EP_S(IJKE,M),EP_S(IJKNE,M),J),I)  !i+1/2, j+1/2, k
      EP_SE = AVG_X(AVG_Y(EP_S(IJKS,M),EP_S(IJK,M),JM),&
                    AVG_Y(EP_S(IJKSE,M),EP_S(IJKE,M),JM),I) !i+1/2, j-1/2, k
      DEPYoy = (EP_NE-EP_SE)*ODY(J)

! evaluate the z-gradient in volume fraction at i+1/2, j, k
! de/dz
      IJKT = TOP_OF(IJK)
      IJKTE = TOP_OF(IJKE)
      IJKB = BOTTOM_OF(IJK)
      IJKBE = BOTTOM_OF(IJKE)
      KM = KM1(K)
      EP_TE = AVG_X(AVG_Z(EP_S(IJK,M),EP_S(IJKT,M),K),&
                    AVG_Z(EP_S(IJKE,M),EP_S(IJKTE,M),K),I) !i+1/2, j, k+1/2
      EP_BE = AVG_X(AVG_Z(EP_S(IJKB,M),EP_S(IJK,M),KM),&
                    AVG_Z(EP_S(IJKBE,M),EP_S(IJKE,M),KM),I)  !i+1/2, j, k-1/2
      DEPZoz = (EP_TE-EP_BE)*ODZ(K)

! evaluate u dot grad epsilon at i+1/2, j, k
! ude/dx+vde/dy+w/x*de/dz
      Udot_gradEP = U_E*DEPX*ODX_E(I) + &
                    V_e*DEPYoy + W_E*OX_E(I)*DEPZoz

! evaluate the second term for mechanical dispersion
! [u dot grad(epsilon)]/mag(u)
      TERM2 = Udot_gradEP*VOL_U(IJK)/UmagE

! dispersion velocity
      UD_E = -(spread_factor/EP)*(TERM1 - TERM2*U_E)

      RETURN
      END SUBROUTINE Sol_DispersionVel_U


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE Gas_DispersionVel_U(IJK,TERM1,TERM2,UD_E)

! Modules
!-----------------------------------------------
      use fldvar, only: u_g, v_g, w_g, ep_g
      use fun_avg, only: avg_x, avg_y, avg_z, avg_y_n, avg_z_t
      use functions, only: ip_of, jm_of, km_of
      use functions, only: east_of, north_of, south_of
      use functions, only: top_of, bottom_of
      use geometry, only: ayz
      use geometry, only: ody, odz, ox_e, odx_e, vol_u
      use indices, only: i_of, j_of, k_of
      use indices, only: jm1, km1
      use param1, only: zero, small_number
      use usr_src
      use usr, only: spread_factor
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! first and second term defining dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: term1, term2
! dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: ud_e

! Local variables
!-----------------------------------------------
! indices
      INTEGER :: I, J, K, JM, KM
      INTEGER :: IPJK, IJMK, IPJMK, IJKM, IPJKM
      INTEGER :: IJKE, IJKN, IJKS, IJKNE, IJKSE
      INTEGER :: IJKT, IJKB, IJKTE, IJKBE
! volume fraction at u-cell center
      DOUBLE PRECISION :: EP
! change in volume fraction across x
      DOUBLE PRECISION :: DEPX
! y-gradient and z-gradient in volume fraction at u-cell center
      DOUBLE PRECISION :: DEPYoy, DEPZoz
! u dot gradient volume fraction
      DOUBLE PRECISION :: Udot_gradEP
! velocity at u-cell center
      DOUBLE PRECISION :: V_E, W_E, U_E
! magnitude of velocity at u-cell center
      DOUBLE PRECISION :: UmagE
! volume fraction at u-cell faces
      DOUBLE PRECISION :: EP_NE, EP_SE, EP_TE, EP_BE

!-----------------------------------------------
      TERM1 = ZERO
      TERM2 = ZERO
      UD_E = ZERO

! determine indices
      I = I_OF(IJK)
      J = J_OF(IJK)
      K = K_OF(IJK)
      IPJK = IP_OF(IJK)
      IJMK = JM_OF(IJK)
      IJKM = KM_OF(IJK)
      IPJMK = IP_OF(IJMK)
      IPJKM = IP_OF(IJKM)

! evaluate magnitude of velocity at i+1/2, j, k
      V_E = AVG_X(AVG_Y_N(V_G(IJMK),V_G(IJK)),&
                  AVG_Y_N(V_G(IPJMK),V_G(IPJK)), I)  !i+1/2, j, k

      W_E = AVG_X(AVG_Z_T(W_G(IJKM),W_G(IJK)),&
                  AVG_Z_T(W_G(IPJKM),W_G(IPJK)),I)   !i+1/2, j, k

      U_E = U_G(IJK)
      UmagE = SQRT(U_E*U_E+V_E*V_E+W_E*W_E)

      IF (ABS(UmagE)<=SMALL_NUMBER) RETURN
! if this is zero, then the first term should evaluate to zero anyway
! however, to avoid division by zero in second term exit now....
! not sure that is the best strategy

! volume fraction at i+1/2, j, k
      IJKE = EAST_OF(IJK)
      EP = AVG_X(EP_G(IJK),EP_G(IJKE),I)
! x-gradient in volume fraction at i+1/2, j, k
      DEPX = (EP_G(IJKE)-EP_G(IJK))

! evaluate the first term for mechanical dispersion
! mag(u)*de/dx
      TERM1 = UmagE*DEPX*AYZ(IJK)

! evaluate y-gradient in volume fraction at i+1/2, j, k
! de/dy
      IJKNE = NORTH_OF(IJKE)
      IJKN = NORTH_OF(IJK)
      IJKSE = SOUTH_OF(IJKE)
      IJKS = SOUTH_OF(IJK)
      JM = JM1(J)
      EP_NE = AVG_X(AVG_Y(EP_G(IJK),EP_G(IJKN),J),&
                    AVG_Y(EP_G(IJKE),EP_G(IJKNE),J),I)  !i+1/2, j+1/2, k
      EP_SE = AVG_X(AVG_Y(EP_G(IJKS),EP_G(IJK),JM),&
                    AVG_Y(EP_G(IJKSE),EP_G(IJKE),JM),I) !i+1/2, j-1/2, k
      DEPYoy = (EP_NE-EP_SE)*ODY(J)

! evaluate the z-gradient in volume fraction at i+1/2, j, k
! de/dz
      IJKT = TOP_OF(IJK)
      IJKTE = TOP_OF(IJKE)
      IJKB = BOTTOM_OF(IJK)
      IJKBE = BOTTOM_OF(IJKE)
      KM = KM1(K)
      EP_TE = AVG_X(AVG_Z(EP_G(IJK),EP_G(IJKT),K),&
                    AVG_Z(EP_G(IJKE),EP_G(IJKTE),K),I) !i+1/2, j, k+1/2
      EP_BE = AVG_X(AVG_Z(EP_G(IJKB),EP_G(IJK),KM),&
                    AVG_Z(EP_G(IJKBE),EP_G(IJKE),KM),I)  !i+1/2, j, k-1/2
      DEPZoz = (EP_TE-EP_BE)*ODZ(K)

! evaluate u dot grad epsilon at i+1/2, j, k
! ude/dx+vde/dy+w/x*de/dz
      Udot_gradEP = U_E*DEPX*ODX_E(I) + &
                    V_e*DEPYoy + W_E*OX_E(I)*DEPZoz

! evaluate the second term for mechanical dispersion
! [u dot grad(epsilon)]/mag(u)
      TERM2 = Udot_gradEP*VOL_U(IJK)/UmagE

! solids dispersion velocity
      UD_E = -(spread_factor/EP)*(TERM1 - TERM2*U_E)

      RETURN
      END SUBROUTINE Gas_DispersionVel_U


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE Sol_DispersionVel_V(IJK,M,TERM1,TERM2,UD_N)

! Modules
!-----------------------------------------------
      use fldvar, only: u_s, v_s, w_s, ep_s
      use fun_avg, only: avg_x, avg_y, avg_z, avg_x_e, avg_z_t
      use functions, only: im_of, jp_of, km_of
      use functions, only: east_of, north_of, south_of
      use functions, only: top_of, bottom_of
      use geometry, only: axz
      use geometry, only: odx, odz, ox, ody_n, vol_v
      use indices, only: i_of, j_of, k_of
      use indices, only: im1, km1
      use param1, only: zero, small_number
      use usr_src
      use usr, only: spread_factor
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! first and second term defining dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: term1, term2
! dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: ud_n
! phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
! indices
      INTEGER :: I, J, K, IM, KM
      INTEGER :: IJPK, IMJK, IJKM, IMJPK, IJPKM
      INTEGER :: IJKE, IJKW, IJKN, IJKNE, IJKNW
      INTEGER :: IJKT, IJKB, IJKTN, IJKBN
! volume fraction at v-cell center
      DOUBLE PRECISION :: EP
! change in volume fraction across y
      DOUBLE PRECISION :: DEPY
! x-gradient and z-gradient in volume fraction at v-cell center
      DOUBLE PRECISION :: DEPXox, DEPZoz
! u dot gradient volume fraction
      DOUBLE PRECISION :: Udot_gradEP
! velocity at v-cell center
      DOUBLE PRECISION :: V_N, W_N, U_N
! magnitude of velocity at v-cell center
      DOUBLE PRECISION :: UmagN
! volume fraction at v-cell faces
      DOUBLE PRECISION :: EP_EN, EP_WN, EP_TN, EP_BN
!-----------------------------------------------
      TERM1 = ZERO
      TERM2 = ZERO
      UD_N = ZERO

! determine indices
      I = I_OF(IJK)
      J = J_OF(IJK)
      K = K_OF(IJK)
      IJPK = JP_OF(IJK)
      IMJK = IM_OF(IJK)
      IJKM = KM_OF(IJK)
      IMJPK = JP_OF(IMJK)
      IJPKM = JP_OF(IJKM)

! evaluate magnitude of velocity at i, j+1/2, k
      U_N = AVG_Y(AVG_X_E(U_S(IMJK,M),U_S(IJK,M),I),&
                  AVG_X_E(U_S(IMJPK,M),U_S(IJPK,M),I), J)  !i, j+1/2, k

      W_N = AVG_Y(AVG_Z_T(W_S(IJKM,M),W_S(IJK,M)),&
                  AVG_Z_T(W_S(IJPKM,M),W_S(IJPK,M)),J)     !i, j+1/2, k

      V_N = V_S(IJK,M)
      UmagN = SQRT(U_N*U_N+V_N*V_N+W_N*W_N)

      IF (ABS(UmagN)<=SMALL_NUMBER) RETURN
! if this is zero, then the first term should evaluate to zero anyway
! however, to avoid division by zero in second term exit now....
! not sure that is the best strategy

! volume fraction at i, j+1/2, k
      IJKN = NORTH_OF(IJK)
      EP = AVG_Y(EP_S(IJK,M),EP_S(IJKN,M),J)
! y-gradient in volume fraction at i, j+1/2, k
      DEPY = (EP_S(IJKN,M)-EP_S(IJK,M))

! evaluate the first term for mechanical dispersion
! mag(v)*de/dx
      TERM1 = UmagN*DEPY*AXZ(IJK)

! evaluate x-gradient in volume fraction at i, j+1/2, k
! de/dx
      IJKE = EAST_OF(IJK)
      IJKNE = EAST_OF(IJKN)
      IJKW = WEST_OF(IJK)
      IJKNW = WEST_OF(IJKN)
      IM = IM1(I)
      EP_EN = AVG_Y(AVG_X(EP_S(IJK,M),EP_S(IJKE,M),I),&
                    AVG_X(EP_S(IJKN,M),EP_S(IJKNE,M),I),J)  !i+1/2, j+1/2, k
      EP_WN = AVG_Y(AVG_X(EP_S(IJKW,M),EP_S(IJK,M),IM),&
                    AVG_X(EP_S(IJKNW,M),EP_S(IJKN,M),IM),J) !i-1/2, j+1/2, k
      DEPXox = (EP_EN-EP_WN)*ODX(I)

! evaluate the z-gradient in volume fraction at i, j+1/2, k
! de/dz
      IJKT = TOP_OF(IJK)
      IJKTN = TOP_OF(IJKN)
      IJKB = BOTTOM_OF(IJK)
      IJKBN = BOTTOM_OF(IJKN)
      KM = KM1(K)
      EP_TN = AVG_Y(AVG_Z(EP_S(IJK,M),EP_S(IJKT,M),K),&
                    AVG_Z(EP_S(IJKN,M),EP_S(IJKTN,M),K),J) !i, j+1/2, k+1/2
      EP_BN = AVG_Y(AVG_Z(EP_S(IJKB,M),EP_S(IJK,M),KM),&
                    AVG_Z(EP_S(IJKBN,M),EP_S(IJKN,M),KM),J)  !i, j+1/2, k-1/2
      DEPZoz = (EP_TN-EP_BN)*ODZ(K)

! evaluate u dot grad epsilon at i, j+1/2, k
! ude/dx+vde/dy+w/x*de/dz
      Udot_gradEP = U_N*DEPXox + &
                    V_N*DEPY*ODY_N(J) + W_N*OX(I)*DEPZoz

! evaluate the second term for mechanical dispersion
! [u dot grad(epsilon)]/mag(u)
      TERM2 = Udot_gradEP*VOL_V(IJK)/UmagN

! dispersion velocity
      UD_N = -(spread_factor/EP)*(TERM1 - TERM2*V_N)

      RETURN
      END SUBROUTINE Sol_DispersionVel_V


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE Gas_DispersionVel_V(IJK,TERM1,TERM2,UD_N)

! Modules
!-----------------------------------------------
      use fldvar, only: u_g, v_g, w_g, ep_g
      use fun_avg, only: avg_x, avg_y, avg_z, avg_x_e, avg_z_t
      use functions, only: im_of, jp_of, km_of
      use functions, only: east_of, north_of, south_of
      use functions, only: top_of, bottom_of
      use geometry, only: axz
      use geometry, only: odx, odz, ox, ody_n, vol_v
      use indices, only: i_of, j_of, k_of
      use indices, only: im1, km1
      use param1, only: zero, small_number
      use usr_src
      use usr, only: spread_factor
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! first and second term defining dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: term1, term2
! dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: ud_n

! Local variables
!-----------------------------------------------
! indices
      INTEGER :: I, J, K, IM, KM
      INTEGER :: IJPK, IMJK, IJKM, IMJPK, IJPKM
      INTEGER :: IJKE, IJKW, IJKN, IJKNE, IJKNW
      INTEGER :: IJKT, IJKB, IJKTN, IJKBN
! volume fraction at v-cell center
      DOUBLE PRECISION :: EP
! change in volume fraction across y
      DOUBLE PRECISION :: DEPY
! x-gradient and z-gradient in volume fraction at v-cell center
      DOUBLE PRECISION :: DEPXox, DEPZoz
! u dot gradient volume fraction
      DOUBLE PRECISION :: Udot_gradEP
! velocity at v-cell center
      DOUBLE PRECISION :: V_N, W_N, U_N
! magnitude of velocity at v-cell center
      DOUBLE PRECISION :: UmagN
! volume fraction at v-cell faces
      DOUBLE PRECISION :: EP_EN, EP_WN, EP_TN, EP_BN
!-----------------------------------------------
      TERM1 = ZERO
      TERM2 = ZERO
      UD_N = ZERO

! determine indices
      I = I_OF(IJK)
      J = J_OF(IJK)
      K = K_OF(IJK)
      IJPK = JP_OF(IJK)
      IMJK = IM_OF(IJK)
      IJKM = KM_OF(IJK)
      IMJPK = JP_OF(IMJK)
      IJPKM = JP_OF(IJKM)

! evaluate magnitude of velocity at i, j+1/2, k
      U_N = AVG_Y(AVG_X_E(U_G(IMJK),U_G(IJK),I),&
                  AVG_X_E(U_G(IMJPK),U_G(IJPK),I), J)  !i, j+1/2, k

      W_N = AVG_Y(AVG_Z_T(W_G(IJKM),W_G(IJK)),&
                  AVG_Z_T(W_G(IJPKM),W_G(IJPK)),J)     !i, j+1/2, k

      V_N = V_G(IJK)
      UmagN = SQRT(U_N*U_N+V_N*V_N+W_N*W_N)

      IF (ABS(UmagN)<=SMALL_NUMBER) RETURN
! if this is zero, then the first term should evaluate to zero anyway
! however, to avoid division by zero in second term exit now....
! not sure that is the best strategy

! volume fraction at i, j+1/2, k
      IJKN = NORTH_OF(IJK)
      EP = AVG_Y(EP_G(IJK),EP_G(IJKN),J)
! y-gradient in volume fraction at i, j+1/2, k
      DEPY = (EP_G(IJKN)-EP_G(IJK))

! evaluate the first term for mechanical dispersion
! mag(v)*de/dx
      TERM1 = UmagN*DEPY*AXZ(IJK)

! evaluate x-gradient in volume fraction at i, j+1/2, k
! de/dx
      IJKE = EAST_OF(IJK)
      IJKNE = EAST_OF(IJKN)
      IJKW = WEST_OF(IJK)
      IJKNW = WEST_OF(IJKN)
      IM = IM1(I)
      EP_EN = AVG_Y(AVG_X(EP_G(IJK),EP_G(IJKE),I),&
                    AVG_X(EP_G(IJKN),EP_G(IJKNE),I),J)  !i+1/2, j+1/2, k
      EP_WN = AVG_Y(AVG_X(EP_G(IJKW),EP_G(IJK),IM),&
                    AVG_X(EP_G(IJKNW),EP_G(IJKN),IM),J) !i-1/2, j+1/2, k
      DEPXox = (EP_EN-EP_WN)*ODX(I)

! evaluate the z-gradient in volume fraction at i, j+1/2, k
! de/dz
      IJKT = TOP_OF(IJK)
      IJKTN = TOP_OF(IJKN)
      IJKB = BOTTOM_OF(IJK)
      IJKBN = BOTTOM_OF(IJKN)
      KM = KM1(K)
      EP_TN = AVG_Y(AVG_Z(EP_G(IJK),EP_G(IJKT),K),&
                    AVG_Z(EP_G(IJKN),EP_G(IJKTN),K),J) !i, j+1/2, k+1/2
      EP_BN = AVG_Y(AVG_Z(EP_G(IJKB),EP_G(IJK),KM),&
                    AVG_Z(EP_G(IJKBN),EP_G(IJKN),KM),J)  !i, j+1/2, k-1/2
      DEPZoz = (EP_TN-EP_BN)*ODZ(K)

! evaluate u dot grad epsilon at i, j+1/2, k
! ude/dx+vde/dy+w/x*de/dz
      Udot_gradEP = U_N*DEPXox + &
                    V_N*DEPY*ODY_N(J) + W_N*OX(I)*DEPZoz

! evaluate the second term for mechanical dispersion
! [u dot grad(epsilon)]/mag(u)
      TERM2 = Udot_gradEP*VOL_V(IJK)/UmagN

! dispersion velocity
      UD_N = -(spread_factor/EP)*(TERM1 - TERM2*V_N)

      RETURN
      END SUBROUTINE Gas_DispersionVel_V


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE Sol_DispersionVel_W(IJK,M,TERM1,TERM2,UD_T)

! Modules
!-----------------------------------------------
      use fldvar, only: u_s, v_s, w_s, ep_s
      use fun_avg, only: avg_x, avg_y, avg_z, avg_x_e, avg_y_n
      use functions, only: im_of, jm_of, kp_of
      use functions, only: east_of, north_of, south_of
      use functions, only: top_of, bottom_of
      use geometry, only: axy
      use geometry, only: odx, ody, ox, odz_t, vol_w
      use indices, only: i_of, j_of, k_of
      use indices, only: im1, jm1
      use param1, only: zero, small_number
      use usr_src
      use usr, only: spread_factor
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! first and second term defining dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: term1, term2
! dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: ud_t
! phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
! indices
      INTEGER :: I, J, K, IM, JM
      INTEGER :: IJKP, IMJK, IJMK, IMJKP, IJMKP
      INTEGER :: IJKE, IJKW, IJKT, IJKTE, IJKTW
      INTEGER :: IJKN, IJKS, IJKTN, IJKTS
! volume fraction at w-cell center
      DOUBLE PRECISION :: EP
! change in volume fraction across z
      DOUBLE PRECISION :: DEPZ
! x-gradient and y-gradient in volume fraction at w-cell center
      DOUBLE PRECISION :: DEPXox, DEPYoy
! u dot gradient volume fraction
      DOUBLE PRECISION :: Udot_gradEP
! velocity at w-cell center
      DOUBLE PRECISION :: V_T, W_T, U_T
! magnitude of velocity at w-cell center
      DOUBLE PRECISION :: UmagT
! volume fraction at w-cell faces
      DOUBLE PRECISION :: EP_ET, EP_WT, EP_NT, EP_ST
!-----------------------------------------------
      TERM1 = ZERO
      TERM2 = ZERO
      UD_T = ZERO

! determine indices
      I = I_OF(IJK)
      J = J_OF(IJK)
      K = K_OF(IJK)
      IJKP = KP_OF(IJK)
      IMJK = IM_OF(IJK)
      IJMK = JM_OF(IJK)
      IMJKP = KP_OF(IMJK)
      IJMKP = KP_OF(IJMK)

! evaluate magnitude of velocity at i, j, k+1/2
      U_T = AVG_Z(AVG_X_E(U_S(IMJK,M),U_S(IJK,M),I),&
                  AVG_X_E(U_S(IMJKP,M),U_S(IJKP,M),I), K)  !i, j, k+1/2

      V_T = AVG_Z(AVG_Y_N(V_S(IJMK,M),V_S(IJK,M)),&
                  AVG_Y_N(V_S(IJMKP,M),V_S(IJKP,M)),K)   !i, j, k+1/2

      W_T = W_S(IJK,M)
      UmagT = SQRT(U_T*U_T+V_T*V_T+W_T*W_T)

      IF (ABS(UmagT)<=SMALL_NUMBER) RETURN
! if this is zero, then the first term should evaluate to zero anyway
! however, to avoid division by zero in second term exit now....
! not sure that is the best strategy

! volume fraction at i, j, k+1/2
      IJKT = TOP_OF(IJK)
      EP = AVG_Z(EP_S(IJK,M),EP_S(IJKT,M),K)
! z-gradient in volume fraction at i, j, k+1/2
      DEPZ = (EP_S(IJKT,M)-EP_S(IJK,M))

! evaluate the first term for mechanical dispersion
! mag(v)*de/dx
      TERM1 = UmagT*DEPZ*AXY(IJK)

! evaluate x-gradient in volume fraction at i, j, k+1/2
! de/dx
      IJKE = EAST_OF(IJK)
      IJKTE = TOP_OF(IJKE)
      IJKW = WEST_OF(IJK)
      IJKTW = TOP_OF(IJKW)
      IM = IM1(I)
      EP_ET = AVG_Z(AVG_X(EP_S(IJK,M),EP_S(IJKE,M),I),&
                    AVG_X(EP_S(IJKT,M),EP_S(IJKTE,M),I),K)  !i+1/2, j, k+1/2
      EP_WT = AVG_Z(AVG_X(EP_S(IJKW,M),EP_S(IJK,M),IM),&
                    AVG_X(EP_S(IJKTW,M),EP_S(IJKT,M),IM),K) !i-1/2, j, k+1/2
      DEPXox = (EP_ET-EP_WT)*ODX(I)

! evaluate the y-gradient in volume fraction at i, j, k+1/2
! de/dy
      IJKN = NORTH_OF(IJK)
      IJKTN = TOP_OF(IJKN)
      IJKS = SOUTH_OF(IJK)
      IJKTS = TOP_OF(IJKS)
      JM = JM1(J)
      EP_NT = AVG_Z(AVG_Y(EP_S(IJK,M),EP_S(IJKN,M),J),&
                    AVG_Y(EP_S(IJKT,M),EP_S(IJKTN,M),J),K)   !i, j+1/2, k+1/2
      EP_ST = AVG_Z(AVG_Y(EP_S(IJKS,M),EP_S(IJK,M),JM),&
                    AVG_Y(EP_S(IJKTS,M),EP_S(IJKT,M),JM),K)  !i, j-1/2, k+1/2
      DEPYoy = (EP_NT-EP_ST)*ODY(J)

! evaluate u dot grad epsilon at i, j, k+1/2
! ude/dx+vde/dy+w/x*de/dz
      Udot_gradEP = U_T*DEPXox + &
                    V_T*DEPYoy + W_T*OX(I)*DEPZ*ODZ_T(K)

! evaluate the second term for mechanical dispersion
! [u dot grad(epsilon)]/mag(u)
      TERM2 = Udot_gradEP*VOL_W(IJK)/UmagT

! dispersion velocity
      UD_T = -(spread_factor/EP)*(TERM1 - TERM2*W_T)

      RETURN
      END SUBROUTINE Sol_DispersionVel_W


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE Gas_DispersionVel_W(IJK,TERM1,TERM2,UD_T)

! Modules
!-----------------------------------------------
      use fldvar, only: u_g, v_g, w_g, ep_g
      use fun_avg, only: avg_x, avg_y, avg_z, avg_x_e, avg_y_n
      use functions, only: im_of, jm_of, kp_of
      use functions, only: east_of, north_of, south_of
      use functions, only: top_of, bottom_of
      use geometry, only: axy
      use geometry, only: odx, ody, ox, odz_t, vol_w
      use indices, only: i_of, j_of, k_of
      use indices, only: im1, jm1
      use param1, only: zero, small_number
      use usr_src
      use usr, only: spread_factor
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! first and second term defining dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: term1, term2
! dispersion velocity
      DOUBLE PRECISION, INTENT(OUT) :: ud_t

! Local variables
!-----------------------------------------------
! indices
      INTEGER :: I, J, K, IM, JM
      INTEGER :: IJKP, IMJK, IJMK, IMJKP, IJMKP
      INTEGER :: IJKE, IJKW, IJKT, IJKTE, IJKTW
      INTEGER :: IJKN, IJKS, IJKTN, IJKTS
! volume fraction at w-cell center
      DOUBLE PRECISION :: EP
! change in volume fraction across z
      DOUBLE PRECISION :: DEPZ
! x-gradient and y-gradient in volume fraction at w-cell center
      DOUBLE PRECISION :: DEPXox, DEPYoy
! u dot gradient volume fraction
      DOUBLE PRECISION :: Udot_gradEP
! velocity at w-cell center
      DOUBLE PRECISION :: V_T, W_T, U_T
! magnitude of velocity at w-cell center
      DOUBLE PRECISION :: UmagT
! volume fraction at w-cell faces
      DOUBLE PRECISION :: EP_ET, EP_WT, EP_NT, EP_ST
!-----------------------------------------------
      TERM1 = ZERO
      TERM2 = ZERO
      UD_T = ZERO

! determine indices
      I = I_OF(IJK)
      J = J_OF(IJK)
      K = K_OF(IJK)
      IJKP = KP_OF(IJK)
      IMJK = IM_OF(IJK)
      IJMK = JM_OF(IJK)
      IMJKP = KP_OF(IMJK)
      IJMKP = KP_OF(IJMK)

! evaluate magnitude of velocity at i, j, k+1/2
      U_T = AVG_Z(AVG_X_E(U_G(IMJK),U_G(IJK),I),&
                  AVG_X_E(U_G(IMJKP),U_G(IJKP),I), K)  !i, j, k+1/2

      V_T = AVG_Z(AVG_Y_N(V_G(IJMK),V_G(IJK)),&
                  AVG_Y_N(V_G(IJMKP),V_G(IJKP)),K)   !i, j, k+1/2

      W_T = W_G(IJK)
      UmagT = SQRT(U_T*U_T+V_T*V_T+W_T*W_T)

      IF (ABS(UmagT)<=SMALL_NUMBER) RETURN
! if this is zero, then the first term should evaluate to zero anyway
! however, to avoid division by zero in second term exit now....
! not sure that is the best strategy

! volume fraction at i, j, k+1/2
      IJKT = TOP_OF(IJK)
      EP = AVG_Z(EP_G(IJK),EP_G(IJKT),K)
! z-gradient in volume fraction at i, j, k+1/2
      DEPZ = (EP_G(IJKT)-EP_G(IJK))

! evaluate the first term for mechanical dispersion
! mag(v)*de/dx
      TERM1 = UmagT*DEPZ*AXY(IJK)

! evaluate x-gradient in volume fraction at i, j, k+1/2
! de/dx
      IJKE = EAST_OF(IJK)
      IJKTE = TOP_OF(IJKE)
      IJKW = WEST_OF(IJK)
      IJKTW = TOP_OF(IJKW)
      IM = IM1(I)
      EP_ET = AVG_Z(AVG_X(EP_G(IJK),EP_G(IJKE),I),&
                    AVG_X(EP_G(IJKT),EP_G(IJKTE),I),K)  !i+1/2, j, k+1/2
      EP_WT = AVG_Z(AVG_X(EP_G(IJKW),EP_G(IJK),IM),&
                    AVG_X(EP_G(IJKTW),EP_G(IJKT),IM),K) !i-1/2, j, k+1/2
      DEPXox = (EP_ET-EP_WT)*ODX(I)

! evaluate the y-gradient in volume fraction at i, j, k+1/2
! de/dy
      IJKN = NORTH_OF(IJK)
      IJKTN = TOP_OF(IJKN)
      IJKS = SOUTH_OF(IJK)
      IJKTS = TOP_OF(IJKS)
      JM = JM1(J)
      EP_NT = AVG_Z(AVG_Y(EP_G(IJK),EP_G(IJKN),J),&
                    AVG_Y(EP_G(IJKT),EP_G(IJKTN),J),K)   !i, j+1/2, k+1/2
      EP_ST = AVG_Z(AVG_Y(EP_G(IJKS),EP_G(IJK),JM),&
                    AVG_Y(EP_G(IJKTS),EP_G(IJKT),JM),K)  !i, j-1/2, k+1/2
      DEPYoy = (EP_NT-EP_ST)*ODY(J)

! evaluate u dot grad epsilon at i, j, k+1/2
! ude/dx+vde/dy+w/x*de/dz
      Udot_gradEP = U_T*DEPXox + &
                    V_T*DEPYoy + W_T*OX(I)*DEPZ*ODZ_T(K)

! evaluate the second term for mechanical dispersion
! [u dot grad(epsilon)]/mag(u)
      TERM2 = Udot_gradEP*VOL_W(IJK)/UmagT

! dispersion velocity
      UD_T = -(spread_factor/EP)*(TERM1 - TERM2*W_T)

      RETURN
      END SUBROUTINE Gas_DispersionVel_W


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CAP_PRESSURE_U(IJK, M, SCP, SCC)

! Modules
!-----------------------------------------------
      use fldvar, only: ep_s, d_p
      use functions, only: east_of
      use fun_avg, only: avg_x
      use geometry, only: ayz
      use indices, only: i_of, j_of, k_of
      use param1, only: zero, one
      use usr_src
      use usr, only: cap_press_type_enum
      use usr, only: grosser_1988
      use usr, only: index_sol
      use usr, only: omega_l
      use toleranc, only: dil_ep_s
      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) :: SCP, SCC
! Phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
! terms for capillary pressure
      DOUBLE PRECISION :: eps_bed, eps_liq, dp_bed
      DOUBLE PRECISION :: depl, deps
      DOUBLE PRECISION :: perm, omega_liq, scp1, scp2
      INTEGER :: isolid
! indices
      INTEGER :: IJKE,I
!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

! indices
      IJKE = EAST_OF(IJK)
      I = I_OF(IJK)
      isolid = index_sol

      eps_bed = AVG_X(EP_S(IJK,isolid),EP_S(IJKE,isolid),I)
      IF (eps_bed <= ZERO) RETURN   !force only acts in the packing region

      eps_liq = AVG_X(EP_S(IJK,M),EP_S(IJKE,M),I)
      omega_liq = AVG_X(OMEGA_L(IJK),OMEGA_L(IJKE),I)
      dp_bed = D_P(IJK,isolid)

      SELECT CASE(CAP_PRESS_TYPE_ENUM)
      CASE (GROSSER_1988)

! Based on Leverett's J Function and expression for permeability
! gradient in liquid volume fraction at i+1/2, j, k
         DEPL = (EP_S(IJKE,M)-EP_S(IJK,M))
! gradient in solids volume fraction at i+1/2, j, k
         DEPS = (EP_S(IJKE,isolid)-EP_S(IJK,isolid))

! closure for (porosity/k)^0.5 where k = permeability
! two comments:
! 1) this is based on ergun's constant. however, the permeability
!    should be made consistent with the drag model
! 2) At this point we assume the permeability is constant. For
!    greater rigor variation in this term should be included in
!    this derivation
         PERM = eps_bed/(dp_bed*(1.d0-eps_bed))*&
            sqrt(180.d0)

! x-component of gradient in capillary pressure
         scp1 = zero
         scp2 = zero
         IF ((eps_liq+eps_bed) < ONE) THEN
            SCP1 =(1.d0/(1.d0-eps_bed-eps_liq))*&
               (DEPS+DEPL)
         ENDIF
         IF (eps_liq > dil_ep_s) THEN  ! possibly unnecessary check now..
            SCP2 = (1.d0/eps_liq)*DEPL
         ENDIF
         SCP = -0.036d0*(SCP1+SCP2)*AYZ(IJK)
         SCP = eps_liq*PERM*omega_liq*SCP
      CASE DEFAULT
         SCP = ZERO
      END SELECT

      RETURN
      END SUBROUTINE CAP_PRESSURE_U


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CAP_PRESSURE_V(IJK, M, SCP, SCC)

! Modules
!-----------------------------------------------
      use fldvar, only: ep_s, d_p
      use functions, only: north_of
      use fun_avg, only: avg_y
      use geometry, only: axz
      use indices, only: j_of
      use param1, only: zero, one
      use usr_src
      use usr, only: cap_press_type_enum
      use usr, only: grosser_1988
      use usr, only: index_sol
      use usr, only: omega_l
      use toleranc, only: dil_ep_s

      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) :: SCP, SCC
! Phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
! terms for capillary pressure
      DOUBLE PRECISION :: eps_bed, eps_liq, dp_bed
      DOUBLE PRECISION :: depl, deps
      DOUBLE PRECISION :: perm, omega_liq, scp1, scp2
      INTEGER :: isolid
! indices
      INTEGER :: IJKN, J
!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero
! indices
      IJKN = NORTH_OF(IJK)
      J = J_OF(IJK)
      isolid = index_sol

      eps_bed = AVG_Y(EP_S(IJK,isolid),EP_S(IJKN,isolid),J)
      IF (eps_bed <= ZERO) RETURN   !force only acts in the packing region

      eps_liq = AVG_Y(EP_S(IJK,M), EP_S(IJKN,M),J)
      omega_liq = AVG_Y(OMEGA_L(IJK),OMEGA_L(IJKN),J)
      dp_bed = D_P(IJK,isolid)

      SELECT CASE(CAP_PRESS_TYPE_ENUM)
      CASE (GROSSER_1988)

! Based on Leverett's J Function and expression for permeability
! gradient in liquid volume fraction at i, j+1/2, k
         DEPL = (EP_S(IJKN,M)-EP_S(IJK,M))
! gradient in solids volume fraction at i, j+1/2, k
         DEPS = (EP_S(IJKN,isolid)-EP_S(IJK,isolid))

! closure for (porosity/k)^0.5 where k = permeability
! two comments:
! 1) this is based on ergun's constant. however, the permeability
!    should be made consistent with the drag model
! 2) At this point we assume the permeability is constant. For
!    greater rigor variation in this term should be included in
!    this derivation
         PERM = eps_bed/(dp_bed*(1.d0-eps_bed))*&
            sqrt(180.d0)

! y-component of gradient in capillary pressure
         SCP1 = zero
         SCP2 = zero
         IF ((eps_liq+eps_bed) < ONE) THEN
            SCP1 = (1.d0/(1.d0-eps_bed-eps_liq))*&
               (DEPS+DEPL)
         ENDIF
         IF (eps_liq > dil_ep_s) THEN  ! possibly unnecessary check now..

            SCP2 = (1.d0/eps_liq)*DEPL
         ENDIF
         SCP = -0.036d0*(SCP1 + SCP2)*AXZ(IJK)
         SCP = eps_liq*PERM*omega_liq*SCP
      CASE DEFAULT
         SCP = ZERO
      END SELECT

      RETURN
      END SUBROUTINE CAP_PRESSURE_V


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE CAP_PRESSURE_W(IJK, M, SCP, SCC)

! Modules
!-----------------------------------------------
      use fldvar, only: ep_s, d_p
      use functions, only: top_of
      use fun_avg, only: avg_z
      use geometry, only: axy
      use indices, only: k_of
      use param1, only: zero, one
      use usr_src
      use usr, only: cap_press_type_enum
      use usr, only: grosser_1988
      use usr, only: index_sol
      use usr, only: omega_l
      use toleranc, only: dil_ep_s

      IMPLICIT NONE

! Dummy arguments
!-----------------------------------------------
! index
      INTEGER, INTENT(IN) :: IJK
! source terms which appear appear in the
! center coefficient (SCP) - part of a_m matrix
! source vector (SCC) - part of b_m vector
      DOUBLE PRECISION, INTENT(OUT) :: SCP, SCC
! Phase index
      INTEGER, INTENT(IN) :: M

! Local variables
!-----------------------------------------------
! terms for capillary pressure
      DOUBLE PRECISION :: eps_bed, eps_liq, dp_bed
      DOUBLE PRECISION :: depl, deps
      DOUBLE PRECISION :: perm, omega_liq, scp1, scp2
      INTEGER :: isolid
! indices
      INTEGER :: IJKT, K
!-----------------------------------------------
! initialize
      SCC = zero
      SCP = zero

! indices
      IJKT = TOP_OF(IJK)
      K = K_OF(IJK)
      isolid = index_sol

      eps_bed = AVG_Z(EP_S(IJK,isolid),EP_S(IJKT,isolid),K)
      IF (eps_bed <= ZERO) RETURN    !force only acts in packing region

      eps_liq = AVG_Z(EP_S(IJK,M),EP_S(IJKT,M),K)
      omega_liq = AVG_Z(OMEGA_L(IJK),OMEGA_L(IJKT),K)
      dp_bed = D_P(IJK,isolid)

      SELECT CASE(CAP_PRESS_TYPE_ENUM)
      CASE (GROSSER_1988)

! Based on Leverett's J Function and expression for permeability
! gradient in liquid volume fraction at i, j, k+1/2
         DEPL = (EP_S(IJKT,M)-EP_S(IJK,M))
! gradient in solids volume fraction at i, j, k+1/2
         DEPS = (EP_S(IJKT,isolid)-EP_S(IJK,isolid))

! closure for (porosity/k)^0.5 where k = permeability
! two comments:
! 1) this is based on ergun's constant. however, the permeability
!    should be made consistent with the drag model
! 2) At this point we assume the permeability is constant. For
!    greater rigor variation in this term should be included in
!    this derivation
         PERM = eps_bed/(dp_bed*(1.d0-eps_bed))*&
            sqrt(180.d0)

! z-component of gradient in capillary pressure
         SCP1 = zero
         SCP2 = zero
         IF ((eps_liq+eps_bed) < ONE) THEN
            SCP1 = (1.d0/(1.d0-eps_bed-eps_liq))*&
               (DEPS+DEPL)
         ENDIF
         IF (eps_liq > dil_ep_s) THEN  ! possibly unnecessary check now..
            SCP2 = (1.d0/eps_liq)*DEPL
         ENDIF
         SCP = -0.036d0*(SCP1+SCP2)*AXY(IJK)
         SCP = eps_liq*PERM*omega_liq*SCP
      CASE DEFAULT
         SCP = ZERO
      END SELECT

      RETURN
      END SUBROUTINE CAP_PRESSURE_W


      END SUBROUTINE USR_SOURCES




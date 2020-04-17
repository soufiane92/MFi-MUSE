!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Namelist: MONITORS                                                  !
!  Author: J.Musser                                   Date: 11-OCT-17  !
!                                                                      !
!  Purpose: List MFIX monitor keywords.                                !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      NAMELIST / MONITORS /&

      MONITOR_DT, MONITOR_NAME, MONITOR_TYPE, &
      MONITOR_X_w, MONITOR_X_e, &
      MONITOR_Y_s, MONITOR_Y_n, &
      MONITOR_Z_b, MONITOR_Z_t, &

! Fluid phase variables
      MONITOR_EP_g, MONITOR_RO_G, &
      MONITOR_P_g, &
      MONITOR_U_g, MONITOR_V_g, MONITOR_W_g,&
      MONITOR_T_g, &
      MONITOR_X_g, &
      MONITOR_K_Turb_G, MONITOR_E_Turb_G, &

! Eulerian solids variables
      MONITOR_EP_s, MONITOR_ROP_s, MONITOR_RO_s,&
      MONITOR_P_star, &
      MONITOR_U_s, MONITOR_V_s, MONITOR_W_s, &
      MONITOR_T_s, &
      MONITOR_X_s, &
      MONITOR_Theta_m, &

      MONITOR_Scalar, MONITOR_RRate

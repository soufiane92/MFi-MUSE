!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Namelist: NUMERICAL_PARAM_LOCKED                                    !
!  Author: J.Musser                                   Date: 03-APR-15  !
!                                                                      !
!  Purpose: List MFIX keywords that CANNOT be edited during a run.     !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
!      NAMELIST / NUMERICAL_PARAM_LOCKED /&
!  Note, there are no locked numerical parameters   -  cgw 2018-01-09
!  If any are added here, you must also modify "read_namelist.f"

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!  Namelist: NUMERICAL_PARAM_UNLOCKED                                  !
!  Author: J.Musser                                   Date: 03-APR-15  !
!                                                                      !
!  Purpose: List MFIX keywords that may be edited during a run by      !
!  invoking a REFRESH.                                                 !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      NAMELIST / NUMERICAL_PARAM_UNLOCKED /&

      MAX_NIT, DETECT_STALL, NORM_G, NORM_S, MAX_INLET_VEL_FAC, &
      TOL_RESID, TOL_RESID_T, TOL_RESID_X, TOL_RESID_TH, &
      TOL_RESID_SCALAR, TOL_DIVERGE, &
      LEQ_IT, LEQ_METHOD, LEQ_SWEEP, LEQ_TOL, LEQ_PC, DO_TRANSPOSE, &
      UR_FAC, UR_F_GS, UR_KTH_SML, &
      DISCRETIZE, DEF_COR, CHI_SCHEME, CN_ON

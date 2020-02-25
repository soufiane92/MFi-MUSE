module check_solids_continuum_mod
contains
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_CONTINUUM_SOLIDS                                  !
!  Purpose: Check kinetic the run control namelist section             !
!                                                                      !
!  Author: P. Nicoletti                               Date: 27-NOV-91  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SOLIDS_CONTINUUM

! Global Variables:
!---------------------------------------------------------------------//
      USE constant
      USE run
      USE physprop

! Global Parameters:
!---------------------------------------------------------------------//
      USE param1, only: zero, one, undefined, undefined_i

! Global Module procedures:
!---------------------------------------------------------------------//
      use error_manager

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
      INTEGER :: LC, M
! counters for number of phases with defined/undefined mu_s0
      INTEGER :: def_mus0, undef_mus0
!......................................................................!

! Initialize the error manager.
      CALL INIT_ERR_MSG("CHECK_SOLIDS_CONTINUUM")

! Check EP_star. This is used to populate ep_star_array which is what
! should be used elsewhere in the code. (see set_constprop)
      IF(EP_STAR == UNDEFINED) THEN
         WRITE(ERR_MSG,1000) 'EP_STAR'
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ELSEIF(EP_STAR < ZERO .OR. EP_STAR > ONE) THEN
         WRITE(ERR_MSG, 1001)'EP_STAR', iVal(EP_STAR)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

! CHECK DIF_s0
      DO M = 1, SMAX
         IF (DIF_S0(M) < ZERO) THEN
            WRITE(ERR_MSG, 1001) trim(iVar('Dif_s0',M)), &
               iVal(Dif_s0(M))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDDO
      DO M = SMAX+1, DIM_M
         IF(DIF_S0(M) /= UNDEFINED)THEN
            WRITE(ERR_MSG,1002) trim(iVar('Dif_s0',M))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDDO

! CHECK MU_s0
      def_mus0 = 0
      DO M = 1, SMAX
         IF (MU_s0(M) /= UNDEFINED) THEN
            def_mus0 = def_mus0 + 1
            IF(MU_s0(M) < ZERO) THEN
               WRITE(ERR_MSG, 1001) trim(iVar('Mu_s0',M)), &
                  iVal(Mu_s0(M))
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ENDIF
      ENDDO
      undef_mus0 = smax - def_mus0

      DO M = SMAX+1, DIM_M
         IF(MU_s0(M) /= UNDEFINED)THEN
            WRITE(ERR_MSG,1002) trim(iVar('Mu_s0',M))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDDO

! Check kinetic theory model
      CALL CHECK_KT_TYPE
! Check solids frictional model
      CALL CHECK_FRICTION_MODEL
! Check solids stress blending function
      CALL CHECK_BLENDING_FUNCTION
! Checks required for the subgrid drag models.
      CALL CHECK_SUBGRID_MODEL

! Solids phase with constant solids viscosity is employed
!---------------------------------------------------------------------//
      IF (def_mus0 > 0) THEN
         IF(GRANULAR_ENERGY) THEN
! calculation of many of the solids phase transport coefficients
! needed by the granular energy equation will be skipped in the
! calc_mu_s if mu_s0 is defined. so make sure that the granular
! energy eqn is not evaluated when the solids viscosity is set to
! a constant.
! Also do not allow a mixed case of constant viscosity and pde
! granular energy. To permit such would require going through the
! KT sections of the code and ensuring that the solids phase with
! granular energy doesn't interact or depend on a phase with a
! constant viscosity.
            WRITE(ERR_MSG,1100)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF

! needed by default solids-solids drag model
         IF (SMAX >=2) THEN
            IF (C_E == UNDEFINED) THEN
               WRITE(ERR_MSG,1101)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ELSEIF (C_F == UNDEFINED) THEN
               WRITE(ERR_MSG,1102)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ENDIF
      ENDIF

! Algebraic granular energy equation
!---------------------------------------------------------------------//
      IF (.NOT.GRANULAR_ENERGY .AND. undef_mus0 > 0) THEN
         IF (C_E == UNDEFINED) THEN
            WRITE(ERR_MSG,1101)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
! needed by default solids-solids drag model. SMAX may be 1 for
! hybrid simulations and C_F is still needed.
         IF (SMAX >=2 .OR. DEM_SOLIDS) THEN
            IF (C_F == UNDEFINED) THEN
               WRITE(ERR_MSG,1102)
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ENDIF
      ENDIF
 1100 FORMAT('Error 1100: Constant viscosity is specified but', /&
         'GRANULAR_ENERGY=.TRUE. Please correct the project settings')
 1101 FORMAT('Error 1101: Coefficient of restitution (C_E) not ',      &
         'specified.',/'Please correct the project settings.')
 1102 FORMAT('Error 1102: Coefficient of friction (C_F) not ',         &
         'specified.',/'Please correct the project settings.')


      IF(YU_STANDISH .AND. FEDORS_LANDEL) THEN
         WRITE(ERR_MSG, 1300)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ELSEIF(YU_STANDISH) THEN
! Yu_Standish correlation checks
         IF(SMAX < 2) THEN
            WRITE(ERR_MSG, 1301)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ELSEIF(FEDORS_LANDEL) THEN
! Fedors_Landel correlation checks.
         IF(SMAX /= 2) THEN
            WRITE(ERR_MSG, 1302)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
      ENDIF
 1300 FORMAT('Error 1300: FEDORS_LANDEL and YU_STANDISH correlations ',&
         'cannot be',/'used at the same time. Please correct the ',    &
         'project settings.')
 1301 FORMAT('Error 1301: YU_STANDISH correlation is for polydisperse',&
         ' mixtures',/'(MMAX >= 2). Please correct the project settings.')
 1302 FORMAT('Error 1302: FEDORS_LANDEL correlation is for binary ',   &
         'mixtures (MMAX=2).',/'Please correct the project settings.')



      IF(MODEL_B) THEN
         DO LC = 1, MMAX
            IF(.NOT.CLOSE_PACKED(LC)) THEN
               WRITE(ERR_MSG, 1400) LC
               CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
            ENDIF
         ENDDO
      ENDIF
 1400 FORMAT('Error 1400: Solids phase ',I2,' is not CLOSE_PACKED.',/, &
         'All solids phases must be CLOSE_PACKED with MODEL_B=.TURE.',/ &
         'Please correct the project settings.')


! Check that phase number where added mass applies is properly defined.
      IF (ADDED_MASS) THEN
         IF(M_AM == UNDEFINED_I)THEN
            WRITE(ERR_MSG, 1500)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF(M_AM == 0 .OR. M_AM > MMAX) THEN
            WRITE(ERR_MSG,1501)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1500 FORMAT('Error 1500: Must specify a disperse phase, M_AM, where ',&
         'the',/'virtual mass applies (ADDED_MASS).',/'Please correct',&
         ' the project settings.')
 1501 FORMAT('Error 1501: M_AM is out of range. [1,MMAX]',/'Please ',  &
         'correct the project settings.')
         ENDIF
      ENDIF


! Check name of radial distribution function
      SELECT CASE(trim(adjustl(RDF_TYPE)))

      CASE ('LEBOWITZ')
         RDF_TYPE_ENUM = LEBOWITZ

      CASE ('MODIFIED_LEBOWITZ')
         RDF_TYPE_ENUM = MODIFIED_LEBOWITZ

      CASE ('MANSOORI')
         RDF_TYPE_ENUM = MANSOORI

      CASE ('MODIFIED_MANSOORI')
         RDF_TYPE_ENUM = MODIFIED_MANSOORI

      CASE DEFAULT
            WRITE(ERR_MSG, 1600)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
       END SELECT

 1600 FORMAT('Error 1600: Unknown RDF_TYPE',/'Please ',  &
         'correct the project settings.')

! If the default (LEBOWITZ) is not set for a monodisperse case, then
! flag the error and exit. Otherwise, change it to CARNAHAN-STARLING.
      IF(MMAX == 1) THEN
         IF(RDF_TYPE_ENUM /= LEBOWITZ) THEN
            WRITE(ERR_MSG, 1601) trim(adjustl(RDF_TYPE))
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

 1601 FORMAT('Error 1601: The RDF_TYPE should NOT be specified when ',&
         'MMAX = 1',/'because Carnahan-Starling is the only available',&
         ' radial distribution',/'function for monodisperse systems. ',&
         'Please correct the project settings.')

         ELSE
            RDF_TYPE_ENUM = CARNAHAN_STARLING
         ENDIF
      ENDIF

 1000 FORMAT('Error 1000: Required input not specified: ',A,/'Please ',&
            'correct the project settings.')
 1001 FORMAT('Error 1001: Illegal or unphysical input: ',A,' = ',A,/   &
         'Please correct the project settings.')
 1002 FORMAT('Error 1002: Illegal input: ',A,' specified out of ',&
         'range.', /,'Please correct the project settings.')


      CALL FINL_ERR_MSG


      RETURN
      END SUBROUTINE CHECK_SOLIDS_CONTINUUM


!----------------------------------------------------------------------!
! Subroutine: CHECK_FRICTION_MODEL                                     !
! Purpose: Check solids frictional stress model                        !
!                                                                      !
! Author: J. Musser                                  Date: 07-FEB-14   !
!----------------------------------------------------------------------!
      SUBROUTINE CHECK_FRICTION_MODEL

! Global Variables:
!---------------------------------------------------------------------//

      USE constant
      USE run
      USE physprop

! Global Parameters:
!---------------------------------------------------------------------//
      USE param1, only: zero, one, undefined, undefined_i

! Global Module procedures:
!---------------------------------------------------------------------//
      use error_manager

      implicit none


! Local Variables:
!---------------------------------------------------------------------//
      DOUBLE PRECISION :: lsin_phi

!......................................................................!

! Initialize the error manager.
      CALL INIT_ERR_MSG("CHECK_FRICTION_MODEL")


! Check name of radial distribution function
      SELECT CASE(trim(adjustl(FRICTION_MODEL)))

      CASE ('SCHAEFFER')
         SCHAEFFER = .TRUE.
         FRICTION = .FALSE.
      CASE ('SRIVASTAVA')
         SCHAEFFER = .FALSE.
         FRICTION = .TRUE.
      CASE ('NONE')
         SCHAEFFER = .FALSE.
         FRICTION = .FALSE.
      CASE DEFAULT
            WRITE(ERR_MSG, 1150)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      END SELECT

 1150 FORMAT('Error 1150: Unknown FRICTION_MODEL',/'Please ',  &
         'correct the project settings.')

! plastic/frictional stress model
      IF (FRICTION) THEN
         IF (.NOT.GRANULAR_ENERGY) THEN
            WRITE(ERR_MSG,1201)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
! Check the value specified for SAVAGE.
         ELSEIF(SAVAGE>2 .OR. SAVAGE<0) THEN
            WRITE(ERR_MSG, 1001)'SAVAGE', iVal(SAVAGE)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF(PHI == UNDEFINED) THEN
            WRITE(ERR_MSG, 1203)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
! used by friction bc
         ELSEIF(PHI_W == UNDEFINED) THEN
            WRITE(ERR_MSG, 1204)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
! PHI & PHI_W are given in degrees but calculated in radian within
! the fortran codes
         SIN_PHI = SIN(PHI*PI/180.D0)
         TAN_PHI_W = TAN(PHI_W*PI/180.D0)
      ENDIF

 1201 FORMAT('Error 1201: The SRIVASTAVA solids stress model requires',&
         /,' GRANULAR_ENERGY /= ALGEBRAIC',/&
        'Please correct the project settings.')
 1204 FORMAT('Error 1204: Angle of particle-wall friction (PHI_W) not',&
         ' specified.',/'Please correct the project settings.')

! plastic/frictional stress model
      IF(SCHAEFFER .AND. PHI == UNDEFINED) THEN
         WRITE(ERR_MSG, 1203)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ELSE
! PHI is given in degrees but calculated in radian within
! the fortran codes
         lsin_phi = sin(phi*PI/180.d0)
         SIN2_PHI = lSIN_PHI*lSIN_PHI
         F_PHI = (3.0D0 - 2.0D0*SIN2_PHI)/3.0D0    ! employed in commented
      ENDIF

 1203 FORMAT('Error 1203: Angle of internal friction (PHI) not ',     &
         'specified.',/'Please correct the project settings.')


      CALL FINL_ERR_MSG

      RETURN

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/   &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_FRICTION_MODEL

!----------------------------------------------------------------------!
! Subroutine: CHECK_BLENDING_FUNCTION                                  !
! Purpose: Check solids blending stress model                          !
!                                                                      !
! Author: J. Musser                                  Date: 07-FEB-14   !
!----------------------------------------------------------------------!
      SUBROUTINE CHECK_BLENDING_FUNCTION

! Global Variables:
!---------------------------------------------------------------------//
! User input keyword
      use run, only: BLENDING_FUNCTION
! Runtime flags (former keywords)
      use run, only: BLENDING_STRESS
      use run, only: TANH_BLEND
      use run, only: SIGM_BLEND

! Flag for Scheffer friction model
      use run, only: SCHAEFFER

! Global Parameters:
!---------------------------------------------------------------------//

! Global Module procedures:
!---------------------------------------------------------------------//
      use error_manager

      implicit none


! Local Variables:
!---------------------------------------------------------------------//
!     NONE

!......................................................................!

! Initialize the error manager.
      CALL INIT_ERR_MSG("CHECK_BLENDING_FUNCTION")

      SELECT CASE(trim(adjustl(BLENDING_FUNCTION)))

      CASE ('NONE');
         BLENDING_STRESS = .FALSE.
         TANH_BLEND=.FALSE.
         SIGM_BLEND=.FALSE.
      CASE ('TANH_BLEND')
         BLENDING_STRESS = .TRUE.
         TANH_BLEND=.TRUE.
         SIGM_BLEND=.FALSE.
      CASE ('GIDASPOW_PCF')
         BLENDING_STRESS = .TRUE.
         TANH_BLEND=.FALSE.
         SIGM_BLEND=.TRUE.

      CASE DEFAULT
         WRITE(ERR_MSG,1001)'BLENDING_FUNCTION', &
            trim(adjustl(BLENDING_FUNCTION))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      END SELECT

 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/   &
         'Please correct the project settings.')

! Verify that stress blending is only turned on with Scheffer.
      IF(.NOT.SCHAEFFER .AND. BLENDING_STRESS) THEN
         WRITE(ERR_MSG, 1202)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

1202  FORMAT('Error 1202: Blending solids stress is only avaible ',    &
         'for Schaeffer',/'friction model.',/'Please correct the ',    &
         'project settings.')

      CALL FINL_ERR_MSG

      RETURN


      END SUBROUTINE CHECK_BLENDING_FUNCTION


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_SUBGRID_MODEL                                     !
!  Purpose: Check the subgrid drag model interactions.                 !
!                                                                      !
!  Author: J.Musser                                   Date: 31-JAN-14  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_SUBGRID_MODEL

! Global Variables:
!---------------------------------------------------------------------//
! Flag: Specify friction model (Schaeffer model/Princeton model)
      USE run, only: FRICTION
! Flag: Solve granular energy eq
      USE run, only: GRANULAR_ENERGY
! Flag: Impose a mean shear on flow field.
      USE run, only: SHEAR
! Flag: Invoke Schaeffer and KT-Theory blending
      USE run, only: BLENDING_STRESS
! User specifed drag model
      USE run, only: DRAG_TYPE
! Ratio of filter size to computational cell size
      USE run, only: FILTER_SIZE_RATIO
! User specifed subgrid model: IGCI or MILIOLI
      USE run, only: SUBGRID_TYPE, SUBGRID_TYPE_ENUM
      USE run, only: UNDEFINED_SUBGRID_TYPE, IGCI, MILIOLI
! Flag: Include wall effect term
      USE run, only: SUBGRID_WALL
! Flag: Solve K-Epsilon Eq.
      USE turb, only: K_EPSILON
! Specularity coefficient for particle-wall collisions
      use constant, only: PHIP
! Flag: Use cartesian grid model
      USE cutcell, only : CARTESIAN_GRID
! Flag: Use discrete element solids model
      use discretelement, only: DISCRETE_ELEMENT
! Flag: Use MP-PIC solids model
      use mfix_pic, only: MPPIC

! Global Parameters:
!---------------------------------------------------------------------//
      USE param1, only: ZERO, UNDEFINED_C


! Global Module procedures:
!---------------------------------------------------------------------//
      USE error_manager

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! NONE

! If the models are not being used, return.
      IF(SUBGRID_TYPE == UNDEFINED_C .AND. .NOT.SUBGRID_WALL) RETURN


! Initialize the error manager.
      CALL INIT_ERR_MSG("CHECK_SUBGRID_MODEL")


      IF(SUBGRID_TYPE == UNDEFINED_C .AND. SUBGRID_WALL) THEN
         WRITE(ERR_MSG,2011)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

 2011 FORMAT('Error 2011: Invalid input. SUBGRID_WALL cannot be used ',&
          'without',/'specifying a SUBGRID_TYPE.',/'Please correct ',  &
          'the project settings.')


      SELECT CASE(trim(adjustl(SUBGRID_TYPE)))

      CASE ('IGCI'); SUBGRID_TYPE_ENUM = IGCI
      CASE ('MILIOLI'); SUBGRID_TYPE_ENUM = MILIOLI
      CASE DEFAULT
         SUBGRID_TYPE_ENUM = UNDEFINED_SUBGRID_TYPE
      END SELECT

      IF(SUBGRID_TYPE_ENUM/= IGCI .AND. SUBGRID_TYPE_ENUM/=MILIOLI) THEN
         WRITE(ERR_MSG,1001) 'SUBGRID_TYPE', SUBGRID_TYPE
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

      IF(DRAG_TYPE /= 'WEN_YU')THEN
         WRITE(ERR_MSG, 2012)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

 2012 FORMAT('Error 2012: Invalid input. WEN_YU is the only DRAG_TYPE',&
          ' available',/'when using the SUBGRID model.',/'Please ',    &
          'correct the project settings.')

      IF(DISCRETE_ELEMENT .OR. MPPIC) THEN
         WRITE(ERR_MSG, 2013)
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

 2013 FORMAT('Error 2013: Invalid input. The SUBGRID model is not ',   &
          'available',/'with discrete solids phases.',/'Please ',      &
          'correct the project settings.')

! Impose the subgrid limitations.
      IF(FILTER_SIZE_RATIO <= ZERO) THEN
         WRITE(ERR_MSG, 1002)'FILTER_SIZE_RATIO', FILTER_SIZE_RATIO
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

      ELSEIF(GRANULAR_ENERGY) THEN
         WRITE(ERR_MSG, 2010) 'GRANULAR_ENERGY', 'FALSE'
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

      ELSEIF(K_EPSILON) THEN
         WRITE(ERR_MSG, 2010) 'K_EPSILON', 'FALSE'
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

      ELSEIF(BLENDING_STRESS) THEN
         WRITE(ERR_MSG, 2010) 'BLENDING_STRESS', 'FALSE'
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

      ELSEIF(FRICTION) THEN
         WRITE(ERR_MSG, 2010) 'FRICTION', 'FALSE'
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

      ELSEIF(SHEAR) THEN
         WRITE(ERR_MSG, 2010) 'SHEAR', 'FALSE'
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

      ELSEIF(PHIP /= ZERO) THEN
         WRITE(ERR_MSG, 2010) 'PHIP', 'ZERO'
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)

      ENDIF

      IF(SUBGRID_WALL .AND. .NOT.CARTESIAN_GRID) THEN
         WRITE(ERR_MSG, 2010) 'CARTESIAN_GRID', 'TRUE'
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
      ENDIF

 2010 FORMAT('Error 2010: Invalid input. ',A,' must be ',A,/'when ',    &
         'using the SUBGRID model.'/,'Please correct the project settings.')

      CALL FINL_ERR_MSG

      RETURN


 1001 FORMAT('Error 1001: Illegal or unknown input: ',A,' = ',A,/      &
         'Please correct the project settings.')

 1002 FORMAT('Error 1002: Illegal or unknown input: ',A,' = ',G14.4,/  &
         'Please correct the project settings.')

 1003 FORMAT('Error 1003: Illegal or unknown input: ',A,' = ',I4,/     &
         'Please correct the project settings.')

      END SUBROUTINE CHECK_SUBGRID_MODEL



!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: CHECK_KT_TYPE                                           !
!  Purpose: Check kinetic theory input specifications. These checks    !
!  are almost all related to the KT_TYPE keyword.                      !
!  Notes: To enter this routine granular_energy must be true           !
!                                                                      !
!  Author: J.Musser                                   Date: 04-FEB-14  !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE CHECK_KT_TYPE


! Global Variables:
!---------------------------------------------------------------------//
      USE constant
      USE run
      USE physprop
      use turb, only: k_epsilon

! Global Parameters:
!---------------------------------------------------------------------//
      USE param1, only: half, one, undefined

! Global Module procedures:
!---------------------------------------------------------------------//
      use error_manager

      IMPLICIT NONE

! Local Variables:
!---------------------------------------------------------------------//
! loop counters
      INTEGER :: I, J

!......................................................................!


! Initialize the error manager.
      CALL INIT_ERR_MSG("CHECK_KT_TYPE")

! Set the PDE flag. Disable for algebraic.
      GRANULAR_ENERGY = .TRUE.

! Check for valid options for kinetic theory models (KT_TYPE)
      SELECT CASE(trim(adjustl(KT_TYPE)))

!``````````````````````````````````````````````````````````````````````
      CASE ('ALGEBRAIC')
         KT_TYPE_ENUM = ALGEBRAIC
         GRANULAR_ENERGY = .FALSE.
         CALL FINL_ERR_MSG
         RETURN


!``````````````````````````````````````````````````````````````````````
      CASE ('IA_NONEP')
         KT_TYPE_ENUM = IA_2005
         IF (C_E == UNDEFINED) THEN
            WRITE(ERR_MSG,1003)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF


!``````````````````````````````````````````````````````````````````````
      CASE ('GD_99')
         KT_TYPE_ENUM = GD_1999
         IF (C_E == UNDEFINED) THEN
            WRITE(ERR_MSG,1003)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF(SMAX > 1) THEN
            WRITE(ERR_MSG,1002) TRIM(KT_TYPE)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF


!``````````````````````````````````````````````````````````````````````
      CASE ('GTSH')
         KT_TYPE_ENUM = GTSH_2012
         IF (C_E == UNDEFINED) THEN
            WRITE(ERR_MSG,1002)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF(SMAX > 1) THEN
            WRITE(ERR_MSG,1002) TRIM(KT_TYPE)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF


!``````````````````````````````````````````````````````````````````````
      CASE ('GHD')
         KT_TYPE_ENUM = GHD_2007
! This variable is only used for GHD at this point...
! Define restitution coefficient matrix
         DO I = 1, SMAX
            DO J = 1, SMAX
               IF(r_p(I,J) == UNDEFINED) THEN
                  IF(C_E == UNDEFINED) THEN
                     WRITE(ERR_MSG,1003)
                     CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
                  ELSE
                     r_p(I,J) = C_e
                  ENDIF
               ENDIF
! just need to define r_p(1,2) and r_p(2,1) will be set.
               r_p(J,I) = r_p(I,J)
            ENDDO
         ENDDO

         IF(DRAG_TYPE_ENUM /= WEN_YU .AND. DRAG_TYPE_ENUM /= HYS) THEN
            WRITE(ERR_MSG, 1030)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF(ADDED_MASS) THEN
            WRITE(ERR_MSG,1031)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF(SMAX > 2) THEN  ! not sure this is still true!
            WRITE(ERR_MSG, 1032)
            CALL FLUSH_ERR_MSG
         ENDIF

! Automatically set SPECIES_EQ(MMAX) = .FALSE. to avoid potential
! checks/loops over the mmax species type eqn which has no meaning
         SPECIES_EQ(MMAX) = .FALSE.
         NMAX_s(MMAX) = 1

! currently set to avoid an overflow error in write_res0
! legacy variable?
         NMAX(MMAX) = 1

 1030 FORMAT('Error 1030: KT_TYPE = "GHD" is restricted to DRAG_TYPE', &
         'values of WEN_YU and HYS.',/'Please correct the project settings.')
 1031 FORMAT('Error 1031: ADDED_MASS force cannot be applied with ',   &
         'GHD theory that',/'solves for mixture equations.',/'Please', &
         'correct the mifx.dat file.')
 1032 FORMAT('Warning 1032: GHD theory may not be valid for more ',    &
         'than two solids phases',/'it requires further development.')


!``````````````````````````````````````````````````````````````````````
      CASE ('AHMADI')
         KT_TYPE_ENUM = AHMADI_1995
         AHMADI = .TRUE.
         IF(.NOT.K_EPSILON) THEN
            WRITE(ERR_MSG,1040) 'K_EPSILON = .TRUE.'
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF (C_E == UNDEFINED) THEN
            WRITE(ERR_MSG,1003)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF(C_F == UNDEFINED .AND. SMAX>=2) THEN
            WRITE(ERR_MSG, 1004)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
 1040 FORMAT('Error 1040: KT_TYPE = "AHMADI" requires ',A,/       &
         'Please correct the project settings.')


!``````````````````````````````````````````````````````````````````````
      CASE ('SIMONIN')
         KT_TYPE_ENUM = SIMONIN_1996
         SIMONIN = .TRUE.
         IF(.NOT.K_EPSILON) THEN
            WRITE(ERR_MSG,1050) 'K_EPSILON = .TRUE.'
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF (C_E == UNDEFINED) THEN
            WRITE(ERR_MSG,1003)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF(C_F == UNDEFINED .AND. SMAX>=2) THEN
            WRITE(ERR_MSG, 1004)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF
 1050 FORMAT('Error 1050: KT_TYPE = "SIMONIN" requires ',A,/      &
         'Please correct the project settings.')

! Lun is the default implementation.
!``````````````````````````````````````````````````````````````````````
      CASE ('LUN_1984')
         KT_TYPE_ENUM = LUN_1984
! this version of the restitution coefficient is needed by most KT_TYPE
! models. it is also needed in default solids-solids drag model
         IF (C_E == UNDEFINED) THEN
            WRITE(ERR_MSG,1003)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ELSEIF(C_F == UNDEFINED .AND. SMAX>=2) THEN
            WRITE(ERR_MSG, 1004)
            CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
         ENDIF


      CASE DEFAULT
         WRITE(ERR_MSG,1001) trim(adjustl(KT_TYPE))
         CALL FLUSH_ERR_MSG(ABORT=.TRUE.)
 1001 FORMAT('Error 1001: Invalid or unknown KT_TYPE: ',A,/            &
         'Please correct the project settings.')

      END SELECT

! eventually this should be made specific to lun/ahmadi/simonin
! but because calc_gw_hw_cw in calc_u_friction is not consistent
! it currently must be defined for all kt_types whenever friction
! is invoked...
      ETA = (ONE + C_E)*HALF


 1002 FORMAT('Error 1002: KT_TYPE = ',A,' is for monodisperse',&
         ' solids',/'(MMAX = 1). Please correct the project settings.')

 1003 FORMAT('Error 1003: Coefficient of restitution (C_E) not ',      &
         'specified.',/'Please correct the project settings.')

 1004 FORMAT('Error 1004: Coefficient of friction (C_F) not ',         &
         'specified.',/'Please correct the project settings.')

      CALL FINL_ERR_MSG
      RETURN
      END SUBROUTINE CHECK_KT_TYPE

end module check_solids_continuum_mod

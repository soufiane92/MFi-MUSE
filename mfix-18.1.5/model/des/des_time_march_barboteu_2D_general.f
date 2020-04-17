! -*- F90 -*-
! PARALLEL !

MODULE DES_TIME_MARCH

      
  USE DISCRETELEMENT
  USE DES_ALLOCATE
  USE FUNCTIONS
  USE MACHINE

  
  USE DESGRID, ONLY: DESGRID_PIC

! ======================================================================= !

  !> MPI FUNCTIONALITIES (./MODEL/DMP_MODULES)
  USE MPI_UTILITY
  USE PARALLEL_MPI
  USE SENDRECV
  USE GRIDMAP

  !> ROUTINES FOR (UN)PACKING PARTICLES INTO THE MPI SEND BUFFERS (./MODEL/DES)
  USE DESMPI
  USE DESMPI_WRAPPER
  USE MPI_FUNS_DES

! ======================================================================= !
  
  USE OUTPUT, ONLY: DLB,DLB_TIME
  
  USE OUTPUT_MAN, ONLY: OUTPUT_MANAGER
  
  USE RUN, ONLY: NSTEP
  USE RUN, ONLY: TIME, TSTOP, DT


  use physprop, only: D_p0, RO_s0, MMAX

  
!---------------------------------------------------------------------//
      ! TOTAL NUMBER OF PARTICLES
      INTEGER, SAVE :: NP = 0, P, I

      ! COUNTER
      INTEGER :: PC
      
      ! LOOP COUNTER INDEX FOR ANY INITIAL PARTICLE SETTLING INCOUPLED CASES
      INTEGER :: FACTOR
      
      ! CHANGES IN SOLID TIME STEP
      DOUBLE PRECISION :: DTSOLID_TMP, TC

      LOGICAL :: EXIT_LOOP

      DOUBLE PRECISION ::  RADIUS, EN, ET

      INTEGER :: NLGS, C, CC, CC_START, CC_END, NB_CONTACTS

      ! =============================================================== !
      ! MPI VARIABLES
      
      INTEGER :: IERR, RANK, NB_PROCS, VALUE, LENGTH
      INTEGER, DIMENSION(MPI_STATUS_SIZE) :: STATUS
      
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: VALUES, DATA

      INTEGER :: ID
      CHARACTER(LEN=4) :: CHAR
      
      ! =============================================================== !
      ! ACTIVE SET VARIABLES
      
      DOUBLE PRECISION :: DX_AS, DY_AS, DZ_AS
      DOUBLE PRECISION :: NX_AS, NY_AS, NZ_AS

      DOUBLE PRECISION :: SS

      DOUBLE PRECISION :: UN_NEW, UT_NEW
      DOUBLE PRECISION :: UN_OLD, UT_OLD
      
      DOUBLE PRECISION :: TAU_N, TAU_T, GAMMA, MEQ

      DOUBLE PRECISION :: EC, EC_0

      
      
      ! =============================================================== !
      ! USER VARIABLES
      
      INTEGER, PARAMETER :: AS_DEMI_X = 1, AS_DEMI_Y = 2, AS_DEMI_Z = 3
      INTEGER, PARAMETER :: AS_DEMI_T = 4, AS_DEMI_B = 5, AS_DEMI_P = 6 ! (Angles: Theta,Beta,Phi)
      
      INTEGER, PARAMETER :: AS_OLD_X = 7, AS_OLD_Y = 8, AS_OLD_Z = 9
      INTEGER, PARAMETER :: AS_OLD_U = 10, AS_OLD_V = 11, AS_OLD_W = 12
      INTEGER, PARAMETER :: AS_OLD_T = 25, AS_OLD_B = 26, AS_OLD_P = 27 ! (Angles: Theta,Beta,Phi)
      INTEGER, PARAMETER :: AS_OLD_DT= 28, AS_OLD_DB= 29, AS_OLD_DP= 30
      
      INTEGER, PARAMETER :: AS_NEW_X = 13, AS_NEW_Y = 14, AS_NEW_Z = 15
      INTEGER, PARAMETER :: AS_NEW_U = 16, AS_NEW_V = 17, AS_NEW_W = 18
      INTEGER, PARAMETER :: AS_NEW_T = 31, AS_NEW_B = 32, AS_NEW_P = 33 ! (Angles: Theta,Beta,Phi)
      INTEGER, PARAMETER :: AS_NEW_DT= 34, AS_NEW_DB= 35, AS_NEW_DP= 36

      INTEGER, PARAMETER :: AS_FREE_X = 19, AS_FREE_Y = 20, AS_FREE_Z = 21
      INTEGER, PARAMETER :: AS_FREE_U = 22, AS_FREE_V = 23, AS_FREE_W = 24
      INTEGER, PARAMETER :: AS_FREE_T = 37, AS_FREE_B = 38, AS_FREE_P = 39 ! (Angles: Theta,Beta,Phi)
      INTEGER, PARAMETER :: AS_FREE_DT= 40, AS_FREE_DB= 41, AS_FREE_DP= 42

      INTEGER, PARAMETER :: AS_PMASS = 43, AS_RO_SOL = 44

      INTEGER :: CPT_INIT
        
      
!......................................................................!

    CONTAINS

!VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV!
!                                                                      !
!     SUBROUTINE: DES_TIME_INIT                                        !
!     AUTHOR: SOUFIANE                                DATE: 21-JUN-04  !
!                                                                      !
!     PURPOSE: MAIN DEM DRIVER ROUTINE                                 !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE DES_TIME_INIT

        USE DISCRETELEMENT

        IMPLICIT NONE
                        
                
        EXIT_LOOP = .FALSE.

        
        !> PAS DE TEMPS
        ! DTSOLID = 1.67983D-4
        ! DTSOLID = 1.42323D-3
        ! DTSOLID = 3.63427D-2
        ! DTSOLID = 8.75931D-2
        ! DTSOLID = 5.13892D-1
        ! DTSOLID = 1.D-4
        ! DTSOLID = 2.5D-4
        ! DTSOLID = 5.D-4
        DTSOLID = 1.D-3
        ! DTSOLID = 5.D-3


        !> JE CHERCHE LE RAYON LE PLUS PETIT
        ! RADIUS = HUGE(0D0)
        ! DO P = 1, PIP
        !    RADIUS=MIN(RADIUS,DES_RADIUS(P))
        ! END DO
        ! DTSOLID = 0.1D00*RADIUS/2.
        
        
        FACTOR = CEILING(REAL((TSTOP-TIME)/DTSOLID))
        DT = DTSOLID

        !> FREE FALLING PARTICLE
        ! DO P = 1, PIP
        !    RADIUS = DES_RADIUS(P)
        ! END DO
        ! DTSOLID = SQRT(2.D00*(0.5D00-RADIUS)/10.D00)/100.D00
        ! DTSOLID = DTSOLID/2.D00

        
        !> COEFFICIENTS DE RESTITUTION NORMAL
        ! EN = 1.D00
        ! EN = 0.9D00
        ! EN = 0.8D00
        EN = 0.7D00
        ! EN = 0.6D00
        ! EN = 0.5D00

        !> COEFFICIENTS DE RESTITUTION TANGENTIEL
        ! ET = 1.D00
        ! ET = 0.9D00
        ! ET = 0.8D00
        ! ET = 0.7D00
        ! ET = 0.6D00
        ET = EN*0.D00
        
        !> NOMBRE DE VARIABLES UTILISATEUR
        DES_USR_VAR_SIZE = 44


        TC = 0.D00
        
        DO P = 1, MAX_PIP
              DES_POS_OLD(P,1:3) = DES_POS_NEW(P,1:3)
              DES_VEL_OLD(P,1:3) = DES_VEL_NEW(P,1:3)
        END DO
        
        
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD ,NB_PROCS,IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,RANK,IERR)

        !> PARALLEL .DAT FILE
        ID = 1024 + RANK
        WRITE (CHAR,'(I3.3)')RANK
        OPEN(UNIT=ID,FILE='pos-'//TRIM(CHAR)//'.dat')


        !> KINETIC ENERGY FILE
        OPEN (UNIT=30, FILE='kinetic_energy.dat')

        
        !> FRONTAL COLLISION VALIDATION
        !> OPENING THE FILE FOR READING
        ! OPEN (1, FILE = 'analytical_solution.dat', STATUS='OLD')
        ! DO P = 1, 2
        !    READ (1,'(10(e15.8,1x))') VEL_ANALYTIC(P,1),VEL_ANALYTIC(P,2)
        ! END DO
        
        !> ABSOLUTE ERROR ANALYTICAL SOLUTION / EXPERIMENTAL SOLUTION >> FRONTAL COLLISION <<
        ! OPEN (41, FILE = 'abs_err_analytical-exp_X.dat')
        ! OPEN (42, FILE = 'abs_err_analytical-exp_Y.dat')
        
        
        !> COMPTEUR DE PARTICULES NORMALES UNIQUEMENT (GHOST EXCLUS)
        CPT_INIT = PIP - IGHOST_CNT

        
      END SUBROUTINE DES_TIME_INIT

! ===================================================== !
! ================= SIGNED DISTANCE =================== !     
! ===================================================== !      
      
      SUBROUTINE GET_SIGNED_DISTANCE(A,B,X,N,DIST)
        
        IMPLICIT NONE

        REAL(KIND=8) :: A(2),B(2),X(2),N(2),DIST
        REAL(KIND=8) :: T(2),D

        ! TANGENTE
        T = B-A

        ! NORMALE
        N(1) = -T(2)
        N(2) =  T(1)
        
        T = T/SQRT(T(1)**2+T(2)**2)
        D = (X(1)-A(1))*T(2)-(X(2)-A(2))*T(1)

        !> NORMALE UNITAIRE
        N = N/SQRT(N(1)**2+N(2)**2)

        DIST = ABS(D) 
        
        
      END SUBROUTINE GET_SIGNED_DISTANCE

! ===================================================== !
! ===================================================== !
! ===================================================== !
      
!VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV!
!                                                                      !
!     SUBROUTINE: DES_TIME_STEP                                        !
!     AUTHOR: SOUFIANE                                DATE: 21-JUN-04  !
!                                                                      !
!     PURPOSE: MAIN DEM DRIVER ROUTINE                                 !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

      SUBROUTINE DES_TIME_STEP(NN)

        USE COMPAR, ONLY: ADJUST_PARTITION
        USE GEOMETRY, ONLY: IMIN2, IMAX2
        USE PARAM, ONLY: DIMENSION_I, DIMENSION_J, DIMENSION_K
        
        IMPLICIT NONE

        INTEGER, INTENT(IN) :: NN
        LOGICAL :: MOD_ASSERTION
        LOGICAL :: FIRST

        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: BUFF_X, BUFF_Y, BUFF_Z
        INTEGER :: ISIGN

        DOUBLE PRECISION :: EX(2), ES(5)
        
        DOUBLE PRECISION :: SS_MAX, VM_N_MAX, VM_T_MAX
        DOUBLE PRECISION :: SS_MIN, VM_N_MIN, VM_T_MIN
        DOUBLE PRECISION :: PN_MAX, DPN_MAX
        DOUBLE PRECISION :: PT_MAX, DPT_MAX

        INTEGER :: IJK, I, J, K

        INTEGER, ALLOCATABLE, DIMENSION(:) :: PARTICLE_CL
        INTEGER :: PTR_PARTICLE_CL, PTR_PARTICLE_CL_GLOB
        INTEGER :: CPT_DEST
        LOGICAL :: FLAG

        DOUBLE PRECISION :: N_CL_CIRCLE(2), DIST_CL_CIRCLE
        DOUBLE PRECISION :: N_CL_INF(2), DIST_CL_INF
        DOUBLE PRECISION :: N_CL_LFT(2), DIST_CL_LFT, N_CL_LFT_VERT(2), DIST_CL_LFT_VERT
        DOUBLE PRECISION :: N_CL_RGHT(2), DIST_CL_RGHT, N_CL_RGHT_VERT(2), DIST_CL_RGHT_VERT
        DOUBLE PRECISION :: N_CL_SUP(2), DIST_CL_SUP

        INTEGER, ALLOCATABLE, DIMENSION(:) :: N_RANK_LOC
        INTEGER, ALLOCATABLE, DIMENSION(:) :: N_RANK_GLOB, N_RANK_GLOB_MOD
        INTEGER :: RANK_ITER

        
        DOUBLE PRECISION, DIMENSION(3,6) :: MP ! Matrice de passage
        DOUBLE PRECISION, DIMENSION(3,6) :: W ! Inverse de la matrice de masse dans le repère local
        DOUBLE PRECISION, DIMENSION(3)   :: IMEFF,PMEFF ! Matrice d'inertie des particules I et P
        DOUBLE PRECISION :: P_OPINERTI,I_OPINERTI
        INTEGER :: IW,JW,KW
        !!!!!!---------------------------------------------------
        DOUBLE PRECISION, PARAMETER :: MU = 3.D-1         ! FROTTEMENT
        !!!!!!---------------------------------------------------

        
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ARRAY_SS
        
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ARRAY_VM_N
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ARRAY_VM_T

        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: PN_ARRAY
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DPN_ARRAY
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: PT_ARRAY
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DPT_ARRAY

        
        !> POS DEMI SELON SI P ET I SONT GHOST - GHOST, GHOST - NORMAL, NORMAL - NORMAL
        ALLOCATE ( BUFF_X( 1:MAX_PIP ) )
        ALLOCATE ( BUFF_Y( 1:MAX_PIP ) )
        ALLOCATE ( BUFF_Z( 1:MAX_PIP ) )


        MOD_ASSERTION = (NN == 1 .OR. MOD(NN,NEIGHBOR_SEARCH_N) == 0)         

        
        TC = TC + DTSOLID


        ! DO P = 1, PIP
        !    IF (P == 1) THEN
        !       PRINT*,NN,"1 --> OLD_U",P,DES_USR_VAR(AS_OLD_U,P)
        !       PRINT*,NN,"1 --> OLD_X",P,DES_USR_VAR(AS_OLD_X,P)
        !    END IF
        ! END DO

        
        !> MFIX NEW VAR -> USR VAR (OLD) 
        DO P = 1, MAX_PIP
           DES_USR_VAR(AS_OLD_X,P) = DES_POS_NEW(P,1)
           DES_USR_VAR(AS_OLD_Y,P) = DES_POS_NEW(P,2)
           DES_USR_VAR(AS_OLD_Z,P) = DES_POS_NEW(P,3)
           ! DES_USR_VAR(AS_OLD_T,P) = OMEGA_OLD(P,1)
           
           DES_USR_VAR(AS_OLD_U,P) = DES_VEL_NEW(P,1)
           DES_USR_VAR(AS_OLD_V,P) = DES_VEL_NEW(P,2)
           DES_USR_VAR(AS_OLD_W,P) = DES_VEL_NEW(P,3)
           ! DES_USR_VAR(AS_OLD_DT,P) = OMEGA_NEW(P,1)
        END DO

        
        ! DO P = 1, PIP
        !    IF (P == 1) THEN
        !       PRINT*,NN,"2 --> OLD_U",P,DES_USR_VAR(AS_OLD_U,P)
        !       PRINT*,NN,"2 --> OLD_X",P,DES_USR_VAR(AS_OLD_X,P)
        !    END IF
        ! END DO
        ! STOP

        
        PC = 1

        
        DO P = 1, MAX_PIP
           IF(PC.GT.PIP) EXIT
           IF(IS_NONEXISTENT(P)) CYCLE
           PC = PC + 1

           !> EN 2D
           PMASS(P) = RO_SOL(P) * ACOS(-1.0D0) * DES_RADIUS(P)**2
           !> EN 3D
           ! PMASS(P) = RO_SOL(P) * (4.D0/3.D0) * ACOS(-1.0) * DES_RADIUS(P)**3
           
           FC(P,1) = 0.0D0  ! IMPULSION DE CONTACT !!
           FC(P,2) = 0.0D0  ! ATTENTION !!
           FC(P,3) = 0.0D0  
           
           !> VITESSE FREE: VITESSE LIBRE SUR EULER IMPLICITE
           DES_USR_VAR(AS_FREE_U,P) = DES_USR_VAR(AS_OLD_U,P) + DTSOLID*(FC(P,1)/PMASS(P) + GRAV(1)) 
           DES_USR_VAR(AS_FREE_V,P) = DES_USR_VAR(AS_OLD_V,P) + DTSOLID*(FC(P,2)/PMASS(P) + GRAV(2))
           DES_USR_VAR(AS_FREE_W,P) = DES_USR_VAR(AS_OLD_W,P) + DTSOLID*(FC(P,3)/PMASS(P) + GRAV(3))
           ! DES_USR_VAR(AS_FREE_DT,P) = DES_USR_VAR(AS_OLD_DT,P)
           
           !> SERGE: POSITION FREE CENTRE DE GRAVITE
           DES_USR_VAR(AS_FREE_X,P) = DES_USR_VAR(AS_OLD_X,P) + DTSOLID*DES_USR_VAR(AS_FREE_U,P)
           DES_USR_VAR(AS_FREE_Y,P) = DES_USR_VAR(AS_OLD_Y,P) + DTSOLID*DES_USR_VAR(AS_FREE_V,P)
           DES_USR_VAR(AS_FREE_Z,P) = DES_USR_VAR(AS_OLD_Z,P) + DTSOLID*DES_USR_VAR(AS_FREE_W,P)
           ! DES_USR_VAR(AS_FREE_T,P) = DES_USR_VAR(AS_OLD_T,P) + DTSOLID*DES_USR_VAR(AS_FREE_DT,P)
           
           !> POSITION DEMI CENTRE DE GRAVITE
           !> SERGE: POSITION DEMI
           ! DES_USR_VAR(AS_DEMI_X,P) = DES_USR_VAR(AS_OLD_X,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_FREE_U,P)
           DES_USR_VAR(AS_DEMI_X,P) = DES_USR_VAR(AS_OLD_X,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_OLD_U,P) !!!!!!!!!!!!!!
           ! DES_USR_VAR(AS_DEMI_Y,P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_FREE_V,P)
           DES_USR_VAR(AS_DEMI_Y,P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_OLD_V,P) !!!!!!!!!!!!!!
           ! DES_USR_VAR(AS_DEMI_Z,P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_FREE_W,P)
           DES_USR_VAR(AS_DEMI_Z,P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_OLD_W,P) !!!!!!!!!!!!!!
           ! DES_USR_VAR(AS_DEMI_T,P) = DES_USR_VAR(AS_OLD_T,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_FREE_DT,P)
           ! DES_USR_VAR(AS_DEMI_T,P) = DES_USR_VAR(AS_OLD_T,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_OLD_DT,P)
           
           !> SERGE: POSITION NEW CENTRE DE GRAVITE
           DES_USR_VAR(AS_NEW_X,P) = DES_USR_VAR(AS_FREE_X,P)
           DES_USR_VAR(AS_NEW_Y,P) = DES_USR_VAR(AS_FREE_Y,P)
           DES_USR_VAR(AS_NEW_Z,P) = DES_USR_VAR(AS_FREE_Z,P)
           ! DES_USR_VAR(AS_NEW_T,P) = DES_USR_VAR(AS_FREE_T,P)   

           !> VITESSE NEW
           DES_USR_VAR(AS_NEW_U,P) = DES_USR_VAR(AS_FREE_U,P)
           DES_USR_VAR(AS_NEW_V,P) = DES_USR_VAR(AS_FREE_V,P)
           DES_USR_VAR(AS_NEW_W,P) = DES_USR_VAR(AS_FREE_W,P)
           ! DES_USR_VAR(AS_NEW_DT,P) = DES_USR_VAR(AS_FREE_DT,P)
        END DO


        ! PRINT*,"====>",DES_USR_VAR(AS_NEW_Y,1),DES_USR_VAR(AS_NEW_V,1)
        ! PRINT*,"====>",DES_POS_NEW(1,2),DES_VEL_NEW(1,2)


        ! DO P = 1, PIP
        !    PRINT*,IGLOBAL_ID(P),EN,ET,MU,DTSOLID,GRAV(2),DES_RADIUS(P),PMASS(P),DES_POS_NEW(P,1),DES_VEL_NEW(P,1)
        ! END DO
        ! CALL MPI_FINALIZE(IERR)
        ! STOP

        
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD ,NB_PROCS,IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD ,RANK,IERR)
        
        
        ALLOCATE(PARTICLE_CL(MAX_PIP))

        
        PTR_PARTICLE_CL = 0.D00
        PTR_PARTICLE_CL_GLOB = 0.D00


        ALLOCATE(N_RANK_LOC(0:NB_PROCS-1))
        
        ALLOCATE(N_RANK_GLOB(0:NB_PROCS-1))
        ALLOCATE(N_RANK_GLOB_MOD(0:NB_PROCS-1))

                
                
! ======================================================================================= !                
! ===================== INCREMENTATION POINTEUR  ======================================== !
! ======================================================================================= !                

        DO P = 1, PIP
           IF ( PARTICLE_STATE(P) == 1 ) THEN

              !> FREELY FALLING PARTICLE / EQUILIBRE 5 PARTICULES
              ! CALL GET_SIGNED_DISTANCE([0.0D0,0.0D0],[1.0D0,0.0D0],DES_POS_NEW(P,1:2),N_CL_INF,DIST_CL_INF)
              !> CARRE 02*08
              CALL GET_SIGNED_DISTANCE([0.0D0,0.2D0],[1.0D0,0.2D0],DES_POS_NEW(P,1:2),N_CL_INF,DIST_CL_INF)
              !> DAM BREAK 00*20
              ! CALL GET_SIGNED_DISTANCE([0.0D0,0.0D0],[2.0D0,0.0D0],DES_POS_NEW(P,1:2),N_CL_INF,DIST_CL_INF)
              IF ((DIST_CL_INF .LT. 2*DES_RADIUS(P))) THEN               
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              END IF


              
              !> CARRE 02*08
              CALL GET_SIGNED_DISTANCE([0.2D0,0.2D0],[0.2D0,1.0D0],DES_POS_NEW(P,1:2),N_CL_LFT,DIST_CL_LFT)
              !> DAM BREAK 00*20
              ! CALL GET_SIGNED_DISTANCE([0.0D0,0.0D0],[0.0D0,2.0D0],DES_POS_NEW(P,1:2),N_CL_LFT,DIST_CL_LFT)
              IF ((DIST_CL_LFT .LT. 2*DES_RADIUS(P))) THEN 
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              END IF


              !> FREELY FALLING PARTICLE 
              ! CALL GET_SIGNED_DISTANCE([1.0D0,0.0D0],[1.0D0,1.0D0],DES_POS_NEW(P,1:2),N_CL_RGHT,DIST_CL_RGHT)
              !> CARRE 02*08
              CALL GET_SIGNED_DISTANCE([0.8D0,0.2D0],[0.8D0,1.0D0],DES_POS_NEW(P,1:2),N_CL_RGHT,DIST_CL_RGHT)
              !> DAM BREAK 00*20
              ! CALL GET_SIGNED_DISTANCE([2.0D0,0.0D0],[2.0D0,2.0D0],DES_POS_NEW(P,1:2),N_CL_RGHT,DIST_CL_RGHT)
              IF ((DIST_CL_RGHT .LT. 2*DES_RADIUS(P))) THEN               
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              END IF



              !> CARRE 02*08
              CALL GET_SIGNED_DISTANCE([0.0D0,0.8D0],[1.0D0,0.8D0],DES_POS_NEW(P,1:2),N_CL_SUP,DIST_CL_SUP)
              !> DAM BREAK 00*20
              ! CALL GET_SIGNED_DISTANCE([0.0D0,2.0D0],[2.0D0,2.0D0],DES_POS_NEW(P,1:2),N_CL_SUP,DIST_CL_SUP)
              IF ((DIST_CL_SUP .LT. 2*DES_RADIUS(P))) THEN               
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              END IF
              
           END IF
        END DO
        
! =================================================================================== !                
! ============================ FIN INCREMENTATION =================================== !
! =================================================================================== !                
        
        N_RANK_LOC = 0.D00
                
        N_RANK_GLOB = 0.D00
        N_RANK_GLOB_MOD = 0.D00
        
        N_RANK_LOC(RANK) = PTR_PARTICLE_CL

                
        CALL MPI_ALLREDUCE(N_RANK_LOC,N_RANK_GLOB,NB_PROCS,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,IERR)
        CALL MPI_ALLREDUCE(PTR_PARTICLE_CL,PTR_PARTICLE_CL_GLOB,NB_PROCS,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,IERR)
        
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)

                
        N_RANK_GLOB_MOD(0) = N_RANK_GLOB(0)
        
        DO RANK_ITER = 1, NB_PROCS - 1
           N_RANK_GLOB_MOD(RANK_ITER) = N_RANK_GLOB(RANK_ITER) + N_RANK_GLOB_MOD(RANK_ITER-1)  
        END DO
        
        N_RANK_GLOB_MOD(RANK) = N_RANK_GLOB_MOD(RANK) - N_RANK_GLOB(RANK) + 1 + IMAX_GLOBAL_ID
        

        PARTICLE_CL = 0.D00
        
        PTR_PARTICLE_CL = 0.D00
        
        
! ================================================================================= !
! ===================== CREATION PARTICULE ======================================== !
! ================================================================================= !
        
        DO P = 1, PIP
           
           IF ( PARTICLE_STATE(P) == 1 ) THEN
              
              !> FLAG POUR CREATION DE PARTICULE CL
              FLAG = .TRUE.
              

              DO I = 1, PTR_PARTICLE_CL
                 IF (PARTICLE_CL(I) == IGLOBAL_ID(P)) FLAG = .FALSE.
              END DO
              

              ! ------------ !
              ! CL 2: CARRE  !
              ! ------------ !
              
! ------------------------------------------------------------------------------------------------------------- !

              !> CL CARRE (ARETE INFERIEURE)
              !> FREELY FALLING PARTICLE / EQUILIBRE 5 PARTICULES
              ! CALL GET_SIGNED_DISTANCE([0.0D0,0.0D0],[1.0D0,0.0D0],DES_POS_NEW(P,1:2),N_CL_INF,DIST_CL_INF)
              !> CARRE 02*08
              CALL GET_SIGNED_DISTANCE([0.0D0,0.2D0],[1.0D0,0.2D0],DES_POS_NEW(P,1:2),N_CL_INF,DIST_CL_INF)
              !> DAM BREAK 00*20
              ! CALL GET_SIGNED_DISTANCE([0.0D0,0.0D0],[2.0D0,0.0D0],DES_POS_NEW(P,1:2),N_CL_INF,DIST_CL_INF)
              
              IF ((DIST_CL_INF .LT. 2*DES_RADIUS(P)) .AND. (FLAG .EQV. .TRUE.)) THEN               
                 
                 PARTICLES = PARTICLES + 1

                 
                 DES_RADIUS(PIP+1) = 1.D-4!1.D12!
                 PMASS(PIP+1) = 1.D12
                 RO_SOL(PIP+1) = PMASS(PIP+1) / (ACOS(-1.0) * DES_RADIUS(PIP+1)**2)
                 
                 DES_POS_NEW(PIP+1,1) = DES_POS_NEW(P,1) - N_CL_INF(1)*(DIST_CL_INF+DES_RADIUS(PIP+1))
                 DES_POS_NEW(PIP+1,2) = DES_POS_NEW(P,2) - N_CL_INF(2)*(DIST_CL_INF+DES_RADIUS(PIP+1))
                 DES_POS_NEW(PIP+1,3) = DES_POS_NEW(P,3)

                 DES_VEL_NEW(PIP+1,1) = 0.0
                 DES_VEL_NEW(PIP+1,2) = 0.0
                 DES_VEL_NEW(PIP+1,3) = 0.0

                 DES_USR_VAR(AS_NEW_X:AS_NEW_Z,PIP+1) =  DES_POS_NEW(PIP+1,1:3) 
                 DES_USR_VAR(AS_OLD_X:AS_OLD_Z,PIP+1) =  DES_POS_NEW(PIP+1,1:3) 
                 DES_USR_VAR(AS_NEW_U:AS_NEW_W,PIP+1) =  DES_VEL_NEW(PIP+1,1:3)
                 DES_USR_VAR(AS_OLD_U:AS_OLD_W,PIP+1) =  DES_VEL_NEW(PIP+1,1:3)
                 
                 CALL PIC_SEARCH(J, DES_POS_NEW(PIP+1,2), YN, DIMENSION_J, JMIN2, JMAX2)
                 CALL PIC_SEARCH(I, DES_POS_NEW(PIP+1,1), XE, DIMENSION_I, IMIN2, IMAX2)
                 
                 K = 1
                 IJK = FUNIJK(I,J,K)
                 
                 PIP = PIP + 1
                 CALL PARTICLE_GROW(PIP)
                 MAX_PIP = MAX(PIP,MAX_PIP)
                 
                 CALL SET_NORMAL(PIP)
                 
                 PIJK(PIP,1) = I
                 PIJK(PIP,2) = J
                 PIJK(PIP,3) = K
                 PIJK(PIP,4) = IJK  
                 PIJK(PIP,5) = 1    ! N° DE PHASE
                 
                 PARTICLE_STATE(PIP) = NORMAL_PARTICLE

                 IGLOBAL_ID(PIP) = N_RANK_GLOB_MOD(RANK)
                 N_RANK_GLOB_MOD(RANK) =  N_RANK_GLOB_MOD(RANK) + 1
                 
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
                 PARTICLE_CL(PTR_PARTICLE_CL) = IGLOBAL_ID(PIP)

              END IF

! ------------------------------------------------------------------------------------------------------------- !
              
              !> CL CARRE (ARETE GAUCHE)
              !> CARRE 02*08
              CALL GET_SIGNED_DISTANCE([0.2D0,0.2D0],[0.2D0,1.0D0],DES_POS_NEW(P,1:2),N_CL_LFT,DIST_CL_LFT)
              !> DAM BREAK 00*20
              ! CALL GET_SIGNED_DISTANCE([0.0D0,0.0D0],[0.0D0,2.0D0],DES_POS_NEW(P,1:2),N_CL_LFT,DIST_CL_LFT)
              
              
              IF ((DIST_CL_LFT .LT. 2*DES_RADIUS(P)) .AND. (FLAG .EQV. .TRUE.)) THEN               
                 
                 PARTICLES = PARTICLES + 1

                 DES_RADIUS(PIP+1) = 1.D-4
                 PMASS(PIP+1) = 1.D12
                 RO_SOL(PIP+1) = PMASS(PIP+1) / (ACOS(-1.0) * DES_RADIUS(PIP+1)**2)

                 DES_POS_NEW(PIP+1,1) = DES_POS_NEW(P,1) + N_CL_LFT(1)*(DIST_CL_LFT+DES_RADIUS(PIP+1))
                 DES_POS_NEW(PIP+1,2) = DES_POS_NEW(P,2) + N_CL_LFT(2)*(DIST_CL_LFT+DES_RADIUS(PIP+1)) 
                 DES_POS_NEW(PIP+1,3) = DES_POS_NEW(P,3)

                 DES_VEL_NEW(PIP+1,1) = 0.0
                 DES_VEL_NEW(PIP+1,2) = 0.0
                 DES_VEL_NEW(PIP+1,3) = 0.0

                 DES_USR_VAR(AS_NEW_X:AS_NEW_Z,PIP+1) =  DES_POS_NEW(PIP+1,1:3) 
                 DES_USR_VAR(AS_OLD_X:AS_OLD_Z,PIP+1) =  DES_POS_NEW(PIP+1,1:3) 
                 DES_USR_VAR(AS_NEW_U:AS_NEW_W,PIP+1) =  DES_VEL_NEW(PIP+1,1:3)
                 DES_USR_VAR(AS_OLD_U:AS_OLD_W,PIP+1) =  DES_VEL_NEW(PIP+1,1:3)
                 
                 CALL PIC_SEARCH(J, DES_POS_NEW(PIP+1,2), YN, DIMENSION_J, JMIN2, JMAX2)
                 CALL PIC_SEARCH(I, DES_POS_NEW(PIP+1,1), XE, DIMENSION_I, IMIN2, IMAX2)
                 
                 K = 1
                 IJK = FUNIJK(I,J,K)
                 
                 PIP = PIP + 1
                 CALL PARTICLE_GROW(PIP)
                 MAX_PIP = MAX(PIP,MAX_PIP)
                 
                 CALL SET_NORMAL(PIP)
                 
                 PIJK(PIP,1) = I
                 PIJK(PIP,2) = J
                 PIJK(PIP,3) = K
                 PIJK(PIP,4) = IJK  
                 PIJK(PIP,5) = 1    ! N° DE PHASE
                 
                 PARTICLE_STATE(PIP) = NORMAL_PARTICLE
                 
                 IGLOBAL_ID(PIP) = N_RANK_GLOB_MOD(RANK)
                 N_RANK_GLOB_MOD(RANK) =  N_RANK_GLOB_MOD(RANK) + 1
                 
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
                 PARTICLE_CL(PTR_PARTICLE_CL) = IGLOBAL_ID(PIP)

              END IF

! ------------------------------------------------------------------------------------------------------------- !

              !> CL CARRE (ARETE DROITE)
              !> FREELY FALLING PARTICLE 
              ! CALL GET_SIGNED_DISTANCE([1.0D0,0.0D0],[1.0D0,1.0D0],DES_POS_NEW(P,1:2),N_CL_RGHT,DIST_CL_RGHT)
              !> CARRE 02*08
              CALL GET_SIGNED_DISTANCE([0.8D0,0.2D0],[0.8D0,1.0D0],DES_POS_NEW(P,1:2),N_CL_RGHT,DIST_CL_RGHT)
              !> DAM BREAK 00*20
              ! CALL GET_SIGNED_DISTANCE([2.0D0,0.0D0],[2.0D0,2.0D0],DES_POS_NEW(P,1:2),N_CL_RGHT,DIST_CL_RGHT)
              
              
              IF ((DIST_CL_RGHT .LT. 2*DES_RADIUS(P)) .AND. (FLAG .EQV. .TRUE.)) THEN               
                 
                 PARTICLES = PARTICLES + 1

                 DES_RADIUS(PIP+1) = 1.D-4!1.D12! 
                 PMASS(PIP+1) = 1.D12
                 RO_SOL(PIP+1) = PMASS(PIP+1) / (ACOS(-1.0) * DES_RADIUS(PIP+1)**2)

                 DES_POS_NEW(PIP+1,1) = DES_POS_NEW(P,1) - N_CL_RGHT(1)*(DIST_CL_RGHT+DES_RADIUS(PIP+1))
                 DES_POS_NEW(PIP+1,2) = DES_POS_NEW(P,2) - N_CL_RGHT(2)*(DIST_CL_RGHT+DES_RADIUS(PIP+1)) 
                 DES_POS_NEW(PIP+1,3) = DES_POS_NEW(P,3)

                 DES_VEL_NEW(PIP+1,1) = 0.0
                 DES_VEL_NEW(PIP+1,2) = 0.0
                 DES_VEL_NEW(PIP+1,3) = 0.0

                 DES_USR_VAR(AS_NEW_X:AS_NEW_Z,PIP+1) =  DES_POS_NEW(PIP+1,1:3) 
                 DES_USR_VAR(AS_OLD_X:AS_OLD_Z,PIP+1) =  DES_POS_NEW(PIP+1,1:3) 
                 DES_USR_VAR(AS_NEW_U:AS_NEW_W,PIP+1) =  DES_VEL_NEW(PIP+1,1:3)
                 DES_USR_VAR(AS_OLD_U:AS_OLD_W,PIP+1) =  DES_VEL_NEW(PIP+1,1:3)
                 
                 CALL PIC_SEARCH(J, DES_POS_NEW(PIP+1,2), YN, DIMENSION_J, JMIN2, JMAX2)
                 CALL PIC_SEARCH(I, DES_POS_NEW(PIP+1,1), XE, DIMENSION_I, IMIN2, IMAX2)
                 
                 K = 1
                 IJK = FUNIJK(I,J,K)
                 
                 PIP = PIP + 1
                 CALL PARTICLE_GROW(PIP)
                 MAX_PIP = MAX(PIP,MAX_PIP)
                 
                 CALL SET_NORMAL(PIP)
                 
                 PIJK(PIP,1) = I
                 PIJK(PIP,2) = J
                 PIJK(PIP,3) = K
                 PIJK(PIP,4) = IJK  
                 PIJK(PIP,5) = 1    ! N° DE PHASE
                 
                 PARTICLE_STATE(PIP) = NORMAL_PARTICLE

                 IGLOBAL_ID(PIP) = N_RANK_GLOB_MOD(RANK)
                 N_RANK_GLOB_MOD(RANK) =  N_RANK_GLOB_MOD(RANK) + 1
                 
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
                 PARTICLE_CL(PTR_PARTICLE_CL) = IGLOBAL_ID(PIP)

              END IF

! ------------------------------------------------------------------------------------------------------------- !

              !> CL CARRE (ARETE SUPERIEURE)
              !> CARRE 02*08
              CALL GET_SIGNED_DISTANCE([0.0D0,0.8D0],[1.0D0,0.8D0],DES_POS_NEW(P,1:2),N_CL_SUP,DIST_CL_SUP)
              !> DAM BREAK 00*20
              ! CALL GET_SIGNED_DISTANCE([0.0D0,2.0D0],[2.0D0,2.0D0],DES_POS_NEW(P,1:2),N_CL_SUP,DIST_CL_SUP)

              
              IF ((DIST_CL_SUP .LT. 2*DES_RADIUS(P)) .AND. (FLAG .EQV. .TRUE.)) THEN               
                 
                 PARTICLES = PARTICLES + 1

                 DES_RADIUS(PIP+1) = 1.D-4
                 PMASS(PIP+1) = 1.D12
                 RO_SOL(PIP+1) = PMASS(PIP+1) / (ACOS(-1.0) * DES_RADIUS(PIP+1)**2)

                 DES_POS_NEW(PIP+1,1) = DES_POS_NEW(P,1) + N_CL_SUP(1)*(DIST_CL_SUP+DES_RADIUS(PIP+1))
                 DES_POS_NEW(PIP+1,2) = DES_POS_NEW(P,2) + N_CL_SUP(2)*(DIST_CL_SUP+DES_RADIUS(PIP+1)) 
                 DES_POS_NEW(PIP+1,3) = DES_POS_NEW(P,3)

                 DES_VEL_NEW(PIP+1,1) = 0.0
                 DES_VEL_NEW(PIP+1,2) = 0.0
                 DES_VEL_NEW(PIP+1,3) = 0.0

                 DES_USR_VAR(AS_NEW_X:AS_NEW_Z,PIP+1) =  DES_POS_NEW(PIP+1,1:3) 
                 DES_USR_VAR(AS_OLD_X:AS_OLD_Z,PIP+1) =  DES_POS_NEW(PIP+1,1:3)
                 DES_USR_VAR(AS_NEW_U:AS_NEW_W,PIP+1) =  DES_VEL_NEW(PIP+1,1:3)
                 DES_USR_VAR(AS_OLD_U:AS_OLD_W,PIP+1) =  DES_VEL_NEW(PIP+1,1:3)
                 
                 CALL PIC_SEARCH(J, DES_POS_NEW(PIP+1,2), YN, DIMENSION_J, JMIN2, JMAX2)
                 CALL PIC_SEARCH(I, DES_POS_NEW(PIP+1,1), XE, DIMENSION_I, IMIN2, IMAX2)
                 
                 K = 1
                 IJK = FUNIJK(I,J,K)
                 
                 PIP = PIP + 1
                 CALL PARTICLE_GROW(PIP)
                 MAX_PIP = MAX(PIP,MAX_PIP)
                 
                 CALL SET_NORMAL(PIP)
                 
                 PIJK(PIP,1) = I
                 PIJK(PIP,2) = J
                 PIJK(PIP,3) = K
                 PIJK(PIP,4) = IJK  
                 PIJK(PIP,5) = 1    ! N° DE PHASE
                 
                 PARTICLE_STATE(PIP) = NORMAL_PARTICLE

                 IGLOBAL_ID(PIP) = N_RANK_GLOB_MOD(RANK)
                 N_RANK_GLOB_MOD(RANK) =  N_RANK_GLOB_MOD(RANK) + 1
                 
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
                 PARTICLE_CL(PTR_PARTICLE_CL) = IGLOBAL_ID(PIP)
                 
              END IF

! ------------------------------------------------------------------------------------------------------------- !
                            
           END IF
           
        END DO


        CALL GLOBAL_SUM(PIP,PARTICLES)

! ================================================================================= !        
! ================================ FIN CREATION =================================== !
! ================================================================================= !

        DES_USR_VAR(AS_PMASS,:) = PMASS
        DES_USR_VAR(AS_RO_SOL,:) = RO_SOL
        
        CALL NEIGHBOUR

        CALL DESGRID_PIC(.TRUE.) 
        CALL DES_PAR_EXCHANGE


        ! DO P = 1, PIP
        !    IF(IS_NONEXISTENT(P)) CYCLE
        !    PRINT'(1X,I4,4(1X,E15.8))',IGLOBAL_ID(P),RO_SOL(P),DES_USR_VAR(AS_RO_SOL,P),PMASS(P),DES_USR_VAR(AS_PMASS,P)
        ! END DO
        ! CALL MPI_FINALIZE(IERR)
        ! STOP
        
        
        PN = 0.D00
        PT = 0.D00
        
        DPN = 0.D00
        DPT = 0.D00
        
        ALLOCATE (ARRAY_VM_N (5*MAX_PIP) )
        ALLOCATE (ARRAY_VM_T (5*MAX_PIP) )
        
        ALLOCATE (ARRAY_SS (5*MAX_PIP) )

        ALLOCATE (PN_ARRAY (5*MAX_PIP) )
        ALLOCATE (DPN_ARRAY(5*MAX_PIP) )
        
        ALLOCATE (PT_ARRAY (5*MAX_PIP) )
        ALLOCATE (DPT_ARRAY(5*MAX_PIP) )
        
! ============================================================================== !        
! ============================ BOUCLE NLGS ===================================== !        
! ============================================================================== !
        
        DO NLGS = 1, 1000
           
           SS_MAX = 0.D00
           SS_MIN = 0.D00

           VM_N_MAX = 0.D00
           VM_N_MIN = 0.D00

           VM_T_MAX = 0.D00
           VM_T_MIN = 0.D00
           
           NB_CONTACTS = 0.D00
           
           ACTIVE = .FALSE.
           
           VM_N = 0.D00
           VM_T = 0.D00
           
           ARRAY_SS = 0.D00 

           ARRAY_VM_N = 0.D00
           ARRAY_VM_T = 0.D00

           PN_ARRAY = 0.D00
           DPN_ARRAY = 0.D00
           
           PT_ARRAY = 0.D00
           DPT_ARRAY = 0.D00
           
           
           DO_NSEARCH = (NN == 1 .OR. MOD(NN,NEIGHBOR_SEARCH_N) == 0)
           IF (DO_NSEARCH .OR. (NUMPES>1) .OR. DES_PERIODIC_WALLS) THEN
              CALL DESGRID_PIC(.TRUE.) 
              CALL DES_PAR_EXCHANGE
           ENDIF

           
! ------------------------------------------------------------------------------------------------------------- !

           !> ITERATION SUR LES GHOST (EN PARALLELE)
           
           !       IF ((PARTICLE_STATE(P) * PARTICLE_STATE(I) == 4) .OR. (PARTICLE_STATE(I) * PARTICLE_STATE(P) == 4)) THEN 
                                     
           !          IF (IGLOBAL_ID(P) .NE. IGLOBAL_ID(I)) THEN
                      
! ------------------------------------------------------------------------------------------------------------- !
           
           !> ITERATION SUR LES NORMALES
           PC = 1
           
           DO P = 1, MAX_PIP
              IF (PC .GT. PIP) EXIT
              IF (IS_NONEXISTENT(P)) CYCLE
              PC = PC + 1
             
              CC_START = 1
              IF (P .GT. 1) CC_START = NEIGHBOR_INDEX(P-1)
              CC_END = NEIGHBOR_INDEX(P)
              NB_CONTACTS = NB_CONTACTS + CC_END - CC_START

                            
              !> CONTACT LOOP NO GHOST PARTICLES
              DO CC = CC_START, CC_END-1

                 I = NEIGHBORS(CC)
                 IF(IS_NONEXISTENT(I)) CYCLE

                 IF ((PARTICLE_STATE(P) * PARTICLE_STATE(I) == 1)) THEN
                                     
                    IF (IGLOBAL_ID(P) .NE. IGLOBAL_ID(I)) THEN
                                              
                       ! BUFF_X(P) = DES_USR_VAR(AS_OLD_X,P) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_U,P) ! BUFF_X(P) = DES_USR_VAR(AS_DEMI_X,P)
                       ! BUFF_X(I) = DES_USR_VAR(AS_OLD_X,I) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_U,I) ! BUFF_X(I) = DES_USR_VAR(AS_DEMI_X,I)
                       BUFF_X(P) = DES_USR_VAR(AS_OLD_X,P) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_U,P) !!!!!!!!!!!!!!!!!!!!
                       BUFF_X(I) = DES_USR_VAR(AS_OLD_X,I) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_U,I) !!!!!!!!!!!!!!!!!!!!
                       
                       ! BUFF_Y(P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_V,P) ! BUFF_Y(P) = DES_USR_VAR(AS_DEMI_Y,P)
                       ! BUFF_Y(I) = DES_USR_VAR(AS_OLD_Y,I) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_V,I) ! BUFF_Y(I) = DES_USR_VAR(AS_DEMI_Y,I)
                       BUFF_Y(P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_V,P) !!!!!!!!!!!!!!!!!!!!
                       BUFF_Y(I) = DES_USR_VAR(AS_OLD_Y,I) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_V,I) !!!!!!!!!!!!!!!!!!!!
                       
                       ! BUFF_Z(P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_W,P) ! BUFF_Z(P) = DES_USR_VAR(AS_DEMI_Z,P)
                       ! BUFF_Z(I) = DES_USR_VAR(AS_OLD_Z,I) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_W,I) ! BUFF_Z(I) = DES_USR_VAR(AS_DEMI_Z,I)
                       BUFF_Z(P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_W,P) !!!!!!!!!!!!!!!!!!!!
                       BUFF_Z(I) = DES_USR_VAR(AS_OLD_Z,I) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_W,I) !!!!!!!!!!!!!!!!!!!!
                                              
                       
                       IF (IGLOBAL_ID(P) .GT. IGLOBAL_ID(I)) THEN
                          DX_AS = BUFF_X(P) - BUFF_X(I)
                          DY_AS = BUFF_Y(P) - BUFF_Y(I)
                          DZ_AS = BUFF_Z(P) - BUFF_Z(I)
                       ELSE
                          DX_AS = BUFF_X(I) - BUFF_X(P)
                          DY_AS = BUFF_Y(I) - BUFF_Y(P)
                          DZ_AS = BUFF_Z(I) - BUFF_Z(P)
                       END IF
                       
                       NX_AS = DX_AS / SQRT(DX_AS**2 + DY_AS**2 + DZ_AS**2) 
                       NY_AS = DY_AS / SQRT(DX_AS**2 + DY_AS**2 + DZ_AS**2) 
                       NZ_AS = DZ_AS / SQRT(DX_AS**2 + DY_AS**2 + DZ_AS**2)
                       IF (ABS(NY_AS) .LT. 1.D-06) NY_AS = 0.D00

                       ! Matrice de passage (2D)!!! DU REPERE GLOBAL AU REPERE LOCAL DU CONTACT
                       MP(1,1) =  NX_AS ! + Normale sortante (pour particule I)
                       MP(2,1) = -NY_AS ! + Tangente (pour particule I)
                       MP(1,2) =  NY_AS
                       MP(2,2) =  NX_AS
                       MP(1,3) =  0.D00 ! Pour les angles
                       MP(2,3) = -DES_RADIUS(I)
                       MP(1,4) = -NX_AS ! - Normale sortante (pour particule P)
                       MP(2,4) =  NY_AS
                       MP(1,5) = -NY_AS ! - Tangente (pour particule P)
                       MP(2,5) = -NX_AS
                       MP(1,6) =  0.D0  ! Pour les angles
                       MP(2,6) = -DES_RADIUS(P)
                       MP(3,1:6)=0.D00 ! Pour l'instant !!
                       ! RMQ : N=(NX,NY,NZ), T1=(-NY,NX,0)/S, T2=(-NX*NZ,-NY*NZ,S**2)/S où S=NX**2+NY**2 en 3D par exemple
                       
                       ! OPERATEURS D'INERTIE DE CYLINDRES (AXE Z) : mettre billes en 3D !!
                       P_OPINERTI=DES_USR_VAR(AS_PMASS,P)*DES_RADIUS(P)*DES_RADIUS(P)/2.D00
                       I_OPINERTI=DES_USR_VAR(AS_PMASS,I)*DES_RADIUS(I)*DES_RADIUS(I)/2.D00


                       ! MEQ = 4.356341934204102E-002 !* 1d-2
                       ! PMASS(P) = 2.D00*MEQ
                       ! PMASS(I) = 2.D00*MEQ
                       ! MEQ = PMASS(P) * PMASS(I) / (PMASS(P) + PMASS(I))
                       MEQ = DES_USR_VAR(AS_PMASS,P) * DES_USR_VAR(AS_PMASS,I) / ( DES_USR_VAR(AS_PMASS,P) + DES_USR_VAR(AS_PMASS,I) )
                       
                       
                       IMEFF(1) = DTSOLID/DES_USR_VAR(AS_PMASS,I)
                       IMEFF(2) = DTSOLID/DES_USR_VAR(AS_PMASS,I)
                       IMEFF(3) = DTSOLID/I_OPINERTI
                       
                       PMEFF(1) = DTSOLID/DES_USR_VAR(AS_PMASS,P)
                       PMEFF(2) = DTSOLID/DES_USR_VAR(AS_PMASS,P)
                       PMEFF(3) = DTSOLID/P_OPINERTI
                       
                       ! Inverse de la matrice de masse dans le repère local
                       DO IW=1,2 ! Pour l'instant 2D !!
                          DO JW=1,2
                             W(IW,JW)=0.D0
                             DO KW=1,3
                                W(IW,JW)=W(IW,JW)+MP(IW,KW  )*MP(JW,KW  )*IMEFF(KW) 
                                W(IW,JW)=W(IW,JW)+MP(IW,KW+3)*MP(JW,KW+3)*PMEFF(KW) 
                             END DO
                          END DO
                       END DO
                       
                       
                       SS = SQRT(DX_AS**2+DY_AS**2+DZ_AS**2) - (DES_RADIUS(P)+DES_RADIUS(I))
                       ! ================================================================================================================ !
                       !> SERGE: SS
                       ! SS = (DES_USR_VAR(AS_NEW_X,P) - DES_USR_VAR(AS_NEW_X,I))**2 + &
                       !      (DES_USR_VAR(AS_NEW_Y,P) - DES_USR_VAR(AS_NEW_Y,I))**2 + &
                       !      (DES_USR_VAR(AS_NEW_Z,P) - DES_USR_VAR(AS_NEW_Z,I))**2
                       ! SS = SQRT(SS) - (DES_RADIUS(P) + DES_RADIUS(I))
                       ! ================================================================================================================ !

                       IF (SS .LT. 0) THEN
                       
                          UN_OLD = &
                               (DES_USR_VAR(AS_OLD_U,I)-DES_USR_VAR(AS_OLD_U,P))*MP(1,1) + &
                               (DES_USR_VAR(AS_OLD_V,I)-DES_USR_VAR(AS_OLD_V,P))*MP(1,2) + &
                               (DES_USR_VAR(AS_OLD_W,I)-DES_USR_VAR(AS_OLD_W,P))*NZ_AS
                          ! UT_OLD = &
                          !      (DES_USR_VAR(AS_OLD_U,I)-DES_USR_VAR(AS_OLD_U,P))*MP(2,1) + &
                          !      (DES_USR_VAR(AS_OLD_V,I)-DES_USR_VAR(AS_OLD_V,P))*MP(2,2) + &
                          !      (DES_USR_VAR(AS_OLD_W,I)-DES_USR_VAR(AS_OLD_W,P))*MP(3,2) + &
                          !      DES_USR_VAR(AS_OLD_DT,I)*MP(2,3)+DES_USR_VAR(AS_OLD_DT,P)*MP(2,6) !! Attention au signe !!
                          
                          UN_NEW = &
                               (DES_USR_VAR(AS_NEW_U,I)-DES_USR_VAR(AS_NEW_U,P))*MP(1,1) + &
                               (DES_USR_VAR(AS_NEW_V,I)-DES_USR_VAR(AS_NEW_V,P))*MP(1,2) + &
                               (DES_USR_VAR(AS_NEW_W,I)-DES_USR_VAR(AS_NEW_W,P))*NZ_AS
                          ! UT_NEW = &
                          !      (DES_USR_VAR(AS_NEW_U,I)-DES_USR_VAR(AS_NEW_U,P))*MP(2,1) + &
                          !      (DES_USR_VAR(AS_NEW_V,I)-DES_USR_VAR(AS_NEW_V,P))*MP(2,2) + &
                          !      (DES_USR_VAR(AS_NEW_W,I)-DES_USR_VAR(AS_NEW_W,P))*MP(3,2) + &
                          !      DES_USR_VAR(AS_NEW_DT,I)*MP(2,3)+DES_USR_VAR(AS_NEW_DT,P)*MP(2,6) !! Attention au signe !!
                          
                          VM_N(CC) = (UN_NEW + UN_OLD * EN) / (1.D0 + EN)
                          ! VM_T(CC) = (UT_NEW + UT_OLD * ET) / (1.D0 + ET)
                          ! PRINT*,"--- MOREAU ---",VM_N(CC)
                          
                          ARRAY_VM_N(CC) = VM_N(CC)
                       
                          
                          GAMMA = 1.D3                                                              
                          TAU_N = PN(CC) - GAMMA * VM_N(CC)
                          ! PRINT*,"--- TAU ---",TAU_N
                          
                          ! PRINT*,NN,TC
                          ! PRINT*,"FC",FC(P,2),GRAV(2)
                          ! IF (NN == 201 .OR. NN == 202 .OR. NN == 203) THEN
                          ! IF (P == 1) THEN
                          !    PRINT*,NN,"3 --> OLD_U",P,DES_USR_VAR(AS_OLD_U,P)
                          !    PRINT*,NN,"3 --> OLD_X",P,DES_USR_VAR(AS_OLD_X,P)
                          ! END IF
                          ! END IF
                          ! PRINT*,"FREE_V",DES_USR_VAR(AS_FREE_V,P)
                          ! PRINT*,"BUFF_Y(P)",BUFF_Y(P)
                          ! PRINT*,"BUFF_Y(I)",BUFF_Y(I)
                          ! PRINT*,"DY_AS",DY_AS
                          ! PRINT*,"AS_NEW_Y",DES_USR_VAR(AS_NEW_Y,P)
                          ! PRINT*,"SS",SS
                          ! PRINT*,"UN_OLD",UN_OLD
                          ! PRINT*,"UN_NEW",UN_NEW
                          ! PRINT*,"VM",VM_N(CC)
                          ! PRINT*,"Tau_n",TAU_N
                          ! PRINT*,"----"
                          
                          
                          IF (TAU_N .GT. 0) THEN
                             ! DPN(CC) = - (VM_N(CC) / W(1,1)) * (1.D00 + EN)
                             DPN(CC) = - (VM_N(CC) * MEQ * (1.D00 + EN)) / (DTSOLID) !!!!!!!!!!!!!!!!!
                             ! PRINT*,"--- DPN ---",DPN(CC)
                             ACTIVE(CC) = .TRUE.
                             
                             ! TAU_T = ABS( PT(CC) - GAMMA * VM_T(CC) ) - MU * PN(CC)
                             
                             ! IF (TAU_T .GT. 0) THEN ! GLISSEMENT
                             !    DPT(CC)= (MU * PN(CC) * (PT(CC) - GAMMA * VM_T(CC)) / ABS( PT(CC) - GAMMA * VM_T(CC)) - PT(CC))
                             ! ELSE ! ADHESION
                             !    DPT(CC) = - VM_T(CC) / W(2,2)
                             ! ENDIF
                          ELSE
                             DPN(CC) = 0.D00
                             ! DPT(CC) = 0.D00
                          END IF
                          
                          ! ! NOUVELLE VITESSE DE MOREAU
                          ! VM_N(CC) = VM_N(CC) + DPN(CC)*W(1,1)
                          ! VM_T(CC) = VM_T(CC) + DPT(CC)*W(2,2)
                          ! DPN(CC) = - UN_NEW/W(1,1)
                          ! !DPT(CC) = - UT_NEW/W(2,2)
                          ! ! NOUVELLE VITESSE DANS LE REPERE LOCAL
                          ! UN_NEW = VM_N(CC) * (1 + EN) - UN_OLD * EN
                          ! UT_NEW = VM_T(CC) * (1 + ET) - UT_OLD * ET
                          ! ! CALCUL DES REACTIONS LOCALES REELLES
                          ! DPN(CC) = DPN(CC) + UN_NEW/W(1,1)
                          ! !DPT(CC) = DPT(CC) + UT_NEW/W(2,2)
                          PN(CC) = PN(CC) + DPN(CC)
                          ! PT(CC) = PT(CC) + DPT(CC)

                       ELSE

                          PN(CC) = 0

                       END IF

                       
                       PN_ARRAY(CC) = PN(CC)
                       DPN_ARRAY(CC) = DPN(CC)
                       
                       ! PRINT*,"! ========== !"
                       ! PRINT*,"  NLGS:",NLGS
                       ! PRINT*,"! ========== !"
                                             
                       ! IF (P == 1) THEN
                       !    PRINT*,"BUFF_Y>",BUFF_Y(P),BUFF_Y(I)
                       !    PRINT*,"BUFF_X>",BUFF_X(P),BUFF_X(I)
                       !    PRINT*,"---->",UN_OLD,UN_NEW,DES_USR_VAR(AS_NEW_U,P)
                       !    PRINT*,"++++>",DES_USR_VAR(AS_OLD_U,P),DES_USR_VAR(AS_OLD_U,I),MP(1,1)
                       !    PRINT*,"TTTT>",VM_T(CC),W(2,2),DPT(CC)
                       !    PRINT*,"NNNN>",VM_N(CC),W(1,1),DPN(CC)
                       ! END IF
                       

                       IF (PARTICLE_STATE(P) == NORMAL_PARTICLE) THEN
                          !> SANS FROTTEMENT
                          DES_USR_VAR(AS_NEW_U,P) = DES_USR_VAR(AS_NEW_U,P) + DPN(CC)*MP(1,4)*PMEFF(1)
                          DES_USR_VAR(AS_NEW_V,P) = DES_USR_VAR(AS_NEW_V,P) + DPN(CC)*MP(1,5)*PMEFF(2)
                          DES_USR_VAR(AS_NEW_W,P) = DES_USR_VAR(AS_NEW_W,P) - DPN(CC)*DTSOLID/PMASS(P)*NZ_AS ! Pas 3D
                          !> AVEC FROTTEMENT
                          ! DES_USR_VAR(AS_NEW_U,P) = DES_USR_VAR(AS_NEW_U,P) + (DPN(CC)*MP(1,4)+DPT(CC)*MP(2,4))*PMEFF(1)
                          ! DES_USR_VAR(AS_NEW_V,P) = DES_USR_VAR(AS_NEW_V,P) + (DPN(CC)*MP(1,5)+DPT(CC)*MP(2,5))*PMEFF(2)
                          ! DES_USR_VAR(AS_NEW_W,P) = DES_USR_VAR(AS_NEW_W,P) - DPN(CC)*DTSOLID/PMASS(P)*NZ_AS ! Pas 3D
                          ! DES_USR_VAR(AS_NEW_DT,P) = DES_USR_VAR(AS_NEW_DT,P) + MP(2,6)*DPT(CC)*PMEFF(3)
                       END IF

                       
                       IF (PARTICLE_STATE(I) == NORMAL_PARTICLE) THEN
                          !> SANS FROTTEMENT
                          DES_USR_VAR(AS_NEW_U,I) = DES_USR_VAR(AS_NEW_U,I) + DPN(CC)*MP(1,1)*IMEFF(1)
                          DES_USR_VAR(AS_NEW_V,I) = DES_USR_VAR(AS_NEW_V,I) + DPN(CC)*MP(1,2)*IMEFF(2)
                          DES_USR_VAR(AS_NEW_W,I) = DES_USR_VAR(AS_NEW_W,I) + DPN(CC)*DTSOLID/PMASS(I)*NZ_AS ! Pas 3D
                          !> AVEC FROTTEMENT
                          ! DES_USR_VAR(AS_NEW_U,I) = DES_USR_VAR(AS_NEW_U,I) + (DPN(CC)*MP(1,1)+DPT(CC)*MP(2,1))*IMEFF(1)
                          ! DES_USR_VAR(AS_NEW_V,I) = DES_USR_VAR(AS_NEW_V,I) + (DPN(CC)*MP(1,2)+DPT(CC)*MP(2,2))*IMEFF(2)
                          ! DES_USR_VAR(AS_NEW_W,I) = DES_USR_VAR(AS_NEW_W,I) + DPN(CC)*DTSOLID/PMASS(I)*NZ_AS ! Pas 3D
                          ! DES_USR_VAR(AS_NEW_DT,I) = DES_USR_VAR(AS_NEW_DT,I) + MP(2,3)*DPT(CC)*IMEFF(3)
                       END IF

                                                                        
                       ! ================================================================================================================ !
                       !> SERGE: CRANCK-NICHOLSON
                       IF (PARTICLE_STATE(P) == NORMAL_PARTICLE) THEN
                          DES_USR_VAR(AS_NEW_X,P) = DES_USR_VAR(AS_OLD_X,P) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_U,P)+DES_USR_VAR(AS_OLD_U,P))
                          DES_USR_VAR(AS_NEW_Y,P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_V,P)+DES_USR_VAR(AS_OLD_V,P))
                          DES_USR_VAR(AS_NEW_Z,P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_W,P)+DES_USR_VAR(AS_OLD_W,P))
                          ! DES_USR_VAR(AS_NEW_T,P) = DES_USR_VAR(AS_OLD_T,P) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_DT,P)+DES_USR_VAR(AS_OLD_DT,P))
                       END IF
                       IF (PARTICLE_STATE(I) == NORMAL_PARTICLE) THEN
                          DES_USR_VAR(AS_NEW_X,I) = DES_USR_VAR(AS_OLD_X,I) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_U,I)+DES_USR_VAR(AS_OLD_U,I))
                          DES_USR_VAR(AS_NEW_Y,I) = DES_USR_VAR(AS_OLD_Y,I) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_V,I)+DES_USR_VAR(AS_OLD_V,I))
                          DES_USR_VAR(AS_NEW_Z,I) = DES_USR_VAR(AS_OLD_Z,I) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_W,I)+DES_USR_VAR(AS_OLD_W,I))
                          ! DES_USR_VAR(AS_NEW_T,I) = DES_USR_VAR(AS_OLD_T,I) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_DT,I)+DES_USR_VAR(AS_OLD_DT,I))
                       END IF
                       ! ================================================================================================================ !
                       
                       IF ((CC_END - CC_START) == 0) THEN
                          EXIT
                       END IF
                       
                    END IF

                 END IF
                 
              END DO
              
           END DO

           
           DO_NSEARCH = (NN == 1 .OR. MOD(NN,NEIGHBOR_SEARCH_N) == 0)
           IF (DO_NSEARCH .OR. (NUMPES>1) .OR. DES_PERIODIC_WALLS) THEN
              CALL DESGRID_PIC(.TRUE.)
              CALL DES_PAR_EXCHANGE
           ENDIF

           
           SS_MAX = MAXVAL(ARRAY_SS)
           SS_MIN = MINVAL(ARRAY_SS)
           
           VM_N_MAX = MAXVAL(ARRAY_VM_N)
           VM_N_MIN = MINVAL(ARRAY_VM_N)
           VM_T_MAX = MAXVAL(ARRAY_VM_T)
           VM_T_MIN = MINVAL(ARRAY_VM_T)

           DPN_MAX = MAXVAL(ABS(DPN))
           PN_MAX = MAXVAL(ABS(PN))
           DPT_MAX = MAXVAL(ABS(DPT))
           PT_MAX = MAXVAL(ABS(PT))
           
           CALL MPI_ALLREDUCE(MPI_IN_PLACE,DPN_MAX,1,MPI_DOUBLE_PRECISION,MPI_MAX,MPI_COMM_WORLD,IERR)
           CALL MPI_ALLREDUCE(MPI_IN_PLACE,PN_MAX,1,MPI_DOUBLE_PRECISION,MPI_MAX,MPI_COMM_WORLD,IERR)

           CALL MPI_ALLREDUCE(MPI_IN_PLACE,SS_MAX,1,MPI_DOUBLE_PRECISION,MPI_MAX,MPI_COMM_WORLD,IERR)
           CALL MPI_ALLREDUCE(MPI_IN_PLACE,SS_MIN,1,MPI_DOUBLE_PRECISION,MPI_MIN,MPI_COMM_WORLD,IERR)

           CALL MPI_ALLREDUCE(MPI_IN_PLACE,VM_N_MAX,1,MPI_DOUBLE_PRECISION,MPI_MAX,MPI_COMM_WORLD,IERR)
           CALL MPI_ALLREDUCE(MPI_IN_PLACE,VM_N_MIN,1,MPI_DOUBLE_PRECISION,MPI_MIN,MPI_COMM_WORLD,IERR)

           !> AVEC FROTTEMENT
           ! IF ((DPN_MAX+DPT_MAX) .LT. 1.D-8) EXIT
           !> SANS FROTTEMENT
           IF ((DPN_MAX) .LT. 1.D-8) EXIT

           
        END DO

        
        PRINT'(2(1X,I4),3(1X,E15.8))',NSTEP,NLGS,DPN_MAX,SS_MAX,VM_N_MAX

        !> 5 STACKED PARTICLES
        ! PC = 1
           
        ! DO P = 1, MAX_PIP
        !    IF (PC .GT. PIP) EXIT
        !    IF (IS_NONEXISTENT(P)) CYCLE
        !    PC = PC + 1
           
        !    CC_START = 1
        !    IF (P .GT. 1) CC_START = NEIGHBOR_INDEX(P-1)
        !    CC_END = NEIGHBOR_INDEX(P)
        !    NB_CONTACTS = NB_CONTACTS + CC_END - CC_START
                      
        !    !> CONTACT LOOP AT LEAST ONE GHOST PARTICLE
        !    DO CC = CC_START, CC_END-1
              
        !       I = NEIGHBORS(CC)
        !       IF(IS_NONEXISTENT(I)) CYCLE
        !       ! PRINT'(4(1X,I4),4(1X,E15.8))',NSTEP,NLGS,IGLOBAL_ID(P),IGLOBAL_ID(I),PN_ARRAY(CC),ARRAY_SS(CC)!,DES_USR_VAR(AS_NEW_Y,6),DES_USR_VAR(AS_NEW_Y,1)
        !       PRINT*,NLGS,IGLOBAL_ID(P),IGLOBAL_ID(I),PN_ARRAY(CC),ARRAY_SS(CC),PMASS(P),GRAV(2)!,S_USR_VAR(AS_NEW_Y,6),DES_USR_VAR(AS_NEW_Y,1)
        !    END DO
        ! END DO
        
! ================================================================================== !                
! ============================ END BOUCLE NLGS ===================================== !        
! ================================================================================== !
        
        
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)

                
        ! ============================================== !        
        ! =================== S T O P ================== !
        ! ============================================== !        
        
        IF (NN == 2000)THEN
           
           CALL MPI_FINALIZE(IERR)
           STOP
           
        END IF
        
        ! ============================================== !                
        ! =================== END STOP ================= !        
        ! ============================================== !        
                

        EC = 0.D00
        PC = 1
        
        DO P = 1, MAX_PIP
           IF(PC .GT. PIP) EXIT
           IF(IS_NONEXISTENT(P)) CYCLE
           PC = PC + 1
           IF(IS_GHOST(P) .OR. IS_ENTERING_GHOST(P) .OR. IS_EXITING_GHOST(P)) CYCLE
           EC = EC + (1.D0/2.D0) * DES_USR_VAR(AS_PMASS,P) * (DES_VEL_NEW(P,1)**2 + DES_VEL_NEW(P,2)**2)
        END DO

        IF (NSTEP == 0) THEN 
           EC_0 = EC
        END IF

        CALL MPI_ALLREDUCE(MPI_IN_PLACE,EC,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,IERR)

        !> KINETIC ENERGY
        WRITE(30,*)NSTEP*DTSOLID,EC
        
! ========================================================================== !        
! ====================== DESTRUCTION ======================================= !
! ========================================================================== !
        
        CPT_DEST = 0
        
        DO I = 1, PTR_PARTICLE_CL
           
           DO P = 1, PIP    

              IF (IS_NONEXISTENT(P)) CYCLE

              IF (IGLOBAL_ID(P) == PARTICLE_CL(I) ) THEN
                               
                 CPT_DEST = CPT_DEST + 1
                 
                 IGLOBAL_ID(P) = -1
                 CALL SET_NONEXISTENT(P)
                 DES_POS_NEW(P,:) = 0.0
                 DES_VEL_NEW(P,:) = 0.0
                 DES_POS_OLD(P,:) = 0.0
                 DES_VEL_OLD(P,:) = 0.0
                 DES_RADIUS(P) = 0.0
                 PMASS(P) = HUGE(0.0)

              END IF

           END DO

        END DO
        
        PIP = PIP - CPT_DEST
        PARTICLES = PARTICLES - CPT_DEST

        DEALLOCATE(PARTICLE_CL)
        
        DEALLOCATE(N_RANK_LOC)
        
        DEALLOCATE(N_RANK_GLOB)
        DEALLOCATE(N_RANK_GLOB_MOD)

        DEALLOCATE(ARRAY_VM_N)
        DEALLOCATE(ARRAY_SS)

        DEALLOCATE(PN_ARRAY)
        DEALLOCATE(DPN_ARRAY)
        
! ========================================================================== !        
! ========================================================================== !        
! ========================================================================== !                

        ! DO P = 1, PIP
        !    IF (P == 1) THEN
        !       PRINT*,NN,"4 --> OLD_U",P,DES_USR_VAR(AS_OLD_U,P)
        !       PRINT*,NN,"4 --> OLD_X",P,DES_USR_VAR(AS_OLD_X,P)
        !    END IF
        ! END DO
        
        !> REACTUALISATION DES VITESSES ET POSITIONS
        DO P = 1, MAX_PIP
           
           IF (PARTICLE_STATE(P) == 1) THEN
              DES_POS_NEW(P,1) = DES_USR_VAR(AS_NEW_X,P)
              DES_POS_NEW(P,2) = DES_USR_VAR(AS_NEW_Y,P)
              DES_POS_NEW(P,3) = DES_USR_VAR(AS_NEW_Z,P)
              ! OMEGA_OLD(P,1) = DES_USR_VAR(AS_NEW_T,P)
           END IF

           DES_VEL_NEW(P,1) = DES_USR_VAR(AS_NEW_U,P)
           DES_VEL_NEW(P,2) = DES_USR_VAR(AS_NEW_V,P)
           DES_VEL_NEW(P,3) = DES_USR_VAR(AS_NEW_W,P)
           ! OMEGA_NEW(P,1) = DES_USR_VAR(AS_NEW_DT,P)
           
           ! DES_POS_OLD(P,1) = DES_USR_VAR(AS_NEW_X,P)
           ! DES_POS_OLD(P,2) = DES_USR_VAR(AS_NEW_Y,P)
           ! DES_POS_OLD(P,3) = DES_USR_VAR(AS_NEW_Z,P)
           
           ! DES_VEL_OLD(P,1) = DES_USR_VAR(AS_NEW_U,P)
           ! DES_VEL_OLD(P,2) = DES_USR_VAR(AS_NEW_V,P)
           ! DES_VEL_OLD(P,3) = DES_USR_VAR(AS_NEW_W,P)

        END DO


        ! PRINT*,"++++>",DES_USR_VAR(AS_NEW_Y,1),DES_USR_VAR(AS_NEW_V,1)
        ! PRINT*,"++++>",DES_POS_NEW(1,2),DES_VEL_NEW(1,2)

        
        ! DO P = 1, PIP
        !    IF (P == 1) THEN
        !       PRINT*,NN,"5 --> OLD_U",P,DES_USR_VAR(AS_OLD_U,P)
        !       PRINT*,NN,"5 --> OLD_X",P,DES_USR_VAR(AS_OLD_X,P)
        !    END IF
        ! END DO

                        
        DO_NSEARCH = (NN == 1 .OR. MOD(NN,NEIGHBOR_SEARCH_N) == 0)
        IF (DO_NSEARCH .OR. (NUMPES>1) .OR. DES_PERIODIC_WALLS) THEN
           CALL DESGRID_PIC(.TRUE.)
           CALL DES_PAR_EXCHANGE
        ENDIF
        CALL NEIGHBOUR
        

        IF (MOD(NN,2) == 0) THEN
           CALL MPI_COMM_RANK(MPI_COMM_WORLD,RANK,IERR)
           ID = 1024 + RANK
           FIRST = .TRUE.
           PC = 1
           DO P = 1, MAX_PIP
              IF(PC .GT. PIP) EXIT
              IF(IS_NONEXISTENT(P)) CYCLE
              PC = PC +1
              IF ((FIRST).AND.COUNT(PARTICLE_STATE==1)>0) THEN
                 WRITE(ID,*)'VARIABLES= X1,X2,Y1,Y2,R,ID,RK'
                 WRITE(ID,'("ZONE T=""",I5 ,""" ")'),NN
                 WRITE(ID,'("SOLUTIONTIME=",F15.8,",STRANDID=",I4)'),S_TIME,RANK+1
                 FIRST = .FALSE.
              END IF
              IF (PARTICLE_STATE(P)==1) THEN
                 WRITE(ID,'(5(1X,E15.8),2(1X,I4))'),DES_POS_NEW(P,1:2),DES_VEL_NEW(P,1:2),DES_RADIUS(P),IGLOBAL_ID(P),RANK!PARTICLE_STATE(P)
              END IF
           END DO
        END IF


        !> 2 PARTICLES X-FRONTAL COLLISION
        ! WRITE(21,*)NSTEP*DTSOLID+DTSOLID,DES_USR_VAR(AS_NEW_X,1),DES_USR_VAR(AS_NEW_U,1)
        ! WRITE(22,*)NSTEP*DTSOLID+DTSOLID,DES_USR_VAR(AS_NEW_X,2),DES_USR_VAR(AS_NEW_U,2)


        !> FRONTAL COLLISION VALIDATION 
        ! DO P = 1, 2
        !    WRITE(41,'(4(1X,E15.8))')NSTEP*DTSOLID+DTSOLID,DES_VEL_NEW(P,1),VEL_ANALYTIC(P,1),ABS(VEL_ANALYTIC(P,1)-DES_VEL_NEW(P,1))
        !    WRITE(42,'(4(1X,E15.8))')NSTEP*DTSOLID+DTSOLID,DES_VEL_NEW(P,2),VEL_ANALYTIC(P,2),ABS(VEL_ANALYTIC(P,2)-DES_VEL_NEW(P,2))
        ! END DO

        
        !> FREELY FALLING PARTICLE
        ! WRITE(11,*)TC,DES_USR_VAR(AS_NEW_Y,1),DES_USR_VAR(AS_NEW_V,1)
        
        
        

        !> EXACT FREE FALLING PARTICLE SOLUTION
        ! EX = EXACT_FREEFALLINGPARTICLE(TC)
        ! WRITE(120,*)TC,DES_POS_NEW(1,2),EX(1)!,ABS(DES_POS_NEW(1,2)-EX(1))
        ! WRITE(130,*)TC,DES_VEL_NEW(1,2),EX(2)!,ABS(DES_VEL_NEW(1,2)-EX(2))
        ! WRITE(140,*)'VARIABLES= X1,X2,Y1,Y2,R'
        ! WRITE(140,'("ZONE T=""",I5 ,""" ")'),NN
        ! WRITE(140,'("SOLUTIONTIME=",F15.8,",STRANDID=",I4)'),S_TIME,RANK+1
        ! WRITE(140,*),DES_POS_NEW(1,1),EX(1),DES_VEL_NEW(1,1),EX(2),DES_RADIUS(1)


        !> PARTICULE GLISS
        ! ES = EXACT_SLIDING_PARTICLE(TC)
        ! PRINT*,TC,DES_POS_NEW(1,1),OMEGA_NEW(1,1),OMEGA_OLD(1,1)
        ! WRITE(50,*),TC,OMEGA_OLD(1,1),ES(2),ABS(OMEGA_OLD(1,1)-ES(2)) ! ANGLE TETA
        ! WRITE(51,*),TC,DES_POS_NEW(1,1),ES(1)!,ABS(DES_POS_NEW(1,1)-ES(1)) ! POSITION
        ! WRITE(10,*),TC,DES_VEL_NEW(1,1),ES(4)!,ABS(DES_VEL_NEW(1,1)-ES(4)) ! VITESSE GLISS
        ! WRITE(11,*),TC,OMEGA_NEW(1,1),ES(3)!,ABS(OMEGA_NEW(1,1)-ES(3)) ! VITESSE ANGULAIRE

        ! PRINT*,"! ====================================================================== !"
        
        !> TEMPS DE SIMULATION + PAS DE TEMPS                        
        S_TIME = S_TIME + DTSOLID
        TIME = S_TIME
        NSTEP = NSTEP + 1


        DLB = .TRUE.
        ! CALL OUTPUT_MANAGER(.FALSE.,.FALSE.)           

        IF(ADJUST_PARTITION) THEN
           EXIT_LOOP = .TRUE.
           RETURN
        ENDIF

        
        DEALLOCATE ( BUFF_X )
        DEALLOCATE ( BUFF_Y )
        DEALLOCATE ( BUFF_Z )

        
        ! CALL WRITE_DES_TECPLOT
        
        RETURN


      END SUBROUTINE DES_TIME_STEP
      
    
!VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV!
!                                                                      !
!     SUBROUTINE: DES_TIME_END                                         !
!     AUTHOR: SOUFIANE                                DATE: 21-JUN-04  !
!                                                                      !
!     PURPOSE: MAIN DEM DRIVER ROUTINE                                 !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

      SUBROUTINE DES_TIME_END
        
         USE DISCRETELEMENT

         IMPLICIT NONE

         CALL MPI_COMM_RANK(MPI_COMM_WORLD,RANK,IERR)
         ID = 1024 + RANK

         ! RESET THE DISCRETE TIME STEP TO ORIGINAL VALUE.
         DTSOLID = DTSOLID_TMP

         CLOSE(1024+RANK)

         CLOSE(1)

         CLOSE(30)
         
         CLOSE(21)

         CLOSE(41)
         CLOSE(42)
         
       END SUBROUTINE DES_TIME_END

! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !

       FUNCTION EXACT_FREEFALLINGPARTICLE(T)

         IMPLICIT NONE
         
         DOUBLE PRECISION :: EXACT_FREEFALLINGPARTICLE(2)
         DOUBLE PRECISION :: T,R,YC,H0,TN,G,E,Y,V,TT,T0,V0,HN

         INTEGER :: NB,N
         
         R = 2.D-2
         G = 9.80665D00
         YC = 5.D-1
         E = 1.D00
         H0 = YC - R
         T0 = -SQRT(2*G*H0)/G
         
         TT = T0
         
         DO N = 1, 2000
            TT = TT + SQRT(8.D00*H0/G)*E**(N-1)
            IF (TT>=T) EXIT
         END DO
         
         TT = TT - SQRT(8.D00*H0/G)*E**(N-1) 
         
         HN = H0*E**(2.D00*(N-1))
         V0 = SQRT(2.D00*G*HN)
         
         TT = T-TT
         Y = TT*(-0.5*G*TT+V0) + R
         V = -1.0*G*TT+V0
         
         EXACT_FREEFALLINGPARTICLE(1) = Y
         EXACT_FREEFALLINGPARTICLE(2) = V
         
       END FUNCTION EXACT_FREEFALLINGPARTICLE

! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !

       FUNCTION EXACT_SLIDING_PARTICLE(T)

         IMPLICIT NONE
         
         DOUBLE PRECISION :: EXACT_SLIDING_PARTICLE(5)
         DOUBLE PRECISION :: R, XC, YC, G, En, Et, MU, UC, TETA_0, TETA_P_0
         DOUBLE PRECISION :: T, X, TETA, TETA_P, V_GLISS, T_ROUL

         INTEGER :: NB, N
         
         R = 1.D-1
         G = 9.80665D00
         XC = R
         YC = R
         En = 1.D00
         Et = 1.D00
         MU = 3.D-1!*0.D00
         UC = 1.D00
         TETA_0 = 0
         TETA_P_0 = 0

         X = -(1.D0/2.D0)*MU*G*T**2 + UC*T + XC
         TETA_P = - (2*MU*G*T) / R
         TETA = - (MU*G*T**2) / R
         V_GLISS = - 3*MU*G*T + UC 
         T_ROUL = UC / (3*MU*G)
         
         EXACT_SLIDING_PARTICLE(1) = X
         EXACT_SLIDING_PARTICLE(2) = TETA
         EXACT_SLIDING_PARTICLE(3) = TETA_P
         EXACT_SLIDING_PARTICLE(4) = V_GLISS
         EXACT_SLIDING_PARTICLE(5) = T_ROUL
         
       END FUNCTION EXACT_SLIDING_PARTICLE

! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !       
       
     END MODULE DES_TIME_MARCH


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

  ! USE SENDRECV



  use physprop, only: D_p0, RO_s0, MMAX


  
!---------------------------------------------------------------------//
      ! TOTAL NUMBER OF PARTICLES
      INTEGER, SAVE :: NP = 0, P

      ! COUNTER
      INTEGER :: PC
      
      ! LOOP COUNTER INDEX FOR ANY INITIAL PARTICLE SETTLING INCOUPLED CASES
      INTEGER :: FACTOR
      
      ! CHANGES IN SOLID TIME STEP
      DOUBLE PRECISION :: DTSOLID_TMP

      LOGICAL :: EXIT_LOOP

      DOUBLE PRECISION ::  RADIUS, EN

      INTEGER :: NLGS, C, CC, CC_START, CC_END, I, NB_CONTACTS

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

      DOUBLE PRECISION :: UN_NEW
      DOUBLE PRECISION :: UN_OLD
      
      DOUBLE PRECISION :: TAU_N, GAMMA, MEQ

      DOUBLE PRECISION :: EC

      
      ! =============================================================== !
      ! USER VARIABLES
      
      INTEGER, PARAMETER :: AS_DEMI_X = 1, AS_DEMI_Y = 2, AS_DEMI_Z = 3
      
      INTEGER, PARAMETER :: AS_OLD_X = 4, AS_OLD_Y = 5, AS_OLD_Z = 6
      INTEGER, PARAMETER :: AS_OLD_U = 7, AS_OLD_V = 8, AS_OLD_W = 9
      
      INTEGER, PARAMETER :: AS_NEW_X = 10, AS_NEW_Y = 11, AS_NEW_Z = 12
      INTEGER, PARAMETER :: AS_NEW_U = 13, AS_NEW_V = 14, AS_NEW_W = 15

      INTEGER, PARAMETER :: AS_FREE_X = 16, AS_FREE_Y = 17, AS_FREE_Z = 18
      INTEGER, PARAMETER :: AS_FREE_U = 19, AS_FREE_V = 20, AS_FREE_W = 21

      INTEGER, PARAMETER :: AS_PMASS = 22, AS_RO_SOL = 23

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
        ! DTSOLID = 1.67983E-4
        ! DTSOLID = 1.42323E-3
        ! DTSOLID = 3.63427E-2
        ! DTSOLID = 8.75931E-2
        ! DTSOLID = 5.13892E-1
        ! DTSOLID = 1E-4
        ! DTSOLID = 2.5E-4
        DTSOLID = 5E-4
        ! DTSOLID = 1E-3
        
        FACTOR = CEILING(REAL((TSTOP-TIME)/DTSOLID))
        DT = DTSOLID

        !> COEFFICIENTS DE RESTITUTION
        ! EN = 1.0
        ! EN = 0.9
        ! EN = 0.8
        EN = 0.7
        ! EN = 0.6
        
        !> NOMBRE DE VARIABLES UTILISATEUR
        DES_USR_VAR_SIZE = 23
        

        DO P = 1, MAX_PIP
              DES_POS_OLD(P,1:3) = DES_POS_NEW(P,1:3)
              DES_VEL_OLD(P,1:3) = DES_VEL_NEW(P,1:3)
        END DO

        
        !> PARALLEL .DAT FILE
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD ,NB_PROCS,IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,RANK,IERR)

        ID = 1024 + RANK
        WRITE (CHAR,'(I3.3)')RANK
        OPEN(UNIT=ID,FILE='pos-'//TRIM(CHAR)//'.dat')

        
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

! ------------------------------------------------------------------------------------------------------------- !

      ! SUBROUTINE GET_SIGNED_DISTANCE_CIRCLE(XC,YC,RC,X,RX,N,DIST)
        
      !   IMPLICIT NONE

      !   REAL(KIND=8) :: XC, YC, RC, RX
      !   REAL(KIND=8) :: A(2), X(2), N(2), DIST
      !   REAL(KIND=8) :: T(2), D

      !   IF ( (X(1)-XC)**2 + (X(2)-YC)**2 - (RC+RX)**2 == 0) THEN

      !      !> NORMALE
      !      N(1) = XC - X(1)
      !      N(2) = YC - X(2)

      !      !> TANGENTE
      !      T(1) = -N(2)
      !      T(2) =  N(1)
           
      !      !> NORMALE UNITAIRE
      !      N = N/SQRT(N(1)**2+N(2)**2)

      !      !> COORDONNEES POINT INTERSECTION
      !      A(1) = X(1) + N(1)*RX
      !      A(2) = X(2) + N(2)*RX
           
      !      D = (X(1)-A(1))*T(2) - (X(2)-A(2))*T(1)

      !      DIST = ABS(D)
           
      !   END IF
                
      ! END SUBROUTINE GET_SIGNED_DISTANCE_CIRCLE
      
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
        DOUBLE PRECISION :: SS_MAX, VM_MAX

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


        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ARRAY_SS
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ARRAY_VM

        
        !> POS DEMI SELON SI P ET I SONT GHOST - GHOST, GHOST - NORMAL, NORMAL - NORMAL
        ALLOCATE ( BUFF_X( 1:MAX_PIP ) )
        ALLOCATE ( BUFF_Y( 1:MAX_PIP ) )
        ALLOCATE ( BUFF_Z( 1:MAX_PIP ) )


        MOD_ASSERTION = (NN == 1 .OR. MOD(NN,NEIGHBOR_SEARCH_N) == 0)         


        !> MFIX NEW VAR -> USR VAR (OLD) 
        DO P = 1, MAX_PIP
           DES_USR_VAR(AS_OLD_X,P) = DES_POS_NEW(P,1)
           DES_USR_VAR(AS_OLD_Y,P) = DES_POS_NEW(P,2)
           DES_USR_VAR(AS_OLD_Z,P) = DES_POS_NEW(P,3)
           
           DES_USR_VAR(AS_OLD_U,P) = DES_VEL_NEW(P,1)
           DES_USR_VAR(AS_OLD_V,P) = DES_VEL_NEW(P,2)
           DES_USR_VAR(AS_OLD_W,P) = DES_VEL_NEW(P,3)
        END DO


        PC = 1

        
        DO P = 1, MAX_PIP
           IF(PC.GT.PIP) EXIT
           IF(IS_NONEXISTENT(P)) CYCLE
           PC = PC + 1
           
           ! FC(P,1) = 0 ! IMPULSION DE CONTACT !!
           ! FC(P,2) = 0 ! ATTENTION 
           ! FC(P,3) = 0 !
           
           !> CHAMP DE FORCE RADIAL
           ! FC(P,1) =  (DES_USR_VAR(AS_OLD_X,P) - 0.5)
           
           
           ! DX_AS =  DES_USR_VAR(AS_OLD_X,P) - 0.5
           ! DY_AS =  DES_USR_VAR(AS_OLD_Y,P) - 0.5
           ! DZ_AS = (DES_USR_VAR(AS_OLD_Z,P) - 0.5)*0
           
           ! NX_AS = (DX_AS+1E-3) /( SQRT(DX_AS**2 + DY_AS**2 + DZ_AS**2)+1E-3)
           ! NY_AS = (DY_AS+1E-3) /( SQRT(DX_AS**2 + DY_AS**2 + DZ_AS**2)+1E-3)
           ! NZ_AS = DZ_AS / SQRT(DX_AS**2 + DY_AS**2 + DZ_AS**2)*0
          
           
           ! NX_AS = (DX_AS+1E-3) /( SQRT(DX_AS**2 + DY_AS**2 + DZ_AS**2)+1E-3)
           ! NY_AS = (DY_AS+1E-3) /( SQRT(DX_AS**2 + DY_AS**2 + DZ_AS**2)+1E-3)
           ! NZ_AS = DZ_AS / SQRT(DX_AS**2 + DY_AS**2 + DZ_AS**2)*0
           

           ! FC(P,1) = NX_AS*1E-2
           ! FC(P,2) = NY_AS*1E-2
           ! FC(P,3) = NZ_AS*0*0
                     
           
           !> VITESSE FREE: VITESSE LIBRE SUR EULER IMPLICITE
           DES_USR_VAR(AS_FREE_U,P) = DES_USR_VAR(AS_OLD_U,P) + DTSOLID*(FC(P,1)/PMASS(P) + GRAV(1)) 
           DES_USR_VAR(AS_FREE_V,P) = DES_USR_VAR(AS_OLD_V,P) + DTSOLID*(FC(P,2)/PMASS(P) + GRAV(2))
           DES_USR_VAR(AS_FREE_W,P) = DES_USR_VAR(AS_OLD_W,P) + DTSOLID*(FC(P,3)/PMASS(P) + GRAV(3))

           
           !> SERGE: POSITION FREE CENTRE DE GRAVITE
           DES_USR_VAR(AS_FREE_X,P) = DES_USR_VAR(AS_OLD_X,P) + DTSOLID*DES_USR_VAR(AS_FREE_U,P)
           DES_USR_VAR(AS_FREE_Y,P) = DES_USR_VAR(AS_OLD_Y,P) + DTSOLID*DES_USR_VAR(AS_FREE_V,P)
           DES_USR_VAR(AS_FREE_Z,P) = DES_USR_VAR(AS_OLD_Z,P) + DTSOLID*DES_USR_VAR(AS_FREE_W,P)

           
           !> POSITION DEMI CENTRE DE GRAVITE
           ! DES_USR_VAR(AS_DEMI_X,P) = DES_USR_VAR(AS_OLD_X,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_OLD_U,P)
           ! DES_USR_VAR(AS_DEMI_Y,P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_OLD_V,P)
           ! DES_USR_VAR(AS_DEMI_Z,P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_OLD_W,P)
           !> SERGE: POSITION DEMI
           DES_USR_VAR(AS_DEMI_X,P) = DES_USR_VAR(AS_OLD_X,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_FREE_U,P)
           DES_USR_VAR(AS_DEMI_Y,P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_FREE_V,P)
           DES_USR_VAR(AS_DEMI_Z,P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5D0*DTSOLID* DES_USR_VAR(AS_FREE_W,P)

           !> SERGE: POSITION NEW CENTRE DE GRAVITE
           DES_USR_VAR(AS_NEW_X,P) = DES_USR_VAR(AS_FREE_X,P)
           DES_USR_VAR(AS_NEW_Y,P) = DES_USR_VAR(AS_FREE_Y,P)
           DES_USR_VAR(AS_NEW_Z,P) = DES_USR_VAR(AS_FREE_Z,P)


           !> VITESSE NEW
           DES_USR_VAR(AS_NEW_U,P) = DES_USR_VAR(AS_FREE_U,P)
           DES_USR_VAR(AS_NEW_V,P) = DES_USR_VAR(AS_FREE_V,P)
           DES_USR_VAR(AS_NEW_W,P) = DES_USR_VAR(AS_FREE_W,P)
           
        END DO

        
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD ,NB_PROCS,IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD ,RANK,IERR)
        
        
        ALLOCATE(PARTICLE_CL(MAX_PIP))
        
        PTR_PARTICLE_CL = 0
        PTR_PARTICLE_CL_GLOB = 0


        ALLOCATE(N_RANK_LOC(0:NB_PROCS-1))
        
        ALLOCATE(N_RANK_GLOB(0:NB_PROCS-1))
        ALLOCATE(N_RANK_GLOB_MOD(0:NB_PROCS-1))

        
! ======================================================================================= !                
! ===================== INCREMENTATION POINTEUR  ======================================== !
! ======================================================================================= !                

        DO P = 1, PIP
           IF ( PARTICLE_STATE(P) == 1 ) THEN
              
              ! CALL GET_SIGNED_DISTANCE_CIRCLE(0.5D0,0.2D0,0.1D0,DES_POS_NEW(P,1:2),DES_RADIUS(P),N_CL_CIRCLE,DIST_CL_CIRCLE)
              ! IF ((DIST_CL_CIRCLE .LT. 2*DES_RADIUS(P))) THEN               
              !    PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              ! END IF
              
              CALL GET_SIGNED_DISTANCE([0.0D0,0.2D0],[1.0D0,0.2D0],DES_POS_NEW(P,1:2),N_CL_INF,DIST_CL_INF)
              IF ((DIST_CL_INF .LT. 2*DES_RADIUS(P))) THEN               
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              END IF

              CALL GET_SIGNED_DISTANCE([0.2D0,0.2D0],[0.2D0,0.8D0],DES_POS_NEW(P,1:2),N_CL_LFT,DIST_CL_LFT)
              IF ((DIST_CL_LFT .LT. 2*DES_RADIUS(P))) THEN               
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              END IF

              CALL GET_SIGNED_DISTANCE([0.8D0,0.2D0],[0.8D0,0.8D0],DES_POS_NEW(P,1:2),N_CL_RGHT,DIST_CL_RGHT)
              IF ((DIST_CL_RGHT .LT. 2*DES_RADIUS(P))) THEN               
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              END IF

              CALL GET_SIGNED_DISTANCE([0.2D0,0.8D0],[0.8D0,0.8D0],DES_POS_NEW(P,1:2),N_CL_SUP,DIST_CL_SUP)
              IF ((DIST_CL_SUP .LT. 2*DES_RADIUS(P))) THEN               
                 PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              END IF
              
           END IF
        END DO
        
! =================================================================================== !                
! ============================ FIN INCREMENTATION =================================== !
! =================================================================================== !                
        
        N_RANK_LOC = 0
                
        N_RANK_GLOB = 0
        N_RANK_GLOB_MOD = 0
        
        N_RANK_LOC(RANK) = PTR_PARTICLE_CL

                
        CALL MPI_ALLREDUCE(N_RANK_LOC,N_RANK_GLOB,NB_PROCS,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,IERR)
        CALL MPI_ALLREDUCE(PTR_PARTICLE_CL,PTR_PARTICLE_CL_GLOB,NB_PROCS,MPI_INTEGER,MPI_SUM,MPI_COMM_WORLD,IERR)
        
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)

                
        N_RANK_GLOB_MOD(0) = N_RANK_GLOB(0)
        
        DO RANK_ITER = 1, NB_PROCS - 1
           N_RANK_GLOB_MOD(RANK_ITER) = N_RANK_GLOB(RANK_ITER) + N_RANK_GLOB_MOD(RANK_ITER-1)  
        END DO
        
        N_RANK_GLOB_MOD(RANK) = N_RANK_GLOB_MOD(RANK) - N_RANK_GLOB(RANK) + 1 + IMAX_GLOBAL_ID
        

        PARTICLE_CL = 0
        
        PTR_PARTICLE_CL = 0
        
        
        
        
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
              

              ! ------------------ !
              ! CL 1: DEMI CERCLE  !
              ! ------------------ !
              
! ------------------------------------------------------------------------------------------------------------- !
              
              !> CL DEMI CERCLE INFERIEUR
              ! CALL GET_SIGNED_DISTANCE_CIRCLE(0.5D0,0.2D0,0.1D0,DES_POS_NEW(P,1:2),DES_RADIUS(P),N_CL_CIRCLE,DIST_CL_CIRCLE)
              ! IF ((DIST_CL_CIRCLE.LT. 2*DES_RADIUS(P)) .AND. (FLAG .EQV. .TRUE.)) THEN               
                 
              !    PARTICLES = PARTICLES + 1

                 
              !    DES_RADIUS(PIP+1) = 1E-4
              !    PMASS(PIP+1) = 1E12
              !    RO_SOL(PIP+1) = PMASS(PIP+1) / ((4.D0/3.D0) * ACOS(-1.0) * DES_RADIUS(PIP+1))

              !    DES_POS_NEW(PIP+1,1) = DES_POS_NEW(P,1) - N_CL_CIRCLE(1)*(DIST_CL_CIRCLE+DES_RADIUS(PIP+1))
              !    DES_POS_NEW(PIP+1,2) = DES_POS_NEW(P,2) - N_CL_CIRCLE(2)*(DIST_CL_CIRCLE+DES_RADIUS(PIP+1))
              !    DES_POS_NEW(PIP+1,3) = DES_POS_NEW(P,3)

              !    DES_VEL_NEW(PIP+1,1) = 0.0
              !    DES_VEL_NEW(PIP+1,2) = 0.0
              !    DES_VEL_NEW(PIP+1,3) = 0.0

              !    DES_USR_VAR(AS_NEW_X:AS_NEW_Z,PIP+1) =  DES_POS_NEW(PIP+1,1:3) 
              !    DES_USR_VAR(AS_OLD_X:AS_OLD_Z,PIP+1) =  DES_POS_NEW(PIP+1,1:3) 
              !    DES_USR_VAR(AS_NEW_U:AS_NEW_W,PIP+1) =  DES_VEL_NEW(PIP+1,1:3)
              !    DES_USR_VAR(AS_OLD_U:AS_OLD_W,PIP+1) =  DES_VEL_NEW(PIP+1,1:3)
                 
              !    CALL PIC_SEARCH(J, DES_POS_NEW(PIP+1,2), YN, DIMENSION_J, JMIN2, JMAX2)
              !    CALL PIC_SEARCH(I, DES_POS_NEW(PIP+1,1), XE, DIMENSION_I, IMIN2, IMAX2)
                 
              !    K = 1
              !    IJK = FUNIJK(I,J,K)
                 
              !    PIP = PIP + 1
              !    CALL PARTICLE_GROW(PIP)
              !    MAX_PIP = MAX(PIP,MAX_PIP)
                 
              !    CALL SET_NORMAL(PIP)
                 
              !    PIJK(PIP,1) = I
              !    PIJK(PIP,2) = J
              !    PIJK(PIP,3) = K
              !    PIJK(PIP,4) = IJK  
              !    PIJK(PIP,5) = 1    ! N° DE PHASE
                 
              !    PARTICLE_STATE(PIP) = NORMAL_PARTICLE

              !    IGLOBAL_ID(PIP) = N_RANK_GLOB_MOD(RANK)
              !    N_RANK_GLOB_MOD(RANK) =  N_RANK_GLOB_MOD(RANK) + 1
                 
              !    PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              !    PARTICLE_CL(PTR_PARTICLE_CL) = IGLOBAL_ID(PIP)

              ! END IF

! ------------------------------------------------------------------------------------------------------------- !              
              
              ! ------------ !
              ! CL 2: CARRE  !
              ! ------------ !
              
! ------------------------------------------------------------------------------------------------------------- !

              !> CL CARRE (ARETE INFERIEURE)
              CALL GET_SIGNED_DISTANCE([0.0D0,0.2D0],[1.0D0,0.2D0],DES_POS_NEW(P,1:2),N_CL_INF,DIST_CL_INF)
              
              IF ((DIST_CL_INF .LT. 2*DES_RADIUS(P)) .AND. (FLAG .EQV. .TRUE.)) THEN               
                 
                 PARTICLES = PARTICLES + 1

                 
                 DES_RADIUS(PIP+1) = 1E-4
                 PMASS(PIP+1) = 1E12
                 RO_SOL(PIP+1) = PMASS(PIP+1) / ((4.D0/3.D0) * ACOS(-1.0) * DES_RADIUS(PIP+1))
                 
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
              CALL GET_SIGNED_DISTANCE([0.2D0,0.2D0],[0.2D0,0.8D0],DES_POS_NEW(P,1:2),N_CL_LFT,DIST_CL_LFT)

              IF ((DIST_CL_LFT .LT. 2*DES_RADIUS(P)) .AND. (FLAG .EQV. .TRUE.)) THEN               
                 
                 PARTICLES = PARTICLES + 1

                 DES_RADIUS(PIP+1) = 1E-4
                 PMASS(PIP+1) = 1E12
                 RO_SOL(PIP+1) = PMASS(PIP+1) / ((4.D0/3.D0) * ACOS(-1.0) * DES_RADIUS(PIP+1))

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
              CALL GET_SIGNED_DISTANCE([0.8D0,0.2D0],[0.8D0,0.8D0],DES_POS_NEW(P,1:2),N_CL_RGHT,DIST_CL_RGHT)

              IF ((DIST_CL_RGHT .LT. 2*DES_RADIUS(P)) .AND. (FLAG .EQV. .TRUE.)) THEN               
                 
                 PARTICLES = PARTICLES + 1

                 DES_RADIUS(PIP+1) = 1E-4
                 PMASS(PIP+1) = 1E12
                 RO_SOL(PIP+1) = PMASS(PIP+1) / ((4.D0/3.D0) * ACOS(-1.0) * DES_RADIUS(PIP+1))

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
              CALL GET_SIGNED_DISTANCE([0.2D0,0.8D0],[0.8D0,0.8D0],DES_POS_NEW(P,1:2),N_CL_SUP,DIST_CL_SUP)

              IF ((DIST_CL_SUP .LT. 2*DES_RADIUS(P)) .AND. (FLAG .EQV. .TRUE.)) THEN               
                 
                 PARTICLES = PARTICLES + 1

                 DES_RADIUS(PIP+1) = 1E-4
                 PMASS(PIP+1) = 1E12
                 RO_SOL(PIP+1) = PMASS(PIP+1) / ((4.D0/3.D0) * ACOS(-1.0) * DES_RADIUS(PIP+1))

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

              !> CL bocal (arete droite inclinee)
              ! call get_signed_distance([0.5d0,0.0d0],[1.0d0,0.5d0],DES_pos_new(p,1:2),n_cl_rght,dist_cl_rght)
              
              ! if ((dist_cl_rght.lt.2*DES_RADIUS(p)).and.(flag.eqv..true.)) then               
                 
              !    PARTICLES = PARTICLES + 1
                 
              !    DES_RADIUS(pip+1) = 1e-4
              !    pmass(pip+1) = 1e12
              !    RO_SOL(PIP+1) = PMASS(PIP+1) / ((4.D0/3.D0) * ACOS(-1.0) * DES_RADIUS(PIP+1))
              
              !    DES_POS_NEW(PIP+1,1) = DES_POS_NEW(P,1) + N_CL_RGHT(1)*(DIST_CL_RGHT+DES_RADIUS(PIP+1))
              !    DES_POS_NEW(PIP+1,2) = DES_POS_NEW(P,2) + N_CL_RGHT(2)*(DIST_CL_RGHT+DES_RADIUS(PIP+1)) 
              !    DES_POS_NEW(PIP+1,3) = DES_POS_NEW(P,3)

              !    DES_VEL_NEW(Pip+1,1) = 0.0
              !    DES_VEL_NEW(Pip+1,2) = 0.0
              !    DES_VEL_NEW(Pip+1,3) = 0.0
              !    DES_USR_VAR(AS_NEW_X:AS_NEW_Z,Pip+1) =  DES_POS_NEW(Pip+1,1:3) ! car on actualise les des_pos_new en fin de subroutine 
              !    DES_USR_VAR(AS_OLD_X:AS_OLD_Z,Pip+1) =  DES_POS_NEW(Pip+1,1:3) !
              !    DES_USR_VAR(AS_NEW_U:AS_NEW_W,Pip+1) =  DES_VEL_NEW(Pip+1,1:3)
              !    DES_USR_VAR(AS_OLD_U:AS_OLD_W,Pip+1) =  DES_VEL_NEW(Pip+1,1:3)
                 
              !    CALL PIC_SEARCH(J, DES_POS_NEW(Pip+1,2), YN, DIMENSION_J, JMIN2, JMAX2)
              !    CALL PIC_SEARCH(I, DES_POS_NEW(Pip+1,1), XE, DIMENSION_I, IMIN2, IMAX2)
                 
              !    K = 1
              !    IJK = FUNIJK(I,J,K)
                 
              !    PIP = PIP + 1
              !    CALL PARTICLE_GROW(PIP)
              !    MAX_PIP = max(PIP,MAX_PIP)
                 
              !    CALL SET_NORMAL(PIP)
                 
              !    PIJK(PIP,1) = I
              !    PIJK(PIP,2) = J
              !    PIJK(PIP,3) = K
              !    PIJK(PIP,4) = IJK  ! 
              !    PIJK(PIP,5) = 1    ! n° de phase
                 
              !    PARTICLE_STATE(Pip) = NORMAL_PARTICLE

              !    IGLOBAL_ID(PIP) = N_RANK_GLOB_MOD(RANK)
              !    N_RANK_GLOB_MOD(RANK) =  N_RANK_GLOB_MOD(RANK) + 1
                 
              !    PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              !    PARTICLE_CL(PTR_PARTICLE_CL) = IGLOBAL_ID(PIP)
                               
              ! END IF

! ------------------------------------------------------------------------------------------------------------- !              

              !> CL bocal (arete droite verticale)
              ! call get_signed_distance([0.95d0,0.0d0],[0.95d0,1.0d0],DES_pos_new(p,1:2),n_cl_RGHT_vert,dist_cl_RGHT_vert)
              ! if ((dist_cl_RGHT_vert.lt.2*DES_RADIUS(p)).and.(flag.eqv..true.)) then               
                 
              !    PARTICLES = PARTICLES + 1
                 
              !    DES_RADIUS(pip+1) = 1e-4
              !    pmass(pip+1) = 1e12
              !    RO_SOL(PIP+1) = PMASS(PIP+1) / ((4.D0/3.D0) * ACOS(-1.0) * DES_RADIUS(PIP+1))
              
              !    DES_POS_NEW(PIP+1,1) = DES_POS_NEW(P,1) - N_CL_RGHT_vert(1)*(DIST_CL_RGHT_vert+DES_RADIUS(PIP+1))
              !    DES_POS_NEW(PIP+1,2) = DES_POS_NEW(P,2) - N_CL_RGHT_vert(2)*(DIST_CL_RGHT_vert+DES_RADIUS(PIP+1)) 
              !    DES_POS_NEW(PIP+1,3) = DES_POS_NEW(P,3)
              
              !    DES_VEL_NEW(Pip+1,1) = 0.0
              !    DES_VEL_NEW(Pip+1,2) = 0.0
              !    DES_VEL_NEW(Pip+1,3) = 0.0
              !    DES_USR_VAR(AS_NEW_X:AS_NEW_Z,Pip+1) =  DES_POS_NEW(Pip+1,1:3) ! car on actualise les des_pos_new en fin de subroutine 
              !    DES_USR_VAR(AS_OLD_X:AS_OLD_Z,Pip+1) =  DES_POS_NEW(Pip+1,1:3) !
              !    DES_USR_VAR(AS_NEW_U:AS_NEW_W,Pip+1) =  DES_VEL_NEW(Pip+1,1:3)
              !    DES_USR_VAR(AS_OLD_U:AS_OLD_W,Pip+1) =  DES_VEL_NEW(Pip+1,1:3)
                 
              !    CALL PIC_SEARCH(J, DES_POS_NEW(Pip+1,2), YN, DIMENSION_J, JMIN2, JMAX2)
              !    CALL PIC_SEARCH(I, DES_POS_NEW(Pip+1,1), XE, DIMENSION_I, IMIN2, IMAX2)
                 
              !    K = 1
              !    IJK = FUNIJK(I,J,K)
                 
              !    PIP = PIP + 1
              !    CALL PARTICLE_GROW(PIP)
              !    MAX_PIP = max(PIP,MAX_PIP)
                 
              !    CALL SET_NORMAL(PIP)
                 
              !    PIJK(PIP,1) = I
              !    PIJK(PIP,2) = J
              !    PIJK(PIP,3) = K
              !    PIJK(PIP,4) = IJK  ! 
              !    PIJK(PIP,5) = 1    ! n° de phase
                 
              !    PARTICLE_STATE(Pip) = NORMAL_PARTICLE
                 
              !    IGLOBAL_ID(PIP) = N_RANK_GLOB_MOD(RANK)
              !    N_RANK_GLOB_MOD(RANK) =  N_RANK_GLOB_MOD(RANK) + 1
                 
              !    PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              !    PARTICLE_CL(PTR_PARTICLE_CL) = IGLOBAL_ID(PIP)   
                               
              ! END IF

! ------------------------------------------------------------------------------------------------------------- !              

              !> CL bocal (arete gauche inclinee)
              ! call get_signed_distance([0.0d0,0.5d0],[0.5d0,0.0d0],DES_pos_new(p,1:2),n_cl_LFT,dist_cl_LFT)
              ! if ((dist_cl_LFT.lt.2*DES_RADIUS(p)).and.(flag.eqv..true.)) then               
                 
              !    PARTICLES = PARTICLES + 1
                 
              !    DES_RADIUS(pip+1) = 1e-4
              !    pmass(pip+1) = 1e12
              !    RO_SOL(PIP+1) = PMASS(PIP+1) / ((4.D0/3.D0) * ACOS(-1.0) * DES_RADIUS(PIP+1))
              
              !    DES_POS_NEW(PIP+1,1) = DES_POS_NEW(P,1) - N_CL_LFT(1)*(DIST_CL_LFT+DES_RADIUS(PIP+1))
              !    DES_POS_NEW(PIP+1,2) = DES_POS_NEW(P,2) - N_CL_LFT(2)*(DIST_CL_LFT+DES_RADIUS(PIP+1)) 
              !    DES_POS_NEW(PIP+1,3) = DES_POS_NEW(P,3)
              
              !    DES_VEL_NEW(Pip+1,1) = 0.0
              !    DES_VEL_NEW(Pip+1,2) = 0.0
              !    DES_VEL_NEW(Pip+1,3) = 0.0
              !    DES_USR_VAR(AS_NEW_X:AS_NEW_Z,Pip+1) =  DES_POS_NEW(Pip+1,1:3) ! car on actualise les des_pos_new en fin de subroutine 
              !    DES_USR_VAR(AS_OLD_X:AS_OLD_Z,Pip+1) =  DES_POS_NEW(Pip+1,1:3) !
              !    DES_USR_VAR(AS_NEW_U:AS_NEW_W,Pip+1) =  DES_VEL_NEW(Pip+1,1:3)
              !    DES_USR_VAR(AS_OLD_U:AS_OLD_W,Pip+1) =  DES_VEL_NEW(Pip+1,1:3)
                 
              !    CALL PIC_SEARCH(J, DES_POS_NEW(Pip+1,2), YN, DIMENSION_J, JMIN2, JMAX2)
              !    CALL PIC_SEARCH(I, DES_POS_NEW(Pip+1,1), XE, DIMENSION_I, IMIN2, IMAX2)
                 
              !    K = 1
              !    IJK = FUNIJK(I,J,K)
                 
              !    PIP = PIP + 1
              !    CALL PARTICLE_GROW(PIP)
              !    MAX_PIP = max(PIP,MAX_PIP)
                 
              !    CALL SET_NORMAL(PIP)
                 
              !    PIJK(PIP,1) = I
              !    PIJK(PIP,2) = J
              !    PIJK(PIP,3) = K
              !    PIJK(PIP,4) = IJK  ! 
              !    PIJK(PIP,5) = 1    ! n° de phase
                 
              !    PARTICLE_STATE(Pip) = NORMAL_PARTICLE
                 
              !    IGLOBAL_ID(PIP) = N_RANK_GLOB_MOD(RANK)
              !    N_RANK_GLOB_MOD(RANK) =  N_RANK_GLOB_MOD(RANK) + 1
                 
              !    PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              !    PARTICLE_CL(PTR_PARTICLE_CL) = IGLOBAL_ID(PIP)   
                               
              ! END IF

! ------------------------------------------------------------------------------------------------------------- !              

              !> CL bocal (arete gauche verticale)
              ! call get_signed_distance([0.05d0,0.0d0],[0.05d0,1.0d0],DES_pos_new(p,1:2),n_cl_LFT_vert,dist_cl_LFT_vert)
              ! if ((dist_cl_LFT_vert.lt.2*DES_RADIUS(p)).and.(flag.eqv..true.)) then               
                 
              !    PARTICLES = PARTICLES + 1
                 
              !    DES_RADIUS(pip+1) = 1e-4
              !    pmass(pip+1) = 1e12
              !    RO_SOL(PIP+1) = PMASS(PIP+1) / ((4.D0/3.D0) * ACOS(-1.0) * DES_RADIUS(PIP+1))

              !    DES_POS_NEW(PIP+1,1) = DES_POS_NEW(P,1) + N_CL_LFT_vert(1)*(DIST_CL_LFT_vert+DES_RADIUS(PIP+1))
              !    DES_POS_NEW(PIP+1,2) = DES_POS_NEW(P,2) + N_CL_LFT_vert(2)*(DIST_CL_LFT_vert+DES_RADIUS(PIP+1)) 
              !    DES_POS_NEW(PIP+1,3) = DES_POS_NEW(P,3)
              
              !    DES_VEL_NEW(Pip+1,1) = 0.0
              !    DES_VEL_NEW(Pip+1,2) = 0.0
              !    DES_VEL_NEW(Pip+1,3) = 0.0
              !    DES_USR_VAR(AS_NEW_X:AS_NEW_Z,Pip+1) =  DES_POS_NEW(Pip+1,1:3) ! car on actualise les des_pos_new en fin de subroutine 
              !    DES_USR_VAR(AS_OLD_X:AS_OLD_Z,Pip+1) =  DES_POS_NEW(Pip+1,1:3) !
              !    DES_USR_VAR(AS_NEW_U:AS_NEW_W,Pip+1) =  DES_VEL_NEW(Pip+1,1:3)
              !    DES_USR_VAR(AS_OLD_U:AS_OLD_W,Pip+1) =  DES_VEL_NEW(Pip+1,1:3)
                 
              !    CALL PIC_SEARCH(J, DES_POS_NEW(Pip+1,2), YN, DIMENSION_J, JMIN2, JMAX2)
              !    CALL PIC_SEARCH(I, DES_POS_NEW(Pip+1,1), XE, DIMENSION_I, IMIN2, IMAX2)
                 
              !    K = 1
              !    IJK = FUNIJK(I,J,K)
                 
              !    PIP = PIP + 1
              !    CALL PARTICLE_GROW(PIP)
              !    MAX_PIP = max(PIP,MAX_PIP)
                 
              !    CALL SET_NORMAL(PIP)
                 
              !    PIJK(PIP,1) = I
              !    PIJK(PIP,2) = J
              !    PIJK(PIP,3) = K
              !    PIJK(PIP,4) = IJK  ! 
              !    PIJK(PIP,5) = 1    ! n° de phase
                 
              !    PARTICLE_STATE(Pip) = NORMAL_PARTICLE
                 
              !    IGLOBAL_ID(PIP) = N_RANK_GLOB_MOD(RANK)
              !    N_RANK_GLOB_MOD(RANK) =  N_RANK_GLOB_MOD(RANK) + 1
                 
              !    PTR_PARTICLE_CL = PTR_PARTICLE_CL + 1
              !    PARTICLE_CL(PTR_PARTICLE_CL) = IGLOBAL_ID(PIP)   
                               
              ! END IF
              
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

        PN = 0

        ALLOCATE (ARRAY_VM (5*MAX_PIP) )
        ALLOCATE (ARRAY_SS (5*MAX_PIP) )

! ============================================================================== !        
! ============================ BOUCLE NLGS ===================================== !        
! ============================================================================== !
        
        DO NLGS = 1, 5000
           
           SS_MAX = 0.D00
           NB_CONTACTS = 0.D00
           
           ACTIVE = .FALSE.
           
           VM = 0.D00
           DPN = 0.D00

           ARRAY_SS = 0.D00 
           ARRAY_VM = 0.D00
                      
           DO_NSEARCH = (NN == 1 .OR. MOD(NN,NEIGHBOR_SEARCH_N) == 0)
           IF (DO_NSEARCH .OR. (NUMPES>1) .OR. DES_PERIODIC_WALLS) THEN
              CALL DESGRID_PIC(.TRUE.) 
              CALL DES_PAR_EXCHANGE
           ENDIF

           
! ------------------------------------------------------------------------------------------------------------- !
        
           !> ITERATION SUR LES GHOST
           PC = 1
           
           DO P = 1, MAX_PIP
              IF (PC .GT. PIP) EXIT
              IF (IS_NONEXISTENT(P)) CYCLE
              PC = PC + 1
              
              CC_START = 1
              IF (P .GT. 1) CC_START = NEIGHBOR_INDEX(P-1)
              CC_END = NEIGHBOR_INDEX(P)
              NB_CONTACTS = NB_CONTACTS + CC_END - CC_START


              
              !> CONTACT LOOP AT LEAST ONE GHOST PARTICLE
              DO CC = CC_START, CC_END-1

                 I = NEIGHBORS(CC)
                 IF(IS_NONEXISTENT(I)) CYCLE

                 IF ((PARTICLE_STATE(P) * PARTICLE_STATE(I) == 4) .OR. (PARTICLE_STATE(P) * PARTICLE_STATE(I) == 4)) THEN 
                    
                    IF (IGLOBAL_ID(P) .NE. IGLOBAL_ID(I)) THEN

                       
                       ! BUFF_X(P) = DES_POS_NEW(P,1) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_U,P)
                       BUFF_X(P) = DES_USR_VAR(AS_OLD_X,P) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_U,P)
                       ! BUFF_X(P) = DES_USR_VAR(AS_DEMI_X,P)
                       
                       ! BUFF_X(I) = DES_POS_NEW(I,1) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_U,I)
                       BUFF_X(I) = DES_USR_VAR(AS_OLD_X,I) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_U,I) 
                       ! BUFF_X(I) = DES_USR_VAR(AS_DEMI_X,I)

                       ! BUFF_Y(P) = DES_POS_NEW(P,2) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_V,P)
                       BUFF_Y(P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_V,P)
                       ! BUFF_Y(P) = DES_USR_VAR(AS_DEMI_Y,P)

                       ! BUFF_Y(I) = DES_POS_NEW(I,2) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_V,I)
                       BUFF_Y(I) = DES_USR_VAR(AS_OLD_Y,I) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_V,I)
                       ! BUFF_Y(I) = DES_USR_VAR(AS_DEMI_Y,I)

                       ! BUFF_Z(P) = DES_POS_NEW(P,3) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_W,P)
                       BUFF_Z(P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_W,P)
                       ! BUFF_Z(P) = DES_USR_VAR(AS_DEMI_Z,P)

                       ! BUFF_Z(I) = DES_POS_NEW(I,3) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_W,I)
                       BUFF_Z(I) = DES_USR_VAR(AS_OLD_Z,I) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_W,I)
                       ! BUFF_Z(I) = DES_USR_VAR(AS_DEMI_Z,I)
                       
                       
                       IF (IGLOBAL_ID(P) > IGLOBAL_ID(I)) THEN 
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

                       
                       ! SS = SQRT(DX_AS**2+DY_AS**2+DZ_AS**2)-(DES_RADIUS(P)+DES_RADIUS(I))
                       ! ================================================================================================================ !
                       !> SERGE: SS
                       SS = (DES_USR_VAR(AS_NEW_X,P) - DES_USR_VAR(AS_NEW_X,I))**2 + &
                            (DES_USR_VAR(AS_NEW_Y,P) - DES_USR_VAR(AS_NEW_Y,I))**2 + &
                            (DES_USR_VAR(AS_NEW_Z,P) - DES_USR_VAR(AS_NEW_Z,I))**2
                       SS = DSQRT(SS) - (DES_RADIUS(P) + DES_RADIUS(I))
                       ! ================================================================================================================ !

                       ARRAY_SS(CC) = SS
                       
                       UN_OLD = &
                            (DES_USR_VAR(AS_OLD_U,I)-DES_USR_VAR(AS_OLD_U,P))*NX_AS + &
                            (DES_USR_VAR(AS_OLD_V,I)-DES_USR_VAR(AS_OLD_V,P))*NY_AS + &
                            (DES_USR_VAR(AS_OLD_W,I)-DES_USR_VAR(AS_OLD_W,P))*NZ_AS*0

                       UN_NEW = &
                            (DES_USR_VAR(AS_NEW_U,I)-DES_USR_VAR(AS_NEW_U,P))*NX_AS + &
                            (DES_USR_VAR(AS_NEW_V,I)-DES_USR_VAR(AS_NEW_V,P))*NY_AS + &
                            (DES_USR_VAR(AS_NEW_W,I)-DES_USR_VAR(AS_NEW_W,P))*NZ_AS*0
                       

                       VM(CC) = (UN_NEW + UN_OLD * EN) / (1.D0 + EN)

                       ARRAY_VM(CC) = VM(CC)
                       
                       ! MEQ = 4.356341934204102E-002 * 1d-2
                       ! PMASS(P) = 2.D00*MEQ
                       ! PMASS(I) = 2.D00*MEQ
                       MEQ = DES_USR_VAR(AS_PMASS,P) * DES_USR_VAR(AS_PMASS,I) / ( DES_USR_VAR(AS_PMASS,P) + DES_USR_VAR(AS_PMASS,I) )

                       
                       GAMMA = 1.D3                                                              
                       TAU_N = PN(CC) - GAMMA * SS

                       IF (TAU_N > 0) THEN
                          DPN(CC) = - VM(CC) * MEQ / DTSOLID * (1.D0 + EN)
                          ACTIVE(CC) = .TRUE.
                       ELSE
                          DPN(CC) = 0.D00
                       END IF
                       
                       IF (PARTICLE_STATE(P) == NORMAL_PARTICLE) THEN
                          DES_USR_VAR(AS_NEW_U,P) = DES_USR_VAR(AS_NEW_U,P) - DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,P)*NX_AS!/(1+EN)
                          DES_USR_VAR(AS_NEW_V,P) = DES_USR_VAR(AS_NEW_V,P) - DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,P)*NY_AS!/(1+EN)
                          DES_USR_VAR(AS_NEW_W,P) = DES_USR_VAR(AS_NEW_W,P) - DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,P)*NZ_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_U,P) = DES_USR_VAR(AS_NEW_U,P) - DPN(CC)*DTSOLID/PMASS(P)*NX_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_V,P) = DES_USR_VAR(AS_NEW_V,P) - DPN(CC)*DTSOLID/PMASS(P)*NY_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_W,P) = DES_USR_VAR(AS_NEW_W,P) - DPN(CC)*DTSOLID/PMASS(P)*NZ_AS!/(1+EN)
                       END IF
                       
                       IF (PARTICLE_STATE(I) == NORMAL_PARTICLE) THEN
                          DES_USR_VAR(AS_NEW_U,I) = DES_USR_VAR(AS_NEW_U,I) + DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,I)*NX_AS!/(1+EN)
                          DES_USR_VAR(AS_NEW_V,I) = DES_USR_VAR(AS_NEW_V,I) + DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,I)*NY_AS!/(1+EN)
                          DES_USR_VAR(AS_NEW_W,I) = DES_USR_VAR(AS_NEW_W,I) + DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,I)*NZ_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_U,I) = DES_USR_VAR(AS_NEW_U,I) + DPN(CC)*DTSOLID/PMASS(I)*NX_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_V,I) = DES_USR_VAR(AS_NEW_V,I) + DPN(CC)*DTSOLID/PMASS(I)*NY_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_W,I) = DES_USR_VAR(AS_NEW_W,I) + DPN(CC)*DTSOLID/PMASS(I)*NZ_AS!/(1+EN)
                       END IF
                          
                       PN(CC) = PN(CC) + DPN(CC)

                       ! ================================================================================================================ !
                       !> SERGE: CRANCK-NICHOLSON
                       IF (PARTICLE_STATE(P) == NORMAL_PARTICLE) THEN
                          DES_USR_VAR(AS_NEW_X,P) = DES_USR_VAR(AS_OLD_X,P) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_U,P)+DES_USR_VAR(AS_OLD_U,P))
                          DES_USR_VAR(AS_NEW_Y,P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_V,P)+DES_USR_VAR(AS_OLD_V,P))
                          DES_USR_VAR(AS_NEW_Z,P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_W,P)+DES_USR_VAR(AS_OLD_W,P))
                       END IF
                       IF (PARTICLE_STATE(I) == NORMAL_PARTICLE) THEN
                          DES_USR_VAR(AS_NEW_X,I) = DES_USR_VAR(AS_OLD_X,I) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_U,I)+DES_USR_VAR(AS_OLD_U,I))
                          DES_USR_VAR(AS_NEW_Y,I) = DES_USR_VAR(AS_OLD_Y,I) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_V,I)+DES_USR_VAR(AS_OLD_V,I))
                          DES_USR_VAR(AS_NEW_Z,I) = DES_USR_VAR(AS_OLD_Z,I) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_W,I)+DES_USR_VAR(AS_OLD_W,I))
                       END IF
                       ! ================================================================================================================ !
                       
                       IF ((CC_END - CC_START) == 0) THEN
                          EXIT
                       END IF

                    END IF

                 END IF
                 
              END DO

           END DO
           
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

                       ! BUFF_X(P) = DES_POS_NEW(P,1) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_U,P)
                       BUFF_X(P) = DES_USR_VAR(AS_OLD_X,P) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_U,P)
                       ! BUFF_X(P) = DES_USR_VAR(AS_DEMI_X,P)
                       
                       ! BUFF_X(I) = DES_POS_NEW(I,1) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_U,I)
                       BUFF_X(I) = DES_USR_VAR(AS_OLD_X,I) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_U,I) 
                       ! BUFF_X(I) = DES_USR_VAR(AS_DEMI_X,I)

                       ! BUFF_Y(P) = DES_POS_NEW(P,2) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_V,P)
                       BUFF_Y(P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_V,P)
                       ! BUFF_Y(P) = DES_USR_VAR(AS_DEMI_Y,P)

                       ! BUFF_Y(I) = DES_POS_NEW(I,2) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_V,I)
                       BUFF_Y(I) = DES_USR_VAR(AS_OLD_Y,I) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_V,I)
                       ! BUFF_Y(I) = DES_USR_VAR(AS_DEMI_Y,I)

                       ! BUFF_Z(P) = DES_POS_NEW(P,3) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_W,P)
                       BUFF_Z(P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_W,P)
                       ! BUFF_Z(P) = DES_USR_VAR(AS_DEMI_Z,P)

                       ! BUFF_Z(I) = DES_POS_NEW(I,3) + 0.5*DTSOLID* DES_USR_VAR(AS_OLD_W,I)
                       BUFF_Z(I) = DES_USR_VAR(AS_OLD_Z,I) + 0.5*DTSOLID* DES_USR_VAR(AS_FREE_W,I)
                       ! BUFF_Z(I) = DES_USR_VAR(AS_DEMI_Z,I)
                       
                       
                       IF (IGLOBAL_ID(P) > IGLOBAL_ID(I)) THEN
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

                       
                       ! SS = SQRT(DX_AS**2+DY_AS**2+DZ_AS**2) - (DES_RADIUS(P)+DES_RADIUS(I))
                       ! ================================================================================================================ !
                       !> SERGE: SS
                       SS = (DES_USR_VAR(AS_NEW_X,P) - DES_USR_VAR(AS_NEW_X,I))**2 + &
                            (DES_USR_VAR(AS_NEW_Y,P) - DES_USR_VAR(AS_NEW_Y,I))**2 + &
                            (DES_USR_VAR(AS_NEW_Z,P) - DES_USR_VAR(AS_NEW_Z,I))**2
                       SS = DSQRT(SS) - (DES_RADIUS(P) + DES_RADIUS(I))
                       ! ================================================================================================================ !

                       ARRAY_SS(CC) = SS
                       
                       UN_OLD = &
                            (DES_USR_VAR(AS_OLD_U,I)-DES_USR_VAR(AS_OLD_U,P))*NX_AS + &
                            (DES_USR_VAR(AS_OLD_V,I)-DES_USR_VAR(AS_OLD_V,P))*NY_AS + &
                            (DES_USR_VAR(AS_OLD_W,I)-DES_USR_VAR(AS_OLD_W,P))*NZ_AS*0

                       UN_NEW = &
                            (DES_USR_VAR(AS_NEW_U,I)-DES_USR_VAR(AS_NEW_U,P))*NX_AS + &
                            (DES_USR_VAR(AS_NEW_V,I)-DES_USR_VAR(AS_NEW_V,P))*NY_AS + &
                            (DES_USR_VAR(AS_NEW_W,I)-DES_USR_VAR(AS_NEW_W,P))*NZ_AS*0
                       

                       VM(CC) = (UN_NEW + UN_OLD * EN) / (1.D0 + EN)
                       
                       ARRAY_VM(CC) = VM(CC)
                       
                       ! MEQ = 4.356341934204102E-002 * 1d-2
                       ! PMASS(P) = 2*MEQ
                       ! PMASS(I) = 2*MEQ
                       MEQ = DES_USR_VAR(AS_PMASS,P) * DES_USR_VAR(AS_PMASS,I) / ( DES_USR_VAR(AS_PMASS,P) + DES_USR_VAR(AS_PMASS,I) )
                       
                       
                       GAMMA = 1.D3                                                              
                       TAU_N = PN(CC) - GAMMA * SS
                       
                       IF (TAU_N>0) THEN
                          DPN(CC) = - VM(CC) * MEQ / DTSOLID * (1.D0 + EN)
                          ACTIVE(CC) = .TRUE.
                       ELSE
                          DPN(CC) = 0.D00
                       END IF

                       
                       IF (PARTICLE_STATE(P) == NORMAL_PARTICLE) THEN
                          DES_USR_VAR(AS_NEW_U,P) = DES_USR_VAR(AS_NEW_U,P) - DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,P)*NX_AS!/(1+EN)
                          DES_USR_VAR(AS_NEW_V,P) = DES_USR_VAR(AS_NEW_V,P) - DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,P)*NY_AS!/(1+EN)
                          DES_USR_VAR(AS_NEW_W,P) = DES_USR_VAR(AS_NEW_W,P) - DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,P)*NZ_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_U,P) = DES_USR_VAR(AS_NEW_U,P) - DPN(CC)*DTSOLID/PMASS(P)*NX_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_V,P) = DES_USR_VAR(AS_NEW_V,P) - DPN(CC)*DTSOLID/PMASS(P)*NY_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_W,P) = DES_USR_VAR(AS_NEW_W,P) - DPN(CC)*DTSOLID/PMASS(P)*NZ_AS!/(1+EN)
                       END IF
                       
                       IF (PARTICLE_STATE(I) == NORMAL_PARTICLE) THEN
                          DES_USR_VAR(AS_NEW_U,I) = DES_USR_VAR(AS_NEW_U,I) + DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,I)*NX_AS!/(1+EN)
                          DES_USR_VAR(AS_NEW_V,I) = DES_USR_VAR(AS_NEW_V,I) + DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,I)*NY_AS!/(1+EN)
                          DES_USR_VAR(AS_NEW_W,I) = DES_USR_VAR(AS_NEW_W,I) + DPN(CC)*DTSOLID/DES_USR_VAR(AS_PMASS,I)*NZ_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_U,I) = DES_USR_VAR(AS_NEW_U,I) + DPN(CC)*DTSOLID/PMASS(I)*NX_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_V,I) = DES_USR_VAR(AS_NEW_V,I) + DPN(CC)*DTSOLID/PMASS(I)*NY_AS!/(1+EN)
                          ! DES_USR_VAR(AS_NEW_W,I) = DES_USR_VAR(AS_NEW_W,I) + DPN(CC)*DTSOLID/PMASS(I)*NZ_AS!/(1+EN)
                       END IF
                          
                          
                       PN(CC) = PN(CC) + DPN(CC)
                       
                       ! ================================================================================================================ !
                       !> SERGE: CRANCK-NICHOLSON
                       IF (PARTICLE_STATE(P) == NORMAL_PARTICLE) THEN
                          DES_USR_VAR(AS_NEW_X,P) = DES_USR_VAR(AS_OLD_X,P) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_U,P)+DES_USR_VAR(AS_OLD_U,P))
                          DES_USR_VAR(AS_NEW_Y,P) = DES_USR_VAR(AS_OLD_Y,P) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_V,P)+DES_USR_VAR(AS_OLD_V,P))
                          DES_USR_VAR(AS_NEW_Z,P) = DES_USR_VAR(AS_OLD_Z,P) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_W,P)+DES_USR_VAR(AS_OLD_W,P))
                       END IF
                       IF (PARTICLE_STATE(I) == NORMAL_PARTICLE) THEN
                          DES_USR_VAR(AS_NEW_X,I) = DES_USR_VAR(AS_OLD_X,I) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_U,I)+DES_USR_VAR(AS_OLD_U,I))
                          DES_USR_VAR(AS_NEW_Y,I) = DES_USR_VAR(AS_OLD_Y,I) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_V,I)+DES_USR_VAR(AS_OLD_V,I))
                          DES_USR_VAR(AS_NEW_Z,I) = DES_USR_VAR(AS_OLD_Z,I) + 0.5D0*DTSOLID*(DES_USR_VAR(AS_NEW_W,I)+DES_USR_VAR(AS_OLD_W,I))
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
           VM_MAX = MAXVAL(ARRAY_VM)
           EC = MAXVAL(ABS(DPN))
           
           CALL MPI_ALLREDUCE(MPI_IN_PLACE,EC,1,MPI_DOUBLE_PRECISION,MPI_MAX,MPI_COMM_WORLD,IERR)
           CALL MPI_ALLREDUCE(MPI_IN_PLACE,SS_MAX,1,MPI_DOUBLE_PRECISION,MPI_MAX,MPI_COMM_WORLD,IERR)
           CALL MPI_ALLREDUCE(MPI_IN_PLACE,VM_MAX,1,MPI_DOUBLE_PRECISION,MPI_MAX,MPI_COMM_WORLD,IERR)

           IF (EC.LT.1E-6) EXIT

           
        END DO
        
        PRINT'(2(1X,I4),3(1X,E15.8))',NSTEP,NLGS,EC,SS_MAX,VM_MAX

! ================================================================================== !                
! ============================ END BOUCLE NLGS ===================================== !        
! ================================================================================== !
        
        
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)

        
! ============================================== !        
! =================== S T O P ================== !
! ============================================== !        
        
        IF (NN == 5000)THEN
           
           CALL MPI_FINALIZE(IERR)
           STOP
           
        END IF
        
! ============================================== !                
! =================== END STOP ================= !        
! ============================================== !        

        
        EC = 0
        PC = 1
        
        DO P = 1, MAX_PIP
           IF(PC .GT. PIP) EXIT
           IF(IS_NONEXISTENT(P)) CYCLE
           PC = PC + 1
           IF(IS_GHOST(P) .OR. IS_ENTERING_GHOST(P) .OR. IS_EXITING_GHOST(P)) CYCLE
           EC = EC + 0.5 * (DES_VEL_NEW(P,1)**2+DES_VEL_NEW(P,2)**2)
        END DO


        CALL MPI_ALLREDUCE(MPI_IN_PLACE,EC,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,IERR)
        

        !> REACTUALISATION DES VITESSES ET POSITIONS
        DO P = 1, MAX_PIP
           
           ! DES_USR_VAR(AS_NEW_X,P) = DES_USR_VAR(AS_OLD_X,P) + DTSOLID*DES_USR_VAR(AS_NEW_U,P)
           ! DES_USR_VAR(AS_NEW_Y,P) = DES_USR_VAR(AS_OLD_Y,P) + DTSOLID*DES_USR_VAR(AS_NEW_V,P)
           ! DES_USR_VAR(AS_NEW_Z,P) = DES_USR_VAR(AS_OLD_Z,P) + DTSOLID*DES_USR_VAR(AS_NEW_W,P)
           !> SERGE
           DES_USR_VAR(AS_NEW_X,P) = DES_USR_VAR(AS_DEMI_X,P) + 0.5D0*DTSOLID*DES_USR_VAR(AS_NEW_U,P)
           DES_USR_VAR(AS_NEW_Y,P) = DES_USR_VAR(AS_DEMI_Y,P) + 0.5D0*DTSOLID*DES_USR_VAR(AS_NEW_V,P)
           DES_USR_VAR(AS_NEW_Z,P) = DES_USR_VAR(AS_DEMI_Z,P) + 0.5D0*DTSOLID*DES_USR_VAR(AS_NEW_W,P)
                         
           DES_VEL_NEW(P,1) = DES_USR_VAR(AS_NEW_U,P)
           DES_VEL_NEW(P,2) = DES_USR_VAR(AS_NEW_V,P)
           DES_VEL_NEW(P,3) = DES_USR_VAR(AS_NEW_W,P)
           
           DES_POS_OLD(P,1) = DES_USR_VAR(AS_NEW_X,P)
           DES_POS_OLD(P,2) = DES_USR_VAR(AS_NEW_Y,P)
           DES_POS_OLD(P,3) = DES_USR_VAR(AS_NEW_Z,P)
           
           DES_VEL_OLD(P,1) = DES_USR_VAR(AS_NEW_U,P)
           DES_VEL_OLD(P,2) = DES_USR_VAR(AS_NEW_V,P)
           DES_VEL_OLD(P,3) = DES_USR_VAR(AS_NEW_W,P)

           IF (PARTICLE_STATE(P) == 1) THEN
              DES_POS_NEW(P,1) = DES_USR_VAR(AS_NEW_X,P)
              DES_POS_NEW(P,2) = DES_USR_VAR(AS_NEW_Y,P)
              DES_POS_NEW(P,3) = DES_USR_VAR(AS_NEW_Z,P)
           END IF
           
        END DO
        
                
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


        !> TEMPS DE SIMULATION + PAS DE TEMPS                        
        S_TIME = S_TIME + DTSOLID
        TIME = S_TIME
        NSTEP = NSTEP + 1

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

        DEALLOCATE(ARRAY_VM)
        DEALLOCATE(ARRAY_SS)
        
! ========================================================================== !        
! ========================================================================== !        
! ========================================================================== !                


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
         
         CLOSE(21)
         
         
       END SUBROUTINE DES_TIME_END

       
     END MODULE DES_TIME_MARCH


      MODULE vtk

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



      INTEGER NUMBER_OF_CELLS
      INTEGER NUMBER_OF_CUT_CELLS
      INTEGER NUMBER_OF_BLOCKED_CELLS
      INTEGER NUMBER_OF_STANDARD_CELLS
      INTEGER NUMBER_OF_VTK_CELLS
      INTEGER NUMBER_OF_VTK_PARTS

      LOGICAL             :: WRITE_VTK_FILES
      LOGICAL             :: TIME_DEPENDENT_FILENAME
      LOGICAL             :: RESET_FRAME_AT_TIME_ZERO=.TRUE.
      CHARACTER (LEN=255)  :: VTU_DIR
      CHARACTER (LEN=255)  :: VTK_FILENAME,FRAME_CHAR,VTU_FILENAME,PVD_FILENAME,PVTU_FILENAME
      CHARACTER (LEN=255)  :: VTU_FRAME_FILENAME='VTU_FRAME_INDEX.TXT'
      CHARACTER (LEN=512) :: BUFFER

      CHARACTER (LEN=1), PARAMETER  :: END_REC = CHAR(10)

      INTEGER :: BOUNDARY_UNIT=122
      INTEGER :: VTK_UNIT=123,VTU_UNIT=124,PVD_UNIT=125
      INTEGER :: PVTU_UNIT=126,VTU_FRAME_UNIT=127
      INTEGER, PARAMETER :: DIM_VTK_VAR = 20
      INTEGER, DIMENSION(DIM_VTK_VAR) :: VTK_VAR


      INTEGER :: POLY_COUNTER,NUMBER_OF_POINTS

      integer, allocatable :: CLEANED_CONNECTIVITY(:,:)
      REAL, allocatable :: COORDS_OF_POINTS(:,:)

      integer, allocatable :: GLOBAL_I_OF(:)
      integer, allocatable :: GLOBAL_J_OF(:)
      integer, allocatable :: GLOBAL_K_OF(:)
      integer, allocatable :: GLOBAL_CONNECTIVITY(:,:)
      integer, allocatable :: GLOBAL_CLEANED_CONNECTIVITY(:,:)
      integer, allocatable :: GLOBAL_NUMBER_OF_NODES(:)

      REAL, allocatable :: GLOBAL_COORDS_OF_POINTS(:,:)

      LOGICAL, allocatable :: GLOBAL_INTERIOR_CELL_AT(:)
      LOGICAL, allocatable :: GLOBAL_BLOCKED_CELL_AT(:)
      LOGICAL, allocatable :: GLOBAL_STANDARD_CELL_AT(:)
      LOGICAL, allocatable :: GLOBAL_CUT_CELL_AT(:)
      LOGICAL, allocatable :: GLOBAL_SNAP(:)
      DOUBLE PRECISION, allocatable :: GLOBAL_F_AT(:)

      double precision, allocatable :: GLOBAL_X_NEW_POINT(:)
      double precision, allocatable :: GLOBAL_Y_NEW_POINT(:)
      double precision, allocatable :: GLOBAL_Z_NEW_POINT(:)

      INTEGER :: GLOBAL_NUMBER_OF_NEW_POINTS


      LOGICAL :: GLOBAL_VAR_ALLOCATED

      LOGICAL :: GRID_INFO_PRINTED_ON_SCREEN

      LOGICAL :: WRITE_ANI_CUTCELL


      INTEGER :: VTU_offset

      LOGICAL, allocatable :: BELONGS_TO_VTK_SUBDOMAIN(:)
      ! LOGICAL, allocatable :: PART_BELONGS_TO_VTK_SUBDOMAIN(:)

      INTEGER, PARAMETER :: DIMENSION_VTK = 100
! Max size for VTK_nRR
      INTEGER, PARAMETER :: VTK_nRRmax = 100
! Max size for VTK_PART_USR_VAR
      INTEGER, PARAMETER :: VTK_PART_USRmax = 100

! Current VTK region
      INTEGER :: VTK_REGION

! Time interval at which vtk files are saved
      DOUBLE PRECISION :: VTK_DT(0:DIMENSION_VTK)

! Flag to distiguish between regular file (VTK_DBG_FILE=.FALSE., default value)
!                        and debug file (VTK_DBG_FILE=.TRUE.)
      LOGICAL :: VTK_DBG_FILE(0:DIMENSION_VTK)

! Current vtk time
      DOUBLE PRECISION :: VTK_TIME(0:DIMENSION_VTK)

! Type of data in vtk region: 'C':cell data, 'P': particle data
      CHARACTER(LEN=1) :: VTK_DATA(0:DIMENSION_VTK)

! FRAME index of vtk file
      INTEGER :: FRAME(0:DIMENSION_VTK)

! PVD file initialization flag
      LOGICAL :: PVD_FILE_INITIALIZED(0:DIMENSION_VTK)=.FALSE.

! Logical variable to determine whether an vtk region is defined
      LOGICAL :: VTK_DEFINED (0:DIMENSION_VTK)

! VTK region West face, X-coordinate
      DOUBLE PRECISION :: VTK_X_w (0:DIMENSION_VTK)

! VTK region East face, X-coordinate
      DOUBLE PRECISION :: VTK_X_e (0:DIMENSION_VTK)

! VTK region South face, Y-coordinate
      DOUBLE PRECISION :: VTK_Y_s (0:DIMENSION_VTK)

! VTK region North face, Y-coordinate
      DOUBLE PRECISION :: VTK_Y_n (0:DIMENSION_VTK)

! VTK region Bottom face, Z-coordinate
      DOUBLE PRECISION :: VTK_Z_b (0:DIMENSION_VTK)

! VTK region Top face, Z-coordinate
      DOUBLE PRECISION :: VTK_Z_t (0:DIMENSION_VTK)

! VTK number of slices in x-direction
      INTEGER :: VTK_NXS(0:DIMENSION_VTK)

! VTK number of slices in y-direction
      INTEGER :: VTK_NYS(0:DIMENSION_VTK)

! VTK number of slices in z-direction
      INTEGER :: VTK_NZS(0:DIMENSION_VTK)

! VTK slice tolerance
      DOUBLE PRECISION :: VTK_SLICE_TOL(0:DIMENSION_VTK)

! Flag to write only cut cell data in VTK file
      LOGICAL :: VTK_CUTCELL_ONLY(0:DIMENSION_VTK)

! VTK filename base
      CHARACTER(LEN=255) :: VTK_FILEBASE(0:DIMENSION_VTK)

! Gas phase volume fraction
      LOGICAL :: VTK_EP_g (0:DIMENSION_VTK)

! Gas pressure
      LOGICAL :: VTK_P_g (0:DIMENSION_VTK)

! Solids pressure
      LOGICAL :: VTK_P_star(0:DIMENSION_VTK)

! X-component of gas velocity
      LOGICAL :: VTK_U_g(0:DIMENSION_VTK)

! X-component of solids phase velocity
      LOGICAL :: VTK_U_s(0:DIMENSION_VTK, DIM_M)

! Y-component of gas velocity
      LOGICAL :: VTK_V_g(0:DIMENSION_VTK)

! Y-component of solids phase velocity
      LOGICAL :: VTK_V_s(0:DIMENSION_VTK, DIM_M)

! Z-component of gas velocity
      LOGICAL :: VTK_W_g(0:DIMENSION_VTK)

! Z-component of solids phase velocity
      LOGICAL :: VTK_W_s(0:DIMENSION_VTK, DIM_M)

! Gas velocity vector
      LOGICAL :: VTK_VEL_g(0:DIMENSION_VTK)

! Solids velocity vector
      LOGICAL :: VTK_VEL_s(0:DIMENSION_VTK, DIM_M)

! Macroscopic density of solids phases
      LOGICAL :: VTK_ROP_s(0:DIMENSION_VTK, DIM_M)

! Solids phase volume fraction
      LOGICAL :: VTK_EP_s (0:DIMENSION_VTK, DIM_M)

! Gas temperature
      LOGICAL :: VTK_T_g(0:DIMENSION_VTK)

! Solids temperature
      LOGICAL :: VTK_T_s(0:DIMENSION_VTK, DIM_M)

! Gas species mass fractions
      LOGICAL :: VTK_X_g(0:DIMENSION_VTK, DIM_N_g)

! Solids species mass fractions
      LOGICAL :: VTK_X_s(0:DIMENSION_VTK, DIM_M, DIM_N_s)

! Granular temperature
      LOGICAL :: VTK_Theta_m(0:DIMENSION_VTK, DIM_M)

! Scalar value
      LOGICAL :: VTK_Scalar(0:DIMENSION_VTK, DIM_scalar)

! Reaction rates
      LOGICAL :: VTK_RRate(0:DIMENSION_VTK,VTK_nRRmax)

! Reaction rates labels
      CHARACTER(LEN=255) :: VTK_RRate_label(0:DIMENSION_VTK,VTK_nRRmax)

! K & Epsilon values
      LOGICAL :: VTK_K_Turb_G(0:DIMENSION_VTK)
      LOGICAL :: VTK_E_Turb_G(0:DIMENSION_VTK)

! Vorticity magnitude
      LOGICAL :: VTK_VORTICITY(0:DIMENSION_VTK)

! Lambda_2 (vortex core indicator)
      LOGICAL :: VTK_LAMBDA_2(0:DIMENSION_VTK)

! Grid partition
      LOGICAL :: VTK_PARTITION(0:DIMENSION_VTK)

! Boundary condition ID
      LOGICAL :: VTK_BC_ID(0:DIMENSION_VTK)

! Wall distance
      LOGICAL :: VTK_DWALL(0:DIMENSION_VTK)

! Facet count (DES)
      LOGICAL :: VTK_FACET_COUNT_DES(0:DIMENSION_VTK)

! Neighboring facets (DES)
      LOGICAL :: VTK_NB_FACET_DES(0:DIMENSION_VTK)

! Cell IJK index
      LOGICAL :: VTK_IJK(0:DIMENSION_VTK)

! Cut face normal vector
      LOGICAL :: VTK_NORMAL(0:DIMENSION_VTK)

! Debug variable
      LOGICAL :: VTK_DEBUG(0:DIMENSION_VTK,15)

      INTEGER, DIMENSION(0:DIMENSION_VTK,DIM_VTK_VAR) :: VTK_VARLIST

! VTK Particle selection: 'C': center, 'P': entire particle, 'I': particle intersect
      CHARACTER(LEN=1) :: VTK_SELECT_MODE(0:DIMENSION_VTK)

! Phase ID to use as labels in vtp files when there is only
! one phase in the vtk regon
      INTEGER :: VTK_PHASE_FOR_DES_X_S(0:DIMENSION_VTK)

! Particle phase

      LOGICAL :: VTK_PART_PHASE(0:DIMENSION_VTK, DIM_M)

! Particle radius
      LOGICAL :: VTK_PART_DIAMETER(0:DIMENSION_VTK)

! Particle velocity
      LOGICAL :: VTK_PART_VEL(0:DIMENSION_VTK)

! Particle angular velocity
      LOGICAL :: VTK_PART_ANGULAR_VEL(0:DIMENSION_VTK)

! Particle orientation
      LOGICAL :: VTK_PART_ORIENTATION(0:DIMENSION_VTK)

! Particle user-defined variable
      LOGICAL :: VTK_PART_USR_VAR(0:DIMENSION_VTK,VTK_PART_USRmax )

! Particle temperature
      LOGICAL :: VTK_PART_TEMP(0:DIMENSION_VTK)

! Particle species mass fraction
      LOGICAL :: VTK_PART_X_s(0:DIMENSION_VTK,100)

! Particle density
      LOGICAL :: VTK_PART_DENSITY(0:DIMENSION_VTK)

! Particle cohesion
      LOGICAL :: VTK_PART_COHESION(0:DIMENSION_VTK)

! Particle rank
      LOGICAL :: VTK_PART_RANK(0:DIMENSION_VTK)

! Particle ID
      LOGICAL :: VTK_PART_ID(0:DIMENSION_VTK)

! Domain decomposition
      LOGICAL :: VTK_DOMAIN_DECOMPOSITION(0:DIMENSION_VTK)

      END MODULE vtk


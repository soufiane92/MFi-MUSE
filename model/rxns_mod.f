      MODULE rxns

        use param, only: dim_m, dim_n_all, dim_n_g, dim_n_s

! reaction rates
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE ::  ReactionRates

! number of ReactionRates
      INTEGER nRR
! total number of species
      INTEGER N_all

      LOGICAL rDatabase(0:DIM_M, DIM_N_g)

!-----------------------------------------------------------------------

! Indicates that reaction rates are to be calculated.
      LOGICAL :: RRATE
! Indicates if the legacy reaction rates file (rrates.f) is used.
      LOGICAL :: USE_RRATES

! Rate of production of gas species
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: R_gp
! Rate of consumption of gas species/X_g
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: RoX_gc
! Net production of gas
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SUM_R_g

! Rate of production of solids species
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: R_sp
! Rate of consumption of solids species/X_s
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: RoX_sc
! Net production of solids
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::  SUM_R_s

! Rate of mass transfer from phase M to Phase L
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::  R_phase

! Actual number of Reactions
      INTEGER NO_OF_RXNS

! Species names -- Legacy variable
      CHARACTER(len=18) SPECIES_NAME(DIM_N_ALL)

! Gas phase species names (database) and aliases
      CHARACTER(len=18) SPECIES_g(DIM_N_g) ! database name
      CHARACTER(len=32)  SPECIES_ALIAS_g(DIM_N_g) ! alias

! Solids phase species names (database) and aliases
      CHARACTER(len=18) SPECIES_s(DIM_M, DIM_N_s) ! database name
      CHARACTER(len=32)  SPECIES_ALIAS_s(DIM_M, DIM_N_s) ! alias

      ! The following data types are used to group chemical reaction data.
      !-----------------------------------------------------------------------

      ! Species belong to PHASE_ associated with a particular reaction.
      TYPE SPECIES_
         ! A link between the reacting species' arbitrary index and the
         ! associated phase index in MFiX.
         INTEGER pMap
         ! A link between the reacting species' arbitrary index and the
         ! associated species index in MFiX.
         INTEGER sMap
         ! Stoichiometric coefficient of the species from chemical equation.
         DOUBLE PRECISION Coeff
         ! Molecular weight
         DOUBLE PRECISION MW
         ! Fractional mass transfer
         DOUBLE PRECISION xXfr
         ! Index indicating enthalpy transfer associated with mass transfer.
         INTEGER mXfr
         ! Molecular weight of speices multiplying the stoichiometric coefficient
         DOUBLE PRECISION MWxStoich
      END TYPE SPECIES_

      ! Grouping of reaction information.
      TYPE REACTION_BLOCK
         ! Name of reaction construct from data file.
         CHARACTER(LEN=32) :: Name
         ! User defined chemical equation from data file.
         CHARACTER(LEN=512) :: ChemEq
         ! Reaction classification: Homogeneous, Heterogeneous, Catalytic.
         CHARACTER(LEN=16) :: Classification
         ! Indicates if the automated heat of reaction is to be calculated (T) or
         ! if the user has supplied a heat of reaction (F).
         LOGICAL Calc_DH
         ! Number of phases associated with the reaction.
         INTEGER nPhases
         ! Number of species associated with the reaction.
         INTEGER nSpecies
         ! User-specified heat of reaction split among phases by fracDH
         DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: HoR
         ! Interphase mass transfer.
         DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: rPhase
         ! Reactant/Product information
         TYPE(SPECIES_), DIMENSION(:), ALLOCATABLE :: Species

      END TYPE REACTION_BLOCK

! Array linking all of the reaction data.
      TYPE(REACTION_BLOCK), DIMENSION(:), TARGET, ALLOCATABLE :: Reaction

      END MODULE rxns

MODULE DES_FUNCTIONS_MOD

CONTAINS

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!
!  Function: DES_GETINDEXFROMPOS
!  Purpose: Knowing the current particle x, y, or z position determine
!  the associated i, j or k index by searching a specific region defined
!  by a lower and upper limit on the index that is known to contain
!  the particles position
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      INTEGER FUNCTION DES_GETINDEXFROMPOS(LIM1,LIM2,PART_POS,&
         GRID_POS,AXIS,AXIS_INDEX)

!-----------------------------------------------
! Modules
!-----------------------------------------------
      USE compar
      USE funits
      IMPLICIT NONE
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
! given i, j, or k index values defining the upper and lower limits of
! the region in space known to contain the particle
      INTEGER, INTENT (IN) :: LIM1, LIM2
! given current x, y, or z position of the particle
      DOUBLE PRECISION, INTENT (IN) :: PART_POS
! given position for the cell faces on the fluid grid in the x, y or z
! direction
      DOUBLE PRECISION, DIMENSION(:), INTENT (IN) :: GRID_POS
! given axis (x, y, or z) and associated index (i, j, or k)
      CHARACTER(LEN=1), INTENT (IN) :: AXIS, AXIS_INDEX
!-----------------------------------------------
! Local Variables
!-----------------------------------------------
! index & loop counter
      INTEGER IND
!-----------------------------------------------

! error condition
      IND = -1

      IF (LIM1 <= LIM2) THEN
         DO IND = LIM1, LIM2
            IF (PART_POS >= GRID_POS(IND-1) .AND. &
                PART_POS <  GRID_POS(IND)) EXIT
         ENDDO
      ELSEIF (LIM1 > LIM2) THEN
         DO IND = LIM1, LIM2, -1
            IF (PART_POS >= GRID_POS(IND-1) .AND. &
                PART_POS <  GRID_POS(IND)) EXIT
         ENDDO
      ENDIF

      IF (IND == -1) THEN
         WRITE (UNIT_LOG, 1001) AXIS_INDEX, AXIS, PART_POS, &
            AXIS_INDEX, LIM1, AXIS_INDEX, LIM2
         WRITE (*,1001) AXIS_INDEX, AXIS, PART_POS, &
            AXIS_INDEX, LIM1, AXIS_INDEX, LIM2
!         call flush_err_msg(abort=.TRUE.)
      ENDIF

      DES_GETINDEXFROMPOS = IND

      RETURN

 1001 FORMAT(/1X,70('*')//,' From: DES_GETINDEXFROMPOS',/,' Message: ',&
         'Could not identify the ', A, ' index associated with the ',&
         'particles ',/10X,A, '-position= ',ES15.5,' between the ',&
         'limits ',A,'=',I5,/10X,' and ',A,'=',I5,'.',/1X,70('*')/)

      END FUNCTION DES_GETINDEXFROMPOS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!                                                                     !
!  Subroutine: DES_COUNT_NORMAL_PARTICLES                             !
!      Author: Hari Sitaraman
!  Purpose: counts normal particles after sorting                     !
!                                                                     !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
SUBROUTINE DES_COUNT_NORMAL_PARTICLES

  use discretelement
        use functions
  implicit none

  integer :: i

  do i=1,MAX_PIP
    if(.NOT.(IS_NORMAL(i))) exit
  enddo

  NP_NORMAL = i-1
  NP_NOT_NORMAL = MAX_PIP-NP_NORMAL

END SUBROUTINE DES_COUNT_NORMAL_PARTICLES
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!                                                                     !
!  Subroutine: DES_GETVALID_FLUID_CELLS                               !
!      Author: Hari Sitaraman                                         !
!  Purpose: tag the valid fluid cells so they can be masked           !
!                                                                     !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
SUBROUTINE DES_GETVALID_FLUID_CELLS()

     use discretelement
     use functions, only: FLUID_AT
     use param, only: DIMENSION_3
     implicit none
     !use compar, only: DEAD_CELL_AT

     integer :: pos1,pos2,ijk,ncells1,ncells2
     integer :: findices1(dimension_3)
     integer :: findices2(dimension_3)
     integer :: i

     valid_fluid_indices = 0
     findices1 = 0
     findices2 = 0

     pos1=1
     pos2=1
     do ijk=1,dimension_3

         !i = i_of(ijk)
         !j = j_of(ijk)
         !k = merge(k_of(ijk), 1, do_k)
  !if( fluid_at(ijk) .and. (.not.dead_cell_at(i,j,k)) ) then

  if(fluid_at(ijk))  then
    if(pinc(ijk) .gt. 0) then
      findices1(pos1)=ijk
      pos1=pos1+1
    else
      findices2(pos2)=ijk
      pos2=pos2+1
    endif
  endif

     enddo

     ncells1=pos1 - 1
     ncells2=pos2 - 1

     ncells_valid = ncells1 + ncells2
     ncells_valid_pinc = ncells1

     do i=1,ncells1
  valid_fluid_indices(i)=findices1(i)
     enddo

     do i=1,ncells2
  valid_fluid_indices(ncells1+i)=findices2(i)
     enddo

END SUBROUTINE DES_GETVALID_FLUID_CELLS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!                                                                     !
!  Subroutine: COUNT_MAX_PIP_EXIST                                    !
!      Author: Hari Sitaraman
!  Purpose:                      !
!                                                                     !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
SUBROUTINE COUNT_MAX_PIP_EXIST

  use discretelement
        use functions
  implicit none

  integer :: i

  do i=1,MAX_PIP
    if(IS_NONEXISTENT(i)) exit
  enddo

  MAX_PIP_EXIST = i-1

END SUBROUTINE COUNT_MAX_PIP_EXIST
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!                                                                     !
!  Subroutine: SWAP_PARTICLE_DATA                                     !
!      Author: Hari Sitaraman
!  Purpose:                      !
!                                                                     !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
SUBROUTINE SWAP_PARTICLE_DATA(i,j)
!-----------------------------------------------
! Modules
!-----------------------------------------------
      use des_rxns
      use des_thermo
      use desgrid
      use desmpi
      use discretelement
      use param, only: dimension_n_s
      use derived_types
      use functions
      use mfix_pic, only: AVGSOLVEL_P, EPG_P
      use mfix_pic, only: MPPIC, DES_STAT_WT, PS_GRAD
      use particle_filter, only: FILTER_CELL, FILTER_WEIGHT
      use particle_filter, only: FILTER_SIZE
      use run, only: ANY_SPECIES_EQ
      use run, only: ENERGY_EQ

      IMPLICIT NONE

      integer, intent(in) :: i,j
      INTEGER :: d,dmax_1,dmax_2,d1,d2

      !sort particle state array
      call DES_SWAPVALUES(PARTICLE_STATE(i),PARTICLE_STATE(j))
      call DES_SWAPVALUES(IGLOBAL_ID(i),IGLOBAL_ID(j))

      ! Physical properties:
      call DES_SWAPVALUES(DES_RADIUS(i),DES_RADIUS(j))
      call DES_SWAPVALUES(RO_Sol(i),RO_Sol(j))
      call DES_SWAPVALUES(PVOL(i),PVOL(j))
      call DES_SWAPVALUES(PMASS(i),PMASS(j))
      call DES_SWAPVALUES(OMOI(i),OMOI(j))

      ! Particle position, velocity, etc
      do d=1,DIMN
            call DES_SWAPVALUES(DES_POS_NEW(i,d),DES_POS_NEW(j,d))
            call DES_SWAPVALUES(DES_VEL_NEW(i,d),DES_VEL_NEW(j,d))
            call DES_SWAPVALUES(OMEGA_NEW(i,d),OMEGA_NEW(j,d))
      enddo

      IF(PARTICLE_ORIENTATION) THEN
           call DES_SWAPVALUES(ORIENTATION(i,1),ORIENTATION(j,1))
           call DES_SWAPVALUES(ORIENTATION(i,2),ORIENTATION(j,2))
           call DES_SWAPVALUES(ORIENTATION(i,3),ORIENTATION(j,3))
      ENDIF

      ! DES grid bin information
      call DES_SWAPVALUES(DG_PIJK(i),DG_PIJK(j))
      call DES_SWAPVALUES(DG_PIJKPRV(i),DG_PIJKPRV(j))
      call DES_SWAPVALUES(IGHOST_UPDATED(i),IGHOST_UPDATED(j))

      ! Fluid cell bin information
      do d=1,5
         call DES_SWAPVALUES(PIJK(i,d),PIJK(j,d))
      enddo

      ! Translation and rotational forces
      do d=1,DIMN
         call DES_SWAPVALUES(FC(i,d),FC(j,d))
         call DES_SWAPVALUES(TOW(i,d),TOW(j,d))
      enddo

     ! Collision data
     dmax_1 = size(WALL_COLLISION_FACET_ID,1)
     do d1=1,dmax_1
        call DES_SWAPVALUES(WALL_COLLISION_FACET_ID(d1,i),WALL_COLLISION_FACET_ID(d1,j))
     enddo
     dmax_1 = size(WALL_COLLISION_PFT,1)
     dmax_2 = size(WALL_COLLISION_PFT,2)

     do d2=1,dmax_2
  do d1=1,dmax_1
            call  DES_SWAPVALUES(WALL_COLLISION_PFT(d1,d2,i),WALL_COLLISION_PFT(d1,d2,j))
  enddo
     enddo

     ! Initializing user defined array
     IF(DES_USR_VAR_SIZE > 0) then
   do d=1,DES_USR_VAR_SIZE
          call DES_SWAPVALUES(DES_USR_VAR(i,d),DES_USR_VAR(j,d))
         enddo
     ENDIF

     do d=1,DIMN
  call DES_SWAPVALUES(PPOS(i,d),PPOS(j,d))
     enddo

    ! Particle center drag coefficient and explit drag force
    call DES_SWAPVALUES(F_GP(i),F_GP(j))
    do d=1,DIMN
  call DES_SWAPVALUES(DRAG_FC(i,d),DRAG_FC(j,d))
    enddo

    ! Interpolation variables.
    IF(FILTER_SIZE > 0)THEN
   do d=1,FILTER_SIZE
          call DES_SWAPVALUES(FILTER_CELL(i,d),FILTER_CELL(j,d))
          call DES_SWAPVALUES(FILTER_WEIGHT(i,d),FILTER_WEIGHT(j,d))
     enddo
    ENDIF

    ! MPPIC variables
    IF(MPPIC) THEN
         call DES_SWAPVALUES(DES_STAT_WT(i),DES_STAT_WT(j))
         call DES_SWAPVALUES(EPG_P(i),EPG_P(j))

   do d=1,DIMN
             call DES_SWAPVALUES(PS_GRAD(i,d),PS_GRAD(j,d))
             call DES_SWAPVALUES(AVGSOLVEL_P(i,d),AVGSOLVEL_P(j,d))
   enddo
    ENDIF

    ! Higher order time integration variables.
    IF (DO_OLD) THEN
  do d=1,DIMN
                call DES_SWAPVALUES(DES_POS_OLD(i,d),DES_POS_OLD(j,d))
          call DES_SWAPVALUES(DES_VEL_OLD(i,d),DES_VEL_OLD(j,d))
          call DES_SWAPVALUES(DES_ACC_OLD(i,d),DES_ACC_OLD(j,d))
          call DES_SWAPVALUES(OMEGA_OLD(i,d),OMEGA_OLD(j,d))
          call DES_SWAPVALUES(ROT_ACC_OLD(i,d),ROT_ACC_OLD(j,d))
  enddo
    ENDIF

    ! Energy equation variables.
    IF(ENERGY_EQ)THEN
        !call DES_SWAPVALUES(DES_T_s_OLD(i),DES_T_s_OLD(j))
        !call DES_SWAPVALUES(DES_T_s_NEW(i),DES_T_s_NEW(j))
        !call DES_SWAPVALUES(DES_C_PS(i),DES_C_PS(j))
  do d=1,DIMENSION_N_S
             call DES_SWAPVALUES(DES_X_s(i,d),DES_X_s(j,d))
             call DES_SWAPVALUES(Q_Source(i),Q_Source(j))
        enddo
        IF (INTG_ADAMS_BASHFORTH) then
            call DES_SWAPVALUES(Q_Source0(i),Q_Source0(j))
  ENDIF
    ENDIF

    ! Chemical reaction variables.
    IF(ANY_SPECIES_EQ) THEN
  do d=1,DIMENSION_N_S
         !  call DES_SWAPVALUES(DES_R_sp(i,d),DES_R_sp(j,d))
         !  call DES_SWAPVALUES(DES_R_sc(i,d),DES_R_sc(j,d))
  enddo
        IF (INTG_ADAMS_BASHFORTH) THEN
          call DES_SWAPVALUES(dMdt_OLD(i),dMdt_OLD(j))
          call DES_SWAPVALUES(dXdt_OLD(i,d),dXdt_OLD(j,d))
        ENDIF
        !call DES_SWAPVALUES(Qint(i),Qint(j))
    ENDIF

   ! Cohesion VDW forces
   IF(USE_COHESION) THEN
         call DES_SWAPVALUES(PostCohesive(i),PostCohesive(j))
   ENDIF

END SUBROUTINE SWAP_PARTICLE_DATA
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                         !
!  Subrourtine: DES_SORT_PARTICLE_ARRAYS                                  !
!  Author: Hari Sitaraman                           Date: 16-Oct-2015  !
!                                                                         !
!  Purpose: Sort all particle arrays.                          !
!                                                                         !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE DES_SORT_PARTICLE_ARRAYS()

!-----------------------------------------------
! Modules
!-----------------------------------------------
      use des_rxns
      use des_thermo
      use desgrid
      use desmpi
      use discretelement
      use derived_types
      use compar, only:ijkstart3,ijkend3
      use functions

      IMPLICIT NONE

      integer :: i,j,ending
      integer :: nblen,nblen_old
      integer :: L
      integer, allocatable :: swapping_map(:)
      integer, allocatable :: nbrcount(:)
      integer, allocatable :: nbrcount_old(:)
      logical :: sortedflag,nbsortflag,nbsortflag_old
      logical :: normalswapflag,nonexswapflag
      integer :: oldpid,npic,ijk,pindx

      allocate(nbrcount(MAX_PIP))
      allocate(nbrcount_old(MAX_PIP))
      allocate(swapping_map(MAX_PIP))

      nblen     = size(NEIGHBORS)
      nblen_old = size(NEIGHBORS_OLD)

      !populate nbrcount
     nbrcount(1)     = NEIGHBOR_INDEX(1)     - 1
     nbrcount_old(1) = NEIGHBOR_INDEX_OLD(1) - 1
     do L=2,MAX_PIP
        nbrcount(L)     = NEIGHBOR_INDEX(L)     - NEIGHBOR_INDEX(L-1)
        nbrcount_old(L) = NEIGHBOR_INDEX_OLD(L) - NEIGHBOR_INDEX_OLD(L-1)
     enddo

     nbsortflag = .true.
     if((NEIGHBOR_INDEX(1) .le. 0))  nbsortflag = .false.

     nbsortflag_old = .true.
     if((NEIGHBOR_INDEX_OLD(1) .le. 0)) nbsortflag_old = .false.

     do i=1,MAX_PIP
        swapping_map(i) = i
     enddo

     ending=MAX_PIP
     i=1
     sortedflag=.false.
     normalswapflag=.false.
     do while(i .lt. ending)

  if( .NOT.(IS_NORMAL(i)) ) then

    do j=ending,i+1,-1
      if(IS_NORMAL(j)) exit
    enddo

    if(j .le. i) then
                     !this means the array is sorted
                     sortedflag=.true.
                else

                    normalswapflag=.true.
                    swapping_map(i) = j
                    swapping_map(j) = i
                    call SWAP_PARTICLE_DATA(i,j)
        ending = j-1

                endif
        ENDIF

        if(sortedflag .eqv. .true.) then
                exit
        endif

        i = i+1

      enddo

      call DES_COUNT_NORMAL_PARTICLES()

      ending = MAX_PIP
      i      = NP_NORMAL+1
      sortedflag    = .false.
      nonexswapflag = .false.
      do while(i .lt. ending)

  if( (IS_NONEXISTENT(i)) ) then

    do j=ending,i+1,-1
      if(.NOT. IS_NONEXISTENT(j)) exit
    enddo

    if(j .le. i) then
                     !this means the array is sorted
                     sortedflag=.true.
                else
                    nonexswapflag=.true.
                    swapping_map(i) = j
                    swapping_map(j) = i
                    call SWAP_PARTICLE_DATA(i,j)
        ending = j-1
                endif
        ENDIF

        if(sortedflag .eqv. .true.) then
                exit
        endif

        i = i+1

      enddo

      call COUNT_MAX_PIP_EXIST()

      if((normalswapflag .eqv. .true.) .or. (nonexswapflag .eqv. .true.))then

         if(nbsortflag .eqv. .true.) then
                call EXCHANGE_NEIGHBOR_DATA(swapping_map,swapping_map,nbrcount,neighbor_index,neighbors,&
                        pft_neighbor,max_pip,nblen)
         endif

         if(nbsortflag_old .eqv. .true.) then
                call EXCHANGE_NEIGHBOR_DATA(swapping_map,swapping_map,nbrcount_old,neighbor_index_old,&
                        neighbors_old,pft_neighbor_old,max_pip,nblen_old)
         endif

         !correct particle numbers in dg_pic and pic arrays
   do ijk=1,size(dg_pic)

      npic = dg_pic(ijk)%isize
      do pindx=1,npic

    oldpid = dg_pic(ijk)%p(pindx)
                dg_pic(ijk)%p(pindx) = swapping_map(oldpid)

      enddo
  enddo

        do ijk=ijkstart3,ijkend3

      npic = pinc(ijk)

      do pindx=1,npic
    oldpid = pic(ijk)%p(pindx)
                pic(ijk)%p(pindx) = swapping_map(oldpid)
      enddo

  enddo

      endif

      deallocate(swapping_map)
      deallocate(nbrcount)
      deallocate(nbrcount_old)

      RETURN
      END SUBROUTINE DES_SORT_PARTICLE_ARRAYS
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!                                                                     !
!  Subroutine: EXCHANGE_NEIGHBOR_DATA                                 !
!      Author: Hari Sitaraman
!  Purpose:                      !
!                                                                     !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
SUBROUTINE EXCHANGE_NEIGHBOR_DATA(swapping_map,inv_swapping_map,&
                                  nbrcount,neighbor_index,&
          neighbor,pft_neighbor,npart,nblen)

      implicit none

      integer, intent(in)    :: npart
      integer, intent(in)    :: nblen
      integer, intent(in)    :: swapping_map(npart)
      integer, intent(in)    :: inv_swapping_map(npart)
      integer, intent(in)    :: nbrcount(npart)
      integer, intent(inout) :: neighbor_index(npart)
      integer, intent(inout) :: neighbor(nblen)
      double precision, intent(inout) :: pft_neighbor(3,nblen)

      integer, allocatable :: nbr(:)
      integer, allocatable :: pft_nbr(:,:)

      integer :: j,newid,oldid,oldnbid
      integer :: cc_start
      integer :: offset
      integer :: sum_count

      allocate(nbr(nblen))
      allocate(pft_nbr(3,nblen))

      offset=0
      do newid=1,npart

        oldid = swapping_map(newid)

  if(oldid .ne. 1) then
    cc_start=neighbor_index(oldid-1)
  else
    cc_start=1
  endif

  do j=1,nbrcount(oldid)

                oldnbid             = neighbor(j+cc_start-1)
    nbr(offset+j)       = inv_swapping_map(oldnbid)
    pft_nbr(:,offset+j) = pft_neighbor(:,cc_start+j-1)

  enddo
  offset = offset+nbrcount(oldid)

      enddo

      neighbor     = nbr
      pft_neighbor = pft_nbr

      !update neighbor_index
      sum_count=0
      do newid=1,npart

        oldid = swapping_map(newid)
        sum_count=sum_count+nbrcount(oldid)
        neighbor_index(newid)=sum_count+1

      enddo


      deallocate(nbr)
      deallocate(pft_nbr)

END SUBROUTINE EXCHANGE_NEIGHBOR_DATA
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!                                                                     !
!  Subroutine: DES_SORT_PARTICLES_SPATIALLY                           !
!      Author: Hari Sitaraman
!  Purpose: sort the particles into spatial locality                  !
!                                                                     !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
SUBROUTINE DES_SORT_PARTICLES_SPATIALLY()

      use des_rxns
      use des_thermo
      use desgrid
      use compar, only:ijkstart3,ijkend3
      use derived_types
      use desmpi
      use discretelement
      use functions

      implicit none
      integer :: i,j,ijk,L
      integer,allocatable  :: swapping_map(:)
      integer,allocatable  :: inv_swapping_map(:)
      integer,allocatable  :: inv_swapping_map_copy(:)
      integer, allocatable :: nbrcount(:)
      integer, allocatable :: nbrcount_old(:)
      integer :: npic,pindx,oldpid,nblen,nblen_old
      logical :: nbsortflag,nbsortflag_old
      integer :: dummyval

      dummyval=2*MAX_PIP+1

      allocate(nbrcount(MAX_PIP))
      allocate(nbrcount_old(MAX_PIP))
      allocate(swapping_map(MAX_PIP))
      allocate(inv_swapping_map(MAX_PIP))
      allocate(inv_swapping_map_copy(MAX_PIP))

      nblen     = size(NEIGHBORS)
      nblen_old = size(NEIGHBORS_OLD)

     !populate nbrcount
     nbrcount(1)     = NEIGHBOR_INDEX(1)     - 1
     nbrcount_old(1) = NEIGHBOR_INDEX_OLD(1) - 1
     do L=2,MAX_PIP
         nbrcount(L)     = NEIGHBOR_INDEX(L)     - NEIGHBOR_INDEX(L-1)
         nbrcount_old(L) = NEIGHBOR_INDEX_OLD(L) - NEIGHBOR_INDEX_OLD(L-1)
     enddo

    nbsortflag = .true.
    if((NEIGHBOR_INDEX(1) .le. 0))  nbsortflag = .false.

    nbsortflag_old = .true.
    if((NEIGHBOR_INDEX_OLD(1) .le. 0)) nbsortflag_old = .false.

    inv_swapping_map=dummyval

    i=1
    do ijk=1,size(dg_pic)

    npic = dg_pic(ijk)%isize

    do pindx=1,npic
    j = dg_pic(ijk)%p(pindx)
    swapping_map(i)     = j
                inv_swapping_map(j) = i
    i=i+1
    enddo
    enddo

    do L=1,MAX_PIP
        if(inv_swapping_map(L) .eq. dummyval) then
                inv_swapping_map(L) = i
                swapping_map(i)     = L
                i=i+1
        endif
    enddo

    inv_swapping_map_copy = inv_swapping_map

    call quicksort(inv_swapping_map_copy,1,MAX_PIP,MAX_PIP)

    if(nbsortflag .eqv. .true.) then
                call EXCHANGE_NEIGHBOR_DATA(swapping_map,inv_swapping_map,nbrcount,neighbor_index,neighbors,&
                        pft_neighbor,max_pip,nblen)
    endif

    if(nbsortflag_old .eqv. .true.) then
                call EXCHANGE_NEIGHBOR_DATA(swapping_map,inv_swapping_map,nbrcount_old,neighbor_index_old,&
                        neighbors_old,pft_neighbor_old,max_pip,nblen_old)
    endif

   !correct particle numbers in dg_pic and pic arrays
     do ijk=1,size(dg_pic)
      npic = dg_pic(ijk)%isize
      do pindx=1,npic
    oldpid = dg_pic(ijk)%p(pindx)
                dg_pic(ijk)%p(pindx) = inv_swapping_map(oldpid)
      enddo
     enddo

     do ijk=ijkstart3,ijkend3
      npic = pinc(ijk)
      do pindx=1,npic
    oldpid = pic(ijk)%p(pindx)
                pic(ijk)%p(pindx) = inv_swapping_map(oldpid)
      enddo
     enddo


END SUBROUTINE DES_SORT_PARTICLES_SPATIALLY
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!                                                                     !
!  Subroutine: QUICKSORT                                              !
!      Author: Hari Sitaraman
!  Purpose: SORTING ROUTINE                                           !
!                                                                     !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
recursive subroutine quicksort(a,lo,hi,n)

        implicit none
        integer, intent(in) :: lo,hi,n
        integer :: a(n)

        integer :: mid,pivotloc


        if(lo .lt. hi) then
                mid = (lo+hi)/2
                call findmedian(a(lo),a(mid),a(hi),lo,mid,hi,pivotloc)

                call partition(a,lo,hi,n,pivotloc)
                call quicksort(a,lo,pivotloc-1,n)
                call quicksort(a,pivotloc+1,hi,n)
        endif


end subroutine quicksort
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!                                                                     !
!  Subroutine: PARTITION                                              !
!      Author: Hari Sitaraman
!  Purpose:                      !
!                                                                     !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
subroutine partition(array,lo,hi,n,p)

        use discretelement, only:des_swapvalues
        implicit none
        integer, intent(in) :: lo,hi,n
        integer, intent(out) :: p
        integer :: array(n)

        integer :: i,j
        integer :: pivotval

        !exchange pivot and last
        !call swapvals(array(hi),array(p))
        call DES_SWAPVALUES(array(hi),array(p))
        call SWAP_PARTICLE_DATA(hi,p)

        pivotval = array(hi)
        i=lo
        do j=lo,hi-1
                if(array(j) .lt. pivotval) then
                        !call swapvals(array(i),array(j))
                        call DES_SWAPVALUES(array(i),array(j))
                        call SWAP_PARTICLE_DATA(i,j)
                        i = i+1
                endif
        enddo

        !call swapvals(array(i),array(hi))
        call DES_SWAPVALUES(array(i),array(hi))
        call SWAP_PARTICLE_DATA(i,hi)

        p = i

end subroutine partition
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!                                                                     !
!  Subroutine: FINDMEDIAN                                             !
!      Author: Hari Sitaraman
!  Purpose: find the median element of an array                       !
!                                                                     !
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
subroutine findmedian(a1,a2,a3,pos1,pos2,pos3,midpos)

        implicit none
        integer, intent(in) :: a1,a2,a3,pos1,pos2,pos3
        integer, intent(out) :: midpos
        integer :: biggest,smallest

        biggest  = max(max(a1,a2),a3)
        smallest = min(min(a1,a2),a3)

        midpos = 0

        if((a1 .ne. biggest) .and. (a1 .ne. smallest)) then
                midpos=pos1
        endif

        if((a2 .ne. biggest) .and. (a2 .ne. smallest)) then
                midpos=pos2
        endif
        if((a3 .ne. biggest) .and. (a3 .ne. smallest)) then
                midpos=pos3
        endif

        !case of equal values
        !assigning it to first one
        if(midpos .eq. 0) then
                midpos=pos1
        endif

end subroutine findmedian
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

END MODULE DES_FUNCTIONS_MOD

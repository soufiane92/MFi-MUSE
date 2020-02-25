module monitor_average_mod

  implicit none

  private
  public :: monitor_average

contains

  !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
  !                                                                      !
  !  Author: J.Musser                                     Date: 11-2017  !
  !  Purpose: Write out point value in monitor region.                   !
  !                                                                      !
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  subroutine monitor_average(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    use monitor, only: monitor_type, monitor_var_count
    use param1,  only: zero, undefined
    use compar,  only: mype,pe_io

    use mpi_utility,       only: global_sum, global_all_sum
    use monitor_functions, only: monitor_collect_data

    integer,          intent(in   ) :: id, i_w, i_e, j_s, j_n, k_b, k_t
    double precision, intent(inout) :: data(:)

    integer :: lc1, var_count

    double precision, allocatable :: gvalues(:,:)
    double precision, allocatable :: lvalues(:,:)

    var_count = monitor_var_count(id)

    allocate(gvalues(var_count,2))
    allocate(lvalues(var_count,2))
    gvalues = zero
    lvalues = zero

    call monitor_collect_data(id, monitor_calc_avg, gvalues, &
         var_count, i_w, i_e, j_s, j_n, k_b, k_t)

    select case(monitor_type(id))

    case(0,1) ! value and sum
       !  0: Point value
       !  1: Sum over region

       call global_sum(gvalues(:,1), data)

    case(  4) ! average

       !  4: Arithmetic average over region
       call global_sum(gvalues, lvalues)

       if(mype == pe_io) then
          do lc1=1, monitor_var_count(id)
             if(lvalues(lc1,2) > 0.0d0) then
                data(lc1) = lvalues(lc1,1)/lvalues(lc1,2)
             else
                data(lc1) = -undefined
             endif
          enddo
       endif

    case(  5) ! Standard deviation

       !  5: Standard deviation over region
       call global_all_sum(gvalues)

       do lc1=1, monitor_var_count(id)
          if(gvalues(lc1,2) > 0.0d0) then
             gvalues(lc1,2) = gvalues(lc1,1)/gvalues(lc1,2)
          else
             gvalues(lc1,2) = -undefined
          endif
       enddo

       gvalues(:,1) = zero
       call monitor_collect_data(id, monitor_calc_std, gvalues, &
            var_count, i_w, i_e, j_s, j_n, k_b, k_t)

       call global_sum(gvalues, lvalues)

       if(mype == pe_io) then
          do lc1=1, monitor_var_count(id)
             if(lvalues(lc1,2) > 0.0d0) then
                data(lc1) = sqrt(lvalues(lc1,1)/lvalues(lc1,2))
             else
                data(lc1) = -undefined
             endif
          enddo
       endif

    case default
       data = undefined
    end select

    if(allocated(gvalues)) deallocate(gvalues)
    if(allocated(lvalues)) deallocate(lvalues)

  end subroutine monitor_average

  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_avg(lc, liw, lie, ljs, ljn, lkb, lkt, &
       lvar, ep, ro, u, v, w, lvalue, lsize)

    use functions, only: funijk, fluid_at
    use compar,    only: istart, jstart, kstart, iend, jend, kend
    use compar,    only: ijkstart3, ijkend3
    use param,     only: dim3 => dimension_3

    integer,          intent(inout) :: lc
    integer,          intent(in   ) :: liw, lie, ljs, ljn, lkt, lkb, lsize

    double precision, intent(in   ), dimension(dim3) :: lvar
    double precision, intent(in   ), dimension(dim3) :: ep, ro
    double precision, intent(in   ), dimension(dim3) :: u, v, w

    double precision, intent(inout) :: lvalue(lsize,2)

    integer :: i,j,k,ijk
    integer :: ilb, iub
    integer :: jlb, jub
    integer :: klb, kub

    ilb = max(istart, liw);    iub = min(lie, iend)
    jlb = max(jstart, ljs);    jub = min(ljn, jend)
    klb = max(kstart, lkb);    kub = min(lkt, kend)

    do k=klb, kub
       do i=ilb, iub
          do j=jlb, jub
             ijk = funijk(i,j,k)
             if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                ! flag error (debug)
             else if(fluid_at(ijk)) then
                lvalue(lc,1) = lvalue(lc,1) + lvar(ijk)
                lvalue(lc,2) = lvalue(lc,2) + 1.0d0
             endif
          enddo
       enddo
    enddo

    lc = lc+1
  end subroutine monitor_calc_avg

  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_std(lc, liw, lie, ljs, ljn, lkb, lkt, &
       lvar, ep, ro, u, v, w, lvalue, lsize)

    use functions, only: funijk, fluid_at
    use compar,    only: istart, jstart, kstart, iend, jend, kend
    use compar,    only: ijkstart3, ijkend3
    use param1,    only: undefined
    use param,     only: dim3 => dimension_3

    integer,          intent(inout) :: lc
    integer,          intent(in   ) :: liw, lie, ljs, ljn, lkt, lkb, lsize

    double precision, intent(in   ), dimension(dim3) :: lvar
    double precision, intent(in   ), dimension(dim3) :: ep, ro
    double precision, intent(in   ), dimension(dim3) :: u, v, w

    double precision, intent(inout) :: lvalue(lsize,2)

    double precision :: mean(lsize)

    integer :: i,j,k,ijk
    integer :: ilb, iub
    integer :: jlb, jub
    integer :: klb, kub

    ilb = max(istart, liw);    iub = min(lie, iend)
    jlb = max(jstart, ljs);    jub = min(ljn, jend)
    klb = max(kstart, lkb);    kub = min(lkt, kend)

    if(lvalue(lc,2) == -undefined) return

    mean = lvalue(:,2)
    lvalue(:,2) = 0.0d0

    do k=klb, kub
       do i=ilb, iub
          do j=jlb, jub
             ijk = funijk(i,j,k)
             if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                ! flag error (debug)
             else if(fluid_at(ijk)) then
                lvalue(lc,1) = lvalue(lc,1) + (lvar(ijk) - mean(lc))**2
                lvalue(lc,2) = lvalue(lc,2) + 1.0d0
             endif
          enddo
       enddo
    enddo
    lc = lc+1

  end subroutine monitor_calc_std

end module monitor_average_mod

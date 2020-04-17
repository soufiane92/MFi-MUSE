module monitor_minmax_mod

  implicit none

  private
  public :: monitor_minmax

contains

  !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
  !                                                                      !
  !  Author: J.Musser                                     Date: 11-2017  !
  !  Purpose: Write out point value in monitor region.                   !
  !                                                                      !
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  subroutine monitor_minmax(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    use monitor, only: monitor_type, monitor_var_count
    use param1,  only: undefined

    use mpi_utility,       only: global_min
    use mpi_utility,       only: global_max
    use monitor_functions, only: monitor_collect_data

    integer,          intent(in   ) :: id, i_w, i_e, j_s, j_n, k_b, k_t
    double precision, intent(inout) :: data(:)

    double precision, allocatable   :: gvalues(:,:)

    integer :: var_count

    var_count = monitor_var_count(id)
    allocate(gvalues(var_count,2))
    gvalues(:,1) =  undefined
    gvalues(:,2) = -undefined

    call monitor_collect_data(id, monitor_calc_minmax, gvalues, &
         var_count, i_w, i_e, j_s, j_n, k_b, k_t)

    if(monitor_type(id) == 2) then
       call global_min(gvalues(:,1), data)
    else
       call global_max(gvalues(:,2), data)
    endif

    if(allocated(gvalues)) deallocate(gvalues)

  end subroutine monitor_minmax

  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_minmax(lc, liw, lie, ljs, ljn, lkb, lkt, &
       lvar, ep, ro, u, v, w, lvalue, lsize)

    use functions, only: funijk, fluid_at
    use compar,    only: istart, jstart, kstart, iend, jend, kend
    use param,     only: dim3 => dimension_3
    use compar,    only: ijkstart3, ijkend3

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
                lvalue(lc,1) = min(lvalue(lc,1), lvar(ijk))
                lvalue(lc,2) = max(lvalue(lc,2), lvar(ijk))
             endif
          enddo
       enddo
    enddo
    lc = lc+1
  end subroutine monitor_calc_minmax

end module monitor_minmax_mod

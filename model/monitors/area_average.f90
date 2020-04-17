module monitor_area_avg_mod

  implicit none

  private
  public :: monitor_area_avg

contains

  !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
  !                                                                      !
  !  Author: J.Musser                                     Date: 11-2017  !
  !  Purpose: Write out point value in monitor region.                   !
  !                                                                      !
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  subroutine monitor_area_avg(id, data, i_w, i_e, j_s, j_n, k_b, k_t)

    use monitor, only: monitor_type, monitor_var_count
    use param1,  only: zero, undefined
    use compar,  only: mype,pe_io

    use mpi_utility,       only: global_sum
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

    select case(monitor_type(id))

    case(6) !  6: Area-weighted average over surface

       call monitor_collect_data(id, monitor_calc_area_avg, gvalues, &
            var_count, i_w, i_e, j_s, j_n, k_b, k_t)
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

    case(7) !  7: Flow rate across surface
       call monitor_collect_data(id, monitor_calc_flow_rate, gvalues, &
            var_count, i_w, i_e, j_s, j_n, k_b, k_t)

       call global_sum(gvalues(:,1), data)

    case(8) !  8: Mass flow rate across surface
       call monitor_collect_data(id, monitor_calc_flow_rate, gvalues, &
            var_count, i_w, i_e, j_s, j_n, k_b, k_t)

       call global_sum(gvalues(:,2), data)

    case(9) !  9: Mass-weighted average over surface
       call monitor_collect_data(id, monitor_calc_mass_avg, gvalues, &
            var_count, i_w, i_e, j_s, j_n, k_b, k_t)

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

    case(10) ! 10: Volumetric flow rate over surface
       call monitor_collect_data(id, monitor_calc_volflow_rate, gvalues, &
            var_count, i_w, i_e, j_s, j_n, k_b, k_t)

       call global_sum(gvalues(:,2), data) ! volume flow rate

    case default; data = undefined
    end select

    if(allocated(gvalues)) deallocate(gvalues)
    if(allocated(lvalues)) deallocate(lvalues)

  end subroutine monitor_area_avg

  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_area_avg(lc,liw,lie,ljs,ljn,lkb,lkt,&
       lvar, ep, ro, u, v, w, lvalue, lsize)

    use functions, only: funijk, fluid_at
    use compar,    only: istart, jstart, kstart, iend, jend, kend
    use compar,    only: ijkstart3, ijkend3
    use param,     only: dim3 => dimension_3

    use param1,    only: zero, undefined
    use geometry,  only: ayz, axz, axy

    integer,          intent(inout) :: lc
    integer,          intent(in   ) :: liw, lie, ljs, ljn, lkt, lkb, lsize

    double precision, intent(in   ), dimension(dim3) :: lvar
    double precision, intent(in   ), dimension(dim3) :: ep, ro
    double precision, intent(in   ), dimension(dim3) :: u, v, w

    double precision, intent(inout) :: lvalue(lsize,2)

    double precision :: weight

    integer :: i,j,k,ijk
    integer :: ilb, iub
    integer :: jlb, jub
    integer :: klb, kub

    ilb = max(istart, liw);    iub = min(lie, iend)
    jlb = max(jstart, ljs);    jub = min(ljn, jend)
    klb = max(kstart, lkb);    kub = min(lkt, kend)

    if(liw == lie) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   weight = abs(ayz(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + lvar(ijk)*weight
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    else if(ljs == ljn) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   weight = abs(axz(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + lvar(ijk)*weight
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    else if(lkb == lkt) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   weight = abs(axy(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + lvar(ijk)*weight
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    endif
    lc = lc+1
  end subroutine monitor_calc_area_avg

  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_flow_rate(lc,liw,lie,ljs,ljn,lkb,lkt,&
       lvar, ep, ro, u, v, w, lvalue, lsize)

    use functions, only: funijk, fluid_at
    use compar,    only: istart, jstart, kstart, iend, jend, kend
    use compar,    only: ijkstart3, ijkend3
    use param,     only: dim3 => dimension_3
    use functions, only: im_of, jm_of, km_of

    use param1,    only: zero, undefined
    use geometry,  only: ayz, axz, axy

    integer,          intent(inout) :: lc
    integer,          intent(in   ) :: liw, lie, ljs, ljn, lkt, lkb, lsize

    double precision, intent(in   ), dimension(dim3) :: lvar
    double precision, intent(in   ), dimension(dim3) :: ep, ro
    double precision, intent(in   ), dimension(dim3) :: u, v, w

    double precision, intent(inout) :: lvalue(lsize,2)

    double precision :: vel, weight

    integer :: i,j,k,ijk
    integer :: ilb, iub
    integer :: jlb, jub
    integer :: klb, kub

    ilb = max(istart, liw);    iub = min(lie, iend)
    jlb = max(jstart, ljs);    jub = min(ljn, jend)
    klb = max(kstart, lkb);    kub = min(lkt, kend)

    if(liw == lie) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   vel = 0.5d0*(u(ijk) + u(im_of(ijk)))
                   weight = ep(ijk)*ro(ijk)*vel*abs(ayz(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + weight*lvar(ijk)
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    else if(ljs == ljn) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   vel = 0.5d0*(v(ijk) + v(jm_of(ijk)))
                   weight = ep(ijk)*ro(ijk)*vel*abs(axz(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + weight*lvar(ijk)
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    else if(lkb == lkt) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   vel = 0.5d0*(w(ijk) + w(km_of(ijk)))
                   weight = ep(ijk)*ro(ijk)*vel*abs(axy(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + weight*lvar(ijk)
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    endif
    lc = lc+1
  end subroutine monitor_calc_flow_rate

  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_mass_avg(lc,liw,lie,ljs,ljn,lkb,lkt,&
       lvar, ep, ro, u, v, w, lvalue, lsize)

    use functions, only: funijk, fluid_at
    use compar,    only: istart, jstart, kstart, iend, jend, kend
    use compar,    only: ijkstart3, ijkend3
    use param,     only: dim3 => dimension_3
    use functions, only: im_of, jm_of, km_of

    use param1,    only: zero, undefined
    use geometry,  only: ayz, axz, axy

    integer,          intent(inout) :: lc
    integer,          intent(in   ) :: liw, lie, ljs, ljn, lkt, lkb, lsize

    double precision, intent(in   ), dimension(dim3) :: lvar
    double precision, intent(in   ), dimension(dim3) :: ep, ro
    double precision, intent(in   ), dimension(dim3) :: u, v, w

    double precision, intent(inout) :: lvalue(lsize,2)

    double precision :: vel, weight

    integer :: i,j,k,ijk
    integer :: ilb, iub
    integer :: jlb, jub
    integer :: klb, kub

    ilb = max(istart, liw);    iub = min(lie, iend)
    jlb = max(jstart, ljs);    jub = min(ljn, jend)
    klb = max(kstart, lkb);    kub = min(lkt, kend)

    if(liw == lie) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   vel = 0.5d0*(u(ijk) + u(im_of(ijk)))
                   weight = ep(ijk)*ro(ijk)*abs(vel*ayz(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + weight*lvar(ijk)
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    else if(ljs == ljn) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   vel = 0.5d0*(v(ijk) + v(jm_of(ijk)))
                   weight = ep(ijk)*ro(ijk)*abs(vel*axz(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + weight*lvar(ijk)
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    else if(lkb == lkt) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   vel = 0.5d0*(w(ijk) + w(km_of(ijk)))
                   weight = ep(ijk)*ro(ijk)*abs(vel*axy(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + weight*lvar(ijk)
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    endif
    lc = lc+1
  end subroutine monitor_calc_mass_avg


  !....................................................................!
  !                                                                    !
  ! Copy value from field array to io array. Do some checks to make    !
  ! sure you will not read/write out of array space.                   !
  !                                                                    !
  !....................................................................!
  subroutine monitor_calc_volflow_rate(lc,liw,lie,ljs,ljn,lkb,lkt,&
       lvar, ep, ro, u, v, w, lvalue, lsize)

    use functions, only: funijk, fluid_at
    use compar,    only: istart, jstart, kstart, iend, jend, kend
    use compar,    only: ijkstart3, ijkend3
    use param,     only: dim3 => dimension_3
    use functions, only: im_of, jm_of, km_of

    use param1,    only: zero, undefined
    use geometry,  only: ayz, axz, axy

    integer,          intent(inout) :: lc
    integer,          intent(in   ) :: liw, lie, ljs, ljn, lkt, lkb, lsize

    double precision, intent(in   ), dimension(dim3) :: lvar
    double precision, intent(in   ), dimension(dim3) :: ep, ro
    double precision, intent(in   ), dimension(dim3) :: u, v, w

    double precision, intent(inout) :: lvalue(lsize,2)

    double precision :: vel, weight

    integer :: i,j,k,ijk
    integer :: ilb, iub
    integer :: jlb, jub
    integer :: klb, kub

    ilb = max(istart, liw);    iub = min(lie, iend)
    jlb = max(jstart, ljs);    jub = min(ljn, jend)
    klb = max(kstart, lkb);    kub = min(lkt, kend)

    if(liw == lie) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   vel = 0.5d0*(u(ijk) + u(im_of(ijk)))
                   weight = ep(ijk)*vel*abs(ayz(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + weight*lvar(ijk)
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    else if(ljs == ljn) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   vel = 0.5d0*(v(ijk) + v(jm_of(ijk)))
                   weight = ep(ijk)*vel*abs(axz(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + weight*lvar(ijk)
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    else if(lkb == lkt) then
       do k=klb, kub
          do i=ilb, iub
             do j=jlb, jub
                ijk = funijk(i,j,k)
                if(ijk < ijkstart3 .or. ijkend3 < ijk) then
                   ! flag error (debug)
                else if(fluid_at(ijk)) then
                   vel = 0.5d0*(w(ijk) + w(km_of(ijk)))
                   weight = ep(ijk)*vel*abs(axy(ijk))
                   lvalue(lc,1) = lvalue(lc,1) + weight*lvar(ijk)
                   lvalue(lc,2) = lvalue(lc,2) + weight
                endif
             enddo
          enddo
       enddo

    endif
    lc = lc+1
  end subroutine monitor_calc_volflow_rate
end module monitor_area_avg_mod

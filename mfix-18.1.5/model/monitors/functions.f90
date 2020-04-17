module monitor_functions

  implicit none

  private

  public :: monitor_open_file
  public :: monitor_write_header
  public :: monitor_write_values
  public :: monitor_collect_data

contains

  !--------------------------------------------------------------------!
  ! Function: write_header                                             !
  !                                                                    !
  ! Purpose: Open the ASCII monitor file and return the file unit.     !
  !                                                                    !
  !--------------------------------------------------------------------!
  subroutine monitor_open_file(lid, lunit)

    use param1, only: undefined_c
    use funits, only: newunit

    use monitor, only: monitor_name

    ! Local ID of monitor
    integer, intent(in   ) :: lid
    integer, intent(  out) :: lunit

    ! monitor ID converted to a 4 char string
    character(len=3)   :: clid
    character(len=512) :: fname
    logical :: exists

    fName = ''
    lunit = newunit()

    if(monitor_name(lid) /= undefined_c) then
       fname = trim(adjustl(monitor_name(lid)))//'.csv'
    else
       write(clid,"(i3.3)") lid
       fname = 'monitor_'//clid//'.csv'
    endif

    inquire(file=trim(fname),exist=exists)

    if(exists) then
       open(unit=lunit, file=fname, position='append', status='old')
    else
       open(unit=lunit, file=trim(fname), status='new')
    endif
    return
  end subroutine monitor_open_file

  !--------------------------------------------------------------------!
  ! Function: write_monitor_header                                     !
  !                                                                    !
  ! Purpose: Write variable names in file header.                      !
  !                                                                    !
  !--------------------------------------------------------------------!
  subroutine monitor_write_header(lid, lunit)

    use param1, only: undefined_c
    use monitor, only: monitor_var_count
    use monitor, only: monitor_ep_g, monitor_p_g, monitor_u_g, monitor_v_g, &
         monitor_w_g, monitor_t_g, monitor_x_g, monitor_k_turb_g, monitor_e_turb_g, &
         monitor_ep_s, monitor_rop_s, monitor_p_star, monitor_u_s, monitor_v_s, &
         monitor_w_s, monitor_t_s, monitor_x_s, monitor_theta_m, monitor_scalar, &
         monitor_rrate, monitor_nrrmax, monitor_ro_g, monitor_ro_s

    use rxns,     only: nrr
    use scalars,  only: nscalar
    USE physprop, only: mmax, nmax
    use run, only: reinitializing, run_type

    use error_manager, only: ivar


    integer, intent(in   ) :: lunit, lid

    integer :: ios, fcnt, lc1, lc2
    character(len=1024) :: input
    character(len=24)   :: name
    character(len=1), parameter :: quote = '"'

    write(lunit,"('# ')")
    if(reinitializing) then
       write(lunit,"('# Reinitialized')")
    else
       write(lunit,"('# Run type: ',a)") trim(run_type)
    endif

    ! Write File header
    write(name,"('# ',A)") quote//'Time'//quote
    write(lunit,"(A)",advance='no') trim(name)

    if(monitor_ep_g(lid)) then
       write(name,"(',',A)") quote//'ep_g'//quote
       write(lunit,"(A)",advance='no') trim(name)
    endif

    if(monitor_ro_g(lid)) then
       write(name,"(',',A)") quote//'ro_g'//quote
       write(lunit,"(A)",advance='no') trim(name)
    endif

    if(monitor_p_g (lid)) then
       write(name,"(',',A)") quote//'p_g'//quote
       write(lunit,"(A)",advance='no') trim(name)
    endif

    if(monitor_u_g (lid)) then
       write(name,"(',',A)") quote//'u_g'//quote
       write(lunit,"(A)",advance='no') trim(name)
    endif

    if(monitor_v_g (lid)) then
       write(name,"(',',A)") quote//'v_g'//quote
       write(lunit,"(A)",advance='no') trim(name)
    endif

    if(monitor_w_g (lid)) then
       write(name,"(',',A)") quote//'w_g'//quote
       write(lunit,"(A)",advance='no') trim(name)
    endif

    if(monitor_t_g (lid)) then
       write(name,"(',',A)") quote//'t_g'//quote
       write(lunit,"(A)",advance='no') trim(name)
    endif

    do lc1 = 1, nmax(0)
       if(monitor_x_g(lid,lc1)) then
          write(name,"(',',A)") quote//trim(ivar('x_g',lc1))//quote
          write(lunit,"(A)",advance='no') trim(name)
       endif
    enddo

    if(monitor_k_turb_g(lid)) then
       write(name,"(',',A)") quote//'k_turb_g'//quote
       write(lunit,"(A)",advance='no') trim(name)
    endif
    if(monitor_e_turb_g(lid)) then
       write(name,"(',',A)") quote//'e_turb_g'//quote
       write(lunit,"(A)",advance='no') trim(name)
    endif

    if(monitor_p_star(lid)) then
       write(name,"(',',A)") quote//'p_star'//quote
       write(lunit,"(A)",advance='no') trim(name)
    endif

    do lc1 = 1, mmax

       if(monitor_ep_s(lid,lc1)) then
          name=''
          write(name,"(',',A)") quote//trim(ivar('ep_s',lc1))//quote
          write(lunit,"(A)",advance='no') trim(name)
       endif

       if(monitor_rop_s(lid,lc1)) then
          name=''
          write(name,"(',',A)") quote//trim(ivar('rop_s',lc1))//quote
          write(lunit,"(A)",advance='no') trim(name)
       endif

       if(monitor_ro_s(lid,lc1)) then
          name=''
          write(name,"(',',A)") quote//trim(ivar('ro_s',lc1))//quote
          write(lunit,"(A)",advance='no') trim(name)
       endif

       if(monitor_u_s(lid,lc1)) then
          name=''
          write(name,"(',',A)") quote//trim(ivar('u_s',lc1))//quote
          write(lunit,"(A)",advance='no') trim(name)
       endif

       if(monitor_v_s(lid,lc1)) then
          name=''
          write(name,"(',',A)") quote//trim(ivar('v_s',lc1))//quote
          write(lunit,"(A)",advance='no') trim(name)
       endif

       if(monitor_w_s(lid,lc1)) then
          name=''
          write(name,"(',',A)") quote//trim(ivar('w_s',lc1))//quote
          write(lunit,"(A)",advance='no') trim(name)
       endif

       if(monitor_t_s(lid,lc1)) then
          name=''
          write(name,"(',',A)") quote//trim(ivar('t_s',lc1))//quote
          write(lunit,"(A)",advance='no') trim(name)
       endif

       if(monitor_theta_m(lid,lc1)) then
          name=''
          write(name,"(',',A)") quote//trim(ivar('theta_m',lc1))//quote
          write(lunit,"(A)",advance='no') trim(name)
       endif

       do lc2 = 1, nmax(lc1)
          if(monitor_x_s(lid,lc1,lc2)) then
             name=''
             write(name,"(',',A)") quote//trim(ivar('x_s',lc1,lc2))//quote
             write(lunit,"(A)",advance='no') trim(name)
          endif
       enddo
    enddo

    do lc1 = 1, nscalar
       if(monitor_scalar(lid,lc1)) then
          name=''
          write(name,"(',',A)") quote//trim(ivar('scalar',lc1))//quote
          write(lunit,"(A)",advance='no') trim(name)
       endif
    enddo

    do lc1 = 1, min(monitor_nrrmax,nrr)
       if(monitor_rrate(lid,lc1)) then
          name=''
          write(name,"(',rrate(',i2,')')") lc1
          write(name,"(',',A)") quote//trim(ivar('rrate',lc1))//quote
       endif
    enddo
    write(lunit,"(' ')",advance='yes')

  end subroutine monitor_write_header


  !--------------------------------------------------------------------!
  ! Function: write_monitor_header                                     !
  !                                                                    !
  ! Purpose: Write variable names in file header.                      !
  !                                                                    !
  !--------------------------------------------------------------------!
  subroutine monitor_write_values(lunit, lvalues)

    USE run, only: time

    integer,          intent(in   ) :: lunit
    double precision, intent(in   ) :: lvalues(:)

    integer :: lc1, cnt

    cnt = size(lvalues)

    write(lunit,"(2x,g15.8)",advance='no') time
    do lc1=1,cnt-1
       write(lunit,"(',',2x,g15.8)",advance='no') lvalues(lc1)
    enddo
    write(lunit,"(',',2x,g15.8)") lvalues(cnt)

  end subroutine monitor_write_values



  !....................................................................!
  !                                                                    !
  ! Populate *values* array with correct field variable data.          !
  !                                                                    !
  !....................................................................!
  subroutine monitor_collect_data(lid, get_value, lvalues, lsize, &
       iw, ie, js, jn, kb, kt)

    use fldvar, only: ep_g, ro_g, u_g, v_g, w_g
    use fldvar, only: ep_s, ro_s, u_s, v_s, w_s
    use fldvar, only: p_g, t_g, x_g, rop_s, p_star, t_s, x_s, theta_m
    use fldvar, only: k_turb_g, e_turb_g, scalar

    use monitor, only: monitor_ep_g, monitor_p_g, monitor_u_g, monitor_v_g, &
         monitor_w_g, monitor_t_g, monitor_x_g, monitor_k_turb_g, monitor_e_turb_g, &
         monitor_ep_s, monitor_rop_s, monitor_p_star, monitor_u_s, monitor_v_s, &
         monitor_w_s, monitor_t_s, monitor_x_s, monitor_theta_m, monitor_scalar, &
         monitor_rrate, monitor_nrrmax, monitor_ro_g, monitor_ro_s

    use rxns,     only: nrr, reactionrates
    use scalars,  only: nscalar, phase4scalar
    USE physprop, only: mmax, nmax
    use compar,   only: ijkstart3, ijkend3

    integer,          intent(in   ) :: lid, lsize, iw, ie, js, jn, kb, kt
    double precision, intent(inout) :: lvalues(lsize,2)

    integer :: lc, lc1, lc2, lc3
    double precision, allocatable :: eps(:)

    lc = 1
    if(monitor_ep_g(lid)) call get_value(lc, iw, ie, js, jn, kb, kt, ep_g, &
         ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)

    if(monitor_ro_g(lid)) call get_value(lc,iw,ie,js,jn,kb,kt, ro_g, &
         ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)

   if(monitor_p_g (lid)) call get_value(lc,iw,ie,js,jn,kb,kt, p_g, &
         ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)

    if(monitor_u_g (lid)) call get_value(lc,iw,ie,js,jn,kb,kt, u_g, &
         ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)

    if(monitor_v_g (lid)) call get_value(lc,iw,ie,js,jn,kb,kt, v_g, &
         ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)

    if(monitor_w_g (lid)) call get_value(lc,iw,ie,js,jn,kb,kt, w_g, &
         ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)

    if(monitor_t_g (lid)) call get_value(lc,iw,ie,js,jn,kb,kt, t_g, &
         ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)

    do lc1 = 1, nmax(0)
       if(monitor_x_g(lid,lc1)) call get_value(lc,iw,ie,js,jn,kb,kt,x_g(:,lc1), &
            ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)
    enddo

    if(monitor_k_turb_g(lid)) call get_value(lc,iw,ie,js,jn,kb,kt, k_turb_g, &
         ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)

    if(monitor_e_turb_g(lid)) call get_value(lc,iw,ie,js,jn,kb,kt, e_turb_g, &
         ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)

    if(monitor_p_star(lid)) call get_value(lc,iw,ie,js,jn,kb,kt, p_star, &
         ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)

    if(mmax > 0) allocate(eps(ijkstart3:ijkend3))

    do lc1 = 1, mmax
       ! ep_s is a function so copy to local array.
       do lc2=ijkstart3, ijkend3
          eps(lc2) = ep_s(lc2,lc1)
       enddo

       if(monitor_ep_s(lid,lc1)) call get_value(lc,iw,ie,js,jn,kb,kt, eps, &
            eps, ro_s(:,lc1), u_s(:,lc1), v_s(:,lc1), w_s(:,lc1), lvalues, lsize)

       if(monitor_rop_s(lid,lc1)) call get_value(lc,iw,ie,js,jn,kb,kt, rop_s(:,lc1), &
            eps, ro_s(:,lc1), u_s(:,lc1), v_s(:,lc1), w_s(:,lc1), lvalues, lsize)

       if(monitor_ro_s(lid,lc1)) call get_value(lc,iw,ie,js,jn,kb,kt, ro_s(:,lc1), &
            eps, ro_s(:,lc1), u_s(:,lc1), v_s(:,lc1), w_s(:,lc1), lvalues, lsize)

       if(monitor_u_s(lid,lc1)) call get_value(lc,iw,ie,js,jn,kb,kt, u_s(:,lc1), &
            eps, ro_s(:,lc1), u_s(:,lc1), v_s(:,lc1), w_s(:,lc1), lvalues, lsize)

       if(monitor_v_s(lid,lc1)) call get_value(lc,iw,ie,js,jn,kb,kt, v_s(:,lc1), &
            eps, ro_s(:,lc1), u_s(:,lc1), v_s(:,lc1), w_s(:,lc1), lvalues, lsize)

       if(monitor_w_s(lid,lc1)) call get_value(lc,iw,ie,js,jn,kb,kt, w_s(:,lc1), &
            eps, ro_s(:,lc1), u_s(:,lc1), v_s(:,lc1), w_s(:,lc1), lvalues, lsize)

       if(monitor_t_s(lid,lc1)) call get_value(lc,iw,ie,js,jn,kb,kt, t_s(:,lc1), &
            eps, ro_s(:,lc1), u_s(:,lc1), v_s(:,lc1), w_s(:,lc1), lvalues, lsize)

       if(monitor_theta_m(lid,lc1)) call get_value(lc,iw,ie,js,jn,kb,kt, theta_m(:,lc1), &
            eps, ro_s(:,lc1), u_s(:,lc1), v_s(:,lc1), w_s(:,lc1), lvalues, lsize)

       do lc2 = 1, nmax(lc1)
          if(monitor_x_s(lid,lc1,lc2)) call get_value(lc,iw,ie,js,jn,kb,kt, x_s(:,lc1,lc2), &
               eps, ro_s(:,lc1), u_s(:,lc1), v_s(:,lc1), w_s(:,lc1), lvalues, lsize)
       enddo
    enddo
    if(allocated(eps)) deallocate(eps)

    do lc1 = 1, nscalar
       if(monitor_scalar(lid,lc1)) then
          if(phase4scalar(lc1) == 0) then
             call get_value(lc, iw, ie, js, jn, kb, kt, scalar(:,lc1),&
                  ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)
          else
             lc2 = phase4scalar(lc1)
             allocate(eps(ijkstart3:ijkend3))
             do lc3=ijkstart3, ijkend3
                eps(lc3) = ep_s(lc3,lc2)
             enddo
             call get_value(lc, iw, ie, js, jn, kb, kt, scalar(:,lc1), &
                  eps, ro_s(:,lc2), u_s(:,lc2), v_s(:,lc2), w_s(:,lc2), lvalues, lsize)
             if(allocated(eps)) deallocate(eps)
          endif
       endif
    enddo

    do lc1 = 1, min(monitor_nrrmax,nrr)
       if(monitor_rrate(lid,lc1)) call get_value(lc,iw,ie,js,jn,kb,kt,reactionrates(:,lc1),&
            ep_g, ro_g, u_g, v_g, w_g, lvalues, lsize)
    enddo

  end subroutine monitor_collect_data


end module monitor_functions

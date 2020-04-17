module crow
   use iso_c_binding
contains

#ifdef CROW

   function c_f_string(c_str) result(f_str)
      use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_char
      type(c_ptr), intent(in) :: c_str
      character(:,kind=c_char), pointer :: f_str
      character(kind=c_char), pointer :: arr(:)
      interface
         function strlen(s) bind(c, name='strlen')
            use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
            implicit none

            type(c_ptr), intent(in), value :: s
            integer(c_size_t) :: strlen
         end function strlen
      end interface

      call c_f_pointer(c_str, arr, [strlen(c_str)])
      call get_scalar_pointer(size(arr), arr, f_str)
   end function c_f_string

   subroutine get_scalar_pointer(scalar_len, scalar, ptr)
      integer, intent(in) :: scalar_len
      character(kind=c_char,len=scalar_len), intent(in), target :: scalar(1)
      character(:,kind=c_char), intent(out), pointer :: ptr
      ptr => scalar(1)
   end subroutine get_scalar_pointer

   subroutine f_c_string_func (c_string, f_string)
      implicit none
      character(len=*), intent(in) :: f_string
      character(len=1,kind=c_char) :: c_string(len_trim(f_string)+1)
      integer                      :: n, i

      n = len_trim(f_string)
      do i = 1, n
         c_string(i) = f_string(i:i)
      end do
      c_string(n + 1) = c_null_char

   end subroutine f_c_string_func

   function get_nit() result(ret) bind (c, name="mfix_nit")
      use iterate, only: nit
      integer(kind=c_int) :: ret
      ret = nit
   end function get_nit

   function get_time() result(ret) bind (c, name="mfix_time")
      use run, only: time
      real(kind=c_double) :: ret
      ret = time
   end function get_time

   function get_tstop() result(ret) bind (c, name="mfix_tstop")
      use run, only: tstop
      real(kind=c_double) :: ret
      ret = tstop
   end function get_tstop

   function get_dt() result(ret) bind (c, name="mfix_dt")
      use run, only: dt
      real(kind=c_double) :: ret
      ret = dt
   end function get_dt

   function get_mype() result(ret) bind (c, name="mfix_mype")
      use compar, only: mype
      integer(kind=c_int) :: ret
      ret = mype
   end function get_mype

   subroutine set_exit_flag(val) bind (c, name="mfix_set_exit_flag")
      use exit, only: exit_flag
      logical(kind=c_bool), value, intent(in) :: val
      exit_flag = val
   end subroutine set_exit_flag

   subroutine set_pause_flag(val) bind (c, name="mfix_set_pause_flag")
     use pause, only: pause_flag
     logical(kind=c_bool), value, intent(in) :: val
     pause_flag = val
   end subroutine set_pause_flag

   subroutine set_autostart_flag(val) bind (c, name="mfix_set_autostart_flag")
     use pause, only: autostart_flag
     logical(kind=c_bool), value, intent(in) :: val
     autostart_flag = val
   end subroutine set_autostart_flag

   subroutine set_reinit_flag(val) bind (c, name="mfix_set_reinit_flag")
     use pause, only: reinit_flag
     logical(kind=c_bool), value, intent(in) :: val
     reinit_flag = val
   end subroutine set_reinit_flag

   function get_wall_time() result(ret) bind (c, name="mfix_wall_time")
     use machine, only: wall_time
     real(kind=c_double) :: ret
     ret = wall_time()
   end function get_wall_time

   function get_wall_start() result(ret) bind (c, name="mfix_wall_start")
     use time_cpu, only: wall_start
     real(kind=c_double) :: ret
     ret = wall_start
   end function get_wall_start

   function get_time_start() result(ret) bind (c, name="mfix_time_start")
     use time_cpu, only: time_start
     real(kind=c_double) :: ret
     ret = time_start
   end function get_time_start

   function get_cpu0() result(ret) bind (c, name="mfix_cpu0")
     use time_cpu, only: cpu0
     real(kind=c_double) :: ret
     ret = cpu0
   end function get_cpu0

   function get_wall0() result(ret) bind (c, name="mfix_wall0")
     use time_cpu, only: wall0
     real(kind=c_double) :: ret
     ret = wall0
   end function get_wall0

   function get_pause_flag() result(ret) bind (c, name="mfix_pause_flag")
     use pause, only: pause_flag
     logical(kind=c_bool) :: ret
     ret = pause_flag
   end function get_pause_flag

   function get_exit_flag() result(ret) bind (c, name="mfix_exit_flag")
     use exit, only: exit_flag
     logical(kind=c_bool) :: ret
     ret = exit_flag
   end function get_exit_flag

   function resid_grp_count() result(ret) bind (c, name="mfix_resid_grp_string_count")
     use residual_pub, only: get_resid_grp_string_count
     integer(kind=c_int) :: ret
     ret = get_resid_grp_string_count()
   end function resid_grp_count

   function resid_count() result(ret) bind (c, name="mfix_resid_string_count")
     use residual_pub, only: get_resid_string_count
     integer(kind=c_int) :: ret
     ret = get_resid_string_count()
   end function resid_count

   subroutine resid_grp_s(i, rg) bind (c, name="mfix_resid_grp_string")
     use residual_pub, only: resid_grp_string
     integer(kind=c_int), value :: i
     character(c_char), dimension(8), intent(out) :: rg

     call f_c_string_func(rg, resid_grp_string(i))
   end subroutine resid_grp_s

   subroutine resid_s(i, rs) bind (c, name="mfix_resid_string")
     use residual_pub, only: resid_string
     integer(kind=c_int), value :: i
     character(c_char), dimension(4), intent(out) :: rs

     call f_c_string_func(rs, resid_string(i))
   end subroutine resid_s

   function resid_grp(i) result(rs) bind (c, name="mfix_resid_grp")
     use residual_pub, only: get_resid_grp
     integer(kind=c_int), value :: i
     real(kind=c_double) :: rs
     rs = get_resid_grp(i)
   end function resid_grp

   function resid(i) result(rs) bind (c, name="mfix_resid")
     use residual_pub, only: get_resid
     integer(kind=c_int), value :: i
     real(kind=c_double) :: rs
     rs = get_resid(i)
   end function resid

   function groupresid() result(gr) bind (c, name="mfix_group_resid")
     use residual_pub, only: group_resid
     logical(kind=c_bool) :: gr
     gr = group_resid
   end function groupresid

   subroutine set_reinit(mfix_dat_c) bind (C, name="mfix_set_reinit_data")
     use pause, only: set_reinit_data
     implicit none

     type(c_ptr), value, intent(in) :: mfix_dat_c
     character(len=100000) :: mfix_dat

     mfix_dat = c_f_string(mfix_dat_c)
     call set_reinit_data(mfix_dat)
   end subroutine set_reinit

#endif

end module crow

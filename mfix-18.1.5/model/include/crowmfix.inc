   interface

! should match crow_init() in crowmfix.h
      subroutine crow_init(run_name_c, port_c, init_paused, init_clean, init_daemon) bind (c, name="crow_init")
         use iso_c_binding;
         CHARACTER(KIND=C_CHAR), intent(in) :: run_name_c(*)
         CHARACTER(KIND=C_CHAR), intent(in) :: port_c(*)
         logical(kind=c_bool), intent(in), VALUE :: init_paused
         logical(kind=c_bool), intent(in), VALUE :: init_clean
         logical(kind=c_bool), intent(in), VALUE :: init_daemon
      end subroutine crow_init

! should match crow_shutdown() in crowmfix.h
      subroutine crow_shutdown() bind (c, name="crow_shutdown")
         use iso_c_binding;
      end subroutine crow_shutdown

   end interface

      MODULE usr
!
!       Declare the user-defined namelist variables (usr_read_namelist.f) in this module.
!       Also Include user-defined variables in this module.  To access the
!       variables from a subroutine add the statement "Use usr".  If allocatable
!       arrays are defined in this module allocate them in usr0.  To turn on the
!       user defined subroutines (usr0, usr1, and usr2) set call_usr to true in
!       mfix.dat.

        ! Array for keeping a running average of particle-wall heat flux
        DOUBLE PRECISION, DIMENSION (:,:), ALLOCATABLE :: DES_QwFlux_AVG
        ! Value that stores how long the running averaging window is
        DOUBLE PRECISION :: QW_SAMPLE_TIME
      END MODULE usr

! GNU Fortran
#if defined(__GFORTRAN__)

#if ( __GNUC__ > 4 || ( __GNUC__ == 4 && __GNUC_MINOR__ > 5 ) )

! supports Fortran 2008
#define ERROR_STOP ERROR STOP

#else

! does not support Fortran 2008
#define ERROR_STOP STOP
#define EXECUTE_COMMAND_LINE SYSTEM

#endif

! Intel Fortran
#elif ( defined(__INTEL_COMPILER) )

#if (__INTEL_COMPILER > 1400 && __INTEL_COMPILER != 9999)

! supports Fortran 2008
#define ERROR_STOP ERROR STOP

#else

! does not support Fortran 2008
#define ERROR_STOP STOP
#define EXECUTE_COMMAND_LINE SYSTEM
#define c_sizeof sizeof

#endif

#else

! For all other compilers, do not assume Fortran 2008 is supported
#define ERROR_STOP STOP
#define EXECUTE_COMMAND_LINE SYSTEM
#define c_sizeof sizeof

#endif

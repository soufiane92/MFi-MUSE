cmake_minimum_required(VERSION 3.4)

# remove OpenMP flags from environment (only wanted if ENABLE_OpenMP not set)
# remove -ftree-vectorize (causes failure in leq_bicgs.f)
foreach(flag -fopenmp -ftree-vectorize)
  string(REPLACE ${flag} "" tmp "$ENV{DEBUG_FFLAGS}")
  set(ENV{DEBUG_FFLAGS} ${tmp})
  string(REPLACE ${flag} "" tmp "$ENV{DEBUG_FORTRANFLAGS}")
  set(ENV{DEBUG_FORTRANFLAGS} ${tmp})
  string(REPLACE ${flag} "" tmp "$ENV{FFLAGS}")
  set(ENV{FFLAGS} ${tmp})
  string(REPLACE ${flag} "" tmp "$ENV{FORTRANFLAGS}")
  set(ENV{FORTRANFLAGS} ${tmp})
endforeach()

option(ENABLE_MPI "Build with MPI (dmp) support" OFF)
option(ENABLE_OpenMP "Build with OpenMP (smp) support" OFF)
option(ENABLE_CROW "Build with crow interactive support" OFF)
option(ENABLE_NETCDF "Build with NetCDF support" OFF)
option(ENABLE_COVERAGE "Build with gcov/lcov code coverage" OFF)
option(ENABLE_CTEST "Enable building MFiX CTest Suite" OFF)
if(DEFINED ENV{AR} AND NOT WIN32)
  set(CMAKE_AR "$ENV{AR}" CACHE FILEPATH "Archiver")
endif()

###########################
# Configure Compiler flags
###########################

# Default for build type
if(NOT(CMAKE_BUILD_TYPE OR CMAKE_Fortran_FLAGS))
  set( CMAKE_BUILD_TYPE "RelWithDebInfo"
  CACHE STRING "Set CMAKE_BUILD_TYPE: Debug Release RelWithDebInfo MinSizeRel"
  FORCE )
endif()

if(ENABLE_MPI)
  if(MPI_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER ${MPI_Fortran_COMPILER})
  else()
    message(FATAL_ERROR "ENABLE_MPI is set but the compiler [wrapper] is not explicitly specified. Set MPI_Fortran_COMPILER to your MPI wrapper (mpifort, mpiifort, etc.)")
  endif()
endif()

if(ENABLE_CROW)
  set(languages CXX Fortran)
elseif(ENABLE_PYMFIX)
  set(languages C Fortran)
else()
  set(languages Fortran)
endif()


set( VERSION_FILE ${CMAKE_CURRENT_SOURCE_DIR}/.tarball-version )
if(VERSION)
  # running from build_mfixsolver (either source or package)
  set(MFIX_VERSION ${VERSION})

elseif(EXISTS ${VERSION_FILE})
  # running cmake directly from source tarball
  file(READ ${VERSION_FILE} VERSION_FILE_VERSION)
  set(MFIX_VERSION ${VERSION_FILE_VERSION})

elseif(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  # running cmake directly from a git repo
  find_package(Git)
  if(Git_FOUND)
    execute_process(COMMAND ${GIT_EXECUTABLE} describe --abbrev=0
      OUTPUT_VARIABLE GIT_REPO_VERSION
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      )
    string(STRIP "${GIT_REPO_VERSION}" GIT_REPO_VERSION)
    set(MFIX_VERSION "${GIT_REPO_VERSION}")

  else()
    message(WARNING "Directory .git but git is not installed, unknown CMake version")
    set(MFIX_VERSION 0.0)
  endif()

else()
  message(WARNING "Unknown CMake version")
  set(MFIX_VERSION 0.0)

endif()

project(MFiX
  LANGUAGES ${languages}
  )

if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
    # require at least gcc 4.6
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.6)
        message(FATAL_ERROR "GFortran version ${CMAKE_Fortran_COMPILER_VERSION} found. MFiX requires GFortran 4.6 or later.")
    endif()
endif()

message( STATUS "MFIX build settings summary: " )
message( STATUS "   Build type        = ${CMAKE_BUILD_TYPE}" )
message( STATUS "   CMake version     = ${CMAKE_VERSION}" )
message( STATUS "   Fortran compiler  = ${CMAKE_Fortran_COMPILER}" )
if(CMAKE_BUILD_TYPE)
  string(TOUPPER ${CMAKE_BUILD_TYPE} BUILDTYPE)
  message( STATUS "   Fortran flags     = ${CMAKE_Fortran_FLAGS_${BUILDTYPE}}" )
else()
  message( STATUS "   Fortran flags     = ${CMAKE_Fortran_FLAGS}" )
endif()
message( STATUS "   ENABLE_MPI        = ${ENABLE_MPI}" )
message( STATUS "   ENABLE_OpenMP     = ${ENABLE_OpenMP}" )
message( STATUS "   ENABLE_NETCDF     = ${ENABLE_NETCDF}" )
message( STATUS "   ENABLE_CTEST      = ${ENABLE_CTEST}" )
message( STATUS "   ENABLE_COVERAGE   = ${ENABLE_COVERAGE}" )

# Load Code Coverage module
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/build-aux/cmake)
if(ENABLE_COVERAGE)
    include(CodeCoverage)
endif()

############################################################
# Static library: target udfs (User-defined functions)
############################################################

if(NOT UDF_DIR)
  # not building from build_mfixsolver
  set(UDF_DIR ${CMAKE_CURRENT_BINARY_DIR})
endif()

# On Windows change slashes: \ to /
file(TO_CMAKE_PATH ${UDF_DIR} UDF_DIR)

if(EXISTS ${UDF_DIR}/usrnlst.inc)
  file(COPY ${PROJECT_SOURCE_DIR}/model/usr_read_namelist.f
    DESTINATION ${UDF_DIR})
endif()

file(GLOB USR_OVERRIDES
  ${UDF_DIR}/*.f
  ${UDF_DIR}/des/*.f
)

if(USR_OVERRIDES)

  set_source_files_properties(
    ${USR_OVERRIDES} PROPERTIES
    Fortran_FORMAT "FREE"
    )

  if((EXISTS ${UDF_DIR}/usr_rates.f) OR
      (EXISTS ${UDF_DIR}/usr_rates_des.f))

    file( GLOB mfxs ${UDF_DIR}/*.mfx )

    if(EXISTS ${UDF_DIR}/mfix.dat)
      set(PROJECT_FILE ${UDF_DIR}/mfix.dat)
    elseif(EXISTS ${mfxs})
      get_filename_component(PROJECT_FILE ${mfxs} REALPATH)
    else()
      message( FATAL_ERROR "\
    usr_rates.f or usr_rates_des.f exists, \
    but no project file: mfix.dat nor *.mfx" )
    endif()

    set(SPECIES_INC ${CMAKE_BINARY_DIR}/species.inc)

    find_program(PYTHON python)
    set(RXN_PREPROC_PY ${CMAKE_CURRENT_SOURCE_DIR}/build-aux/rxn_preproc.py)
    add_custom_command(OUTPUT ${SPECIES_INC}
      COMMAND ${CMAKE_COMMAND} -E env RUN_DIR=${UDF_DIR} ${PYTHON} ${RXN_PREPROC_PY} ${PROJECT_FILE}
      WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
      DEPENDS ${CMAKE_BINARY_DIR}
      COMMENT "Generating species.inc")

    set(USR_OVERRIDES ${USR_OVERRIDES} ${SPECIES_INC})

  endif()

  add_library(udfs OBJECT
    ${USR_OVERRIDES}
    )
  target_compile_definitions( udfs PUBLIC USR_NAMELIST )
  set_property(TARGET udfs PROPERTY POSITION_INDEPENDENT_CODE ON)

  # for species.inc
  target_include_directories(udfs PUBLIC ${CMAKE_BINARY_DIR})

endif()


############################################################
# Static library: target mfixcore
############################################################

add_subdirectory( model )

if(USR_OVERRIDES)

  # Inherit compilation options from mfixcore
  add_dependencies( udfs mfixcore )

  target_compile_definitions(udfs PUBLIC
    $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_DEFINITIONS>)

  target_compile_options(udfs PUBLIC
    $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_OPTIONS>)

  target_include_directories(udfs PUBLIC
    $<TARGET_PROPERTY:mfixcore,INTERFACE_INCLUDE_DIRECTORIES>)

  if(ENABLE_COVERAGE)
    target_compile_options(udfs PUBLIC ${COVERAGE_COMPILER_FLAGS})
  endif()

endif()


######################################################
# Main target:  mfixsolver executable
######################################################

set(MAIN_sources
  ${CMAKE_CURRENT_SOURCE_DIR}/model/mfix.f)

if(USR_OVERRIDES)
  set( UDF_OBJS $<TARGET_OBJECTS:udfs> )
else()
  set( UDF_OBJS )
endif()

add_executable( mfixsolver ${MAIN_sources} ${UDF_OBJS} )

set_target_properties(mfixsolver PROPERTIES Fortran_FORMAT "FREE")
set_target_properties(mfixsolver PROPERTIES LINKER_LANGUAGE Fortran)

target_link_libraries( mfixsolver mfixcore )

# Inherit compilation options from mfixcore
target_compile_definitions(mfixsolver PUBLIC
  $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_DEFINITIONS>)

target_compile_options(mfixsolver PUBLIC
  $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_OPTIONS>)

target_include_directories(mfixsolver PUBLIC
  $<TARGET_PROPERTY:mfixcore,INTERFACE_INCLUDE_DIRECTORIES>)


######################################################
# target:  postmfix executable
######################################################
add_subdirectory( post_mfix )


######################################################
# Crow support for mfixsolver executable
######################################################

if(ENABLE_CROW)
  add_subdirectory( ${PROJECT_SOURCE_DIR}/model/crow )
  target_compile_definitions( mfixsolver PUBLIC CROW )
  target_link_libraries( mfixsolver mfixcrow )
endif()

######################################################
# mfixsolver extension module for pymfix
######################################################
if(ENABLE_PYMFIX)
  include(PyMFIX)
endif()

######################################################
# Link Windows-specific libraries on Windows
######################################################

if(WIN32)
  target_link_libraries(
    mfixsolver
    advapi32
    gcc_eh
    shell32
    userenv
    ws2_32
    wsock32
    )
endif()


######################################################
# Add targets for running ctest, and for code coverage
######################################################

if(ENABLE_COVERAGE)
  target_compile_options(mfixcore PUBLIC ${COVERAGE_COMPILER_FLAGS})
  target_compile_options(mfixsolver PUBLIC ${COVERAGE_COMPILER_FLAGS})
endif()

if(ENABLE_CTEST)

  # Tests
  enable_testing()

  # Run "make build_test" to build all tests
  set(BUILD_TEST build_test)
  add_custom_target(${BUILD_TEST})

  set(SUBDIRS
    benchmarks/dem
    benchmarks/tfm/ParallelBenchmarkCases
    legacy_tests
    legacy_tests/dem-tests
    legacy_tests/dem-tests/dmp_tests
    legacy_tutorials
    legacy_tutorials/Cartesian_grid_tutorials
    legacy_tutorials/DEM_CG_TUTORIALS
    legacy_tutorials/QMOM_TUTORIALS
    tests/dem
    tests/fluid
    tests/mms
    tutorials
    tutorials/Hopper
    )

  foreach( SUBDIR ${SUBDIRS} )
    add_subdirectory( ${SUBDIR} )
  endforeach()

endif()

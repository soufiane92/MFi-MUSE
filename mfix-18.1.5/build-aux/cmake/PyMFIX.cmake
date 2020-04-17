cmake_minimum_required(VERSION 3.4)

if(NOT "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  message(FATAL_ERROR "ENABLE_PYMFIX is only supported for GNU Fortran compiler")
endif()

find_package(PythonInterp)

###########################
## Extension module file suffix (for example .cpython-36m-x86_64-linux-gnu.so)
###########################
if(NOT F2PY_SUFFIX)
  execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c
    "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX') or sysconfig.get_config_var('SO'))"
    OUTPUT_VARIABLE PYTHON_EXT_SUFFIX
    RESULT_VARIABLE FOUND_PYTHON_EXTENSION)
  string(STRIP "${PYTHON_EXT_SUFFIX}" PYTHON_EXT_SUFFIX)
  if(NOT FOUND_PYTHON_EXTENSION EQUAL 0)
    set(F2PY_SUFFIX "" CACHE STRING "Suffix added by F2Py to the module name to get the output file name.")
    message(FATAL_ERROR "Unable to determine file extension of compiled Python modules - specify it with F2PY_SUFFIX")
  endif()
  set(F2PY_SUFFIX "${PYTHON_EXT_SUFFIX}" CACHE STRING INTERNAL)
endif()

###########################
## Find the path to the f2py executable
###########################
if(WIN32)
  set(BAT ".bat")
endif()
find_program(F2PY_EXECUTABLE NAMES
  "f2py${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}${BAT}"
  "f2py-${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}${BAT}"
  "f2py${PYTHON_VERSION_MAJOR}${BAT}"
  "f2py${BAT}"
  REQUIRED)

###########################
## Find numpy  include
###########################
execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c
  "import numpy; print(numpy.get_include(), end='')"
  OUTPUT_VARIABLE NUMPY_INCLUDE
  RESULT_VARIABLE FOUND_NUMPY_INCLUDE)
file(TO_CMAKE_PATH "${NUMPY_INCLUDE}" NUMPY_INCLUDE)

###########################
## Find the path to the f2py src directory
###########################

execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c "import numpy.f2py; import os.path; print(os.path.join(os.path.dirname(numpy.f2py.__file__),'src'), end='')"
  OUTPUT_VARIABLE F2PY_SRC
  RESULT_VARIABLE FOUND_F2PY_SRC)
file(TO_CMAKE_PATH "${F2PY_SRC}" F2PY_SRC)

###########################
## Copy wrapped mfix sources to .f90 extension
###########################

set(WRAPPED_SOURCES
  dmp_modules/compar_mod.f
  dmp_modules/parallel_mpi_mod.f
  exit.f
  iterate.f
  machine_mod.f
  main.f
  param_mod.f
  pause.f
  residual_pub_mod.f
  run_mod.f
  time_cpu_mod.f
  )

foreach(SOURCE ${WRAPPED_SOURCES})
  file(COPY
    ${PROJECT_SOURCE_DIR}/model/${SOURCE}
    DESTINATION
    ${PROJECT_BINARY_DIR}/pymfix)
endforeach()

file(GLOB WRAPPED_F_SRCS
  ${PROJECT_BINARY_DIR}/pymfix/*.f
)

foreach(SOURCE ${WRAPPED_F_SRCS})
  file(RENAME
    ${SOURCE}
    ${SOURCE}90)
endforeach()

file(GLOB WRAPPED_F90_SRCS
  ${PROJECT_BINARY_DIR}/pymfix/*.f90
)

###########################
## Generate wrapper sources from wrapped sources
###########################

set(C_WRAPPER ${CMAKE_BINARY_DIR}/mfixsolvermodule.c)
set(F90_WRAPPER ${CMAKE_BINARY_DIR}/mfixsolver-f2pywrappers2.f90)
set(F_WRAPPER ${CMAKE_BINARY_DIR}/mfixsolver-f2pywrappers.f)

add_custom_command(OUTPUT ${C_WRAPPER} ${F_WRAPPER} ${F90_WRAPPER}
  COMMAND ${CMAKE_COMMAND} -E env "${F2PY_EXECUTABLE}" -m mfixsolver --lower
  ${WRAPPED_F90_SRCS}

  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
  DEPENDS ${CMAKE_BINARY_DIR}
  COMMENT "Generating f2py")

###########################
## Add mfixsolver Python extension module shared library
###########################

if(USR_OVERRIDES)
  set( UDF_OBJS $<TARGET_OBJECTS:udfs> )
else()
  set( UDF_OBJS )
endif()

add_library(mfixsolver_ext SHARED
  ${C_WRAPPER} ${F_WRAPPER} ${F90_WRAPPER}
  ${WRAPPED_F90_SRCS}
  ${F2PY_SRC}/fortranobject.c
  ${UDF_OBJS}
  )

# disable adding prefix "lib" (mfixsolver.so not libmfixsolver.so)
set_target_properties(mfixsolver_ext PROPERTIES
  PREFIX ""
  OUTPUT_NAME "mfixsolver"
  SUFFIX "${F2PY_SUFFIX}"
  )

set_source_files_properties(${F90_WRAPPER} PROPERTIES Fortran_FORMAT "FREE")
set_source_files_properties(${F_WRAPPER} PROPERTIES Fortran_FORMAT "FIXED")

set_source_files_properties(
  ${F90_SRCS}
  ${F_WRAPPER} ${F90_WRAPPER}
  PROPERTIES
  COMPILE_FLAGS "-fno-second-underscore"
  )

## Link extension with mfixcore and use mfixcore compiler flags
target_link_libraries( mfixsolver_ext mfixcore )

target_compile_definitions(mfixsolver_ext PUBLIC $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_DEFINITIONS>)
target_compile_options(mfixsolver_ext PUBLIC $<TARGET_PROPERTY:mfixcore,INTERFACE_COMPILE_OPTIONS>)
target_include_directories(mfixsolver_ext PUBLIC $<TARGET_PROPERTY:mfixcore,INTERFACE_INCLUDE_DIRECTORIES>)

## Compile and link extension with Python
find_package(PythonLibs)
target_include_directories(mfixsolver_ext PUBLIC ${NUMPY_INCLUDE})
target_include_directories(mfixsolver_ext PUBLIC ${PYTHON_INCLUDE_DIR}
  ${F2PY_SRC}
)

if(APPLE)
  target_link_libraries(mfixsolver_ext "-undefined dynamic_lookup")
else()
  target_link_libraries(mfixsolver_ext ${PYTHON_LIBRARY})
endif()

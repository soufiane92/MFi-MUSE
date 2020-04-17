# Check prereqs
find_program( GCOV_PATH gcov )
find_program( LCOV_PATH lcov )
find_program( GENHTML_PATH genhtml )
find_program( GCOVR_PATH gcovr PATHS ${CMAKE_SOURCE_DIR}/scripts/test )
find_program( SIMPLE_PYTHON_EXECUTABLE python )

if(NOT GCOV_PATH)
  message(FATAL_ERROR "gcov not found! Aborting...")
endif()

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "(Apple)?[Cc]lang")
  if("${CMAKE_Fortran_COMPILER_VERSION}" VERSION_LESS 3)
    message(FATAL_ERROR "Clang version must be 3.0.0 or greater! Aborting...")
  endif()
elseif(NOT CMAKE_COMPILER_IS_GNUG77)
  message(FATAL_ERROR "Compiler is not GNU gcc! Aborting...")
endif()

set(COVERAGE_COMPILER_FLAGS -g -O0 --coverage -fprofile-arcs -ftest-coverage
  CACHE INTERNAL "")

set( CMAKE_Fortran_FLAGS_COVERAGE
  ${COVERAGE_COMPILER_FLAGS}
  CACHE STRING "Flags used by the Fortran compiler during coverage builds."
  FORCE )
set( CMAKE_EXE_LINKER_FLAGS_COVERAGE
  ""
  CACHE STRING "Flags used for linking binaries during coverage builds."
  FORCE )
set( CMAKE_SHARED_LINKER_FLAGS_COVERAGE
  ""
  CACHE STRING
  "Flags used by the shared libraries linker during coverage builds."
  FORCE )
mark_as_advanced(
  CMAKE_Fortran_FLAGS_COVERAGE
  CMAKE_EXE_LINKER_FLAGS_COVERAGE
  CMAKE_SHARED_LINKER_FLAGS_COVERAGE)

if(NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
  message(WARNING "Code coverage may not work with non-Debug build")
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  link_libraries(gcov)
else()
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} --coverage")
endif()

if(NOT LCOV_PATH)
  message(FATAL_ERROR "lcov not found! Aborting...")
endif()

if(NOT GENHTML_PATH)
  message(FATAL_ERROR "genhtml not found! Aborting...")
endif()

# Cleanup lcov
add_custom_target(clean_coverage
  COMMAND ${LCOV_PATH} --directory . --zerocounters
  COMMENT "Resetting code coverage counters to zero."
  )

set(Coverage_NAME
  coverage
  )

set(COVERAGE_EXCLUDES
  ${CMAKE_SOURCE_DIR}/model/BLAS.f
  ${CMAKE_SOURCE_DIR}/model/DGTSV.f
  ${CMAKE_SOURCE_DIR}/model/ODEPACK.F
  ${CMAKE_SOURCE_DIR}/postmfix/*
  )

# Cleanup lcov
add_custom_target(${Coverage_NAME}

  # Capturing lcov counters and generating report
  COMMAND ${LCOV_PATH}
  --directory .
  --capture
  --output-file ${Coverage_NAME}.info

  COMMAND ${LCOV_PATH}
  --remove ${Coverage_NAME}.info ${COVERAGE_EXCLUDES}
  --output-file ${Coverage_NAME}.info.cleaned

  COMMAND ${GENHTML_PATH} --ignore-errors source
  -o ${Coverage_NAME} ${Coverage_NAME}.info.cleaned

  COMMAND ${CMAKE_COMMAND} -E remove ${Coverage_NAME}.info
  ${Coverage_NAME}.info.cleaned

  COMMENT "Processing code coverage counters and generating report."
  )

# Show info where to find the report
add_custom_command(TARGET coverage POST_BUILD
  COMMAND ;
  COMMENT "View coverage report in ./${Coverage_NAME}/index.html"
  )

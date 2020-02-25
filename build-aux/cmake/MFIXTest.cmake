set(TESTS ${DMP_TESTS} ${SERIAL_TESTS} ${SMP_TESTS})

foreach( THIS_TEST ${TESTS} )

  string( REPLACE "/" "_" TEST_NAME ${TEST_PATH}/${THIS_TEST} )

  set(TEST_DIR ${CMAKE_BINARY_DIR}/${TEST_PATH}/${THIS_TEST})
  set(SOURCE_DIR ${PROJECT_SOURCE_DIR}/${TEST_PATH}/${THIS_TEST})

  if(EXISTS ${SOURCE_DIR}/usrnlst.inc)
    file(COPY ${PROJECT_SOURCE_DIR}/model/usr_read_namelist.f
      DESTINATION ${SOURCE_DIR})
  endif()

  # Setup for building a test
  file(GLOB USR_OVERRIDES ${SOURCE_DIR}/*.f)
  set_source_files_properties(
    ${USR_OVERRIDES} PROPERTIES COMPILE_FLAGS "-DUSR_NAMELIST")

  set(TEST_EXE mfixsolver_${TEST_NAME})

  add_executable(${TEST_EXE} EXCLUDE_FROM_ALL ${MAIN_sources} ${USR_OVERRIDES})
  add_dependencies(${TEST_EXE} postmfix)
  if(ENABLE_COVERAGE)
    target_compile_options(${TEST_EXE} PUBLIC ${COVERAGE_COMPILER_FLAGS})
  endif()

  target_include_directories(${TEST_EXE} BEFORE PUBLIC
    ${TEST_DIR} ${CMAKE_Fortran_MODULE_DIRECTORY})

  set_target_properties( ${TEST_EXE} PROPERTIES
    Fortran_FORMAT "FREE"
    Fortran_MODULE_DIRECTORY ${TEST_DIR}
    RUNTIME_OUTPUT_DIRECTORY ${TEST_DIR} )

  target_link_libraries( ${TEST_EXE} mfixcore )
  target_include_directories(${TEST_EXE} PUBLIC ${MFIXCORE_MODDIR})

  if(ENABLE_MPI)
    target_link_libraries(${TEST_EXE} ${MPI_Fortran_LIBRARIES})
  endif()

  # Accumulate the build for each test into target "build_test"
  add_dependencies( ${BUILD_TEST} ${TEST_EXE} )

  # Create test dir in build path if needed
  # Update test files if needed
  add_custom_command(TARGET ${TEST_EXE} PRE_BUILD
    COMMAND [ -d ${TEST_DIR} ] || mkdir ${TEST_DIR}
    COMMAND cp -R ${SOURCE_DIR}/* ${TEST_DIR}

    # The following lines are only used for tests/mms/MMS*
    COMMAND [ -d ${SOURCE_DIR}/../nonuniform_grids_3d ] && cp -R ${SOURCE_DIR}/../nonuniform_grids_3d/mesh_* ${TEST_DIR} || exit 0
    COMMAND [ -d ${SOURCE_DIR}/../usr_common ] && cp -R ${SOURCE_DIR}/../usr_common ${TEST_DIR}/.. || exit 0
    WORKING_DIRECTORY  ${TEST_DIR}
    COMMENT "Updating test folder")

  #
  # Run rx_preproc.sh to generate species.inc if usr_rates[_des].f is present
  #

  if((EXISTS ${SOURCE_DIR}/usr_rates.f) OR (EXISTS ${SOURCE_DIR}/usr_rates_des.f))

    file( GLOB mfxs ${SOURCE_DIR}/*.mfx )

    if(EXISTS ${SOURCE_DIR}/mfix.dat)
      set(PROJECT_FILE mfix.dat)
    elseif(EXISTS ${mfxs})
      get_filename_component(PROJECT_FILE ${mfxs} NAME)
    else()
      message( FATAL_ERROR "usr_rates.f or usr_rates_des.f exists, but no project file: mfix.dat nor *.mfx" )
    endif()

    find_program(PYTHON python)
    set(RXN_PREPROC_PY ${PROJECT_SOURCE_DIR}/build-aux/rxn_preproc.py)
    add_custom_target( ${TEST_EXE}_species.inc
      COMMAND cp -R ${SOURCE_DIR}/* ${TEST_DIR}
      COMMAND ${CMAKE_COMMAND} -E env RUN_DIR=${TEST_DIR} ${PYTHON} ${RXN_PREPROC_PY} ${PROJECT_FILE}
      WORKING_DIRECTORY ${TEST_DIR}
      COMMENT "Generating species.inc" )

    add_dependencies(${TEST_EXE} ${TEST_EXE}_species.inc)

  endif()

  find_program(NUMDIFF numdiff)
  if(NOT NUMDIFF)
    message(FATAL_ERROR "
    numdiff not found! Unable to build with CTests, the MFiX ctests test suite requires the numdiff command.
    Install numdiff to your PATH.
    ")
  endif()

  # Setup for running the a single test
  set(TEST_RUN run_${TEST_NAME})
  add_custom_target( ${TEST_RUN} DEPENDS ${TEST_EXE}
    COMMAND ${CMAKE_COMMAND} -E env ENABLE_MPI=0 ENABLE_OMP=0 OMP_NUM_THREADS=1 MPIRANK=${rank_${TEST_NAME}}
    ${PROJECT_SOURCE_DIR}/build-aux/runtests.sh ${TEST_DIR}/${TEST_EXE} ${CMAKE_BINARY_DIR}/postmfix
    WORKING_DIRECTORY  ${TEST_DIR} )

  # Setup for running the whole suite of tests via ctest
  add_test(NAME ${TEST_NAME}
    COMMAND make ${TEST_RUN}
    WORKING_DIRECTORY ${PROJECT_BINDARY_DIR})

endforeach()

foreach( THIS_TEST ${DMP_TESTS} )
  string( REPLACE "/" "_" TEST_NAME ${TEST_PATH}/${THIS_TEST} )
  set_tests_properties( ${TEST_NAME} PROPERTIES LABELS "DMP" )
endforeach()

foreach( THIS_TEST ${SERIAL_TESTS} )
  string( REPLACE "/" "_" TEST_NAME ${TEST_PATH}/${THIS_TEST} )
  set_tests_properties( ${TEST_NAME} PROPERTIES LABELS "SERIAL" )
endforeach()

foreach( THIS_TEST ${SMP_TESTS} )
  string( REPLACE "/" "_" TEST_NAME ${TEST_PATH}/${THIS_TEST} )
  set_tests_properties( ${TEST_NAME} PROPERTIES LABELS "SMP" )
endforeach()

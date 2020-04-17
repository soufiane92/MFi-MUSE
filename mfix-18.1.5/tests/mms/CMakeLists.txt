set( DMP_TESTS
  MMS01
  MMS02
  MMS03
  MMS04
  MMS05
  )

set (TEST_PATH
  tests/mms
  )

include(MFIXTest)

foreach( THIS_TEST ${TESTS})

  STRING(REPLACE "/" "_" TEST_NAME ${TEST_PATH}/${THIS_TEST} )
  set(TEST_EXE mfixsolver_${TEST_NAME})

  target_sources( ${TEST_EXE} PUBLIC
    ${PROJECT_SOURCE_DIR}/${TEST_PATH}/usr_common/usr3.f
    ${PROJECT_SOURCE_DIR}/${TEST_PATH}/usr_common/usr_mod.f
    )

endforeach( THIS_TEST ${TESTS} )

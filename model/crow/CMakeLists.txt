enable_language(CXX)
set (CMAKE_CXX_STANDARD 11)

## Crow source

set(CROW_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(CROW_H ${CROW_DIR}/crow.h)


# Building from conda package if model/crow/crow.h exists
# Building from source directory if model/crow/crow.h missing
if(NOT EXISTS ${CROW_H})

  set(CROW_DOWNLOAD ON)
  message( STATUS "Downloading Crow because header not found at ${CROW_H}" )

  include(${CMAKE_ROOT}/Modules/ExternalProject.cmake)

  find_package(Git)

  set(CROW_HOME crow_repo)
  externalproject_add(crow
    PREFIX ${CROW_HOME}
    GIT_REPOSITORY https://github.com/ipkn/crow.git
    GIT_TAG master
    GIT_SHALLOW 1
    GIT_PROGRESS 1
    STEP_TARGETS download
    EXCLUDE_FROM_ALL TRUE
    )
  externalproject_get_property(crow source_dir)

  unset(CROW_H)
  set(CROW_DIR ${source_dir}/include)

endif()


add_library( mfixcrow STATIC
  ${CMAKE_CURRENT_SOURCE_DIR}/crowmfix.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/crowmfix_server.cpp
  ${CROW_H}
  )

if(CROW_DOWNLOAD)
  add_dependencies( mfixcrow crow-download )
endif()

target_include_directories( mfixcrow PUBLIC ${CROW_DIR} )


## Crow linking dependencies

find_package( Threads )

set(Boost_USE_STATIC_LIBS ON)
find_package(Boost REQUIRED COMPONENTS filesystem regex system)
include_directories( SYSTEM ${Boost_INCLUDE_DIRS} )

target_link_libraries( mfixcrow
  ${CMAKE_THREAD_LIBS_INIT}
  ${Boost_LIBRARIES}
  )

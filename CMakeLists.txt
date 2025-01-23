cmake_minimum_required(VERSION 3.17.5...3.27.7)

# define build environments
set(PROJECT_NAME "HeatFlow")
set(PROJECT_LIBRARY_NAME "HeatFlow_library")

set( CMAKE_INSTALL_PREFIX "$ENV{HOME}/.local/${PROJECT_NAME}"
  CACHE STRING "Select where to install the program." )
set(CMAKE_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  CACHE STRING "Select where to build the program." )
set(MODULE_DIR ${CMAKE_BUILD_DIR}/mod)


# set compiler
set(CMAKE_Fortran_COMPILER gfortran
  CACHE STRING "Select Fortran compiler." )  # Change this to your desired compiler
set(CMAKE_C_COMPILER gcc
  CACHE STRING "Select C compiler." )  # Change this to your desired compiler
set(CMAKE_Fortran_STANDARD 2018)

# set the project name
project(HeatFlow LANGUAGES C Fortran)

# set options for building tests and examples
option(BUILD_TESTS "Build the unit tests" On)

# set coverage compiler flags
if (CMAKE_BUILD_TYPE MATCHES "Coverage")
  list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
  set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
      include(CodeCoverage)
      setup_target_for_coverage_gcovr_html(
        NAME coverage
        EXECUTABLE ctest
        EXCLUDE "${CMAKE_SOURCE_DIR}/test/*")
  endif()
endif()

# enable testing
enable_testing()

set( PROJECT_DESCRIPTION
  "Fortran heat flow" )
set( PROJECT_URL "https://github.com/ExeQuantCode/HeatFlow" )
set( CMAKE_CONFIGURATION_TYPES "Release" "Parallel" "Serial" "Dev" "Debug" "Coverage" "Bigmem"
  CACHE STRING "List of configurations types." )
set( CMAKE_BUILD_TYPE "Release"
  CACHE STRING "Select which configuration to build." )

# Define the sources
set(SRC_DIR src)
set(LIB_DIR ${SRC_DIR}/heatflow)

set(LIB_FILES
    mod_constants.f90
    mod_constructions.f90
    mod_SPtype.f90
    mod_global.f90
    mod_Sparse.f90
    mod_inputs.f90
    mod_material.f90
    mod_hmatrix.f90
    mod_init_evolve.f90
    mod_setup.f90
    mod_boundary.f90
    mod_heating.f90
    mod_cattaneo.f90
    mod_tempdep.f90
    mod_evolve.f90
    mod_output.f90
)

set(SRC_FILES
    heatflow.f90
)

foreach(lib ${LIB_FILES})
    list(APPEND PREPENDED_LIB_FILES ${CMAKE_CURRENT_LIST_DIR}/${LIB_DIR}/${lib})
endforeach()

foreach(src ${SRC_FILES})
    list(APPEND PREPENDED_SRC_FILES ${CMAKE_CURRENT_LIST_DIR}/${SRC_DIR}/${src})
endforeach()


# initialise flags
set(CPPFLAGS "")
set(CFLAGS "")
set(MODULEFLAGS "")
set(MPFLAGS "")
set(WARNFLAGS "")
set(DEVFLAGS "")
set(DEBUGFLAGS "")
set(MEMFLAGS "")
set(OPTIMFLAGS "")
set(FASTFLAGS "")

# set flags based on compiler
if (CMAKE_Fortran_COMPILER MATCHES ".*gfortran.*" OR CMAKE_Fortran_COMPILER MATCHES ".*gcc.*")
    message(STATUS "Using gfortran compiler")
    set(PPFLAGS -cpp)
    set(MPFLAGS -fopenmp)
    set(WARNFLAGS -Wall)
    set(DEVFLAGS -g -fbacktrace -fcheck=all -fbounds-check -Og)
    set(DEBUGFLAGS -fbounds-check)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3 -march=native)
    set(FASTFLAGS -Ofast -march=native)
elseif (CMAKE_Fortran_COMPILER MATCHES ".*nag.*")
    message(STATUS "Using nag compiler")
    set(PPFLAGS -f2018 -fpp)
    set(MPFLAGS -openmp)
    set(WARNFLAGS -Wall)
    set(DEVFLAGS -g -mtrace -C=all -colour -O0)
    set(DEBUGFLAGS -C=array)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3)
    set(FASTFLAGS -Ofast)
elseif (CMAKE_Fortran_COMPILER MATCHES ".*ifort.*" OR CMAKE_Fortran_COMPILER MATCHES ".*ifx.*")
    message(STATUS "Using intel compiler")
    set(PPFLAGS -fpp)
    set(MPFLAGS -qopenmp)
    set(WARNFLAGS -warn all)
    set(DEVFLAGS -check all -warn)
    set(DEBUGFLAGS -check all -fpe0 -warn -tracekback -debug extended)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3)
    set(FASTFLAGS -Ofast)
else()
    # Code for other Fortran compilers
    message(STATUS "Using a different Fortran compiler")
endif()



set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${PPFLAGS}")


message(STATUS "library files: ${PREPENDED_LIB_FILES}")
message(STATUS "source files: ${PREPENDED_SRC_FILES}")
message(STATUS "project name: ${PROJECT_NAME}")

# create the library
add_library(${PROJECT_LIBRARY_NAME} STATIC ${PREPENDED_LIB_FILES})
set_target_properties(${PROJECT_LIBRARY_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})
target_link_libraries(${PROJECT_LIBRARY_NAME} PUBLIC)
target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Release>:${OPTIMFLAGS}>")
target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Bigmem>:${MEMFLAGS}>")
target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Parallel>:${OPTIMFLAGS}>")
target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Parallel>:${MPFLAGS}>")
target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Dev>:${DEVFLAGS}>")
target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Debug>:${DEBUGFLAGS}>")
target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Debug>:${WARNFLAGS}>")
if(NOT CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Release>:${MEMFLAGS}>")
    target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Parallel>:${MEMFLAGS}>")
    target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Dev>:${MEMFLAGS}>")
    target_compile_options(${PROJECT_LIBRARY_NAME} PUBLIC "$<$<CONFIG:Debug>:${MEMFLAGS}>")
endif()

add_executable(${PROJECT_NAME} ${PREPENDED_SRC_FILES})
target_link_libraries(${PROJECT_NAME} PRIVATE ${PROJECT_LIBRARY_NAME})
install(TARGETS ${PROJECT_NAME} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})

# set compile options based on different build configurations
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Release>:${OPTIMFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Bigmem>:${MEMFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Parallel>:${OPTIMFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Parallel>:${MPFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Dev>:${DEVFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Debug>:${DEBUGFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Debug>:${WARNFLAGS}>")
if(NOT CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Release>:${MEMFLAGS}>")
    target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Parallel>:${MEMFLAGS}>")
    target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Dev>:${MEMFLAGS}>")
    target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Debug>:${MEMFLAGS}>")
endif()


# include the build test directory
if(BUILD_TESTS)
  add_subdirectory(test)
endif()

if ( ( CMAKE_Fortran_COMPILER MATCHES ".*gfortran.*" OR CMAKE_Fortran_COMPILER MATCHES ".*gcc.*" ) AND 
     ( CMAKE_BUILD_TYPE MATCHES "Coverage" ) )

    append_coverage_compiler_flags()

endif()

if(BUILD_TESTS)
    # Create a directory for test data
    add_custom_command(
        OUTPUT ${CMAKE_BUILD_DIR}/test/test_dir_created  # Temporary marker file to indicate the directory was created
        COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BUILD_DIR}/test/test
        COMMENT "Creating test directory"
    )

    # Copy test data files
    add_custom_command(
        OUTPUT ${CMAKE_BUILD_DIR}/test/test_data_copied  # Temporary marker file to indicate the data was copied
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/test/data ${CMAKE_BUILD_DIR}/test/test/data
        DEPENDS ${CMAKE_CURRENT_LIST_DIR}/test/data ${CMAKE_BUILD_DIR}/test/test_dir_created  # Ensure the source data exists and the directory was created
        COMMENT "Copying test data files"
    )

    # Create a custom target to ensure that the copy commands are executed
    add_custom_target(copy_test_data ALL
        DEPENDS ${CMAKE_BUILD_DIR}/test/test_data_copied  # Ensure it depends on the data copying command
    )

    # Add dependency to your main project target
    add_dependencies(${PROJECT_NAME} copy_test_data)

    # Ensure that ctest waits for copy_test_data
    add_custom_command(TARGET copy_test_data POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E echo "Test data files copied successfully"
    )

    # Get all registered tests
    get_property(all_tests GLOBAL PROPERTY TESTS)

    # Loop through all tests and add the dependency
    foreach(test_name IN LISTS all_tests)
        add_dependencies(${test_name} copy_test_data)
    endforeach()

endif()
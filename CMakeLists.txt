cmake_minimum_required(VERSION 3.17.5...3.27.7)

# define build environments
set(PROJECT_NAME "HeatFlow")
set(PROJECT_LIBRARY_NAME "HeatFlow_library")

set( CMAKE_INSTALL_PREFIX "$ENV{HOME}/.local/${PROJECT_NAME}"
  CACHE STRING "Select where to install the program." )
set(CMAKE_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  CACHE STRING "Select where to build the program." )
set(MODULE_DIR ${CMAKE_BUILD_DIR}/mod)


# set compiler (auto-detected, MPI wrapper preferred; override with -DCMAKE_Fortran_COMPILER=...)
if(NOT DEFINED CMAKE_Fortran_COMPILER)
  find_program(HF_Fortran_COMPILER NAMES mpifort mpif90 gfortran ifx ifort nagfor)
  if(HF_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER "${HF_Fortran_COMPILER}"
      CACHE STRING "Select Fortran compiler." )
  endif()
endif()
if(NOT DEFINED CMAKE_C_COMPILER)
  find_program(HF_C_COMPILER NAMES mpicc gcc clang icx icc)
  if(HF_C_COMPILER)
    set(CMAKE_C_COMPILER "${HF_C_COMPILER}"
      CACHE STRING "Select C compiler." )
  endif()
endif()
set(CMAKE_Fortran_STANDARD 2018)

# set the project name
project(HeatFlow LANGUAGES C Fortran)

# set options for building tests and examples
option(BUILD_TESTS "Build the unit tests" On)
option(HEATFLOW_PETSC "Build with PETSc solver support (auto-detected; legacy-solver-only build if not found)" On)
option(HEATFLOW_HDF5 "Build with HDF5 output support" Off)

if (HEATFLOW_HDF5)
    # Prefer pkg-config (Debian/Ubuntu hdf5-openmpi), fall back to CMake's FindHDF5
    find_package(PkgConfig QUIET)
    if (PkgConfig_FOUND)
        pkg_search_module(HDF5_PC QUIET hdf5-openmpi hdf5-mpi hdf5)
    endif()
    if (HDF5_PC_FOUND)
        set(HDF5_Fortran_INCLUDE_DIRS "${HDF5_PC_INCLUDE_DIRS}")
        find_library(HDF5_FORTRAN_LIB NAMES hdf5_fortran HINTS ${HDF5_PC_LIBRARY_DIRS})
        if (NOT HDF5_FORTRAN_LIB)
            message(FATAL_ERROR "HDF5 Fortran library (libhdf5_fortran) not found")
        endif()
        set(HDF5_Fortran_LIBRARIES "${HDF5_FORTRAN_LIB};${HDF5_PC_LIBRARIES}")
    else()
        find_package(HDF5 REQUIRED COMPONENTS Fortran)
    endif()
endif()

# MPI is required (mod_inputs/mod_setup use mpi)
find_package(MPI REQUIRED)

# PETSc is optional: without it the code builds a legacy-solver-only version
if (HEATFLOW_PETSC)
    find_package(PkgConfig QUIET)
    if (PkgConfig_FOUND)
        pkg_search_module(PETSC QUIET petsc PETSc)
    endif()
    # Fall back to PETSC_DIR/PETSC_ARCH (e.g. bespoke or module-built PETSc)
    if (NOT PETSC_FOUND)
        if (NOT DEFINED PETSC_DIR)
            set(PETSC_DIR $ENV{PETSC_DIR})
        endif()
        if (NOT DEFINED PETSC_ARCH)
            set(PETSC_ARCH $ENV{PETSC_ARCH})
        endif()
        if (PETSC_DIR)
            set(PETSC_INCLUDE_DIRS "${PETSC_DIR}/include")
            set(HF_PETSC_LIB_HINTS "${PETSC_DIR}/lib")
            if (PETSC_ARCH)
                list(APPEND PETSC_INCLUDE_DIRS "${PETSC_DIR}/${PETSC_ARCH}/include")
                list(APPEND HF_PETSC_LIB_HINTS "${PETSC_DIR}/${PETSC_ARCH}/lib")
            endif()
            find_library(PETSC_LIBRARY NAMES petsc HINTS ${HF_PETSC_LIB_HINTS})
            if (PETSC_LIBRARY)
                set(PETSC_FOUND TRUE)
                set(PETSC_LIBRARIES "${PETSC_LIBRARY}")
            endif()
        endif()
    endif()
endif()

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
    mod_output_hdf5.f90
    mod_material.f90
    mod_hmatrix.f90
    mod_init_evolve.f90
    mod_petsc_solver.f90
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

# set flags based on compiler ID (robust across wrappers and platforms)
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    message(STATUS "Using GNU compiler: ${CMAKE_Fortran_COMPILER}")
    set(PPFLAGS -cpp)
    set(MPFLAGS -fopenmp)
    set(WARNFLAGS -Wall)
    set(DEVFLAGS -g -fbacktrace -fcheck=all -fbounds-check -Og)
    set(DEBUGFLAGS -fbounds-check)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3 -march=native)
    set(FASTFLAGS -Ofast -march=native)
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
    message(STATUS "Using nag compiler")
    set(PPFLAGS -f2018 -fpp)
    set(MPFLAGS -openmp)
    set(WARNFLAGS -Wall)
    set(DEVFLAGS -g -mtrace -C=all -colour -O0)
    set(DEBUGFLAGS -C=array)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3)
    set(FASTFLAGS -Ofast)
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
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
    message(STATUS "Using a different Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}")
endif()



set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${PPFLAGS}")


message(STATUS "library files: ${PREPENDED_LIB_FILES}")
message(STATUS "source files: ${PREPENDED_SRC_FILES}")
message(STATUS "project name: ${PROJECT_NAME}")

# create the library
add_library(${PROJECT_LIBRARY_NAME} STATIC ${PREPENDED_LIB_FILES})
set_target_properties(${PROJECT_LIBRARY_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})
target_link_libraries(${PROJECT_LIBRARY_NAME} PUBLIC MPI::MPI_Fortran)
if (PETSC_FOUND)
    message(STATUS "PETSc found: building with PETSc solver support")
    target_compile_definitions(${PROJECT_LIBRARY_NAME} PUBLIC HAVE_PETSC)
    target_include_directories(${PROJECT_LIBRARY_NAME} PUBLIC ${PETSC_INCLUDE_DIRS})
    target_link_directories(${PROJECT_LIBRARY_NAME} PUBLIC ${PETSC_LIBRARY_DIRS})
    target_link_libraries(${PROJECT_LIBRARY_NAME} PUBLIC ${PETSC_LIBRARIES})
else()
    message(STATUS "PETSc not found: building legacy-solver-only version")
endif()
if (HEATFLOW_HDF5)
    message(STATUS "Building with HDF5 output support")
    target_compile_definitions(${PROJECT_LIBRARY_NAME} PUBLIC USE_HDF5)
    target_include_directories(${PROJECT_LIBRARY_NAME} PUBLIC ${HDF5_Fortran_INCLUDE_DIRS} ${HDF5_INCLUDE_DIRS})
    target_link_directories(${PROJECT_LIBRARY_NAME} PUBLIC ${HDF5_PC_LIBRARY_DIRS})
    target_link_libraries(${PROJECT_LIBRARY_NAME} PUBLIC ${HDF5_Fortran_LIBRARIES} ${HDF5_LIBRARIES})
endif()
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

if ( ( CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" ) AND
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
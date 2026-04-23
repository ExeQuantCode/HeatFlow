cmake_minimum_required(VERSION 3.17.5...3.27.7)

# ========================
# Project metadata
# ========================
set(PROJECT_NAME "HeatFlow")
set(PROJECT_LIBRARY_NAME "HeatFlow_library")

project(HeatFlow
  DESCRIPTION "Fortran heat flow"
  LANGUAGES C Fortran)

set(CMAKE_INSTALL_PREFIX "$ENV{HOME}/.local/${PROJECT_NAME}"
  CACHE STRING "Install prefix")

set(CMAKE_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  CACHE STRING "Build directory")

set(MODULE_DIR ${CMAKE_BUILD_DIR}/mod)

set(CMAKE_Fortran_STANDARD 2018)

option(BUILD_TESTS "Build unit tests" ON)

# ========================
# Build types
# ========================
set(CMAKE_CONFIGURATION_TYPES
  "Release" "Parallel" "Serial" "Dev" "Debug" "Coverage" "Bigmem"
  CACHE STRING "Build types")

set(CMAKE_BUILD_TYPE "Release"
  CACHE STRING "Default build type")

# ========================
# Compiler detection
# ========================
if(NOT DEFINED CMAKE_Fortran_COMPILER)
  find_program(DEFAULT_Fortran_COMPILER NAMES mpifort mpif90 gfortran ifx ifort nagfor)
  if(DEFAULT_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER "${DEFAULT_Fortran_COMPILER}"
      CACHE STRING "Fortran compiler")
  endif()
endif()

if(NOT DEFINED CMAKE_C_COMPILER)
  find_program(DEFAULT_C_COMPILER NAMES mpicc gcc clang icx icc)
  if(DEFAULT_C_COMPILER)
    set(CMAKE_C_COMPILER "${DEFAULT_C_COMPILER}"
      CACHE STRING "C compiler")
  endif()
endif()

# ========================
# Flags
# ========================
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(PPFLAGS -cpp)
    set(MPFLAGS -fopenmp)
    set(WARNFLAGS -Wall)
    set(DEVFLAGS -g -fbacktrace -fcheck=all -fbounds-check -Og)
    set(DEBUGFLAGS -fbounds-check)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3 -march=native)

elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
    set(PPFLAGS -f2018 -fpp)
    set(MPFLAGS -openmp)
    set(WARNFLAGS -Wall)
    set(DEVFLAGS -g -C=all -O0)
    set(DEBUGFLAGS -C=array)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3)

elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(PPFLAGS -fpp)
    set(MPFLAGS -qopenmp)
    set(WARNFLAGS -warn all)
    set(DEVFLAGS -check all -warn)
    set(DEBUGFLAGS -check all -fpe0 -traceback)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3)
endif()

# ========================
# Sources
# ========================
set(SRC_DIR src)
set(LIB_DIR ${SRC_DIR}/heatflow)
set(PETSC_DIR_SRC ${SRC_DIR}/petsc_solver)

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

# PETSc solver now isolated
set(PETSC_SOLVER_FILE
  ${CMAKE_CURRENT_LIST_DIR}/${PETSC_DIR_SRC}/mod_petsc_solver.f90)
set(PETSC_SOLVER_OBJECT ${CMAKE_CURRENT_BINARY_DIR}/mod_petsc_solver.o)

# ========================
# MPI
# ========================
get_filename_component(FORTRAN_COMPILER_NAME "${CMAKE_Fortran_COMPILER}" NAME)

add_library(HeatFlowMPI INTERFACE)
add_library(HeatFlow::MPI ALIAS HeatFlowMPI)

if(FORTRAN_COMPILER_NAME MATCHES "^mpi(fort|f90)$")
  message(STATUS "Using MPI wrapper ${CMAKE_Fortran_COMPILER}")
else()
  find_package(MPI REQUIRED COMPONENTS Fortran)
  target_link_libraries(HeatFlowMPI INTERFACE MPI::MPI_Fortran)
endif()

set(PETSC_MPI_INCLUDE_FLAGS "")
if(NOT FORTRAN_COMPILER_NAME MATCHES "^mpi(fort|f90)$")
  foreach(mpi_include_dir IN LISTS MPI_Fortran_INCLUDE_DIRS)
    if(mpi_include_dir)
      get_filename_component(mpi_include_dir "${mpi_include_dir}" ABSOLUTE)
      list(APPEND PETSC_MPI_INCLUDE_FLAGS "-I${mpi_include_dir}")
    endif()
  endforeach()
endif()

# ========================
# PETSc (REQUIRED)
# ========================
add_library(HeatFlowPetsc INTERFACE)
add_library(HeatFlow::PETSc ALIAS HeatFlowPetsc)

find_package(PkgConfig QUIET)

if(PKG_CONFIG_FOUND)
  pkg_search_module(PETSC QUIET IMPORTED_TARGET petsc PETSc)
endif()

if(TARGET PkgConfig::PETSC)
  message(STATUS "Using PETSc via pkg-config")
  target_link_libraries(HeatFlowPetsc INTERFACE PkgConfig::PETSC)

else()
  if(NOT DEFINED PETSC_DIR)
    set(PETSC_DIR $ENV{PETSC_DIR})
  endif()

  if(NOT DEFINED PETSC_ARCH)
    set(PETSC_ARCH $ENV{PETSC_ARCH})
  endif()

  if(NOT PETSC_DIR OR NOT PETSC_ARCH)
    message(FATAL_ERROR "PETSc not found. Set PETSC_DIR and PETSC_ARCH.")
  endif()

  set(PETSC_ROOT_DIR "${PETSC_DIR}")
  set(PETSC_INCLUDE_DIRS
    "${PETSC_ROOT_DIR}/include"
    "${PETSC_ROOT_DIR}/${PETSC_ARCH}/include")

  find_library(PETSC_LIBRARY
    NAMES petsc
    HINTS "${PETSC_ROOT_DIR}/${PETSC_ARCH}/lib"
    REQUIRED)

  target_include_directories(HeatFlowPetsc INTERFACE ${PETSC_INCLUDE_DIRS})
  target_link_libraries(HeatFlowPetsc INTERFACE ${PETSC_LIBRARY})
endif()

set(PETSC_SOLVER_INCLUDE_FLAGS)

foreach(petsc_include_dir IN LISTS PETSC_INCLUDE_DIRS)
  if(petsc_include_dir)
    get_filename_component(petsc_include_dir "${petsc_include_dir}" ABSOLUTE)
    list(APPEND PETSC_SOLVER_INCLUDE_FLAGS "-I${petsc_include_dir}")
  endif()
endforeach()

# ========================
# PETSc solver (FIXED STALL ROOT CAUSE)
# ========================
add_custom_command(
  OUTPUT ${PETSC_SOLVER_OBJECT}
  BYPRODUCTS ${MODULE_DIR}/petsc_solver.mod
  COMMAND ${CMAKE_COMMAND} -E make_directory ${MODULE_DIR}
  COMMAND ${CMAKE_COMMAND} -E env
          FC=${CMAKE_Fortran_COMPILER}
          ${CMAKE_Fortran_COMPILER}
          ${PPFLAGS}
          ${PETSC_MPI_INCLUDE_FLAGS}
          ${PETSC_SOLVER_INCLUDE_FLAGS}
          -J${MODULE_DIR}
          -I${MODULE_DIR}
          -c ${PETSC_SOLVER_FILE}
          -o ${PETSC_SOLVER_OBJECT}
  DEPENDS ${PETSC_SOLVER_FILE}
  VERBATIM)

add_custom_target(petsc_solver_object DEPENDS ${PETSC_SOLVER_OBJECT})
set_source_files_properties(${PETSC_SOLVER_OBJECT} PROPERTIES GENERATED TRUE EXTERNAL_OBJECT TRUE)

# ========================
# Main library
# ========================
add_library(${PROJECT_LIBRARY_NAME} STATIC)

target_sources(${PROJECT_LIBRARY_NAME}
  PRIVATE ${PREPENDED_LIB_FILES} ${PETSC_SOLVER_OBJECT})

add_dependencies(${PROJECT_LIBRARY_NAME} petsc_solver_object)

set_target_properties(${PROJECT_LIBRARY_NAME}
  PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})

target_include_directories(${PROJECT_LIBRARY_NAME}
  PUBLIC ${MODULE_DIR})

target_compile_options(${PROJECT_LIBRARY_NAME}
  PRIVATE ${PPFLAGS})

target_link_libraries(${PROJECT_LIBRARY_NAME}
  PUBLIC HeatFlow::MPI HeatFlow::PETSc)

# ========================
# Executable
# ========================
add_executable(${PROJECT_NAME})

target_sources(${PROJECT_NAME}
  PRIVATE ${PREPENDED_SRC_FILES})

set_target_properties(${PROJECT_NAME}
  PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})

target_link_libraries(${PROJECT_NAME}
  PRIVATE ${PROJECT_LIBRARY_NAME})

# ========================
# Coverage
# ========================
if (CMAKE_BUILD_TYPE MATCHES "Coverage")
  list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
  include(CodeCoverage)
  append_coverage_compiler_flags()
endif()

# ========================
# Testing
# ========================
enable_testing()

if(BUILD_TESTS)
  add_subdirectory(test)
endif()

# ========================
# Install
# ========================
install(TARGETS ${PROJECT_NAME}
  DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
cmake_minimum_required(VERSION 3.17.5...3.27.7)

# define build environments
set(PROJECT_NAME "HeatFlow")

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

set( PROJECT_DESCRIPTION
  "Fortran heat flow" )
set( PROJECT_URL "https://github.com/ExeQuantCode/HeatFlow" )
set( CMAKE_CONFIGURATION_TYPES "Release" "Parallel" "Serial" "Dev" "Debug" "Coverage" "Bigmem"
  CACHE STRING "List of configurations types." )
set( CMAKE_BUILD_TYPE "Release"
  CACHE STRING "Select which configuration to build." )

# Define the sources
set(SRC_DIR src)
set(LIB_DIR ${SRC_DIR})

set(LIB_FILES
    heatflow/mod_constants.f90
    heatflow/mod_constructions.f90
    heatflow/mod_SPtype.f90
    heatflow/mod_global.f90
    heatflow/mod_Sparse.f90
    heatflow/mod_inputs.f90
    heatflow/mod_material.f90
    heatflow/mod_hmatrix.f90
    heatflow/mod_init_evolve.f90
    heatflow/mod_setup.f90
    heatflow/mod_boundary.f90
    heatflow/mod_heating.f90
    heatflow/mod_cattaneo.f90
    heatflow/mod_tempdep.f90
    heatflow/mod_evolve.f90
    heatflow/mod_output.f90
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
add_executable(${PROJECT_NAME} ${PREPENDED_LIB_FILES} ${PREPENDED_SRC_FILES})
set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})
install(TARGETS ${PROJECT_NAME} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)

# # replace ".f90" with ".mod"
# string(REGEX REPLACE "\\.[^.]*$" ".mod" MODULE_FILES "${SRC_FILES}")

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


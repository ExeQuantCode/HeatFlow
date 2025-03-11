####################################################################
#   11 Jun 2024                                                    #
####################################################################
#
SHELL = /bin/sh
#
#  The machine (platform) identifier to append to the library names
#
PLAT = _linux
#
#


##########################################
# CODE DIRECTORIES AND FILES
##########################################
mkfile_path := $(abspath $(firstword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))
BIN_DIR := ./bin
SRC_DIR := ./src
BUILD_DIR = ./obj

SRCS := /heatflow/mod_constants.f90 \
		/heatflow/mod_constructions.f90 \
		/heatflow/mod_SPtype.f90 \
		/heatflow/mod_global.f90 \
		/heatflow/mod_Sparse.f90 \
		/heatflow/mod_inputs.f90 \
		/heatflow/mod_material.f90 \
		/heatflow/mod_hmatrix.f90 \
		/heatflow/mod_init_evolve.f90 \
		/heatflow/mod_setup.f90 \
		/heatflow/mod_boundary.f90 \
		/heatflow/mod_heating.f90 \
		/heatflow/mod_cattaneo.f90 \
		/heatflow/mod_tempdep.f90 \
		/heatflow/mod_evolve.f90 \
        /heatflow/mod_output.f90 \
        heatflow.f90
OBJS := $(addprefix $(SRC_DIR)/,$(SRCS))


FFLAGS = -O3 
MODULEFLAGS = -J
FC = gfortran

##########################################
# LIBRARY SECTION
##########################################
MKLROOT?="/usr/local/intel/parallel_studio_xe_2017/compilers_and_libraries_2017/linux/mkl/lib/intel64_lin"


NAME = ThermalFlow.x
programs = $(BIN_DIR)/$(NAME)
all: $(programs)

$(BIN_DIR):
	mkdir -p $@

$(BUILD_DIR):
	mkdir -p $@

$(programs) : $(OBJS) | $(BIN_DIR) $(BUILD_DIR)
	$(FC) -O3 -fopenmp $(MODULEFLAGS) $(BUILD_DIR) $(OBJS) -o $@

debug :  $(OBJS)
	$(FC) -O0 -Wall -g -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all -fbounds-check  $(MODULEFLAGS) $(BUILD_DIR) $(OBJS) -o $(programs)

OMP: $(programs)
	./util/DShell/omp_exec.sh

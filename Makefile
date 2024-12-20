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

SRCS := constants.f90 \
		constructions.f90 \
		SPtype.f90 \
		Globe_data.f90 \
		Sparse.f90 \
		inputs.f90 \
		material.f90 \
		hmatrix.f90 \
		init_evolve.f90 \
		setup.f90 \
		boundary.f90 \
		heating.f90 \
		cattaneo.f90 \
		tempdep.f90 \
		evolve.f90 \
        output.f90 \
        main.f90
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
	$(FC) -O3 -mcmodel=large -Wall -g -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all -fbounds-check  $(MODULEFLAGS) $(BUILD_DIR) $(OBJS) -o $(programs)

OMP: $(programs)
	./util/DShell/omp_exec.sh

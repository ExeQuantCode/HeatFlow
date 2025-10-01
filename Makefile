####################################################################
#   11 Jun 2024                                                    #
####################################################################
SHELL = /bin/sh
PLAT = _linux

##########################################
# CODE DIRECTORIES AND FILES
##########################################
mkfile_path := $(abspath $(firstword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))
BIN_DIR := ./bin
SRC_DIR := ./src
BUILD_DIR = ./obj

SRCS := heatflow/mod_constants.f90 \
		heatflow/mod_constructions.f90 \
		heatflow/mod_SPtype.f90 \
		heatflow/mod_global.f90 \
		heatflow/mod_Sparse.f90 \
		heatflow/mod_inputs.f90 \
		heatflow/mod_material.f90 \
		heatflow/mod_hmatrix.f90 \
		heatflow/mod_init_evolve.f90 \
		heatflow/mkl_pardiso.f90 \
		heatflow/mod_sparse_solver.f90 \
		heatflow/mod_setup.f90 \
		heatflow/mod_boundary.f90 \
		heatflow/mod_heating.f90 \
		heatflow/mod_cattaneo.f90 \
		heatflow/mod_tempdep.f90 \
		heatflow/mod_evolve.f90 \
        heatflow/mod_output.f90 \
        heatflow.f90

OBJS := $(addprefix $(BUILD_DIR)/,$(notdir $(SRCS:.f90=.o)))

# MKL configuration
MKLROOT ?= /opt/intel/oneapi/mkl/latest
MKL_LIB_DIR = $(MKLROOT)/lib/intel64
MKL_INCLUDE_DIR = $(MKLROOT)/include
#MKL_FLAGS = -L$(MKL_LIB_DIR) -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl
MKL_FLAGS = -L$(MKL_LIB_DIR) -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl

#FFLAGS = -O3 -I$(MKL_INCLUDE_DIR)
MODULEFLAGS = -J$(BUILD_DIR)
FC = gfortran
NCORES := $(shell nproc)
FFLAGS = -O3 -fopenmp -I$(MKL_INCLUDE_DIR)

##########################################
# TARGETS
##########################################
NAME = ThermalFlow.x
programs = $(BIN_DIR)/$(NAME)

.PHONY: all debug clean OMP

all: $(programs)

$(BIN_DIR):
	mkdir -p $@

$(BUILD_DIR):
	mkdir -p $@

# Pattern rule for compiling Fortran files
$(BUILD_DIR)/%.o: $(SRC_DIR)/heatflow/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) $(MODULEFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) $(MODULEFLAGS) -c $< -o $@

$(programs): $(OBJS) | $(BIN_DIR)
	$(FC) -O3 -fopenmp $(OBJS) -o $@ $(MKL_FLAGS)

.PHONY: run
run: $(programs)
    OMP_NUM_THREADS=$(NCORES) MKL_NUM_THREADS=$(NCORES) MKL_DYNAMIC=FALSE OMP_PROC_BIND=spread OMP_PLACES=cores ./bin/$(NAME)

debug: FFLAGS = -O0 -Wall -g -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fcheck=all -fbounds-check -I$(MKL_INCLUDE_DIR)
debug: $(OBJS) | $(BIN_DIR)
	$(FC) $(FFLAGS) $(OBJS) -o $(programs) $(MKL_FLAGS)

OMP: $(programs)
	./util/DShell/omp_exec.sh

clean:
	rm -f $(BUILD_DIR)/*.o $(BUILD_DIR)/*.mod $(programs)

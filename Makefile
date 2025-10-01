####################################################################
# HeatFlow Build System (MKL + optional PETSc + OpenMP)
####################################################################

SHELL        = /bin/sh

# Directories
SRC_DIR      := ./src
BUILD_DIR    := ./obj
BIN_DIR      := ./bin

# Compiler
FC           := gfortran

# Core count
NCORES       := $(shell nproc)

# MKL
MKLROOT      ?= /opt/intel/oneapi/mkl/latest
MKL_LIB_DIR  := $(MKLROOT)/lib/intel64
MKL_INCLUDE  := $(MKLROOT)/include
MKL_FLAGS    := -L$(MKL_LIB_DIR) -lmkl_gf_lp64 -lmkl_gnu_thread -lmkl_core -lgomp -lpthread -lm -ldl

# PETSc (manual fallback if petsc-config missing)
PETSC_PREFIX    := /usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real
PETSC_FINCLUDE  := /usr/share/petsc/3.15/include
PETSC_AINCLUDE  := $(PETSC_PREFIX)/include
PETSC_LIBDIR    := $(PETSC_PREFIX)/lib

PETSC_CONFIG := $(shell command -v petsc-config 2>/dev/null)
ifeq ($(PETSC_CONFIG),)
  PETSC_INC  := -I$(PETSC_FINCLUDE) -I$(PETSC_AINCLUDE)
  PETSC_LIB  := -L$(PETSC_LIBDIR) -lpetsc
  PETSC_NOTE := (PETSc manual paths)
else
  PETSC_INC  := $(shell petsc-config --cflags)
  PETSC_LIB  := $(shell petsc-config --libs)
  PETSC_NOTE := (petsc-config)
endif

# Flags
OPTFLAGS    := -O3
OMPFLAGS    := -fopenmp
WARNFLAGS   := -Wall
MODDIR_FLAG := -J$(BUILD_DIR)

FFLAGS      := -cpp $(OPTFLAGS) $(OMPFLAGS) $(WARNFLAGS) -I$(MKL_INCLUDE) $(PETSC_INC) $(MODDIR_FLAG)
DEBUGFLAGS  := -cpp -O0 -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow -fbounds-check -I$(MKL_INCLUDE) $(PETSC_INC) $(MODDIR_FLAG)

# Program
NAME    := ThermalFlow.x
TARGET  := $(BIN_DIR)/$(NAME)

# Sources (module order)
SRCS := \
  heatflow/mod_constants.f90 \
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
  heatflow/mod_petsc_solver.f90 \
  heatflow/mod_boundary.f90 \
  heatflow/mod_heating.f90 \
  heatflow/mod_cattaneo.f90 \
  heatflow/mod_tempdep.f90 \
  heatflow/mod_evolve.f90 \
  heatflow/mod_output.f90 \
  heatflow/mod_setup.f90 \
  heatflow.f90

OBJS := $(addprefix $(BUILD_DIR)/,$(notdir $(SRCS:.f90=.o)))

.PHONY: all debug clean distclean run help show

all: show $(TARGET)

show:
	@printf 'Building %s %s\n' '$(NAME)' '$(PETSC_NOTE)'

$(BIN_DIR) $(BUILD_DIR):
	mkdir -p $@

# Compile module sources
$(BUILD_DIR)/%.o: $(SRC_DIR)/heatflow/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Main program
$(BUILD_DIR)/heatflow.o: $(SRC_DIR)/heatflow.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Link (single definition)
$(TARGET): $(BIN_DIR) $(OBJS)
	$(FC) $(OPTFLAGS) $(OMPFLAGS) $(OBJS) -o $@ $(MKL_FLAGS) $(PETSC_LIB) -Wl,-rpath,$(PETSC_LIBDIR)

debug: FFLAGS = $(DEBUGFLAGS)
debug: clean show $(TARGET)

run: $(TARGET)
	OMP_NUM_THREADS=$(NCORES) \
	MKL_NUM_THREADS=$(NCORES) \
	MKL_DYNAMIC=FALSE \
	OMP_PROC_BIND=spread \
	OMP_PLACES=cores \
	$< $(RUN_ARGS)

clean:
	@echo "[CLEAN] objects and modules"
	@rm -f $(BUILD_DIR)/*.o $(BUILD_DIR)/*.mod

distclean: clean
	@echo "[CLEAN] executable"
	@rm -f $(TARGET)

help:
	@echo "Targets:"
	@echo "  make / make all    - build optimized"
	@echo "  make debug         - debug build"
	@echo "  make run           - run with all cores"
	@echo "  make clean         - remove objects/modules"
	@echo "  make distclean     - remove executable"
	@echo "Variables:"
	@echo "  RUN_ARGS='-ksp_type cg -pc_type gamg -ksp_rtol 1e-8 -ksp_monitor'"
	@echo "Parallel build: make -j$(NCORES)"

####################################################################
# End
####################################################################
####################################################################
# HeatFlow Build System (MKL + optional PETSc + OpenMP)
####################################################################

SHELL        = /bin/sh

# Directories
SRC_DIR      := ./src
BUILD_DIR    := ./obj
BIN_DIR      := ./bin

# Compiler (prefer system MPI wrapper for PETSc builds, allow user override)
SYSTEM_PATH := PATH=/usr/bin:/bin
ifeq ($(origin FC), default)
ifneq ($(wildcard /usr/bin/mpifort),)
FC := env $(SYSTEM_PATH) /usr/bin/mpifort
else ifneq ($(wildcard /usr/bin/gfortran),)
FC := env $(SYSTEM_PATH) /usr/bin/gfortran
else
FC := gfortran
endif
endif

# Core count
NCORES       := $(shell nproc)

# Detect conda environment for BLAS/LAPACK (fallback if no system libs)
CONDA_PREFIX ?= $(shell conda info --base 2>/dev/null || echo /home/hm556/miniforge3)

# PETSc (discover dynamically when possible)
ifneq ($(wildcard /usr/bin/pkg-config),)
PKG_CONFIG := env $(SYSTEM_PATH) /usr/bin/pkg-config
else
PKG_CONFIG := pkg-config
endif
PETSC_PKG_CFLAGS := $(shell $(PKG_CONFIG) --cflags petsc 2>/dev/null || $(PKG_CONFIG) --cflags PETSc 2>/dev/null)
PETSC_PKG_LIBS   := $(shell $(PKG_CONFIG) --libs petsc 2>/dev/null || $(PKG_CONFIG) --libs PETSc 2>/dev/null)

ifdef PETSC_DIR
PETSC_DIR_INC := -I$(PETSC_DIR)/include
ifdef PETSC_ARCH
PETSC_DIR_INC += -I$(PETSC_DIR)/$(PETSC_ARCH)/include
PETSC_DIR_LIB := -L$(PETSC_DIR)/$(PETSC_ARCH)/lib -Wl,-rpath,$(PETSC_DIR)/$(PETSC_ARCH)/lib
endif
endif

ifeq ($(strip $(PETSC_PKG_CFLAGS)),)
PETSC_INC  := $(PETSC_DIR_INC)
PETSC_LIB  := $(PETSC_DIR_LIB) -lpetsc
PETSC_NOTE := (PETSc from PETSC_DIR/PETSC_ARCH)
else
PETSC_INC  := $(PETSC_PKG_CFLAGS)
PETSC_LIB  := $(PETSC_PKG_LIBS)
PETSC_NOTE := (PETSc via pkg-config)
endif

ifeq ($(strip $(PETSC_INC)),)
PETSC_INC  := -I/usr/share/petsc/3.15/include -I/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/include
PETSC_LIB  := -L/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/lib -lpetsc -Wl,-rpath,/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/lib
PETSC_NOTE := (legacy PETSc 3.15 fallback)
endif

# BLAS/LAPACK backend
OPENBLAS_LIBS := $(shell $(PKG_CONFIG) --libs openblas 2>/dev/null)
CONDA_BLAS_LIB := $(firstword $(wildcard $(CONDA_PREFIX)/lib/libblas.so.3 $(CONDA_PREFIX)/lib/libblas.so))
CONDA_LAPACK_LIB := $(firstword $(wildcard $(CONDA_PREFIX)/lib/liblapack.so.3 $(CONDA_PREFIX)/lib/liblapack.so))
ifeq ($(strip $(OPENBLAS_LIBS)),)
ifneq ($(strip $(CONDA_BLAS_LIB)$(CONDA_LAPACK_LIB)),)
BLAS_FLAGS := -Wl,--disable-new-dtags -Wl,-rpath,$(CONDA_PREFIX)/lib -Wl,--no-as-needed $(CONDA_BLAS_LIB) $(CONDA_LAPACK_LIB) -Wl,--as-needed -lpthread -lm
BLAS_NOTE := (OpenBLAS from CONDA_PREFIX)
else
BLAS_FLAGS := -lblas -llapack -lpthread -lm
BLAS_NOTE := (system BLAS/LAPACK fallback)
endif
else
BLAS_FLAGS := $(OPENBLAS_LIBS) -lpthread -lm
BLAS_NOTE := (OpenBLAS via pkg-config)
endif

# Flags
OPTFLAGS    := -O3
OMPFLAGS    := -fopenmp
WARNFLAGS   := -Wall
MODDIR_FLAG := -J$(BUILD_DIR)

FFLAGS      := -cpp $(OPTFLAGS) $(OMPFLAGS) $(WARNFLAGS) $(PETSC_INC) $(MODDIR_FLAG)
DEBUGFLAGS  := -cpp -O0 -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow -fbounds-check $(PETSC_INC) $(MODDIR_FLAG)

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
  heatflow/mod_output_hdf5.f90 \
  heatflow/mod_material.f90 \
  heatflow/mod_hmatrix.f90 \
  heatflow/mod_init_evolve.f90 \
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

.NOTPARALLEL:

.PHONY: all debug clean distclean run help show

all: show $(TARGET)

show:
	@printf 'Building %s %s %s\n' '$(NAME)' '$(PETSC_NOTE)' '$(BLAS_NOTE)'

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
	$(FC) $(OPTFLAGS) $(OMPFLAGS) $(OBJS) -o $@ $(BLAS_FLAGS) $(PETSC_LIB)

debug: FFLAGS = $(DEBUGFLAGS)
debug: clean show $(TARGET)

run: $(TARGET)
	mpiexec -n $(NCORES) \
	OMP_NUM_THREADS=1 \
	OPENBLAS_NUM_THREADS=1 \
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
	@echo "  make run           - run distributed across all cores via MPI"
	@echo "  make clean         - remove objects/modules"
	@echo "  make distclean     - remove executable"
	@echo "Variables:"
	@echo "  RUN_ARGS='-ksp_type cg -pc_type gamg -ksp_rtol 1e-8 -ksp_monitor'"
	@echo "Parallel build: make -j$(NCORES)"

####################################################################
# End
####################################################################

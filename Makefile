####################################################################
# HeatFlow Build System (MKL + optional PETSc + OpenMP)
####################################################################

SHELL        = /bin/sh

# Directories
SRC_DIR      := ./src
BUILD_DIR    := ./obj
BIN_DIR      := ./bin

# Compiler (gfortran with OpenMP for threading)
FC           := gfortran

# Core count
NCORES       := $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2)

# Detect conda environment for BLAS/LAPACK (fallback if no system libs)
CONDA_PREFIX ?= $(shell conda info --base 2>/dev/null || echo /home/hm556/miniforge3)

# PETSc Configuration
BREW_PETSC := $(shell brew --prefix petsc 2>/dev/null)
ifneq ($(BREW_PETSC),)
    PETSC_INC := -I$(BREW_PETSC)/include
    PETSC_LIB := -L$(BREW_PETSC)/lib -lpetsc -Wl,-rpath,$(BREW_PETSC)/lib
    PETSC_NOTE := (Homebrew PETSc)
else
    # PETSc (system installation - Linux fallback)
    PETSC_INC  := -I/usr/share/petsc/3.15/include -I/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/include
    PETSC_LIB  := -L/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/lib -lpetsc -Wl,-rpath,/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/lib
    PETSC_NOTE := (system PETSc 3.15)
endif

# HDF5 Support
# Run `make USE_HDF5=1` to enable
ifeq ($(USE_HDF5),1)
    # Check for Homebrew HDF5 on macOS
    BREW_HDF5 := $(shell brew --prefix hdf5-mpi 2>/dev/null || brew --prefix hdf5 2>/dev/null)
    ifneq ($(BREW_HDF5),)
        HDF5_INC   := -I$(BREW_HDF5)/include
        HDF5_LIB   := -L$(BREW_HDF5)/lib -lhdf5_fortran -lhdf5 -Wl,-rpath,$(BREW_HDF5)/lib
        HDF5_FLAGS := -DUSE_HDF5 $(HDF5_INC)
        HDF5_NOTE  := (+ HDF5)
    else
        # Linux fallback
        HDF5_INC   := -I/usr/include/hdf5/openmpi
        HDF5_LIB   := -L/usr/lib/x86_64-linux-gnu/hdf5/openmpi -lhdf5_fortran -lhdf5
        HDF5_FLAGS := -DUSE_HDF5 $(HDF5_INC)
        HDF5_NOTE  := (+ HDF5)
    endif
else
    HDF5_FLAGS :=
    HDF5_LIB   :=
    HDF5_NOTE  :=
endif

# BLAS/LAPACK: Use Apple Accelerate on macOS, OpenBLAS on Linux
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
    # macOS SDK sysroot (fixes 'library System not found' with Homebrew gfortran)
    MACOS_SDK := $(shell xcrun --show-sdk-path 2>/dev/null)
    ifneq ($(MACOS_SDK),)
        SYSROOT_FLAGS := -L$(MACOS_SDK)/usr/lib -F$(MACOS_SDK)/System/Library/Frameworks
    else
        SYSROOT_FLAGS :=
    endif
    # Apple Accelerate framework - optimized for Apple Silicon
    BLAS_FLAGS := $(SYSROOT_FLAGS) -framework Accelerate -lgomp -lpthread -lm
    BLAS_NOTE := (Apple Accelerate)
else
    # Linux: Use OpenBLAS for multi-threaded BLAS/LAPACK
    BLAS_FLAGS := -lopenblas -lgomp -lpthread -lm
    BLAS_NOTE := (OpenBLAS)
endif

# Flags
OPTFLAGS    := -O3
OMPFLAGS    := -fopenmp
WARNFLAGS   := -Wall
MODDIR_FLAG := -J$(BUILD_DIR)

FFLAGS      := -cpp $(OPTFLAGS) $(OMPFLAGS) $(WARNFLAGS) $(PETSC_INC) $(HDF5_FLAGS) $(MODDIR_FLAG)
DEBUGFLAGS  := -cpp -O0 -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow -fbounds-check $(PETSC_INC) $(HDF5_FLAGS) $(MODDIR_FLAG)

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
	$(FC) $(OPTFLAGS) $(OMPFLAGS) $(OBJS) -o $@ $(BLAS_FLAGS) $(PETSC_LIB) $(HDF5_LIB)

debug: FFLAGS = $(DEBUGFLAGS)
debug: clean show $(TARGET)

run: $(TARGET)
ifeq ($(UNAME_S),Darwin)
	OMP_NUM_THREADS=$(NCORES) \
	VECLIB_MAXIMUM_THREADS=$(NCORES) \
	OMP_PROC_BIND=spread \
	OMP_PLACES=cores \
	$< $(RUN_ARGS)
else
	OMP_NUM_THREADS=$(NCORES) \
	OPENBLAS_NUM_THREADS=$(NCORES) \
	OMP_PROC_BIND=spread \
	OMP_PLACES=cores \
	$< $(RUN_ARGS)
endif

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
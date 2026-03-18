####################################################################
# HeatFlow Build System (PETSc + OpenMP)
# PETSc is REQUIRED — the source has no fallback stubs.
####################################################################

SHELL        = /bin/sh

# Directories
SRC_DIR      := ./src
BUILD_DIR    := ./obj
BIN_DIR      := ./bin

# Compiler
FC           := gfortran

# Core count
NCORES       := $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2)

# Common flags
OPTFLAGS    := -O3
OMPFLAGS    := -fopenmp
WARNFLAGS   := -Wall
MODDIR_FLAG := -J$(BUILD_DIR)

####################################################################
# Platform detection — one big block per OS
####################################################################
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
    ################################################################
    # macOS (Homebrew)
    ################################################################

    # --- PETSc (Homebrew) ---
    BREW_PETSC := $(shell brew --prefix petsc 2>/dev/null)
    ifneq ($(BREW_PETSC),)
        PETSC_INC  := -I$(BREW_PETSC)/include
        PETSC_LIB  := -L$(BREW_PETSC)/lib -lpetsc -Wl,-rpath,$(BREW_PETSC)/lib
        PETSC_NOTE := (Homebrew PETSc)
    else
        $(error PETSc not found via Homebrew — install with: brew install petsc)
    endif

    # --- HDF5 (Homebrew, optional) ---
    ifeq ($(USE_HDF5),1)
        BREW_HDF5 := $(shell brew --prefix hdf5-mpi 2>/dev/null || brew --prefix hdf5 2>/dev/null)
        ifneq ($(BREW_HDF5),)
            HDF5_INC   := -I$(BREW_HDF5)/include
            HDF5_LIB   := -L$(BREW_HDF5)/lib -lhdf5_fortran -lhdf5 -Wl,-rpath,$(BREW_HDF5)/lib
            HDF5_FLAGS := -DUSE_HDF5 $(HDF5_INC)
            HDF5_NOTE  := (+ HDF5)
        else
            $(error HDF5 requested but not found — install with: brew install hdf5)
        endif
    else
        HDF5_FLAGS :=
        HDF5_LIB   :=
        HDF5_NOTE  :=
    endif

    # --- BLAS/LAPACK (Apple Accelerate) ---
    MACOS_SDK := $(shell xcrun --show-sdk-path 2>/dev/null)
    ifneq ($(MACOS_SDK),)
        SYSROOT_FLAGS := -L$(MACOS_SDK)/usr/lib -F$(MACOS_SDK)/System/Library/Frameworks
    else
        SYSROOT_FLAGS :=
    endif
    BLAS_FLAGS := $(SYSROOT_FLAGS) -framework Accelerate -lgomp -lpthread -lm
    BLAS_NOTE  := (Apple Accelerate)

    # --- Runtime environment ---
    RUN_ENV := OMP_NUM_THREADS=$(NCORES) \
               VECLIB_MAXIMUM_THREADS=$(NCORES) \
               OMP_PROC_BIND=spread \
               OMP_PLACES=cores

else
    ################################################################
    # Linux
    ################################################################

    # --- PETSc (pkg-config with Fortran include discovery) ---
    PETSC_PKGCONFIG := $(shell pkg-config --cflags petsc 2>/dev/null)
    ifneq ($(PETSC_PKGCONFIG),)
        # pkg-config gives us the C include path and libs
        PETSC_INC_BASE := $(shell pkg-config --cflags petsc)
        PETSC_LIB      := $(shell pkg-config --libs petsc)
        # Also need the Fortran finclude path (not provided by pkg-config)
        # Find petsc/finclude/petscsys.h under /usr/share/petsc/
        PETSC_FINCLUDE := $(shell find /usr/share/petsc -path '*/petsc/finclude/petscsys.h' -printf '%h/../..\n' 2>/dev/null | head -1)
        ifneq ($(PETSC_FINCLUDE),)
            PETSC_INC := $(PETSC_INC_BASE) -I$(PETSC_FINCLUDE)
        else
            PETSC_INC := $(PETSC_INC_BASE)
        endif
        PETSC_NOTE := (pkg-config PETSc)
    else
        # Debian/Ubuntu fallback (hardcoded paths)
        PETSC_DEBIAN := $(shell test -d /usr/lib/petscdir/petsc3.19/x86_64-linux-gnu-real/include && echo yes)
        PETSC_DEBIAN_OLD := $(shell test -d /usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/include && echo yes)
        ifneq ($(PETSC_DEBIAN),)
            PETSC_INC  := -I/usr/share/petsc/3.19t64/include -I/usr/lib/petscdir/petsc3.19/x86_64-linux-gnu-real/include
            PETSC_LIB  := -L/usr/lib/petscdir/petsc3.19/x86_64-linux-gnu-real/lib -lpetsc_real -Wl,-rpath,/usr/lib/petscdir/petsc3.19/x86_64-linux-gnu-real/lib
            PETSC_NOTE := (system PETSc 3.19)
        else ifneq ($(PETSC_DEBIAN_OLD),)
            PETSC_INC  := -I/usr/share/petsc/3.15/include -I/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/include
            PETSC_LIB  := -L/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/lib -lpetsc -Wl,-rpath,/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/lib
            PETSC_NOTE := (system PETSc 3.15)
        else
            $(error PETSc not found — install via: sudo apt install libpetsc-real-dev pkgconf)
        endif
    endif

    # --- HDF5 (system, optional) ---
    ifeq ($(USE_HDF5),1)
        HDF5_INC   := -I/usr/include/hdf5/openmpi
        HDF5_LIB   := -L/usr/lib/x86_64-linux-gnu/hdf5/openmpi -lhdf5_fortran -lhdf5
        HDF5_FLAGS := -DUSE_HDF5 $(HDF5_INC)
        HDF5_NOTE  := (+ HDF5)
    else
        HDF5_FLAGS :=
        HDF5_LIB   :=
        HDF5_NOTE  :=
    endif

    # --- BLAS/LAPACK (OpenBLAS) ---
    BLAS_FLAGS := -lopenblas -lgomp -lpthread -lm
    BLAS_NOTE  := (OpenBLAS)

    # --- Runtime environment ---
    RUN_ENV := OMP_NUM_THREADS=$(NCORES) \
               OPENBLAS_NUM_THREADS=$(NCORES) \
               OMP_PROC_BIND=spread \
               OMP_PLACES=cores

endif

####################################################################
# Compiler flags
####################################################################
FFLAGS      := -cpp $(OPTFLAGS) $(OMPFLAGS) $(WARNFLAGS) $(PETSC_INC) $(HDF5_FLAGS) $(MODDIR_FLAG)
DEBUGFLAGS  := -cpp -O0 -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow -fbounds-check $(PETSC_INC) $(HDF5_FLAGS) $(MODDIR_FLAG)

####################################################################
# Program
####################################################################
NAME    := ThermalFlow.x
TARGET  := $(BIN_DIR)/$(NAME)

# Sources (module order matters)
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

####################################################################
# Targets
####################################################################
.PHONY: all debug clean distclean run help show

all: show $(TARGET)

show:
	@printf 'Building %s %s %s %s\n' '$(NAME)' '$(PETSC_NOTE)' '$(HDF5_NOTE)' '$(BLAS_NOTE)'

$(BIN_DIR) $(BUILD_DIR):
	mkdir -p $@

# Compile module sources
$(BUILD_DIR)/%.o: $(SRC_DIR)/heatflow/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Main program
$(BUILD_DIR)/heatflow.o: $(SRC_DIR)/heatflow.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

# Link
$(TARGET): $(BIN_DIR) $(OBJS)
	$(FC) $(OPTFLAGS) $(OMPFLAGS) $(OBJS) -o $@ $(BLAS_FLAGS) $(PETSC_LIB) $(HDF5_LIB)

debug: FFLAGS = $(DEBUGFLAGS)
debug: clean show $(TARGET)

run: $(TARGET)
	$(RUN_ENV) $< $(RUN_ARGS)

clean:
	@echo "[CLEAN] objects and modules"
	@rm -f $(BUILD_DIR)/*.o $(BUILD_DIR)/*.mod

distclean: clean
	@echo "[CLEAN] executable"
	@rm -f $(TARGET)

help:
	@echo "Targets:"
	@echo "  make / make all    - build with PETSc (required)"
	@echo "  make debug         - debug build"
	@echo "  make run           - run with all cores"
	@echo "  make clean         - remove objects/modules"
	@echo "  make distclean     - remove executable"
	@echo "Options:"
	@echo "  USE_HDF5=1         - enable HDF5 output support"
	@echo "  RUN_ARGS='...'     - pass PETSc runtime flags"
	@echo "Parallel build: make -j$(NCORES)"

####################################################################
# End
####################################################################
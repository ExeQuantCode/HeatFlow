####################################################################
# HeatFlow Build System (PETSc + OpenMP)
# PETSc is REQUIRED — the source has no fallback stubs.
####################################################################

SHELL        = /bin/sh

# Directories
SRC_DIR      := ./src
BUILD_DIR    := ./obj
BIN_DIR      := ./bin

# Compilers and tools
FC           := mpifort
MPIEXEC      := mpiexec
PKG_CONFIG   := pkg-config

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
            $(error HDF5 requested but not found — install with: brew install hdf5-mpi)
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

    # --- PETSc (pkg-config) ---
    # Distributions use both PETSc.pc and petsc.pc, so accept either.
    PETSC_PC := $(shell if $(PKG_CONFIG) --exists PETSc 2>/dev/null; then echo PETSc; \
                         elif $(PKG_CONFIG) --exists petsc 2>/dev/null; then echo petsc; fi)
    PETSC_INC  := $(shell $(PKG_CONFIG) --cflags $(PETSC_PC) 2>/dev/null)
    PETSC_LIB  := $(shell $(PKG_CONFIG) --libs $(PETSC_PC) 2>/dev/null)
    PETSC_NOTE := (pkg-config $(PETSC_PC))

    # --- HDF5 (pkg-config, optional) ---
    ifeq ($(USE_HDF5),1)
        HDF5_PC    := hdf5_fortran
        HDF5_INC   := $(shell $(PKG_CONFIG) --cflags $(HDF5_PC) 2>/dev/null)
        HDF5_LIB   := $(shell $(PKG_CONFIG) --libs $(HDF5_PC) 2>/dev/null)
        HDF5_FLAGS := -DUSE_HDF5 $(HDF5_INC)
        HDF5_NOTE  := (+ HDF5)
    else
        HDF5_FLAGS :=
        HDF5_LIB   :=
        HDF5_NOTE  :=
    endif

    # --- BLAS/LAPACK (provided by PETSc) ---
    # HeatFlow has no direct BLAS calls; PETSc carries its own link dependency.
    BLAS_FLAGS :=
    BLAS_NOTE  := (PETSc BLAS/LAPACK)

    # --- Runtime environment ---
    RUN_ENV := OMP_NUM_THREADS=$(NCORES) \
               OPENBLAS_NUM_THREADS=1 \
               OMP_PROC_BIND=spread \
               OMP_PLACES=cores

endif

####################################################################
# Compiler flags
####################################################################
FFLAGS      := -cpp $(OPTFLAGS) $(OMPFLAGS) $(WARNFLAGS) $(PETSC_INC) $(HDF5_FLAGS) $(MODDIR_FLAG)
DEBUGFLAGS  := -cpp -O0 -g $(OMPFLAGS) -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -fbounds-check $(PETSC_INC) $(HDF5_FLAGS) $(MODDIR_FLAG)

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
.NOTPARALLEL:
.DELETE_ON_ERROR:
.PHONY: all check check-deps debug clean distclean run help show

all: check-deps show $(TARGET)

check-deps:
	@$(FC) --version >/dev/null 2>&1 || { echo "MPI Fortran compiler '$(FC)' not found" >&2; exit 1; }
	@test -n "$(PETSC_INC)" -a -n "$(PETSC_LIB)" || { echo "PETSc not found; see Install_guide.md" >&2; exit 1; }
	@if test "$(USE_HDF5)" = 1 -a -z "$(HDF5_LIB)"; then echo "HDF5 Fortran library not found; see Install_guide.md" >&2; exit 1; fi

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
debug: clean check-deps show $(TARGET)

run: check-deps $(TARGET)
	$(RUN_ENV) $(MPIEXEC) -n 1 $< $(RUN_ARGS)

# Run the PETSc example in an ignored scratch directory. The solver uses
# PETSC_COMM_SELF, so one MPI rank is the supported execution mode.
check: check-deps $(TARGET)
	@echo "[CHECK] one-rank MPI/PETSc smoke test $(HDF5_NOTE)"
	@rm -rf $(BUILD_DIR)/check
	@mkdir -p $(BUILD_DIR)/check/inputs $(BUILD_DIR)/check/outputs
	@cp test/test_run/inputs/mat.in test/test_run/inputs/param.in test/test_run/inputs/system.in $(BUILD_DIR)/check/inputs/
	@cd $(BUILD_DIR)/check && OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 \
		$(MPIEXEC) -n 1 ../../$(TARGET) > run.log 2>&1
	@test -s $(BUILD_DIR)/check/outputs/TempDis.dat
	@grep -q "all done" $(BUILD_DIR)/check/run.log
	@if test "$(USE_HDF5)" = 1; then test -s $(BUILD_DIR)/check/outputs/output_pump_probe_30uW.h5; fi
	@echo "[PASS] Runtime completed and output files were created"

clean:
	@echo "[CLEAN] objects, modules, and test output"
	@rm -f $(BUILD_DIR)/*.o $(BUILD_DIR)/*.mod
	@rm -rf $(BUILD_DIR)/check

distclean: clean
	@echo "[CLEAN] executable"
	@rm -f $(TARGET)

help:
	@echo "Targets:"
	@echo "  make / make all    - build with PETSc (required)"
	@echo "  make check         - run the one-rank MPI smoke test"
	@echo "  make debug         - debug build"
	@echo "  make run           - run one MPI rank with OpenMP threads"
	@echo "  make clean         - remove objects/modules"
	@echo "  make distclean     - remove executable"
	@echo "Options:"
	@echo "  USE_HDF5=1         - enable HDF5 output support"
	@echo "  RUN_ARGS='...'     - pass PETSc runtime flags"
	@echo "  FC=...             - override the MPI Fortran compiler"
	@echo "Fortran modules are compiled safely in source order."

####################################################################
# End
####################################################################

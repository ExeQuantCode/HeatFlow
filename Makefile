####################################################################
# HeatFlow Build System (MKL + optional PETSc + OpenMP)
####################################################################

SHELL        = /bin/sh

# Directories
SRC_DIR      := ./src
BUILD_DIR    := ./obj
BIN_DIR      := ./bin

# MPI (required: the sources contain "use mpi")
#
# Do not pin the compiler to /usr/bin or scrub PATH: MPI is commonly installed
# under Homebrew (/opt/homebrew), conda ($CONDA_PREFIX), environment modules,
# Intel oneAPI, MacPorts or Spack.  Search the active environment first, then
# well-known prefixes, and skip wrappers that cannot run (a broken wrapper
# produces a confusing "mpi.mod not found" error later on).
MPI_WRAPPER_NAMES := mpifort mpif90 mpifc mpiifort mpiifx

# Detect conda environment (used for MPI and BLAS/LAPACK)
CONDA_PREFIX ?= $(shell conda info --base 2>/dev/null)

MPI_PREFIX_BIN   := $(addsuffix /bin,$(MPI_HOME) $(MPI_DIR) $(I_MPI_ROOT) $(OPAL_PREFIX) $(CONDA_PREFIX))
MPI_FALLBACK_BIN := \
  /opt/homebrew/bin /usr/local/bin /opt/local/bin \
  /usr/lib64/openmpi/bin /usr/lib/x86_64-linux-gnu/openmpi/bin \
  /usr/lib64/mpich/bin /usr/lib/x86_64-linux-gnu/mpich/bin \
  /opt/intel/oneapi/mpi/latest/bin /usr/bin

# Remember whether the user chose FC before assigning a default
FC_ORIGIN := $(origin FC)

ifneq ($(filter default undefined,$(FC_ORIGIN)),)
MPI_FC := $(shell for name in $(MPI_WRAPPER_NAMES); do \
    for dir in $(MPI_PREFIX_BIN) $$PATH $(MPI_FALLBACK_BIN); do \
      candidate="$$dir/$$name"; \
      if [ -x "$$candidate" ] && { "$$candidate" --version >/dev/null 2>&1 || "$$candidate" -v >/dev/null 2>&1; }; then \
        echo "$$candidate"; exit 0; \
      fi; \
    done; \
  done)
FC := $(if $(strip $(MPI_FC)),$(MPI_FC),mpifort)
else
MPI_FC :=
endif

# Fail at build time (not for clean/help) when no usable MPI was found
ifeq ($(strip $(FC)),)
MPI_MISSING := 1
endif
ifneq ($(filter default undefined,$(FC_ORIGIN)),)
ifeq ($(strip $(MPI_FC)),)
MPI_MISSING := 1
endif
endif

# MPI launcher: prefer one next to the compiler wrapper, then PATH/prefixes
FC_DIR  := $(dir $(FC))
MPIEXEC ?= $(firstword \
  $(wildcard $(FC_DIR)mpiexec) $(wildcard $(FC_DIR)mpirun) \
  $(shell command -v mpiexec 2>/dev/null) \
  $(shell command -v mpirun 2>/dev/null) \
  $(foreach d,$(MPI_PREFIX_BIN) $(MPI_FALLBACK_BIN),$(wildcard $(d)/mpiexec) $(wildcard $(d)/mpirun)))

# Core count (nproc on Linux, sysctl on macOS)
NCORES       := $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2)

# Platform detection
UNAME_S      := $(shell uname -s)

# pkg-config (used for PETSc and BLAS detection)
PKG_CONFIG ?= pkg-config

# PETSc (discover dynamically when possible; disable with: make PETSC=0)
ifneq ($(PETSC),0)
ifeq ($(UNAME_S),Darwin)
# --- macOS: PETSc via Homebrew ---
BREW_PETSC := $(shell brew --prefix petsc 2>/dev/null)
ifneq ($(BREW_PETSC),)
PETSC_INC  := -I$(BREW_PETSC)/include
PETSC_LIB  := -L$(BREW_PETSC)/lib -lpetsc -Wl,-rpath,$(BREW_PETSC)/lib
PETSC_NOTE := (Homebrew PETSc)
endif
else
# --- Linux: pkg-config, then PETSC_DIR/PETSC_ARCH, then Debian paths ---
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
ifneq ($(strip $(PETSC_INC)),)
PETSC_LIB  := $(PETSC_DIR_LIB) -lpetsc
PETSC_NOTE := (PETSc from PETSC_DIR/PETSC_ARCH)
endif
else
PETSC_INC  := $(PETSC_PKG_CFLAGS)
PETSC_LIB  := $(PETSC_PKG_LIBS)
PETSC_NOTE := (PETSc via pkg-config)
endif

# Legacy hardcoded fallback, only if it exists on this system
ifeq ($(strip $(PETSC_INC)),)
ifneq ($(wildcard /usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/include),)
PETSC_INC  := -I/usr/share/petsc/3.15/include -I/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/include
PETSC_LIB  := -L/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/lib -lpetsc -Wl,-rpath,/usr/lib/petscdir/petsc3.15/x86_64-linux-gnu-real/lib
PETSC_NOTE := (legacy PETSc 3.15 fallback)
endif
endif
endif
endif

# If no PETSc was found, build the legacy-solver-only version
ifeq ($(strip $(PETSC_INC)),)
PETSC_DEF  :=
PETSC_LIB  :=
PETSC_NOTE := (no PETSc found - legacy solver only)
else
PETSC_DEF  := -DHAVE_PETSC
endif

# HDF5 (optional; enable with: make USE_HDF5=1)
ifeq ($(USE_HDF5),1)
ifeq ($(UNAME_S),Darwin)
# --- macOS: HDF5 via Homebrew ---
BREW_HDF5 := $(shell brew --prefix hdf5-mpi 2>/dev/null || brew --prefix hdf5 2>/dev/null)
ifneq ($(BREW_HDF5),)
HDF5_INC   := -I$(BREW_HDF5)/include
HDF5_LIB   := -L$(BREW_HDF5)/lib -lhdf5_fortran -lhdf5 -Wl,-rpath,$(BREW_HDF5)/lib
else
$(error HDF5 requested (USE_HDF5=1) but not found - install with: brew install hdf5-mpi)
endif
else
# --- Linux: system HDF5 (OpenMPI variant) ---
HDF5_INC   := -I/usr/include/hdf5/openmpi
HDF5_LIB   := -L/usr/lib/x86_64-linux-gnu/hdf5/openmpi -lhdf5_fortran -lhdf5
endif
HDF5_FLAGS := -DUSE_HDF5 $(HDF5_INC)
HDF5_NOTE  := (+ HDF5)
else
HDF5_FLAGS :=
HDF5_LIB   :=
HDF5_NOTE  :=
endif

# BLAS/LAPACK backend (platform specific)
ifeq ($(UNAME_S),Darwin)
# --- macOS: Apple Accelerate framework ---
MACOS_SDK := $(shell xcrun --show-sdk-path 2>/dev/null)
ifneq ($(MACOS_SDK),)
SYSROOT_FLAGS := -L$(MACOS_SDK)/usr/lib -F$(MACOS_SDK)/System/Library/Frameworks
endif
BLAS_FLAGS := $(SYSROOT_FLAGS) -framework Accelerate -lgomp -lpthread -lm
BLAS_NOTE  := (Apple Accelerate)
RUN_ENV    := OMP_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 OMP_PROC_BIND=spread OMP_PLACES=cores
else
# --- Linux: OpenBLAS (pkg-config, conda, then system) ---
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
RUN_ENV    := OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 OMP_PROC_BIND=spread OMP_PLACES=cores
endif

# Flags
OPTFLAGS    := -O3
OMPFLAGS    := -fopenmp
WARNFLAGS   := -Wall
MODDIR_FLAG := -J$(BUILD_DIR)

FFLAGS      := -cpp $(PETSC_DEF) $(OPTFLAGS) $(OMPFLAGS) $(WARNFLAGS) $(PETSC_INC) $(HDF5_FLAGS) $(MODDIR_FLAG)
DEBUGFLAGS  := -cpp $(PETSC_DEF) -O0 -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow -fbounds-check $(PETSC_INC) $(HDF5_FLAGS) $(MODDIR_FLAG)

# Program
NAME    := HeatFlow
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

.PHONY: all debug clean distclean run help show check-mpi

all: check-mpi show $(TARGET)

show:
	@printf 'Building %s %s %s %s\n' '$(NAME)' '$(PETSC_NOTE)' '$(HDF5_NOTE)' '$(BLAS_NOTE)'
	@printf '  Fortran compiler : %s\n' '$(FC)'
	@printf '  MPI launcher     : %s\n' '$(if $(strip $(MPIEXEC)),$(MPIEXEC),not found)'

check-mpi:
	@if [ -n "$(strip $(MPI_MISSING))" ]; then \
	  echo "ERROR: no working MPI Fortran compiler found." >&2; \
	  echo "HeatFlow requires MPI; the sources contain 'use mpi'." >&2; \
	  echo "Install an MPI implementation and ensure its Fortran wrapper is on PATH:" >&2; \
	  echo "  Debian/Ubuntu : sudo apt install gfortran openmpi-bin libopenmpi-dev" >&2; \
	  echo "  Fedora/RHEL   : sudo dnf install gcc-gfortran openmpi-devel" >&2; \
	  echo "  macOS         : brew install gcc open-mpi" >&2; \
	  echo "  conda         : conda install -c conda-forge openmpi-mpifort" >&2; \
	  echo "Or pass a wrapper explicitly: make FC=/path/to/mpifort" >&2; \
	  exit 1; \
	fi
	@case "$(notdir $(FC))" in \
	  mpi*) ;; \
	  *) echo "WARNING: FC=$(FC) is not an MPI compiler wrapper." >&2; \
	     echo "         HeatFlow uses 'use mpi'; if the build fails with 'mpi.mod' not found," >&2; \
	     echo "         rerun with FC=mpifort (or set PATH to include your MPI bin directory)." >&2 ;; \
	esac

$(BIN_DIR) $(BUILD_DIR):
	mkdir -p $@

# Compile module sources
$(BUILD_DIR)/%.o: $(SRC_DIR)/heatflow/%.f90 | $(BUILD_DIR) check-mpi
	$(FC) $(FFLAGS) -c $< -o $@

# Main program
$(BUILD_DIR)/heatflow.o: $(SRC_DIR)/heatflow.f90 | $(BUILD_DIR) check-mpi
	$(FC) $(FFLAGS) -c $< -o $@

# Link (single definition)
$(TARGET): $(BIN_DIR) $(OBJS) | check-mpi
	$(FC) $(OPTFLAGS) $(OMPFLAGS) $(OBJS) -o $@ $(BLAS_FLAGS) $(PETSC_LIB) $(HDF5_LIB)

debug: FFLAGS = $(DEBUGFLAGS)
debug: check-mpi clean show $(TARGET)

run: $(TARGET)
	@if [ -z "$(strip $(MPIEXEC))" ]; then \
	  echo "ERROR: no MPI launcher (mpiexec/mpirun) found." >&2; \
	  echo "Set MPIEXEC=/path/to/mpiexec or add it to PATH." >&2; \
	  exit 1; \
	fi
	$(RUN_ENV) "$(MPIEXEC)" -n $(NCORES) $< $(RUN_ARGS)

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
	@echo "  FC=<path>          - MPI Fortran wrapper (default: auto-detected)"
	@echo "  MPIEXEC=<path>     - MPI launcher used by 'make run' (default: auto-detected)"
	@echo "  PETSC=0            - build without PETSc (legacy solver only)"
	@echo "  USE_HDF5=1         - build with HDF5 output support"
	@echo "  RUN_ARGS='-ksp_type cg -pc_type gamg -ksp_rtol 1e-8 -ksp_monitor'"
	@echo "Parallel build: make -j$(NCORES)"

####################################################################
# End
####################################################################
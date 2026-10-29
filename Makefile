####################################################################
# HeatFlow Build System (BLAS/LAPACK + optional PETSc + OpenMP)
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

# ---------------------------------------------------------------------------
# BLAS/LAPACK
# ---------------------------------------------------------------------------
# The current sources do not call BLAS/LAPACK directly, so a missing backend is
# not fatal.  If one is found it is linked (needed for static PETSc builds and
# future BLAS/LAPACK calls).
#
#   BLAS=0                  disable BLAS/LAPACK
#   BLAS_LIBS='...'         explicit linker flags (wins over detection)
#   BLAS_BACKEND=auto|accelerate|openblas|mkl|reference|download|none
#
# "download" (and `make deps-blas`) builds OpenBLAS from source into external/.
# Plain `make` never downloads anything.
# ---------------------------------------------------------------------------

BLAS         ?= 1
BLAS_BACKEND ?= auto
BLAS_LIBS    ?=

# Bundled OpenBLAS (opt-in)
DEPS_DIR         := $(abspath ./external)
OPENBLAS_VERSION ?= 0.3.34
OPENBLAS_SHA256  ?= cd7e129868320cc2d033afa920e31202dfe0b8066a5b66661900ccc0f197dfed
OPENBLAS_URL     ?= https://github.com/OpenMathLib/OpenBLAS/releases/download/v$(OPENBLAS_VERSION)/OpenBLAS-$(OPENBLAS_VERSION).tar.gz
OPENBLAS_TARBALL := $(DEPS_DIR)/downloads/OpenBLAS-$(OPENBLAS_VERSION).tar.gz
OPENBLAS_SRC     := $(DEPS_DIR)/src/OpenBLAS-$(OPENBLAS_VERSION)
OPENBLAS_PREFIX  := $(DEPS_DIR)/openblas-$(OPENBLAS_VERSION)
OPENBLAS_STAMP   := $(OPENBLAS_PREFIX)/.installed
OPENBLAS_JOBS    ?= $(NCORES)
OPENBLAS_CC      ?= cc
OPENBLAS_FC      ?= gfortran
OPENBLAS_FLAGS   ?=
SUBMAKE          := $(MAKE)

# Candidate library directories and helpers
BLAS_LIB_DIRS := \
  $(OPENBLAS_PREFIX)/lib \
  $(CONDA_PREFIX)/lib $(CONDA_PREFIX)/lib64 \
  $(MKLROOT)/lib/intel64 $(MKLROOT)/lib \
  $(OPENBLAS_ROOT)/lib $(OPENBLAS_HOME)/lib $(EBROOTOPENBLAS)/lib \
  $(AOCL_ROOT)/lib $(ARMPL_DIR)/lib \
  /opt/intel/oneapi/mkl/latest/lib/intel64 /opt/intel/oneapi/mkl/latest/lib \
  /opt/homebrew/lib /opt/homebrew/opt/openblas/lib \
  /usr/local/lib /usr/local/lib64 /usr/local/opt/openblas/lib /opt/local/lib \
  /usr/lib64/openblas /usr/lib64/openblas-pthread /usr/lib64/openblas-openmp \
  /usr/lib/x86_64-linux-gnu/openblas-pthread /usr/lib/x86_64-linux-gnu/openblas-openmp \
  /usr/lib/x86_64-linux-gnu /usr/lib/aarch64-linux-gnu /usr/lib64 /usr/lib

find_lib = $(firstword $(wildcard $(foreach d,$(BLAS_LIB_DIRS),$(addprefix $(d)/,$(1)))))
rpath_flags = $(sort $(foreach d,$(patsubst -L%,%,$(filter -L%,$(1))),-Wl,-rpath,$(d)))
lib_rpath = $(if $(filter /%,$(1)),-Wl,-rpath,$(patsubst %/,%,$(dir $(1))))

BUNDLED_OPENBLAS := $(firstword $(wildcard \
  $(OPENBLAS_PREFIX)/lib/libopenblas.so $(OPENBLAS_PREFIX)/lib/libopenblas.so.0 \
  $(OPENBLAS_PREFIX)/lib/libopenblas.dylib $(OPENBLAS_PREFIX)/lib/libopenblas.a))
PKG_OPENBLAS_LIBS := $(shell $(PKG_CONFIG) --libs openblas 2>/dev/null)
PKG_MKL_LIBS := $(shell for p in mkl-sdl mkl-dynamic-lp64-gomp mkl-dynamic-lp64-iomp mkl-dynamic-lp64-seq; do \
    libs=$$($(PKG_CONFIG) --libs $$p 2>/dev/null) && { echo "$$libs"; exit 0; }; \
  done)
PKG_BLAS_LIBS   := $(shell $(PKG_CONFIG) --libs blas 2>/dev/null)
PKG_LAPACK_LIBS := $(shell $(PKG_CONFIG) --libs lapack 2>/dev/null)
SYSTEM_OPENBLAS := $(call find_lib,libopenblas.so libopenblas.so.0 libopenblas.dylib libopenblas.a)
SYSTEM_MKL      := $(call find_lib,libmkl_rt.so libmkl_rt.dylib)
SYSTEM_BLAS     := $(call find_lib,libblas.so libblas.so.3 libblas.dylib libblas.a)
SYSTEM_LAPACK   := $(call find_lib,liblapack.so liblapack.so.3 liblapack.dylib liblapack.a)

BLAS_BACKEND_REQUESTED := $(BLAS_BACKEND)
BLAS_BACKEND_RESOLVED  := $(BLAS_BACKEND)

ifeq ($(BLAS),0)
BLAS_BACKEND_RESOLVED := none
BLAS_DISABLED := 1
else ifeq ($(BLAS_BACKEND),none)
BLAS_DISABLED := 1
endif

# Resolve "auto" from the available backends.  A previously downloaded
# OpenBLAS (make deps-blas) is preferred when present.
BLAS_AUTO_BACKEND := \
  $(if $(strip $(BUNDLED_OPENBLAS)),openblas-bundled, \
  $(if $(filter Darwin,$(UNAME_S)),accelerate, \
  $(if $(strip $(PKG_OPENBLAS_LIBS)),openblas-pkg, \
  $(if $(strip $(SYSTEM_OPENBLAS)),openblas-file, \
  $(if $(strip $(PKG_MKL_LIBS)),mkl-pkg, \
  $(if $(strip $(SYSTEM_MKL)),mkl-file, \
  $(if $(and $(strip $(SYSTEM_BLAS)),$(strip $(SYSTEM_LAPACK))),reference-file,none)))))))

ifeq ($(BLAS_BACKEND_RESOLVED),auto)
BLAS_BACKEND_RESOLVED := $(strip $(BLAS_AUTO_BACKEND))
else ifeq ($(BLAS_BACKEND_RESOLVED),download)
BLAS_BACKEND_RESOLVED := openblas-download
BLAS_DEPS             := $(OPENBLAS_STAMP)
else ifeq ($(BLAS_BACKEND_RESOLVED),openblas)
ifneq ($(strip $(BUNDLED_OPENBLAS)),)
BLAS_BACKEND_RESOLVED := openblas-bundled
else ifneq ($(strip $(PKG_OPENBLAS_LIBS)),)
BLAS_BACKEND_RESOLVED := openblas-pkg
else ifneq ($(strip $(SYSTEM_OPENBLAS)),)
BLAS_BACKEND_RESOLVED := openblas-file
else
BLAS_BACKEND_RESOLVED := none
BLAS_MISSING := 1
endif
else ifeq ($(BLAS_BACKEND_RESOLVED),mkl)
ifneq ($(strip $(PKG_MKL_LIBS)),)
BLAS_BACKEND_RESOLVED := mkl-pkg
else ifneq ($(strip $(SYSTEM_MKL)),)
BLAS_BACKEND_RESOLVED := mkl-file
else
BLAS_BACKEND_RESOLVED := none
BLAS_MISSING := 1
endif
else ifeq ($(BLAS_BACKEND_RESOLVED),reference)
ifneq ($(and $(strip $(PKG_BLAS_LIBS)),$(strip $(PKG_LAPACK_LIBS))),)
BLAS_BACKEND_RESOLVED := reference-pkg
else ifneq ($(and $(strip $(SYSTEM_BLAS)),$(strip $(SYSTEM_LAPACK))),)
BLAS_BACKEND_RESOLVED := reference-file
else
BLAS_BACKEND_RESOLVED := none
BLAS_MISSING := 1
endif
else ifeq ($(BLAS_BACKEND_RESOLVED),accelerate)
ifneq ($(UNAME_S),Darwin)
BLAS_BACKEND_RESOLVED := none
BLAS_MISSING := 1
endif
else ifeq ($(BLAS_BACKEND_RESOLVED),none)
# keep none
else
BLAS_BACKEND_RESOLVED := none
BLAS_MISSING := 1
endif

# Explicit linker flags win over backend selection (but BLAS=0 still disables)
ifneq ($(strip $(BLAS_LIBS)),)
ifneq ($(BLAS),0)
BLAS_FLAGS      := $(BLAS_LIBS)
BLAS_NOTE       := (user-specified BLAS/LAPACK)
BLAS_THREAD_ENV :=
BLAS_DEPS       :=
BLAS_MISSING    :=
BLAS_DISABLED   :=
BLAS_BACKEND_RESOLVED := user
endif
endif

ifeq ($(BLAS_BACKEND_RESOLVED),user)
# flags already set above
else ifeq ($(BLAS_BACKEND_RESOLVED),none)
BLAS_FLAGS      :=
BLAS_THREAD_ENV :=
ifneq ($(strip $(BLAS_DISABLED)),)
BLAS_NOTE := (BLAS/LAPACK disabled)
else ifeq ($(strip $(BLAS_MISSING)),1)
BLAS_NOTE := (requested BLAS backend '$(BLAS_BACKEND_REQUESTED)' not found)
else
BLAS_NOTE := (no BLAS/LAPACK found - not linked)
endif
else ifeq ($(BLAS_BACKEND_RESOLVED),accelerate)
MACOS_SDK := $(shell xcrun --show-sdk-path 2>/dev/null)
ifneq ($(MACOS_SDK),)
SYSROOT_FLAGS := -L$(MACOS_SDK)/usr/lib -F$(MACOS_SDK)/System/Library/Frameworks
endif
BLAS_FLAGS      := $(SYSROOT_FLAGS) -framework Accelerate -lpthread -lm
BLAS_NOTE       := (Apple Accelerate)
BLAS_THREAD_ENV := VECLIB_MAXIMUM_THREADS=1
else ifeq ($(BLAS_BACKEND_RESOLVED),openblas-bundled)
BLAS_FLAGS      := -L$(OPENBLAS_PREFIX)/lib -lopenblas -Wl,-rpath,$(OPENBLAS_PREFIX)/lib -lpthread -lm
BLAS_NOTE       := (OpenBLAS $(OPENBLAS_VERSION) bundled)
BLAS_THREAD_ENV := OPENBLAS_NUM_THREADS=1
else ifeq ($(BLAS_BACKEND_RESOLVED),openblas-download)
BLAS_DEPS       := $(OPENBLAS_STAMP)
BLAS_FLAGS      := -L$(OPENBLAS_PREFIX)/lib -lopenblas -Wl,-rpath,$(OPENBLAS_PREFIX)/lib -lpthread -lm
BLAS_NOTE       := (OpenBLAS $(OPENBLAS_VERSION) downloaded)
BLAS_THREAD_ENV := OPENBLAS_NUM_THREADS=1
else ifeq ($(BLAS_BACKEND_RESOLVED),openblas-pkg)
BLAS_FLAGS      := $(PKG_OPENBLAS_LIBS) $(call rpath_flags,$(PKG_OPENBLAS_LIBS)) -lpthread -lm
BLAS_NOTE       := (OpenBLAS via pkg-config)
BLAS_THREAD_ENV := OPENBLAS_NUM_THREADS=1
else ifeq ($(BLAS_BACKEND_RESOLVED),openblas-file)
BLAS_FLAGS      := $(SYSTEM_OPENBLAS) $(call lib_rpath,$(SYSTEM_OPENBLAS)) -lpthread -lm
BLAS_NOTE       := (OpenBLAS: $(SYSTEM_OPENBLAS))
BLAS_THREAD_ENV := OPENBLAS_NUM_THREADS=1
else ifeq ($(BLAS_BACKEND_RESOLVED),mkl-pkg)
BLAS_FLAGS      := $(PKG_MKL_LIBS) $(call rpath_flags,$(PKG_MKL_LIBS))
BLAS_NOTE       := (MKL via pkg-config)
BLAS_THREAD_ENV := MKL_NUM_THREADS=1
else ifeq ($(BLAS_BACKEND_RESOLVED),mkl-file)
BLAS_FLAGS      := $(SYSTEM_MKL) $(call lib_rpath,$(SYSTEM_MKL)) -lpthread -lm $(if $(filter Linux,$(UNAME_S)),-ldl)
BLAS_NOTE       := (MKL: $(SYSTEM_MKL))
BLAS_THREAD_ENV := MKL_NUM_THREADS=1
else ifeq ($(BLAS_BACKEND_RESOLVED),reference-pkg)
BLAS_FLAGS      := $(PKG_LAPACK_LIBS) $(PKG_BLAS_LIBS) $(call rpath_flags,$(PKG_LAPACK_LIBS) $(PKG_BLAS_LIBS)) -lpthread -lm
BLAS_NOTE       := (reference BLAS/LAPACK via pkg-config)
BLAS_THREAD_ENV :=
else ifeq ($(BLAS_BACKEND_RESOLVED),reference-file)
BLAS_FLAGS      := $(SYSTEM_LAPACK) $(SYSTEM_BLAS) $(call lib_rpath,$(SYSTEM_LAPACK)) $(call lib_rpath,$(SYSTEM_BLAS)) -lpthread -lm
BLAS_NOTE       := (reference BLAS/LAPACK)
BLAS_THREAD_ENV :=
endif

RUN_ENV := OMP_NUM_THREADS=1 OMP_PROC_BIND=spread OMP_PLACES=cores $(BLAS_THREAD_ENV)

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

.PHONY: all debug clean distclean run help show check-mpi check-blas \
        deps deps-blas clean-deps

all: check-mpi check-blas $(BLAS_DEPS) show $(TARGET)

show:
	@printf 'Building %s %s %s\n' '$(NAME)' '$(PETSC_NOTE)' '$(HDF5_NOTE)'
	@printf '  Fortran compiler : %s\n' '$(FC)'
	@printf '  MPI launcher     : %s\n' '$(if $(strip $(MPIEXEC)),$(MPIEXEC),not found)'
	@printf '  BLAS/LAPACK      : %s\n' '$(BLAS_NOTE)'

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

check-blas:
	@if [ -n "$(strip $(BLAS_MISSING))" ]; then \
	  echo "ERROR: requested BLAS backend '$(BLAS_BACKEND_REQUESTED)' was not found." >&2; \
	  echo "Install it, set BLAS_LIBS='...', or run: make deps-blas" >&2; \
	  exit 1; \
	fi
	@if [ "$(BLAS_BACKEND_RESOLVED)" = "none" ] && [ -z "$(strip $(BLAS_DISABLED))" ]; then \
	  echo "WARNING: no BLAS/LAPACK backend found; linking without it." >&2; \
	  echo "         The current sources do not call BLAS/LAPACK directly." >&2; \
	  echo "         Run 'make deps-blas' to build OpenBLAS locally," >&2; \
	  echo "         or set BLAS_LIBS='...' to use an existing installation." >&2; \
	fi

$(BIN_DIR) $(BUILD_DIR):
	mkdir -p $@

# Compile module sources
$(BUILD_DIR)/%.o: $(SRC_DIR)/heatflow/%.f90 | $(BUILD_DIR) check-mpi check-blas $(BLAS_DEPS)
	$(FC) $(FFLAGS) -c $< -o $@

# Main program
$(BUILD_DIR)/heatflow.o: $(SRC_DIR)/heatflow.f90 | $(BUILD_DIR) check-mpi check-blas $(BLAS_DEPS)
	$(FC) $(FFLAGS) -c $< -o $@

# Link (single definition)
$(TARGET): $(BIN_DIR) $(OBJS) | check-mpi check-blas $(BLAS_DEPS)
	$(FC) $(OPTFLAGS) $(OMPFLAGS) $(OBJS) -o $@ $(PETSC_LIB) $(BLAS_FLAGS) $(HDF5_LIB)

debug: FFLAGS = $(DEBUGFLAGS)
debug: check-mpi check-blas $(BLAS_DEPS) clean show $(TARGET)

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

# ---------------------------------------------------------------------------
# Optional bundled OpenBLAS (strictly opt-in: make deps-blas)
# ---------------------------------------------------------------------------
deps: deps-blas

deps-blas: $(OPENBLAS_STAMP)

$(OPENBLAS_TARBALL):
	@mkdir -p $(dir $@)
	@echo "[DEPS] Downloading OpenBLAS $(OPENBLAS_VERSION)"
	@if command -v curl >/dev/null 2>&1; then \
	  curl -fL --retry 3 "$(OPENBLAS_URL)" -o "$@.tmp"; \
	elif command -v wget >/dev/null 2>&1; then \
	  wget -O "$@.tmp" "$(OPENBLAS_URL)"; \
	else \
	  echo "ERROR: curl or wget is required to download OpenBLAS." >&2; \
	  exit 1; \
	fi
	@if command -v sha256sum >/dev/null 2>&1; then \
	  sum=$$(sha256sum "$@.tmp" | awk '{print $$1}'); \
	elif command -v shasum >/dev/null 2>&1; then \
	  sum=$$(shasum -a 256 "$@.tmp" | awk '{print $$1}'); \
	else \
	  echo "ERROR: sha256sum or shasum is required to verify the download." >&2; \
	  rm -f "$@.tmp"; exit 1; \
	fi; \
	if [ -z "$(OPENBLAS_SHA256)" ]; then \
	  echo "WARNING: OPENBLAS_SHA256 is empty; skipping checksum verification." >&2; \
	elif [ "$$sum" != "$(OPENBLAS_SHA256)" ]; then \
	  echo "ERROR: OpenBLAS checksum mismatch." >&2; \
	  echo "  expected: $(OPENBLAS_SHA256)" >&2; \
	  echo "  actual:   $$sum" >&2; \
	  rm -f "$@.tmp"; exit 1; \
	fi
	@mv "$@.tmp" "$@"

$(OPENBLAS_SRC)/Makefile: $(OPENBLAS_TARBALL)
	@command -v tar >/dev/null 2>&1 || { \
	  echo "ERROR: tar is required to extract OpenBLAS." >&2; exit 1; \
	}
	@mkdir -p $(DEPS_DIR)/src
	@echo "[DEPS] Extracting OpenBLAS $(OPENBLAS_VERSION)"
	@rm -rf "$(OPENBLAS_SRC)"
	@tar -xzf "$<" -C "$(DEPS_DIR)/src"
	@touch "$@"

$(OPENBLAS_STAMP): $(OPENBLAS_SRC)/Makefile
	@command -v $(OPENBLAS_CC) >/dev/null 2>&1 || { \
	  echo "ERROR: C compiler '$(OPENBLAS_CC)' not found (set OPENBLAS_CC=...)." >&2; exit 1; \
	}
	@command -v $(OPENBLAS_FC) >/dev/null 2>&1 || { \
	  echo "ERROR: Fortran compiler '$(OPENBLAS_FC)' not found (set OPENBLAS_FC=...)." >&2; exit 1; \
	}
	@echo "[DEPS] Building OpenBLAS $(OPENBLAS_VERSION) with $(OPENBLAS_JOBS) jobs"
	$(SUBMAKE) -C "$(OPENBLAS_SRC)" -j$(OPENBLAS_JOBS) \
	  CC="$(OPENBLAS_CC)" FC="$(OPENBLAS_FC)" NO_LAPACKE=1 \
	  $(OPENBLAS_FLAGS) shared
	@echo "[DEPS] Installing OpenBLAS into $(OPENBLAS_PREFIX)"
	$(SUBMAKE) -C "$(OPENBLAS_SRC)" install PREFIX="$(OPENBLAS_PREFIX)" \
	  CC="$(OPENBLAS_CC)" FC="$(OPENBLAS_FC)" NO_LAPACKE=1 \
	  $(OPENBLAS_FLAGS)
	@touch "$@"

clean-deps:
	@if [ -z "$(DEPS_DIR)" ] || [ "$(DEPS_DIR)" = "/" ]; then \
	  echo "ERROR: refusing to remove '$(DEPS_DIR)'" >&2; exit 1; \
	fi
	@echo "[CLEAN] downloaded dependencies"
	@rm -rf "$(DEPS_DIR)"

help:
	@echo "Targets:"
	@echo "  make / make all    - build optimized"
	@echo "  make debug         - debug build"
	@echo "  make run           - run distributed across all cores via MPI"
	@echo "  make clean         - remove objects/modules"
	@echo "  make distclean     - remove executable"
	@echo "  make deps          - download and build OpenBLAS (BLAS/LAPACK)"
	@echo "  make deps-blas     - same as 'make deps'"
	@echo "  make clean-deps    - remove downloaded dependencies"
	@echo "Variables:"
	@echo "  FC=<path>          - MPI Fortran wrapper (default: auto-detected)"
	@echo "  MPIEXEC=<path>     - MPI launcher used by 'make run' (default: auto-detected)"
	@echo "  PETSC=0            - build without PETSc (legacy solver only)"
	@echo "  USE_HDF5=1         - build with HDF5 output support"
	@echo "  BLAS=0             - build without linking BLAS/LAPACK"
	@echo "  BLAS_BACKEND=...   - auto|accelerate|openblas|mkl|reference|download|none"
	@echo "  BLAS_LIBS='...'    - override BLAS/LAPACK linker flags"
	@echo "  OPENBLAS_VERSION=<v> - OpenBLAS version used by 'make deps-blas'"
	@echo "  OPENBLAS_JOBS=...  - parallel jobs for the OpenBLAS build (default: $(NCORES))"
	@echo "  OPENBLAS_FLAGS='...' - extra flags for the OpenBLAS build (e.g. TARGET=...)"
	@echo "  RUN_ARGS='-ksp_type cg -pc_type gamg -ksp_rtol 1e-8 -ksp_monitor'"
	@echo "Parallel build: make -j$(NCORES)"

####################################################################
# End
####################################################################
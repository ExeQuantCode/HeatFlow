# Installing HeatFlow on Red Hat Enterprise Linux without sudo

This guide builds HeatFlow with MPI, PETSc, HDF5, and OpenMP on a Red Hat Enterprise Linux (RHEL) system where you do not have administrator access. The recommended route uses a self-contained conda-forge environment, so it does not modify system packages.

The procedure was tested on:

- RHEL 9.8, x86_64
- GNU Fortran 15.3.0 from conda-forge
- Open MPI 5.0.10
- PETSc 3.25.5 (real, 32-bit integer build)
- HDF5 2.2.0 with Fortran and MPI support

####################################################################
# 1. Install Miniforge without sudo
####################################################################

Skip this section if `conda` or `mamba` is already available.

Check the machine architecture:

```bash
uname -m
```

For the tested `x86_64` architecture, install Miniforge in your home
directory:

```bash
curl -L -o /tmp/Miniforge3.sh \
  https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-Linux-x86_64.sh
bash /tmp/Miniforge3.sh -b -p "$HOME/miniforge3"
source "$HOME/miniforge3/etc/profile.d/conda.sh"
```

For an `aarch64` machine, replace `Linux-x86_64` in the download name with
`Linux-aarch64`. The build and runtime validation below was performed on
`x86_64`.

####################################################################
# 2. Create the MPI, PETSc, and HDF5 environment
####################################################################

Create one environment containing the compiler and all linked libraries:

```bash
conda create -y -n heatflow-build \
  -c conda-forge --strict-channel-priority \
  compilers make pkg-config \
  "mpi=1.0=openmpi" openmpi petsc hdf5
conda activate heatflow-build
```

If a centrally installed Conda cannot write to its package cache, select a user-writable cache before running `conda create`:

```bash
export CONDA_PKGS_DIRS="${TMPDIR:-/tmp}/${USER}-conda-pkgs"
mkdir -p "$CONDA_PKGS_DIRS"
```

Do not combine a system MPI compiler with Conda PETSc, or Conda MPI with a system PETSc. MPI libraries from different installations are not generally ABI-compatible. Activating the single environment above keeps the compiler, PETSc, and HDF5 on the same MPI implementation.

Verify that the active tools and libraries all come from the environment:

```bash
command -v mpifort mpiexec pkg-config
mpifort --showme:version
pkg-config --modversion PETSc
pkg-config --modversion hdf5_fortran
```

Each path printed by `command -v` should begin with the active Conda
environment path. Both `pkg-config` version commands must succeed.

####################################################################
# 3. Compile HeatFlow
####################################################################

From the repository root, clean any objects made with a different compiler or feature set and build with HDF5 enabled:

```bash
make distclean
make USE_HDF5=1
```

The executable is created at:

```text
bin/ThermalFlow.x
```

The expected build banner resembles:

```text
Building ThermalFlow.x (pkg-config PETSc) (+ HDF5) (PETSc BLAS/LAPACK)
```

The Makefile uses `mpifort` so the MPI compiler matches PETSc. PETSc and HDF5 flags are obtained from their `pkg-config` files instead of hard-coded Debian, RHEL, or Conda paths. HeatFlow does not call BLAS directly, so PETSc provides the appropriate BLAS/LAPACK dependency.

The Fortran sources depend on generated module files and are intentionally compiled in source order. Supplying `make -j` is safe, but does not make this part of the build concurrent.

Whenever `USE_HDF5`, the compiler, or the Conda environment changes, run `make distclean` before rebuilding. Make does not otherwise know that compilerflags or library paths changed.


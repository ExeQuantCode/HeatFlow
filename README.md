[![License workflow](https://img.shields.io/badge/License-GPLv3-yellow.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html "View GPLv3 license")
[![CMAKE](https://img.shields.io/badge/cmake-3.27.7-red)](https://github.com/Kitware/CMake/releases/tag/v3.27.7 "View cmake")
[![GCC compatibility](https://img.shields.io/badge/gcc-14.1.0-green)](https://gcc.gnu.org/gcc-14/ "View GCC")


# HeatFlow

by Harry Mclean, Francis Huw Davies, Ned Thaddeus Taylor, and Steven Paul Hepplestone

HeatFlow is Fortran-based a software package for modelling dynamical heat transport in systems using finite difference methods.
The software is primarily designed to utilise the Cattaneo method.
However, the Fourier method can be used instead.

**IMPORTANT NOTICE: Repository Migration to GitHub**

Dear users and contributors,

This repository has been to be migrated from the University of Exeter GitLab to GitHub to facilitate community interaction and support.
The latest version, updates, and collaboration now take place on this GitHub repository.

**GitLab Repository (Archived):** https://git.exeter.ac.uk/hepplestone/heatflow-mk2

## Why the Migration?

It was decided that this project should be migrated to allow for better community support (i.e. allowing community users to raise issues).
All information has been ported over where possible.
Releases prior to `HeatFlow_CattaneoPaper` have had their history modified to remove history of files over 50MB in size.


---


## Requirements

- Fortran compiler supporting Fortran 2003 standard or later
- MPI (e.g. OpenMPI) — required
- PETSc — optional, enables the `ilu`, `lu` and `gamg` solvers (without it only the `legacy` solver is available)
- HDF5 — optional, enables HDF5 output (`_HDF5Output` flag)
- BLAS/LAPACK — optional (auto-detected; `make deps-blas` can build OpenBLAS locally)
- GNU Make or CMake (fpm can also be used)

On macOS, dependencies are available via Homebrew: `brew install open-mpi petsc hdf5-mpi`

The software bas been developed and tested using the following Fortran compilers:
- gfortran -- gcc 13.2.0
- gfortran -- gcc 14.1.0

## Installation

To install HeatFlow, the source must be obtained from the git repository. Use the following commands to get started:
```
 git clone https://github.com/ExeQuantCode/HeatFlow.git
 cd HeatFlow
```

### make

The simplest way to build is with the provided Makefile. PETSc is detected automatically;
if it is not found, a legacy-solver-only version is built:

```
make
```

To force a build without PETSc even when it is installed:

```
make PETSC=0
```

To build with HDF5 output support:

```
make USE_HDF5=1
```

MPI is located by searching for an MPI Fortran wrapper (`mpifort`, `mpif90`, ...) on `PATH` and in common install prefixes (Homebrew, conda, distro packages, Intel oneAPI). If your MPI is in a non-standard location, point the build at its wrapper explicitly:

```
make FC=/path/to/mpifort
```

BLAS/LAPACK are auto-detected as well: Apple Accelerate on macOS, and OpenBLAS, MKL or reference BLAS/LAPACK on Linux (including conda, Homebrew and module prefixes). The current sources do not call BLAS/LAPACK directly, so a missing backend is not fatal and the build continues without one. To use an existing installation explicitly:

```
make BLAS_LIBS='-L/path/to/lib -lopenblas'
```

To download and build OpenBLAS locally (strictly opt-in; requires network access, `tar`, `cc`, `gfortran` and `curl` or `wget`):

```
make deps-blas
make
```

or in one step:

```
make BLAS_BACKEND=download
```

OpenBLAS is installed under `external/` (gitignored); `make clean-deps` removes it. To build without linking BLAS/LAPACK at all, use `make BLAS=0`. The download is pinned to OpenBLAS 0.3.34 (BSD-3-Clause) and verified against a SHA-256 checksum; `OPENBLAS_VERSION`, `OPENBLAS_CC`, `OPENBLAS_FC`, `OPENBLAS_JOBS` and `OPENBLAS_FLAGS` can be overridden if needed (for example `OPENBLAS_FLAGS='TARGET=ARMV8 DYNAMIC_ARCH=1'`). If you change `OPENBLAS_VERSION`, also set `OPENBLAS_SHA256` to the matching checksum (or set it empty to skip verification).

PETSc is detected via pkg-config or `PETSC_DIR`/`PETSC_ARCH` on Linux, and via Homebrew on macOS. The executable is written to `bin/HeatFlow`.

### cmake

For cmake installation, start within the repository root directory, run the following commands:

```
mkdir build
cd build
cmake [-DCMAKE_BUILD_TYPE=Release] [-DHEATFLOW_PETSC=ON] [-DHEATFLOW_HDF5=ON] \
      [-DHEATFLOW_BLAS=ON|OFF] \
      [-DHEATFLOW_BLAS_BACKEND=auto|accelerate|openblas|mkl|reference|download|none] \
      [-DHEATFLOW_BLAS_LIBRARIES='-L/path/to/lib;-lopenblas'] ..
make install
```

BLAS/LAPACK are auto-detected (Accelerate on macOS; OpenBLAS, MKL or reference BLAS/LAPACK on Linux). Use `-DHEATFLOW_BLAS=OFF` to disable linking, or `-DHEATFLOW_BLAS_LIBRARIES=...` to select a specific installation. To build OpenBLAS locally first (strictly opt-in; requires `make` and network access), configure with `-DHEATFLOW_BLAS_DOWNLOAD=ON` or `-DHEATFLOW_BLAS_BACKEND=download`; this runs `make deps-blas` and links the result from `external/`.

This will build and install the executable in the following directory:
```
${HOME}/.local/HeatFlow/bin/HeatFlow
```

This executable can now be called to run the HeatFlow software package and simulate heat transport.
If the `${HOME}/.local/HeatFlow/bin` is added to your `PATH` environment variable, then the program can be called as a terminal command.
This can be done with the following command (works on a per-terminal basis, if you want to update it for all, include this in your source shell file):

```
export PATH="${PATH}:${HOME}/.local/HeatFlow/bin"
```

### fpm

To build using fpm, run one of the following commands in the repository root directory:

```
fpm build --profile release          # legacy-only build (no PETSc/HDF5)
./build_fpm.sh                       # auto-detect and link BLAS/LAPACK
BLAS=0 ./build_fpm.sh                # build without BLAS/LAPACK
BLAS_BACKEND=download ./build_fpm.sh # opt-in local OpenBLAS build
```

For PETSc support:

```
PETSC_DIR=/path/to/petsc PETSC_ARCH=arch-your-build ./build_fpm_with_petsc.sh
```

`PETSC_ARCH` is optional for an installed PETSc. The wrapper scripts reuse the Makefile's BLAS/LAPACK detection and support `BLAS_LIBS`, `BLAS_BACKEND`, `FPM_PROFILE`, `FPM_FFLAGS` and `FPM_LDFLAGS`. The `mpi` dependency is resolved by fpm; the manifest is compatible with fpm 0.10 and later. HDF5 can be enabled by passing its flags explicitly through `FPM_FFLAGS`/`FPM_LDFLAGS`.

### Troubleshooting

- PETSc 3.23+ on macOS: `Cannot create PETSC_NULL_XXX object` — see [docs/PETSC_NULL_OBJECT_FIX.md](docs/PETSC_NULL_OBJECT_FIX.md).

## Running

The simulation is controlled by input files (`param.in`, `mat.in`, `system.in`), read by default from `./inputs`, with output written to `./outputs` (restart files in `./restart`). The directories can be selected on the command line — `-i`/`--input-directory`, `-o`/`--output-directory`, or the `--directory` alias which maps to `<dir>/inputs`, `<dir>/outputs` and `<dir>/restart` (the output directory is created if it does not exist).

Example command:

```
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 mpiexec -n 2 ./bin/HeatFlow -i ./inputs -o ./outputs
```

or equivalently, with a single run directory:

```
mpiexec -n 2 ./bin/HeatFlow --directory .
```

The linear solver is chosen at runtime with the `solver` keyword in `param.in` (`legacy`, `ilu`, `lu` or `gamg`; default `ilu`). PETSc runtime options (e.g. `-ksp_monitor`) can be passed on the command line alongside the directory options:

```
mpiexec -n 2 ./bin/HeatFlow -i ./inputs -o ./outputs -ksp_monitor
```

Setting `_HDF5Output = T` in `param.in` writes the temperature field to `outputs/output_<RunName>.h5` (requires a build with HDF5 support).

See `docs/USER_MANUAL.md` for the full list of input keywords and options.

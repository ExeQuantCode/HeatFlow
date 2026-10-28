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

## How to Contribute on GitHub?

Thank you for your understanding and continued support!

---


## Requirements

- Fortran compiler supporting Fortran 2003 standard or later
- MPI (e.g. OpenMPI) — required
- PETSc — optional, enables the `ilu`, `lu` and `gamg` solvers (without it only the `legacy` solver is available)
- GNU Make or CMake (fpm can also be used)

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

The executable is written to `bin/ThermalFlow.x`.

### cmake

For cmake installation, start within the repository root directory, run the following commands:

```
mkdir build
cd build
cmake [-DCMAKE_BUILD_TYPE=Release] [-DHEATFLOW_PETSC=OFF] ..
make install
```

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

To install using fpm, run the following command in the repository root directory:

```
fpm build --profile=release
```

## Running

The simulation is controlled by input files (`param.in`, `mat.in`, `system.in`), read by default from `./inputs`, with output written to `./outputs`. Both directories can be selected with the `-i` and `-o` command line options (the output directory is created if it does not exist).

Example command:

```
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 mpiexec -n 2 ./bin/ThermalFlow.x -i ./inputs -o ./outputs
```

The linear solver is chosen at runtime with the `solver` keyword in `param.in` (`legacy`, `ilu`, `lu` or `gamg`; default `ilu`). PETSc runtime options (e.g. `-ksp_monitor`) can be passed on the command line alongside `-i`/`-o`:

```
mpiexec -n 2 ./bin/ThermalFlow.x -i ./inputs -o ./outputs -ksp_monitor
```

See `docs/USER_MANUAL.md` for the full list of input keywords and options.

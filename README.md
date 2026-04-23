[![License workflow](https://img.shields.io/badge/License-GPLv3-yellow.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html "View GPLv3 license")
[![CMAKE](https://img.shields.io/badge/cmake-3.27.7-red)](https://github.com/Kitware/CMake/releases/tag/v3.27.7 "View cmake")
[![GCC compatibility](https://img.shields.io/badge/gcc-14.1.0-green)](https://gcc.gnu.org/gcc-14/ "View GCC")


# HeatFlow

by Harry Mclean, Francis Huw Davies, Ned Thaddeus Taylor, and Steven Paul Hepplestone

HeatFlow is a Fortran-based software package for modelling dynamical heat transport in systems using finite difference methods.
The software is primarily designed to utilise the Cattaneo method, although the Fourier method can also be used.

---

## IMPORTANT NOTICE: Repository Migration to GitHub

This repository has been migrated from the University of Exeter GitLab to GitHub to facilitate community interaction and support.
The latest version, updates, and collaboration now take place here.

**GitLab Repository (Archived):**
[https://git.exeter.ac.uk/hepplestone/heatflow-mk2](https://git.exeter.ac.uk/hepplestone/heatflow-mk2)

### Why the Migration?

The move enables better community support, including issue tracking and collaboration.
All information has been ported where possible.
Releases prior to `HeatFlow_CattaneoPaper` have had their history modified to remove files larger than 50 MB.

---

## Requirements

* Fortran compiler supporting Fortran 2003 or later
* fpm or CMake
* PETSc
* MPI

Tested with:

* gfortran (GCC 13.2.0)
* gfortran (GCC 14.1.0)

---

## Installation

First obtain the source:

```bash
git clone https://github.com/ExeQuantCode/HeatFlow.git
cd HeatFlow
```

---

## Building with fpm

PETSc is configured at build time (not hard-coded), making the build portable across systems.

### 1. Set PETSc environment variables

```bash
export PETSC_DIR=/path/to/petsc
export PETSC_ARCH=arch-your-build
```

These are standard variables provided by PETSc.

---

### 2. Build

```bash
fpm build \
  --flag "-I${PETSC_DIR}/include -I${PETSC_DIR}/${PETSC_ARCH}/include" \
  --link-flag "-L${PETSC_DIR}/${PETSC_ARCH}/lib -Wl,-rpath,${PETSC_DIR}/${PETSC_ARCH}/lib"
```

---

### 3. Run

```bash
fpm run --profile petsc -- [ALL PROGRAM OPTIONS]
```

---

### Alternative: pkg-config

If PETSc provides pkg-config:

```bash
fpm build \
  --flag "$(pkg-config --cflags petsc)" \
  --link-flag "$(pkg-config --libs petsc)"
```

---

### Notes (fpm)

* No PETSc paths are stored in `fpm.toml`
* All system-specific configuration is provided at build time
* Works well on clusters, macOS, and CI environments

---

## Building with CMake

CMake provides automatic detection of MPI and PETSc.

### 1. Configure PETSc

If PETSc is not installed system-wide:

```bash
export PETSC_DIR=/path/to/petsc
export PETSC_ARCH=arch-your-build
```

---

### 2. Configure and build

```bash
cmake -B build -S . -DCMAKE_BUILD_TYPE=Release
cmake --build build
```

Or explicitly:

```bash
cmake -B build -S . \
  -DCMAKE_BUILD_TYPE=Release \
  -DPETSC_DIR=$PETSC_DIR \
  -DPETSC_ARCH=$PETSC_ARCH
```

---

### 3. Install

```bash
cmake --install build
```

This installs the executable to:

```bash
${HOME}/.local/HeatFlow/bin/HeatFlow
```

Add to your `PATH` if desired:

```bash
export PATH="${PATH}:${HOME}/.local/HeatFlow/bin"
```

---

### 4. Run

```bash
HeatFlow
```

Optional directory flags:

```bash
HeatFlow --input-directory /path/to/run/inputs --output-directory /path/to/run/outputs
```

Compatibility alias for the legacy run-layout:

```bash
HeatFlow --directory /path/to/run
```

The legacy alias maps to `/path/to/run/inputs`, `/path/to/run/outputs`, and `/path/to/run/restart`.

---

### Using pkg-config (recommended)

If PETSc supports pkg-config, it will be detected automatically:

```bash
cmake -B build -S .
cmake --build build
```

---

### Notes (CMake)

* PETSc detection order:

  1. pkg-config
  2. `PETSC_DIR` / `PETSC_ARCH`
  3. Local `../petsc` fallback

* MPI is detected automatically

* Configuration fails with a clear error if PETSc is not found

---

## Summary

* **fpm**: lightweight and flexible, ideal for development
* **CMake**: robust and portable, suited for deployment

In both cases, PETSc is configured at build time rather than hard-coded, ensuring portability across systems.

# GPU Walkthrough

This document describes the currently working path for building and running the HeatFlow GPU backend on a fresh Linux machine or HPC system.

It matches the current GPU flow in this repository:

- HeatFlow GPU backend uses PETSc plus Kokkos plus CUDA
- GPU build target is `make gpu`
- Benchmark script is `./benchmark.sh`
- The current verified runtime uses `-use_gpu_aware_mpi 0` unless your MPI is GPU-aware

This guide is written for the `117-gpu-cuda` branch and is based on a verified working setup on Linux with an NVIDIA GPU.

## 1. What You Need

At minimum, you need:

- A Linux machine or HPC login/build environment
- An NVIDIA GPU on the target runtime nodes
- A working NVIDIA driver on the runtime nodes
- A CUDA toolkit, or the CUDA component from NVHPC
- A C compiler, C++ compiler, and Fortran compiler
- MPI wrappers: `mpicc`, `mpicxx`, `mpifort`
- `make`, `git`, `python3`, and `pkg-config`
- BLAS/LAPACK, ideally OpenBLAS

For an HPC system, this usually means loading modules first. A typical starting point is:

```bash
module purge
module load gcc
module load openmpi
module load cuda
module load cmake
module load python
module load openblas
```

Use the module names that exist on your site.

## 2. Important Compatibility Rule

The most important build rule is:

**PETSc Fortran modules and HeatFlow must be compiled with the same underlying GNU Fortran major version.**

If PETSc is built with one `gfortran` and HeatFlow is built with another, you will usually see an error like:

```text
Cannot read module file 'petscksp.mod' ... because it was created by a different version of GNU Fortran
```

Before building anything, check what your MPI wrapper is really using:

```bash
which mpifort
mpifort --version
mpifort -show
which gfortran
gfortran --version
```

If `mpifort -show` resolves to a different `gfortran` than the one you expect, fix your `PATH` or your module load order before continuing.

## 3. Suggested Directory Layout

The exact paths do not matter, but using a clean layout helps.

```bash
export HF_ROOT=$HOME/HeatFlow
export PETSC_SRC=$HOME/petsc-src
export PETSC_PREFIX=$HOME/petsc-kokkos
export CUDA_DIR=$CUDA_HOME
export CUDA_ARCH=86
```

Notes:

- `HF_ROOT` is the HeatFlow repository root
- `PETSC_SRC` is a PETSc source checkout
- `PETSC_PREFIX` is the install prefix for the GPU-enabled PETSc build
- `CUDA_DIR` should point at the CUDA toolkit root used for the PETSc build
- `CUDA_ARCH` is your GPU compute capability without the dot, for example `80`, `86`, `90`

If you are not sure about the CUDA architecture, check your GPU model and map it to the correct SM value before running PETSc configure.

## 4. Clone HeatFlow

```bash
git clone https://github.com/ExeQuantCode/HeatFlow.git "$HF_ROOT"
cd "$HF_ROOT"
git checkout 117-gpu-cuda
```

## 5. Build PETSc With CUDA and Kokkos

HeatFlow's GPU build expects PETSc to be installed and to provide:

- shared libraries
- Fortran modules
- CUDA support
- Kokkos support
- Kokkos-Kernels support
- the file `lib/petsc/conf/petscvariables` under the PETSc install prefix

### Preferred PETSc Configure Path

On a fresh machine, start with the simplest PETSc configure that lets PETSc download Kokkos and Kokkos-Kernels itself:

```bash
git clone -b v3.21.6 https://gitlab.com/petsc/petsc.git "$PETSC_SRC"
cd "$PETSC_SRC"

./configure \
  --prefix="$PETSC_PREFIX" \
  --with-cc="$(which mpicc)" \
  --with-cxx="$(which mpicxx)" \
  --with-fc="$(which mpifort)" \
  --with-cuda=1 \
  --with-cuda-dir="$CUDA_DIR" \
  --with-cuda-arch="$CUDA_ARCH" \
  --with-kokkos=1 \
  --download-kokkos \
  --with-kokkos-kernels=1 \
  --download-kokkos-kernels \
  --with-openmp=1 \
  --with-shared-libraries=1 \
  --with-debugging=0 \
  --with-x=0 \
  COPTFLAGS="-O3" \
  CXXOPTFLAGS="-O3" \
  FOPTFLAGS="-O3" \
  CUDAOPTFLAGS="-O3"

make all -j"$(nproc)"
make install
```

### If Your Site Blocks PETSc Downloads

If the cluster blocks external downloads during configure, install Kokkos and Kokkos-Kernels separately first, then point PETSc at them:

```bash
./configure \
  --prefix="$PETSC_PREFIX" \
  --with-cc="$(which mpicc)" \
  --with-cxx="$(which mpicxx)" \
  --with-fc="$(which mpifort)" \
  --with-cuda=1 \
  --with-cuda-dir="$CUDA_DIR" \
  --with-cuda-arch="$CUDA_ARCH" \
  --with-kokkos=1 \
  --with-kokkos-dir="$PETSC_PREFIX" \
  --with-kokkos-kernels=1 \
  --with-kokkos-kernels-dir="$PETSC_PREFIX" \
  --with-openmp=1 \
  --with-shared-libraries=1 \
  --with-debugging=0 \
  --with-x=0
```

### PETSc Sanity Checks

After install, verify these paths exist:

```bash
ls "$PETSC_PREFIX/include/petscksp.mod"
ls "$PETSC_PREFIX/lib/libpetsc.so"
ls "$PETSC_PREFIX/lib/petsc/conf/petscvariables"
```

If those files are missing, HeatFlow's GPU build is not ready yet.

## 6. Build HeatFlow CPU First

This is optional, but it is the fastest way to confirm that the repo and case files are healthy before you introduce GPU-specific complexity.

```bash
cd "$HF_ROOT"
make clean
make
```

The CPU build uses system PETSc discovery through `pkg-config` when available.

## 7. Build HeatFlow GPU

The current Makefile exposes three important GPU build knobs:

- `GPU_PETSC_DIR`
- `GPU_GFORTRAN_PATH`
- `GPU_MPI_LIBDIRS` and `GPU_EXTRA_LIBS` if your library paths differ from the current defaults

### Recommended GPU Build Command

```bash
cd "$HF_ROOT"

export GFORTRAN_BIN="$(dirname "$(readlink -f "$(which gfortran)")")"

make gpuclean
make gpu \
  GPU_PETSC_DIR="$PETSC_PREFIX" \
  GPU_GFORTRAN_PATH="$GFORTRAN_BIN:/usr/bin:/bin"
```

This works because `GPU_GFORTRAN_PATH` is used to make sure the `mpifort` wrapper sees the same `gfortran` family that was used to generate PETSc's `.mod` files.

### When You Need Extra Link Overrides

On some HPC systems, you may need to override additional library search paths from the command line, for example:

```bash
make gpu \
  GPU_PETSC_DIR="$PETSC_PREFIX" \
  GPU_GFORTRAN_PATH="$GFORTRAN_BIN:/usr/bin:/bin" \
  GPU_MPI_LIBDIRS="-L/path/to/mpi/lib -Wl,-rpath,/path/to/mpi/lib" \
  GPU_EXTRA_LIBS="-L/path/to/cuda/targets/x86_64-linux/lib -Wl,-rpath,/path/to/cuda/targets/x86_64-linux/lib -lnvJitLink /path/to/libudev.so.1 /path/to/libcap.so.2"
```

The exact values are site-specific. The key point is that the GPU link step must be able to resolve:

- PETSc
- Kokkos and Kokkos-Kernels
- CUDA runtime libraries
- MPI libraries
- any extra transitive system libraries your site requires

## 8. Prepare a Case Directory

HeatFlow should be run from a directory containing an `inputs/` directory with at least:

- `inputs/param.in`
- `inputs/system.in`
- `inputs/mat.in`

Example layout:

```text
case-root/
  inputs/
    param.in
    system.in
    mat.in
```

## 9. Run the GPU Build

Start with a single MPI rank first. That removes most cluster-specific MPI placement issues while you validate the toolchain.

```bash
cd /path/to/case-root
OMP_NUM_THREADS=1 \
OPENBLAS_NUM_THREADS=1 \
OMP_PROC_BIND=spread \
OMP_PLACES=threads \
mpiexec -n 1 "$HF_ROOT/bin/ThermalFlow-gpu.x" -use_gpu_aware_mpi 0
```

### Why `-use_gpu_aware_mpi 0`?

The current verified build runs on an OpenMPI installation that is not GPU-aware. Without this flag, PETSc aborts with a message similar to:

```text
PETSc is configured with GPU support, but your MPI is not GPU-aware
```

If your MPI is GPU-aware, you can remove this option.

If your cluster is not GPU-aware and you do not want to type the flag every time, set:

```bash
export PETSC_OPTIONS="-use_gpu_aware_mpi 0"
```

## 10. Run the CPU vs GPU Benchmark

The repository already contains a benchmark helper:

```bash
cd "$HF_ROOT"
./benchmark.sh /path/to/case-root 3
```

What it does:

- rebuilds the CPU binary
- rebuilds the GPU binary
- runs both backends
- extracts `simulation wall time`
- writes results to `benchmark_results.txt` in the case directory

The current script already applies the `-use_gpu_aware_mpi 0` fallback to the GPU run.

## 11. Known-Good Example

On the verified Cloak case used during development, a single-run benchmark produced:

- CPU: `94.802 s`
- GPU: `25.370 s`
- speedup: about `3.74x`

Treat this only as a sanity check, not as a guarantee for another cluster or another case.

## 12. Troubleshooting

### PETSc Fortran module mismatch

Symptom:

```text
Cannot read module file 'petscksp.mod' ... created by a different version of GNU Fortran
```

Fix:

- build PETSc and HeatFlow with the same underlying `gfortran` major version
- inspect `mpifort -show`
- set `GPU_GFORTRAN_PATH` so the wrapper picks up the correct compiler

### PETSc says MPI is not GPU-aware

Symptom:

```text
PETSc is configured with GPU support, but your MPI is not GPU-aware
```

Fix:

- add `-use_gpu_aware_mpi 0`
- or set `PETSC_OPTIONS="-use_gpu_aware_mpi 0"`

### Linker cannot find CUDA, Kokkos, or MPI libraries

Fix:

- check that `GPU_PETSC_DIR` points at the PETSc install prefix, not the PETSc source tree
- verify `$GPU_PETSC_DIR/lib/petsc/conf/petscvariables` exists
- override `GPU_MPI_LIBDIRS` and `GPU_EXTRA_LIBS` for your site

### PETSc configure/build fails on NVTX headers

Older PETSc plus newer CUDA toolkits can fail on `nvToolsExt.h` or the NVTX logging code.

If you hit this:

- prefer a newer PETSc release if your site allows it
- or patch PETSc so NVTX-specific files are guarded by `HAVE_CUDA_NVTX` rather than `HAVE_CUDA`
- keep `--with-x=0` in the PETSc configure to avoid unrelated X11 viewer build failures on headless systems

### Kokkos or Kokkos-Kernels configure fails inside PETSc

Fixes that usually help:

- make sure `nvcc` is visible on `PATH`
- make sure `--with-cuda-dir` points at the real CUDA toolkit root
- if PETSc cannot build bundled Kokkos cleanly on your site, preinstall Kokkos and Kokkos-Kernels and use `--with-kokkos-dir` and `--with-kokkos-kernels-dir`

## 13. Minimal Fresh-Machine Checklist

Use this as a final pass before you blame HeatFlow itself:

1. `mpicc`, `mpicxx`, `mpifort`, and `gfortran` all resolve to the toolchain you intend to use.
2. PETSc builds with CUDA, Kokkos, Kokkos-Kernels, shared libs, and Fortran enabled.
3. `$PETSC_PREFIX/lib/petsc/conf/petscvariables` exists.
4. `make` builds the CPU binary.
5. `make gpu` builds the GPU binary.
6. A one-rank GPU run works with `-use_gpu_aware_mpi 0`.
7. `./benchmark.sh /path/to/case-root 1` completes and writes `benchmark_results.txt`.

If all seven steps pass, you have a good baseline install.
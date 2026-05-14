# GPU Walkthrough

This is the full end-to-end setup guide for the current HeatFlow GPU backend on a fresh Linux machine or HPC system.

It covers:

- system prerequisites
- PETSc installation from source
- CUDA and Kokkos requirements
- HeatFlow GPU build setup
- running on a GPU node
- benchmarking CPU vs GPU
- common failure modes

This guide matches the current GPU flow in this repository:

- GPU backend is PETSc plus Kokkos plus CUDA
- HeatFlow GPU target is `make gpu`
- benchmark helper is `./benchmark.sh`
- the currently verified runtime uses `-use_gpu_aware_mpi 0` unless the MPI on your site is GPU-aware

This guide is written for the `117-gpu-cuda` branch.

## 1. Scope and Current Assumptions

The repository currently assumes:

- PETSc is installed separately and already includes CUDA, Kokkos, and Kokkos-Kernels support
- the PETSc install prefix contains `lib/petsc/conf/petscvariables`
- HeatFlow is built against that install prefix

Important: the GPU section of the current Makefile still contains some machine-specific defaults. On a fresh HPC system, you should expect to override at least some of these at build time:

- `GPU_PETSC_DIR`
- `GPU_FC`
- `GPU_GFORTRAN_PATH`
- `GPU_MPI_LIBDIRS`
- `GPU_EXTRA_LIBS`

This walkthrough shows how to do that explicitly.

## 2. What You Need

You need all of the following on the build side:

- a Linux environment
- `git`
- `make`
- `python3`
- `pkg-config`
- a C compiler
- a C++ compiler
- a Fortran compiler
- MPI wrapper compilers: `mpicc`, `mpicxx`, `mpifort`
- BLAS and LAPACK, ideally OpenBLAS
- a CUDA toolkit or NVHPC CUDA installation

You also need an NVIDIA driver and GPU on the runtime nodes.

### Package-manager example

If you are on a normal Linux machine with root access, install the basics first. For Ubuntu-like systems this is a reasonable starting point:

```bash
sudo apt-get update
sudo apt-get install -y \
  build-essential \
  gfortran \
  openmpi-bin \
  libopenmpi-dev \
  libopenblas-dev \
  pkg-config \
  python3 \
  git \
  cmake
```

Then install CUDA using your site or vendor instructions.

### HPC module example

On an HPC system, you usually do not install packages directly. Instead, load modules similar to:

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

## 3. Check the Toolchain Before You Build Anything

The single most important compatibility rule is:

**PETSc Fortran modules and HeatFlow must be built with the same underlying GNU Fortran major version.**

If they are not, you will usually see an error like:

```text
Cannot read module file 'petscksp.mod' ... because it was created by a different version of GNU Fortran
```

Check your actual wrappers and compiler resolution first:

```bash
which mpicc
which mpicxx
which mpifort
which gfortran

mpicc --version
mpicxx --version
mpifort --version
gfortran --version

mpifort -show
```

You want `mpifort -show` and `gfortran --version` to point to the compiler family you intend to use for both PETSc and HeatFlow.

Also verify CUDA visibility:

```bash
nvidia-smi
nvcc --version
```

If you are using NVHPC instead of a standalone CUDA toolkit, find the CUDA root manually and use that path later for PETSc.

## 4. Pick a Clean Directory Layout

The exact paths do not matter, but using a stable layout makes debugging much easier.

```bash
export HF_ROOT=$HOME/HeatFlow
export PETSC_SRC=$HOME/petsc-src
export PETSC_PREFIX=$HOME/petsc-kokkos
export CUDA_DIR=${CUDA_HOME:-/usr/local/cuda}
export CUDA_ARCH=86
```

Meaning:

- `HF_ROOT` is the HeatFlow repository root
- `PETSC_SRC` is the PETSc source checkout
- `PETSC_PREFIX` is the PETSc install prefix used by HeatFlow GPU builds
- `CUDA_DIR` is the root of the CUDA toolkit used for PETSc
- `CUDA_ARCH` is your GPU SM version without the dot, for example `80`, `86`, or `90`

If you do not know your SM version yet, stop and look it up before running PETSc configure.

## 5. Get HeatFlow

```bash
git clone https://github.com/ExeQuantCode/HeatFlow.git "$HF_ROOT"
cd "$HF_ROOT"
git checkout 117-gpu-cuda
```

## 6. Install PETSc

This is the part that must work before `make gpu` can work.

HeatFlow expects PETSc to provide:

- shared libraries
- Fortran module files like `petscksp.mod`
- CUDA support
- Kokkos support
- Kokkos-Kernels support
- an installed `petscvariables` file under the install prefix

### 6.1 Recommended PETSc version

The current branch has been validated against a PETSc 3.21.x based install. If you are building from scratch, start with the current stable PETSc if it configures cleanly on your site. If you want to reproduce the currently verified stack more closely, use:

```bash
git clone -b v3.21.6 https://gitlab.com/petsc/petsc.git "$PETSC_SRC"
```

### 6.2 Build PETSc in a clean compiler environment

Before configure, decide which `gfortran` you are really using and make sure the MPI wrappers see that same compiler.

```bash
export MPI_CC=$(command -v mpicc)
export MPI_CXX=$(command -v mpicxx)
export MPI_FC=$(command -v mpifort)
export GFORTRAN_BIN=$(dirname "$(readlink -f "$(command -v gfortran)")")
export BUILD_PATH="$GFORTRAN_BIN:/usr/bin:/bin"

env PATH="$BUILD_PATH" "$MPI_FC" --version
env PATH="$BUILD_PATH" "$MPI_FC" -show
```

If that output does not match the compiler family you expect, fix that first.

### 6.3 Preferred PETSc configure command

This is the simplest starting point on a fresh machine where PETSc is allowed to download Kokkos and Kokkos-Kernels itself:

```bash
cd "$PETSC_SRC"

env PATH="$BUILD_PATH" ./configure \
  --prefix="$PETSC_PREFIX" \
  --with-cc="$MPI_CC" \
  --with-cxx="$MPI_CXX" \
  --with-fc="$MPI_FC" \
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
```

Then build and install:

```bash
make all -j"$(nproc)"
make install
```

### 6.4 If the cluster blocks PETSc downloads

Some clusters block downloads from compute or login nodes. In that case you have three realistic options:

1. Stage the PETSc dependency tarballs manually and point PETSc at them.
2. Build Kokkos and Kokkos-Kernels separately, install them under the same prefix, and point PETSc at that prefix.
3. Use a site-provided PETSc if it already has CUDA and Kokkos enabled.

If you already installed Kokkos and Kokkos-Kernels separately into `$PETSC_PREFIX`, configure PETSc like this:

```bash
env PATH="$BUILD_PATH" ./configure \
  --prefix="$PETSC_PREFIX" \
  --with-cc="$MPI_CC" \
  --with-cxx="$MPI_CXX" \
  --with-fc="$MPI_FC" \
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

### 6.5 PETSc install checks

After install, all of these should exist:

```bash
ls "$PETSC_PREFIX/include/petscksp.mod"
ls "$PETSC_PREFIX/include/petscmat.h"
ls "$PETSC_PREFIX/lib/libpetsc.so"
ls "$PETSC_PREFIX/lib/petsc/conf/petscvariables"
```

Also verify that the installed PETSc exposes the Kokkos types HeatFlow uses:

```bash
rg -n "MATAIJKOKKOS|MATSEQAIJKOKKOS|MATMPIAIJKOKKOS" "$PETSC_PREFIX/include"
rg -n "VECKOKKOS|VECSEQKOKKOS|VECMPIKOKKOS" "$PETSC_PREFIX/include"
```

### 6.6 PETSc notes for newer CUDA stacks

If you pin an older PETSc release on a newer CUDA stack, you may hit issues such as:

- NVTX headers missing even though CUDA is enabled
- Kokkos configure failures because `nvcc` is not visible on `PATH`
- link issues around `libnvJitLink`

If PETSc configure fails, prefer fixing PETSc first rather than trying to work around it in HeatFlow.

## 7. Build HeatFlow CPU First

This is optional but strongly recommended. It gives you a known-good baseline before you debug the GPU toolchain.

```bash
cd "$HF_ROOT"
make clean
make
```

The CPU path uses PETSc discovery through `pkg-config` when available.

## 8. Build HeatFlow GPU

This is where fresh-machine and HPC installs usually need the most adjustment.

### 8.1 Important current Makefile behavior

The GPU build in the current repository has local defaults for:

- the MPI Fortran wrapper path
- MPI library search paths
- CUDA extra libraries

On a fresh system, do not assume the defaults are correct.

### 8.2 Minimum variables to override

At minimum, pass these:

```bash
export GFORTRAN_BIN=$(dirname "$(readlink -f "$(command -v gfortran)")")
export BUILD_PATH="$GFORTRAN_BIN:/usr/bin:/bin"
export MPI_FC=$(command -v mpifort)
```

Then build with:

```bash
cd "$HF_ROOT"

make gpuclean
make gpu \
  GPU_PETSC_DIR="$PETSC_PREFIX" \
  GPU_FC="env PATH=$BUILD_PATH $MPI_FC" \
  GPU_GFORTRAN_PATH="$BUILD_PATH"
```

This does two important things:

- it points HeatFlow at the PETSc install prefix, not the PETSc source tree
- it forces the MPI Fortran wrapper to see the same GNU Fortran family used to build PETSc modules

### 8.3 When you need MPI or CUDA link overrides

If the GPU link step fails, derive the missing paths instead of guessing.

Helpful commands:

```bash
mpifort -show
mpifort -showme:link
PKG_CONFIG_PATH="$PETSC_PREFIX/lib/pkgconfig" pkg-config --libs --static PETSc
```

Then build again with explicit overrides, for example:

```bash
make gpu \
  GPU_PETSC_DIR="$PETSC_PREFIX" \
  GPU_FC="env PATH=$BUILD_PATH $MPI_FC" \
  GPU_GFORTRAN_PATH="$BUILD_PATH" \
  GPU_MPI_LIBDIRS="-L/path/to/mpi/lib -Wl,-rpath,/path/to/mpi/lib" \
  GPU_EXTRA_LIBS="-L/path/to/cuda/targets/x86_64-linux/lib -Wl,-rpath,/path/to/cuda/targets/x86_64-linux/lib -lnvJitLink /path/to/libudev.so.1 /path/to/libcap.so.2"
```

The exact values are site-specific.

### 8.4 What a successful GPU build produces

The expected GPU binary is:

```bash
ls "$HF_ROOT/bin/ThermalFlow-gpu.x"
```

## 9. Prepare a Run Directory

HeatFlow expects a run directory with an `inputs/` subdirectory containing:

- `inputs/param.in`
- `inputs/system.in`
- `inputs/mat.in`

Example:

```text
case-root/
  inputs/
    param.in
    system.in
    mat.in
```

## 10. Run on a GPU Node

If you are on a cluster, get a GPU allocation first. For Slurm, a typical interactive example is:

```bash
salloc -N 1 -n 1 --gres=gpu:1 --cpus-per-task=8 --time=01:00:00
```

Use your scheduler's equivalent command if you are not using Slurm.

Then run HeatFlow from the case directory:

```bash
cd /path/to/case-root
OMP_NUM_THREADS=1 \
OPENBLAS_NUM_THREADS=1 \
OMP_PROC_BIND=spread \
OMP_PLACES=threads \
mpiexec -n 1 "$HF_ROOT/bin/ThermalFlow-gpu.x" -use_gpu_aware_mpi 0
```

### Why `-use_gpu_aware_mpi 0`?

The currently verified setup uses an MPI that is not GPU-aware. Without this flag, PETSc aborts with a message like:

```text
PETSc is configured with GPU support, but your MPI is not GPU-aware
```

If your site has GPU-aware MPI, you can remove the option.

If your MPI is not GPU-aware and you want a persistent default, do this instead:

```bash
export PETSC_OPTIONS="-use_gpu_aware_mpi 0"
```

## 11. Run the CPU vs GPU Benchmark

The repository already contains a benchmark helper that rebuilds both binaries and runs them:

```bash
cd "$HF_ROOT"
./benchmark.sh /path/to/case-root 3
```

What it does:

- rebuilds the CPU binary
- rebuilds the GPU binary
- runs both backends
- extracts the `simulation wall time`
- writes results to `benchmark_results.txt` in the run directory

The current script already applies `-use_gpu_aware_mpi 0` for the GPU path.

## 12. Known-Good Example

On the currently verified Cloak case, a single-repeat benchmark gave approximately:

- CPU: `94.802 s`
- GPU: `25.370 s`
- speedup: `3.74x`

Treat this as a sanity check only. Another system or another case can be faster or slower.

## 13. Common Failure Modes

### PETSc module version mismatch

Symptom:

```text
Cannot read module file 'petscksp.mod' ... created by a different version of GNU Fortran
```

Fix:

- verify `mpifort -show`
- verify `gfortran --version`
- rebuild PETSc and HeatFlow with the same compiler family
- override `GPU_FC` and `GPU_GFORTRAN_PATH` explicitly during `make gpu`

### MPI is not GPU-aware

Symptom:

```text
PETSc is configured with GPU support, but your MPI is not GPU-aware
```

Fix:

- run with `-use_gpu_aware_mpi 0`
- or export `PETSC_OPTIONS="-use_gpu_aware_mpi 0"`

### Linker cannot find MPI libraries

Fix:

- inspect `mpifort -showme:link`
- pass the correct `GPU_MPI_LIBDIRS`

### Linker cannot find CUDA transitive libraries

Examples:

- `libnvJitLink.so.12`
- `libudev.so.1`
- `libcap.so.2`

Fix:

- find the actual library locations on your site
- pass them through `GPU_EXTRA_LIBS`

### Kokkos configure fails inside PETSc

Fix:

- make sure `nvcc` is visible on `PATH`
- make sure `--with-cuda-dir` points to the real CUDA root
- if bundled downloads fail, preinstall Kokkos and Kokkos-Kernels and point PETSc at them

### PETSc install is incomplete

Symptom:

- `make gpu` fails because `petscksp.mod` or `petscvariables` is missing

Fix:

- re-run PETSc build
- re-run `make install`
- verify the install prefix, not the source tree

## 14. Short Checklist

If you want the shortest possible validation path, check these in order:

1. `mpicc`, `mpicxx`, `mpifort`, and `gfortran` all resolve to the toolchain you really intend to use.
2. `nvidia-smi` and `nvcc --version` both work.
3. PETSc builds with CUDA, Kokkos, Kokkos-Kernels, shared libs, and Fortran enabled.
4. `$PETSC_PREFIX/include/petscksp.mod` exists.
5. `$PETSC_PREFIX/lib/petsc/conf/petscvariables` exists.
6. `make` builds the CPU binary.
7. `make gpu ...` builds the GPU binary.
8. A one-rank GPU run works with `-use_gpu_aware_mpi 0`.
9. `./benchmark.sh /path/to/case-root 1` completes and writes a valid result file.

If all nine steps pass, your fresh-machine GPU install is in good shape.

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
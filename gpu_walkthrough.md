# GPU Walkthrough

This is the full end-to-end setup guide for the current HeatFlow GPU backend on a fresh Linux machine or HPC system.

It covers:

- system prerequisites
- PETSc installation from source
- the current native CUDA backend requirements
- HeatFlow GPU build setup
- running on a GPU node
- benchmarking CPU vs GPU
- common failure modes

This guide matches the current GPU flow in this repository:

- GPU backend is PETSc native CUDA, using `MATAIJCUSPARSE` and `VECCUDA`
- HeatFlow GPU target is `make gpu`
- benchmark helper is `./benchmark.sh`
- the currently verified runtime uses `-use_gpu_aware_mpi 0` unless the MPI on your site is GPU-aware

This guide is written for the `117-gpu-cuda` branch.

If you are targeting AMD, use `amd_gpu_walkthrough.md` instead. If you are targeting Bede specifically, use `bede.md` for the cluster-specific module stack and notes.

## 1. Scope and current assumptions

The repository currently assumes:

- PETSc is installed separately and already includes CUDA support
- the PETSc install prefix contains `lib/petsc/conf/petscvariables`
- HeatFlow is built against that install prefix for the GPU path

Important: the GPU section of the current `Makefile` still contains some machine-specific defaults. On a fresh machine or HPC system, expect to override at least some of these at build time:

- `GPU_PETSC_DIR`
- `GPU_FC`
- `GPU_GFORTRAN_PATH`
- `GPU_MPI_LIBDIRS`
- `GPU_EXTRA_LIBS`

This walkthrough shows how to do that explicitly.

## 2. What you need

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
- a CUDA toolkit or site-provided CUDA installation

You also need an NVIDIA driver and GPU on the runtime node.

### Package-manager example

If you are on a normal Linux machine with root access, this is a reasonable starting point on Ubuntu-like systems:

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

On an HPC system, you normally load modules instead of installing packages directly:

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

## 3. Check the toolchain before building anything

The most important compatibility rule is:

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

If you are using a site-managed CUDA install rather than `/usr/local/cuda`, note the real CUDA root now because you will need it for PETSc configure.

## 4. Pick a clean directory layout

The exact paths do not matter, but using a stable layout makes debugging much easier.

```bash
export HF_ROOT=$HOME/HeatFlow
export PETSC_SRC=$HOME/petsc-src
export PETSC_PREFIX=$HOME/petsc-cuda
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

## 6. Install PETSc for the native CUDA backend

This is the part that must work before `make gpu` can work.

HeatFlow expects PETSc to provide:

- shared libraries
- Fortran module files like `petscksp.mod`
- CUDA support
- native CUDA matrix and vector families such as `MATAIJCUSPARSE` and `VECCUDA`
- an installed `petscvariables` file under the install prefix

### 6.1 Recommended PETSc version

Use a current PETSc release with native CUDA support. A good baseline is:

```bash
git clone -b v3.25.1 https://gitlab.com/petsc/petsc.git "$PETSC_SRC"
```

### 6.2 Build PETSc in a clean compiler environment

Before configure, decide which `gfortran` you are really using and make sure the MPI wrappers see that same compiler.

```bash
export MPI_CC=$(command -v mpicc)
export MPI_CXX=$(command -v mpicxx)
export MPI_FC=$(command -v mpifort)
export GFORTRAN_BIN=$(dirname "$(readlink -f "$(command -v gfortran)")")
export BUILD_PATH="$GFORTRAN_BIN:$PATH"

env PATH="$BUILD_PATH" "$MPI_FC" --version
env PATH="$BUILD_PATH" "$MPI_FC" -show
```

If that output does not match the compiler family you expect, fix that first.

### 6.3 Preferred PETSc configure command

This is the simplest starting point on a fresh machine or cluster for the current HeatFlow GPU path:

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

If you want the first error to be easier to read on a cluster, start with `make all -j1` instead.

### 6.4 If you already have a PETSc install

You do not need to rebuild PETSc if your existing prefix already exposes the native CUDA types HeatFlow now uses.

For example, a prefix called `petsc-kokkos` is still acceptable if the actual installed PETSc headers contain `MATAIJCUSPARSE` and `VECCUDA`.

### 6.5 PETSc install checks

After install, all of these should exist:

```bash
ls "$PETSC_PREFIX/include/petscksp.mod"
ls "$PETSC_PREFIX/include/petscmat.h"
ls "$PETSC_PREFIX/lib/libpetsc.so"
ls "$PETSC_PREFIX/lib/petsc/conf/petscvariables"
```

Also verify that the installed PETSc exposes the native CUDA types HeatFlow now uses:

```bash
rg -n "MATAIJCUSPARSE|MATSEQAIJCUSPARSE|MATMPIAIJCUSPARSE" "$PETSC_PREFIX/include"
rg -n "VECCUDA|VECSEQCUDA|VECMPICUDA" "$PETSC_PREFIX/include"
```

If those names are missing, you built the wrong PETSc variant for the current GPU path.

## 7. Build HeatFlow CPU first

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
export BUILD_PATH="$GFORTRAN_BIN:$PATH"
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

When the updated code runs successfully, the banner should say:

```text
[Solver] Backend: PETSc native CUDA (GPU)
```

## 9. Prepare a run directory

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

## 10. Run on a GPU node

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

## 11. Run the CPU vs GPU benchmark

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

## 12. Fresh benchmark on this machine

The current code path was re-tested on this machine after switching the GPU solver from PETSc Kokkos types to PETSc native CUDA types.

Machine:

- GPU: NVIDIA RTX A1000
- Driver: `580.126.09`
- Case: `Cloak`
- Repeats: `3`

Measured results:

- CPU median: `94.079 s`
- GPU median: `26.208 s`
- GPU speedup: `3.59x`

Individual runs:

- CPU: `94.079 94.033 94.213`
- GPU: `26.128 26.208 26.707`

Treat this as a sanity check rather than a universal performance target. Another GPU, another PETSc build, or another case can be faster or slower.

## 13. Common failure modes

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

### PETSc install does not expose native CUDA types

Symptom:

- `MATAIJCUSPARSE` is missing from `petscmat.h`
- `VECCUDA` is missing from `petscvec.h`
- the GPU build fails when compiling `mod_petsc_solver.f90`

Fix:

- rebuild PETSc with `--with-cuda=1`
- verify the installed headers under your chosen prefix
- point `GPU_PETSC_DIR` at the correct install prefix

### PETSc configure cannot find CUDA cleanly

Fix:

- make sure `nvcc` is visible on `PATH`
- set `CUDA_DIR` to the real CUDA root
- pass `--with-cuda-dir` explicitly
- verify `--with-cuda-arch` matches the actual GPU

## 14. Minimal checklist

1. Load a consistent compiler, MPI, BLAS, and CUDA toolchain.
2. Verify `mpifort` and `gfortran` resolve to the same compiler family.
3. Build or reuse a PETSc install that exposes `MATAIJCUSPARSE` and `VECCUDA`.
4. Build HeatFlow CPU first.
5. Build HeatFlow GPU with explicit `GPU_PETSC_DIR`, `GPU_FC`, and `GPU_GFORTRAN_PATH`.
6. Override `GPU_MPI_LIBDIRS` and `GPU_EXTRA_LIBS` if your site paths differ from the Makefile defaults.
7. Start runtime tests with `-use_gpu_aware_mpi 0`.
8. Use `./benchmark.sh /path/to/case-root 3` to confirm the end-to-end CPU/GPU speedup.
# HeatFlow GPU Walkthrough on Bede

This version of the Bede guide uses the simpler deployment model: build one PETSc install per GPU vendor and use PETSc's native device backends instead of PETSc+Kokkos.

For Bede, that means a native CUDA PETSc build.

The Bede module stack used below is:

- GCC 12.2
- OpenMPI 4.1.6
- CUDA 12.3.2
- CMake 3.30.5
- OpenBLAS 0.3.26

Bede uses Hopper-class NVIDIA GPUs, so the correct CUDA architecture is `90` unless the node you allocate reports something different.

## 1. Backend model

The intended split is one PETSc build per vendor, with the same solver structure in HeatFlow and only the PETSc matrix/vector backend names changing.

| Vendor | PETSc configure flag | Native sparse matrix family | Native vector family | Notes |
| --- | --- | --- | --- | --- |
| NVIDIA | `--with-cuda` | `MATAIJCUSPARSE` (`MATSEQAIJCUSPARSE` / `MATMPIAIJCUSPARSE`) | `VECCUDA` (`VECSEQCUDA` / `VECMPICUDA`) | Best fit for Bede |
| AMD | `--with-hip` | `MATAIJHIPSPARSE` (`MATSEQAIJHIPSPARSE` / `MATMPIAIJHIPSPARSE`) | `VECHIP` (`VECSEQHIP` / `VECMPIHIP`) | Best fit for MI2xx and MI3xx systems |
| Intel | `--with-sycl` | PETSc has SYCL device support, but the current tree does not expose a native sparse `MATAIJSYCL` family analogous to CUDA and HIP | PETSc does not expose a matching `VECSYCL` family analogous to CUDA and HIP | Treat as a separate experimental path rather than a drop-in third column |

For HeatFlow's distributed MPI solver, prefer the root aliases `MATAIJCUSPARSE` and `VECCUDA` on NVIDIA, and `MATAIJHIPSPARSE` and `VECHIP` on AMD. PETSc will resolve those to the correct sequential or MPI implementation automatically.

If you only care about a one-rank local test, the sequential concrete names are `MATSEQAIJCUSPARSE` and `VECSEQCUDA` on NVIDIA, and `MATSEQAIJHIPSPARSE` and `VECSEQHIP` on AMD.

## 2. Current HeatFlow source status

The repository is not yet wired for this native-backend layout out of the box.

The current GPU path still hard-codes PETSc Kokkos types in `src/heatflow/mod_petsc_solver.f90`, and the current `Makefile` still labels the GPU build as `PETSc+Kokkos/CUDA` and defaults `GPU_PETSC_DIR` to a `petsc-kokkos` install.

That means this guide is the target build layout, not an unchanged build of the current branch.

The minimal source change is small: keep the solver structure the same, but replace the backend-specific PETSc type names.

Conceptually, the GPU-specific matrix and vector setup should look like this:

```fortran
#ifdef HEATFLOW_GPU_CUDA
  call MatSetType(A_saved, MATAIJCUSPARSE, ierr)
  call VecSetType(bb_saved, VECCUDA, ierr)
#elif defined(HEATFLOW_GPU_HIP)
  call MatSetType(A_saved, MATAIJHIPSPARSE, ierr)
  call VecSetType(bb_saved, VECHIP, ierr)
#else
  call MatSetType(A_saved, MATAIJ, ierr)
#endif
```

Apply the same vector-family choice to every explicit GPU `VecSetType()` call and update the startup banner so it no longer claims `PETSc+Kokkos/CUDA`.

If you want one source tree to support both NVIDIA and AMD cleanly, the right approach is a small compile-time abstraction layer for the matrix and vector type names rather than duplicating solver logic.

## 3. Load the Bede module stack

```bash
module purge
module load gcc/12.2
module load cuda/12.3.2
module load openmpi/4.1.6
module load cmake/3.30.5
module load openblas/0.3.26
```

If your account uses a different module collection, keep the compiler, MPI, and CUDA stack internally consistent.

## 4. Set up working directories and compiler paths

```bash
export HF_ROOT=$HOME/HeatFlow
export PETSC_SRC=$HOME/petsc-src
export PETSC_PREFIX=$HOME/petsc-cuda
export CUDA_DIR=/opt/software/builder/developers/compilers/cuda/12.3.2/1/default
export CUDA_ARCH=90

export MPI_CC=$(command -v mpicc)
export MPI_CXX=$(command -v mpicxx)
export MPI_FC=$(command -v mpifort)
export GFORTRAN_BIN=$(dirname "$(readlink -f "$(command -v gfortran)")")
export BUILD_PATH="$GFORTRAN_BIN:$PATH"
```

Sanity-check the toolchain before building:

```bash
mpicxx --version
mpifort -show
gfortran --version
nvcc --version
nvidia-smi --query-gpu=name --format=csv,noheader
```

## 5. Clone PETSc and HeatFlow

```bash
git clone -b v3.25.1 https://gitlab.com/petsc/petsc.git "$PETSC_SRC"
git clone https://github.com/ExeQuantCode/HeatFlow.git "$HF_ROOT"

cd "$HF_ROOT"
git checkout 117-gpu-cuda
```

PETSc 3.25.x is a good baseline here because it has current CUDA and HIP backend support without forcing the build down the older Kokkos-specific path.

## 6. Configure PETSc for native CUDA

This is the core change from the old Bede guide: no Kokkos and no Kokkos-Kernels.

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

If PETSc fails to find BLAS or LAPACK from the OpenBLAS module, add the site-specific OpenBLAS prefix with `--with-blaslapack-dir=<openblas-prefix>`.

## 7. Build and install PETSc

Start conservatively, then scale up if the node has headroom.

```bash
make all -j4 2>&1 | tee petsc-build.log
make install
```

If you want the first failure to be maximally easy to read, use `-j1` on the first build.

## 8. Verify the PETSc install

```bash
ls "$PETSC_PREFIX/include/petscksp.mod"
ls "$PETSC_PREFIX/include/petscmat.h"
ls "$PETSC_PREFIX/lib/libpetsc.so"
ls "$PETSC_PREFIX/lib/petsc/conf/petscvariables"
```

Confirm that the installed headers expose the native CUDA types you plan to use:

```bash
rg -n "MATAIJCUSPARSE|MATSEQAIJCUSPARSE|MATMPIAIJCUSPARSE" "$PETSC_PREFIX/include"
rg -n "VECCUDA|VECSEQCUDA|VECMPICUDA" "$PETSC_PREFIX/include"
```

## 9. Build HeatFlow after the backend-type substitutions

Once `mod_petsc_solver.f90` has been switched from Kokkos types to CUDA native PETSc types, build HeatFlow the same way as before, but point the GPU build at the native CUDA PETSc prefix.

First build the CPU version as a baseline:

```bash
cd "$HF_ROOT"
make clean
make
```

Then build the GPU binary:

```bash
make gpuclean
make gpu \
  GPU_PETSC_DIR="$PETSC_PREFIX" \
  GPU_FC="env PATH=$BUILD_PATH $MPI_FC" \
  GPU_GFORTRAN_PATH="$BUILD_PATH"
```

## 10. Grace-specific link-path warning

The current GPU `Makefile` still contains x86_64-centric library defaults. On Grace-based Bede nodes those can be wrong.

If the link step fails on paths like `/usr/lib/x86_64-linux-gnu/...`, derive the actual linker inputs from the live environment instead of guessing:

```bash
mpifort -show
mpifort -showme:link
PKG_CONFIG_PATH="$PETSC_PREFIX/lib/pkgconfig" pkg-config --libs --static PETSc
```

Then override the problematic paths explicitly if needed:

```bash
make gpu \
  GPU_PETSC_DIR="$PETSC_PREFIX" \
  GPU_FC="env PATH=$BUILD_PATH $MPI_FC" \
  GPU_GFORTRAN_PATH="$BUILD_PATH" \
  GPU_MPI_LIBDIRS="-L/path/to/mpi/lib -Wl,-rpath,/path/to/mpi/lib" \
  GPU_EXTRA_LIBS="-L/path/to/cuda/lib64 -Wl,-rpath,/path/to/cuda/lib64 -lnvJitLink /path/to/libudev.so.1 /path/to/libcap.so.2"
```

## 11. Run on a Bede GPU node

Allocate a GPU node with the scheduler command appropriate for your account. A typical Slurm-style example is:

```bash
salloc -N 1 -n 1 --gres=gpu:1 --cpus-per-task=8 --time=01:00:00
```

From a case directory containing `inputs/param.in`, `inputs/system.in`, and `inputs/mat.in`:

```bash
cd /path/to/case-root
OMP_NUM_THREADS=1 \
OPENBLAS_NUM_THREADS=1 \
OMP_PROC_BIND=spread \
OMP_PLACES=threads \
mpiexec -n 1 "$HF_ROOT/bin/ThermalFlow-gpu.x" -use_gpu_aware_mpi 0
```

Start with `-use_gpu_aware_mpi 0`. Remove it only if you know the Bede MPI build is GPU-aware.

If you want that as a default:

```bash
export PETSC_OPTIONS="-use_gpu_aware_mpi 0"
```

## 12. AMD native HIP example

The AMD path is the same idea: separate PETSc install, same solver structure, different PETSc backend names.

This is the equivalent native HIP configure shape for an MI300A-style system:

```bash
module purge
module load gcc/<site-version>
module load rocm/<site-version>
module load openmpi/<site-version>
module load cmake/<site-version>
module load openblas/<site-version>

export PETSC_PREFIX_HIP=$HOME/petsc-hip
export ROCM_DIR=<site-rocm-prefix>
export MPI_CC=$(command -v mpicc)
export MPI_CXX=$(command -v mpicxx)
export MPI_FC=$(command -v mpifort)

unset GPU_TARGETS AMDGPU_TARGETS HCC_AMDGPU_TARGET
export HSA_XNACK=1

env PATH="$PATH" ./configure \
  --prefix="$PETSC_PREFIX_HIP" \
  --with-cc="$MPI_CC" \
  --with-cxx="$MPI_CXX" \
  --with-fc="$MPI_FC" \
  --with-hip=1 \
  --with-hip-dir="$ROCM_DIR" \
  --with-hipc=hipcc \
  --with-hip-arch=gfx942_apu \
  --with-shared-libraries=1 \
  --with-debugging=0 \
  --with-x=0 \
  COPTFLAGS="-O3" \
  CXXOPTFLAGS="-O3" \
  FOPTFLAGS="-O3"
```

On the source side, the matching substitutions are `MATAIJHIPSPARSE` for the matrix family and `VECHIP` for the vector family.

## 13. Common failure modes in the native-backend layout

### `unsupported gpu architecture`

Your device architecture flag is wrong for the node. On Bede Hopper-class nodes, use `--with-cuda-arch=90`.

### `Cannot read module file 'petscksp.mod'`

PETSc and HeatFlow were built with different Fortran compiler stacks. Recheck:

```bash
mpifort -show
gfortran --version
```

Then rebuild both with the same compiler family.

### `PETSc is configured with GPU support, but your MPI is not GPU-aware`

Run with `-use_gpu_aware_mpi 0`.

### GPU link step fails on x86_64-specific paths

The current `Makefile` defaults are still machine-specific. Override `GPU_MPI_LIBDIRS` and `GPU_EXTRA_LIBS` with paths from the active Bede node.

## 14. Minimal Bede checklist

1. Load GCC 12.2, CUDA 12.3.2, OpenMPI 4.1.6, CMake 3.30.5, and OpenBLAS 0.3.26.
2. Set `CUDA_ARCH=90` for Bede Hopper-class nodes.
3. Build PETSc with `--with-cuda`, not `--with-kokkos`.
4. Verify the install exposes `MATAIJCUSPARSE` and `VECCUDA`.
5. Replace the Kokkos-specific PETSc type names in the solver with the CUDA native PETSc types.
6. Point `make gpu` at the native CUDA PETSc prefix.
7. Override the current x86_64-biased link paths if the Grace node layout differs.
8. Start runtime testing with `-use_gpu_aware_mpi 0`.
# Copilot Prompt: Make HeatFlow Build And Run Efficiently On A Local Supercomputer

You are working in the `HeatFlow/` Fortran codebase. Act as a senior Fortran/HPC engineer. Your task is to make this project compile reliably and run efficiently in a local supercomputer environment that uses compiler/MPI/PETSc/HDF5/BLAS modules, usually via Slurm.

Do not change the heat-transfer physics unless a change is required to fix a bug. Prefer small, reviewable patches. Keep the existing executable behavior and input-file format compatible unless you document a deliberate migration.

## Current Codebase Shape

- Main program: `src/heatflow.f90`
- Core modules: `src/heatflow/*.f90`
- Build systems: `Makefile`, `CMakeLists.txt`, `fpm.toml`
- Tests and sample data: `test/`
- PETSc solver module: `src/heatflow/mod_petsc_solver.f90`
- Time stepping: `src/heatflow/mod_evolve.f90`
- Matrix construction: `src/heatflow/mod_setup.f90`, `src/heatflow/mod_hmatrix.f90`
- Output: `src/heatflow/mod_output.f90`, `src/heatflow/mod_output_hdf5.f90`

## Problems Observed During Review

1. `make -C HeatFlow` succeeds only as a serial build after `make clean`.
2. `make -C HeatFlow -j2` fails because Fortran module dependencies are not represented. Example failure: `mod_constructions.f90` can compile before `constants.mod` exists.
3. Stale objects can break optional HDF5 builds. A previous `mod_output_hdf5.o` compiled with `USE_HDF5=1` caused HDF5 link symbols to appear even when `USE_HDF5` was not requested.
4. `CMakeLists.txt` appears stale for the PETSc-enabled source tree. It omits `mod_petsc_solver.f90` and `mod_output_hdf5.f90`, hard-codes compilers, and does not find/link PETSc/HDF5/OpenMP/BLAS.
5. `fpm.toml` does not describe PETSc/HDF5 linkage. Either make fpm work with external libraries or document that fpm is not the supported HPC path.
6. `test/data/mat.in` is missing the mandatory `vel = ...` material field. Running the executable with the bundled test inputs stops in `read_mat` until `vel = 0 0 0` is added.
7. Runtime output is not HPC-friendly. There are large unconditional debug dumps in `mod_setup.f90`, `mod_evolve.f90`, and `mod_output.f90`, including first-timestep row dumps and grid dumps.
8. `mod_evolve.f90` prints `NA32` before assigning it in the PETSc diagnostics block.
9. `mod_petsc_solver.f90` uses `PETSC_COMM_SELF`, so it is not distributed-memory parallel even though PETSc initializes MPI. Do not claim MPI scaling unless you implement distributed PETSc matrices/vectors.
10. `mod_petsc_solver.f90` defaults to direct LU via `PRECONDITIONER = 'LU'`, which is unsuitable for large HPC runs.
11. The solver rebuild/update path is expensive: it zeroes and refills the PETSc matrix every timestep and allocates/deallocates row and vector index buffers inside hot loops.
12. Input/output paths are hard-coded as `./inputs` and `./outputs`. HPC jobs should be able to run from scratch directories and should create output directories safely.
13. Some defaults are derived before grid dimensions are known, for example output ranges using `nx`, `ny`, and `nz` during `read_param`. Initialize all defaults explicitly and finalize dimension-dependent defaults after `read_system`.

## Goal

Make HeatFlow a robust HPC-ready Fortran application with:

- Reliable parallel builds from a clean checkout.
- PETSc, OpenMP, optional HDF5, and BLAS/LAPACK discovery suitable for module-based clusters.
- A documented supercomputer build/run workflow.
- A smoke-testable executable using the included test data.
- Reduced unnecessary memory allocation, matrix rebuilds, and diagnostic output.

## Build-System Requirements

Use CMake/Ninja as the preferred HPC path if possible, while keeping the existing Makefile working.

For CMake:

- Do not force `gfortran`/`gcc` inside `CMakeLists.txt`; respect `FC`, `CC`, `CMAKE_Fortran_COMPILER`, and compiler wrapper choices such as `mpifort`.
- Include all current source files, especially `mod_petsc_solver.f90` and `mod_output_hdf5.f90`.
- Add robust discovery/linkage for PETSc, OpenMP, optional HDF5 Fortran, and BLAS/LAPACK.
- Support options like `HEATFLOW_USE_HDF5`, `HEATFLOW_BUILD_TESTS`, and `HEATFLOW_ENABLE_OPENMP`.
- Ensure Fortran module output directories and target dependencies work with `cmake --build ... -j`.
- Ensure tests link against the same library and external dependencies as the executable.

For Makefile:

- Avoid hard-coded Homebrew/Debian assumptions in the default HPC path.
- Support compiler wrappers: `make FC=mpifort`.
- Prefer PETSc metadata from `pkg-config`, `PETSC_DIR`/`PETSC_ARCH`, or PETSc make variables.
- Make `make -j` reliable by generating or explicitly encoding Fortran module dependencies.
- Avoid stale objects when preprocessor options change. Use separate build directories per configuration or generated dependency/config stamps.
- Keep `make clean` and `make distclean` safe and complete for generated objects/modules/executables.

For fpm:

- Either make it work with documented external PETSc/HDF5 flags or mark it clearly as unsupported for HPC/PETSc builds.

## Source-Code Fixes And Performance Work

Prioritize these changes:

1. Fix test/sample inputs:
   - Add the missing `vel` material property to `test/data/mat.in` if it is required by `read_mat`.
   - Add a smoke test or documented smoke run using `test/data`.

2. Fix noisy diagnostics:
   - Guard all large setup/evolve/output debug dumps behind `IVERB >= 5` or a compile-time flag such as `HEATFLOW_DEBUG_DIAGNOSTICS`.
   - Keep normal `IVERB=0..1` runs quiet enough for batch jobs.

3. Fix obvious correctness bugs:
   - Assign `NA32` before printing it in `mod_evolve.f90`.
   - Initialize all input defaults explicitly. Finalize output ranges after `nx`, `ny`, and `nz` are read.
   - Create `outputs/` before opening files, or allow an output directory option/env var.

4. Improve PETSc solver behavior:
   - Keep `KSPSetFromOptions` so users can choose solvers at runtime with PETSc flags.
   - Replace the hard-coded direct LU default with a scalable iterative default suitable for large sparse diffusion-like systems, for example GMRES/CG-compatible choices with GAMG where mathematically appropriate.
   - Do not use direct LU as the default for large runs.
   - Reuse PETSc matrix/vector/KSP objects across timesteps.
   - If the matrix does not change between timesteps, assemble it once and only update RHS/initial guess thereafter.
   - If the matrix can change for temperature-dependent properties, detect that and rebuild only when needed.
   - Avoid per-row allocate/deallocate in hot loops. Preallocate reusable buffers or use PETSc CSR APIs for bulk assembly.
   - Use `PetscInt` consistently for PETSc index arrays or validate downcasts from 64-bit indices before converting.
   - Be explicit that current `PETSC_COMM_SELF` is single-rank. If you implement true MPI scaling, change the matrix/vector ownership model to `PETSC_COMM_WORLD` and use distributed AIJ matrices correctly. If not, document the current single-rank plus OpenMP scaling model honestly.

5. HPC runtime controls:
   - Add documentation and examples for `OMP_NUM_THREADS`, `OMP_PROC_BIND`, `OMP_PLACES`, `OPENBLAS_NUM_THREADS`, `MKL_NUM_THREADS`, `PETSC_OPTIONS`, `srun`, and `mpirun`.
   - Add a Slurm job script template under `docs/` or `scripts/`.
   - Make input and output directories configurable by command-line argument or environment variable, while preserving default `./inputs` and `./outputs`.

## Documentation To Add Or Update

Create or update `docs/HPC_BUILD_AND_RUN.md` with:

- Example module loads with placeholders:
  - `module load gcc openmpi petsc hdf5 cmake ninja`
- CMake configure/build examples:
  - `cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release -DHEATFLOW_USE_HDF5=OFF`
  - `cmake --build build -j`
- Makefile fallback examples:
  - `make clean`
  - `make FC=mpifort -j`
- Smoke-run instructions using `test/data` copied into a run directory.
- Slurm single-node OpenMP example.
- Slurm PETSc/MPI example only if true MPI-distributed solving is implemented. Otherwise say the current solver is single-rank PETSc plus OpenMP.
- PETSc runtime examples:
  - iterative solver/preconditioner flags
  - convergence monitor flags
  - memory/logging flags such as `-log_view`

## Acceptance Criteria

Before finishing, verify and report exact commands and results:

1. Fresh serial build succeeds.
2. Fresh parallel build succeeds, for example `make -j4` or CMake/Ninja equivalent.
3. A no-HDF5 build does not link HDF5 symbols.
4. If HDF5 is enabled and libraries are available, the HDF5 build links and writes output.
5. The bundled test-data smoke run completes from a separate run directory with `inputs/` and `outputs/`.
6. Normal runtime output is concise at `IVERB=0` and `IVERB=1`.
7. Tests pass, or any remaining failing tests are documented with specific causes.
8. No generated `.mod`, `.o`, or executable artifacts are committed unless the repository already intentionally tracks them.

When you are done, summarize:

- Build-system changes.
- Source-code fixes.
- Runtime/performance changes.
- Exact verification commands.
- Remaining limitations, especially whether true MPI distributed-memory scaling is implemented or not.

# HeatFlow User Manual

This manual provides a concise guide to configuring and running simulations using the **HeatFlow** software. The software simulates heat transport using finite difference methods, primarily focusing on the Cattaneo (hyperbolic heat equation) and Fourier models.

## Building

Build with `make` (MPI is required). The Makefile auto-detects an MPI Fortran wrapper on `PATH` and in common install prefixes; if your MPI is elsewhere, use `make FC=/path/to/mpifort`. BLAS/LAPACK are optional and auto-detected (Apple Accelerate on macOS; OpenBLAS, MKL or reference BLAS/LAPACK on Linux). If no backend is found, run `make deps-blas` to download and build OpenBLAS locally (opt-in; network required), or `make BLAS=0` to skip BLAS/LAPACK. PETSc is detected automatically; if it is not found, a legacy-solver-only version is built (only `solver = legacy` is available). To force a build without PETSc even when it is installed, use `make PETSC=0`.

## Command Line Options

The input and output directories can be selected on the command line (defaults: `./inputs`, `./outputs`, restart files in `./restart`):

| Option | Description |
| :--- | :--- |
| `-i <dir>` / `--input-directory <dir>` / `--input-directory=<dir>` | Directory containing `param.in`, `mat.in` and `system.in`. The restart directory is derived from it when it ends in `/inputs`. |
| `-o <dir>` / `--output-directory <dir>` / `--output-directory=<dir>` | Directory for all output files (created if it does not exist). |
| `--directory <dir>` / `--directory=<dir>` | Legacy alias: uses `<dir>/inputs`, `<dir>/outputs` and `<dir>/restart`. |

PETSc runtime options (e.g. `-ksp_monitor`) can be passed alongside these options.

## Input Files

The simulation is controlled by three main input files located in the `inputs/` directory:
1.  **`param.in`**: Simulation parameters (time steps, flags, boundary conditions).
2.  **`mat.in`**: Material properties.
3.  **`system.in`**: Geometry and grid definition.

### 1. `param.in` (Simulation Parameters)

This file uses a `KEYWORD = VALUE` format. Comments can be added using `!`.

#### General Settings
| Keyword | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `_RunName` | String | `default` | Name of the simulation run. |
| `IVERB` | Integer | `1` | Verbosity level (higher = more output). |
| `ntime` | Integer | `10` | Total number of time steps. |
| `time_step` | Double | `1.0` | Time step size. |
| `freq` | Double | `1.0` | Frequency of the heater. |
| `icattaneo` | Integer | `1` | Switch for Cattaneo term (`1` = On, `0` = Off/Fourier). |
| `isteady` | Integer | `0` | Steady state switch (`1` = Steady state, `0` = Transient). |
| `heattime` | Integer | `0` | Number of steps for which heating is applied (case 2). |
| `TempDepProp`| Integer | `0` | Flag for temperature dependent properties. |
| `solver` | String | `ilu` | Linear solver: `legacy` (original CG solver) or PETSc preconditioners `ilu`, `lu`, `gamg`. |

#### Boundary & Conditions
| Keyword | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `Periodic` | String | `''` | Periodic boundaries. Contains 'x', 'y', or 'z' (e.g., `'xy'`). |
| `kappaBound` | Double | `0.0` | Global boundary thermal conductivity (sets all planes). |
| `kappaBoundx1`...`z2` | Double | `0.0` | Specific boundary conductivity (e.g., `kappaBoundx1` for x=1 plane). |
| `T_System` | Double | `300.0` | Initial system temperature. |
| `T_Bath` | Double | - | Global bath temperature (sets all boundaries boundaries). |
| `T_Bathx1`...`z2` | Double | `T_Bath` | Specific boundary temperatures. |
| `T_BathCG` | Double | `0.0` | Constant gradient bath temperature. |
| `CG_dir` | String | `' '` | Direction for constant gradient (e.g., `'+x'`, `'-y'`). |
| `BR` | Double | `1.0` | Bath Ratio (scaling factor). |

#### Power
| Keyword | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `power_in` | Double | `0.0` | Power input for the heater. |

#### Flags (Logical)
All flags default to `.False.`. Set to `.True.` (or `T`) to enable.
- `_Check_Sparse_Full`: Check if simulation is sparse or full.
- `_Check_Stability`: Perform stability check.
- `_Check_Steady_State`: Check for steady state convergence.
- `_WriteToTxt`: Enable writing output to text files.
- `_Percentage_Completion`: Show progress % in output.
- `_Test_Run`: Flag for test runs.
- `_InputTempDis`: Load initial temperature distribution from file.
- `_FullRestart`: Perform a full restart.
- `_CompressedOutput`: Write the temperature output in compressed binary form.
- `_HDF5Output`: Write the temperature output to an HDF5 file (`output_<RunName>.h5`, GZIP compressed). Requires a build with HDF5 support (`make USE_HDF5=1` or `-DHEATFLOW_HDF5=ON`).

#### Output Control
Defines the region of the grid to write to output.
| Keyword | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `write_every` | Integer | `1` | Write output every N steps. |
| `T_decimals` | Integer | `3` | Decimal places for temperatures in the output file (e.g. `3` gives `100.001` and `1000.001`). |
| `start_ix`, `end_ix` | Integer | `1`..`Nx` | X-range for output. |
| `start_iy`, `end_iy` | Integer | `1`..`Ny` | Y-range for output. |
| `start_iz`, `end_iz` | Integer | `1`..`Nz` | Z-range for output. |

---

### 2. `mat.in` (Material Properties)

Defines the physical properties for each material index used in the system. The file ends with a line containing `0`.

**Format:**
```
<Material_Index>
keyword = value
...
0
```

| Keyword | Description |
| :--- | :--- |
| `heat_capacity` | Specific heat capacity. |
| `kappa` | Thermal conductivity. |
| `rho` | Density. |
| `tau` | Relaxation time (for Cattaneo). |
| `em` | Emissivity / Parameter (usage depends on physics context). |

**Example:**
```
1
heat_capacity = 4200
kappa = 0.541
rho = 997
tau = 1e-12
0
```

---

### 3. `system.in` (Geometry/Mesh)

Defines the simulation grid and the material distribution.

**Structure:**
1.  **Grid Dimensions**: `nx ny nz`
2.  **Physical Dimensions**: `Lx Ly Lz`
3.  **Grid Data**: A list of `MaterialID:HeaterID` for every cell.

The file is read in the order: Z-planes, then Y-rows, then X-columns.
Each line in the file (after header) corresponds to one row (X-direction).

**Example:**
```
10 10 1
0.01 0.01 0.001

! Z=1, Y=1 Row
1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0
! Z=1, Y=2 Row
1:0 1:0 ...
```
- `1:0` means Material ID 1, Heater ID 0 (no heater).
- `1:1` means Material ID 1, Heater ID 1 (active heater).

## Execution

Ensure the `inputs/` directory exists with the three required files. Run the executable from the directory containing `inputs/`.

```bash
./HeatFlow
```
or via `fpm`:
```bash
fpm run --profile release
```

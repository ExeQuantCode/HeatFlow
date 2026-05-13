# HeatFlow User Manual

This manual provides a concise guide to configuring and running simulations using the **HeatFlow** software. The software simulates heat transport using finite difference methods, primarily focusing on the Cattaneo (hyperbolic heat equation) and Fourier models. Both Cartesian and cylindrical (axisymmetric) coordinate systems are supported.

## Compilation

To build the software, use the provided `Makefile`.

**Standard Build:**
```bash
make
```

**Build with HDF5 Support:**
To enable `.h5` output files, you must compile with the `USE_HDF5` flag set. Ensure you have HDF5 libraries installed (specifically the Fortran modules).
```bash
make USE_HDF5=1
```

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
| `_SolverMethod` | String | `GAMG` | PETSc solver profile. See [Solver Methods](#solver-methods). |

#### Boundary & Conditions
| Keyword | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `iboundary` | Integer | `1` | Boundary condition type. |
| `Periodic` | String | `''` | Periodic boundaries. Contains 'x', 'y', or 'z' (e.g., `'xy'`). |
| `kappaBound` | Double | `0.0` | Global boundary thermal conductivity (sets all planes). |
| `kappaBoundx1`...`z2` | Double | `0.0` | Specific boundary conductivity (e.g., `kappaBoundx1` for x=1 plane). |
| `T_System` | Double | `300.0` | Initial system temperature. |
| `T_Bath` | Double | - | Global bath temperature (sets all boundaries boundaries). |
| `T_Bathx1`...`z2` | Double | `T_Bath` | Specific boundary temperatures. |
| `T_BathCG` | Double | `0.0` | Constant gradient bath temperature. |
| `CG_dir` | String | `' '` | Direction for constant gradient (e.g., `'+x'`, `'-y'`). |
| `T_BathCC` | Logical| `F` | Scale constant gradient with DeltaT. |
| `BR` | Double | `1.0` | Bath Ratio (scaling factor). |
| `kappaBoundNr` | Double | `0.0` | Boundary thermal conductivity at the outer radius (cylindrical mode only). |
| `T_BathNr` | Double | `T_Bath` | Bath temperature at the outer radius (cylindrical mode only). |

#### Power
| Keyword | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `power_in` | Double | `0.0` | Power input for the heater. |

#### Heating Types
The heating type is defined in the `system.in` file (second integer in the material/heater pair).

| ID | Description |
| :--- | :--- |
| `0` | No heating. |
| `1` | Constant heating (`Q = power_in`). |
| `2` | Pulse heating. On for `heattime` steps, then off. Includes Cattaneo transient corrections if enabled. |
| `3` | AC oscillatory heating (Cosine averaged over time step). |
| `4` | AC oscillatory heating (Sine squared). |
| `5` | AC oscillatory heating with Cattaneo term. |
| `6` | One-step impulse heating (On for step 1 only, then 0). |
| `7` | Square-wave heating. On for `heattime`, off for `heattime`, repeating. |
| `10` | Constant heating minus Cattaneo term (`P - tau*P`). |
| `11` | Constant heating (redundant with 1). |
| `12` | Constant heating plus Cattaneo term (`P + tau*P`). |


#### Flags (Logical)
All flags default to `.False.`. Set to `.True.` (or `T`) to enable.
- `_Check_Sparse_Full`: Check if simulation is sparse or full.
- `_Check_Stability`: Perform stability check.
- `_Check_Steady_State`: Check for steady state convergence.
- `_WriteToTxt`: Enable writing output to text files.
- `_CompressedOutput`: Enable compressed binary output for temperature logs. Appends `.bin` extension.
- `_HDF5Output`: Enable HDF5 compressed output for temperature logs (requires compilation with HDF5 support). Appends `.h5` extension.
- `_Percentage_Completion`: Show progress % in output.
- `_Test_Run`: Flag for test runs.
- `_InputTempDis`: Load initial temperature distribution from file.
- `_FullRestart`: Perform a full restart.
- `_CylindricalGrid`: Enable cylindrical (axisymmetric) coordinate system. See [Cylindrical Grid Mode](#cylindrical-grid-mode).

#### Solver Methods

The PETSc solve is configured with `_SolverMethod` in `param.in`. PETSc command-line options still override the profile, so advanced runs can tune with flags such as `-ksp_monitor`, `-ksp_rtol`, `-pc_type`, or `-ksp_type`.

| Method | Memory | Speed | Use when |
| :--- | :--- | :--- | :--- |
| `LU` | Highest | Fast for small grids | You need robustness on small problems. This is the most likely option to run out of memory. |
| `GAMG` | Medium | Usually fast on large diffusion problems | Default balance for large runs. Try this first when `LU` runs out of memory. |
| `ILU` | Medium-high | Often good on small/medium grids | `GAMG` struggles but the grid is not too large. |
| `GMRES` | Low-medium | Slower | You need less memory than `ILU`/`GAMG` but want a stronger Krylov method. |
| `JACOBI` | Low | Slower | Memory is tight and you can tolerate more iterations. |
| `NONE` | Lowest | Slowest, least robust | Last-resort memory saver or debugging preconditioner effects. |

Example:
```bash
_SolverMethod = JACOBI
```

#### Output Control
Defines the region of the grid to write to output.
| Keyword | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `write_every` | Integer | `1` | Write output every N steps. |
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
| `vel` | Velocity vector (3 components, e.g., `vel = 1.0 0.0 0.0`). |

**Example:**
```
1 ! a comment
heat_capacity = 4200
kappa = 0.541
rho = 997
tau = 1e-12
em = 0.8
vel = 0.0 0.0 0.0
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


---

## Cylindrical Grid Mode

HeatFlow supports an **axisymmetric cylindrical** coordinate system. This mode solves the heat equation on a 2-D radial-axial (r-y) cross-section of a cylinder, assuming full rotational symmetry about the central axis (r = 0). It is enabled by setting `_CylindricalGrid = T` in `param.in`.

### Coordinate mapping

The standard Cartesian grid indices are reinterpreted as follows:

| Grid axis | Physical meaning | Notes |
| :--- | :--- | :--- |
| **x** (radial, r) | Radial direction | `ix = 1` is the innermost shell (centred at r = dr/2); radial distance increases outward. |
| **y** | Axial direction | Identical to Cartesian y. |
| **z** | Azimuthal (unused) | **Must** be set to `nz = 1`. The code will stop with an error if nz is not 1. |

The total cylinder radius is R = Lx and the axial length is Ly, where Lx and Ly are the physical dimensions given in `system.in`.

### Governing equation

In cylindrical coordinates with azimuthal symmetry the heat equation becomes:

    rho * Cv * dT/dt = (1/r) d/dr (r * kappa * dT/dr) + d/dy (kappa * dT/dy) + q

where q is the volumetric heat-source density. The extra 1/r factor in the radial term is what distinguishes cylindrical from Cartesian diffusion; it means that interface areas and cell volumes depend on the radius.

### How the code implements cylindrical symmetry

Internally the code makes three adjustments compared with the Cartesian solver. All other parts of the time-stepping, solver, output, etc. remain the same.

#### 1. Cell volumes

Each cell is a cylindrical **annular shell**. For a cell at radial index `ix` with uniform radial spacing dr = Lx / nx:

    r_in  = (ix - 1) * dr
    r_out = ix * dr
    V(ix) = pi * (r_out^2 - r_in^2) * dy

Note that the innermost cell (`ix = 1`) is a solid disk (r_in = 0), while all other cells are annular rings whose volume grows with radius. These volumes are used when converting total power into volumetric power density: the heater routine divides the input power by the summed `heated_volume` (which is the sum of cylindrical volumes of all heated cells).

#### 2. Heat-matrix (H-matrix) conductivity corrections

The finite-difference discretisation of the radial term `(1/r) d/dr (r * kappa * dT/dr)` produces interface fluxes that scale with the interface radius rather than being uniform. The code multiplies the standard Cartesian conductivity entries by geometric correction factors:

| Neighbour | Interface radius | Correction factor |
| :--- | :--- | :--- |
| Inner (ix - 1) | r_inner = (ix - 1) * dr | (ix - 1) / (ix - 0.5) |
| Outer (ix + 1) | r_outer = ix * dr | ix / (ix - 0.5) |

Here `(ix - 0.5) * dr` is the cell-centre radius. These factors arise because the heat flux through a cylindrical surface of radius r and height dy is proportional to `2 * pi * r * dy`, and the ratio of the interface area to the cell-centre area gives the correction.

**Axial (y) conductivities are unchanged** -- the axial term has the same form as in Cartesian coordinates.

**Z-direction terms are zeroed out** -- since nz = 1, the code explicitly sets the z-neighbour conductivities F and G to zero in the H-matrix.

#### 3. Boundary-vector corrections

The boundary contribution at the outer radius (`ix = nx`) also receives the cylindrical area correction. The boundary conductivity term is multiplied by `r_outer / r_centre = ix / (ix - 0.5)` to account for the larger outer interface area. This ensures the fixed-temperature bath condition at r = R is applied with the correct radial geometry.

### `system.in` format in cylindrical mode

The grid dimensions line still reads three integers (`nx ny nz`), but `nz` **must** be `1`.

The physical dimensions line reads **only two** values:
```
Lx  Ly
```
where `Lx` is the cylinder radius and `Ly` is the axial length. The code internally sets Lz = 1.0 (a dummy value).

The rest of the file (material/heater grid) is written identically to Cartesian mode -- one row of `nx` entries per y-index.

**Example** -- 10-cell radial x 5-cell axial cylinder, radius 0.005 m, length 0.01 m:
```
10 5 1
0.005 0.01

! Z=1, Y=1 Row (top)
1:1 1:1 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0
! Z=1, Y=2 Row
1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0
! Z=1, Y=3 Row
1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0
! Z=1, Y=4 Row
1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0
! Z=1, Y=5 Row (bottom)
1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0 1:0
```
Here `ix = 1,2` at `iy = 1` are heated (the central core at the top of the cylinder).

### Boundary conditions

| Boundary | Behaviour | Keywords |
| :--- | :--- | :--- |
| **r = 0** (centre, inner face of `ix = 1`) | **Symmetry** -- zero radial heat flux. Applied automatically; no user input needed. | -- |
| **r = R** (outer radius, outer face of `ix = nx`) | Fixed-temperature bath with a finite boundary conductivity. | `kappaBoundNr`, `T_BathNr` |
| **y = 0** (top, `iy = 1`) | Standard axial boundary. | `kappaBoundy1`, `T_Bathy1` |
| **y = Ly** (bottom, `iy = ny`) | Standard axial boundary. | `kappaBoundNy`, `T_Bathy2` |

> **Important:** In cylindrical mode the Cartesian x-boundary and z-boundary keywords (`kappaBoundx1`, `kappaBoundNx`, `kappaBoundz1`, `kappaBoundNz`, `T_Bathx1`, `T_Bathx2`, `T_Bathz1`, `T_Bathz2`) are **not used** and are overridden internally. Use `kappaBoundNr` and `T_BathNr` for the outer radial boundary.

If only the global `kappaBound` keyword is set (without an explicit `kappaBoundNr`), the code will issue a warning and use the global value for all cylindrical boundaries.

### Heating in cylindrical mode

Power input works the same as in Cartesian mode: cells are tagged with a heater ID in `system.in`, and the `power_in` value from `param.in` is distributed over all heated cells.

The key difference is that the **heated volume** is now the sum of cylindrical annular shell volumes rather than rectangular brick volumes. The heater routine computes:

    V_heated = SUM over heated cells of: pi * (r_out^2 - r_in^2) * dy

and the volumetric power density applied to each heated cell is:

    q = power_in / V_heated

This means that if the heater covers the inner two radial cells (`ix = 1,2`), the heated volume is `pi * (2*dr)^2 * dy`, not `2 * dr * dy` as it would be in Cartesian mode. All heating types (constant, pulsed, AC, etc.) listed in the [Heating Types](#heating-types) table are available in cylindrical mode.

### Analytical steady-state solution (verification)

For a uniformly heated disk of radius R_H inside a cylinder of outer radius R_B with a fixed boundary temperature T_bath, the steady-state radial temperature profile is:

**Inside the heater (r <= R_H):**

    T(r) = T_bath + (q * R_H^2) / (2 * lambda) * ln(R_B / R_H) + q / (4 * lambda) * (R_H^2 - r^2)

**Outside the heater (r > R_H):**

    T(r) = T_bath + (q * R_H^2) / (2 * lambda) * ln(R_B / r)

where `lambda = rho * Cv * kappa` is the thermal conductivity and q is the volumetric heating rate inside the disk. Inside the heater the profile is parabolic; outside it follows a logarithmic decay. This solution can be used to verify cylindrical-mode simulations against theory.

### Example `param.in` (cylindrical)
```
_RunName = cylinder_test
_CylindricalGrid = T
ntime = 1000
time_step = 1e-6
icattaneo = 0
isteady = 0
power_in = 1.0
kappaBoundNr = 0.5
kappaBoundy1 = 0.5
kappaBoundNy = 0.5
T_Bath = 300.0
T_BathNr = 300.0
_WriteToTxt = T
```

### Checklist for setting up a cylindrical simulation

1. Set `_CylindricalGrid = T` in `param.in`.
2. Set `nz = 1` in `system.in`.
3. Provide **two** physical dimensions (`Lx Ly`) on the second line of `system.in` -- `Lx` is the outer radius, `Ly` is the axial length.
4. Set `kappaBoundNr` and `T_BathNr` for the outer radial boundary.
5. Set `kappaBoundy1`/`kappaBoundNy` and `T_Bathy1`/`T_Bathy2` for the axial boundaries (or set them to zero for insulating/mirror boundaries).
6. Do **not** set `kappaBoundx1`, `kappaBoundNx`, `kappaBoundz1`, or `kappaBoundNz` -- they are overridden.
7. Tag heated cells in `system.in` as usual (`MaterialID:HeaterID`). Remember that heated volume is now cylindrical.

---

## Execution

Ensure the `inputs/` directory exists with the three required files. Run the executable from the directory containing `inputs/`.

```bash
./ThermalFlow.x
```
or via `fpm`:
```bash
fpm run --profile release
```

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

HeatFlow supports an axisymmetric cylindrical coordinate system, enabled by setting `_CylindricalGrid = T` in `param.in`. In this mode the standard Cartesian grid is reinterpreted as a 2D radial–axial (r–y) domain:

| Grid axis | Physical meaning | Notes |
| :--- | :--- | :--- |
| **x** | Radial direction (r) | `x=1` is at the centre of the cylinder, increasing outward. |
| **y** | Axial direction (down the cylinder) | Same as Cartesian y. |
| **z** | Azimuthal (unused) | Must be set to `nz = 1`. |

### How it works

The simulation grid in `system.in` is defined as a standard 2D array (`nx × ny`, `nz = 1`). The physical dimensions `Lx` and `Ly` represent the cylinder radius and axial length respectively. Internally, the code makes two adjustments:

1.  **Cell volumes** — Each radial shell at index `ix` (with `dr = Lx/nx`) has volume:

    `V = π (r_out² − r_in²) × dy × dz`

    where `r_in = (ix−1)·dr` and `r_out = ix·dr`.

2.  **Heat-matrix conductivities** — The discretised radial heat equation in cylindrical coordinates is `(1/r) ∂/∂r (r κ ∂T/∂r)`. The interface area between adjacent radial shells scales with the interface radius. The code applies correction factors to the radial conductivity terms:

    - Inner neighbour: factor = `(ix − 1) / (ix − 0.5)`
    - Outer neighbour: factor = `ix / (ix − 0.5)`

    Axial (y) conductivities are unchanged.

### Boundary conditions

| Boundary | Behaviour |
| :--- | :--- |
| **r = 0** (centre, `ix = 1` inner face) | Symmetry boundary — zero radial heat flux. Automatic, no user input needed. |
| **r = R** (outer radius, `ix = nx` outer face) | Controlled by `kappaBoundNr` and `T_BathNr` in `param.in`. |
| **y = 0** and **y = Ly** (top/bottom) | Standard Cartesian boundaries (`kappaBoundy1`/`kappaBoundNy`, `T_Bathy1`/`T_Bathy2`). |

> **Note:** In cylindrical mode the Cartesian x and z boundary keywords (`kappaBoundx1`, `kappaBoundNx`, `kappaBoundz1`, `kappaBoundNz`) are not used. Set `kappaBoundNr` and `T_BathNr` instead.

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

### Example `system.in` (cylindrical)
A 10-cell radial × 5-cell axial cylinder, radius 0.005 m, length 0.01 m:
```
10 5 1
0.005 0.01 0.001

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
Here `x=1,2` at `y=1` are heated (the central core at the top of the cylinder).

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

#!/Users/fdavies/opt/miniconda3/envs/nova/bin/python
import sys, os, glob
import numpy as np
import matplotlib
matplotlib.use("Qt5Agg")
import matplotlib.pyplot as plt

from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure

from PyQt5 import QtWidgets, QtCore

#############################
# Utility functions to read files
#############################
def read_param_file(param_path):
    """
    Parses a param.in file (ignoring lines starting with '!').
    Returns a dictionary of parameters.
    """
    params = {}
    with open(param_path, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("!"):
                continue
            if '=' in line:
                parts = line.split('=')
                key = parts[0].strip()
                value = parts[1].split('!')[0].strip().strip('"')
                try:
                    if '.' in value or 'E' in value.upper():
                        params[key] = float(value)
                    else:
                        params[key] = int(value)
                except ValueError:
                    params[key] = value
    return params

def read_system_file(system_path):
    """
    Reads system.in and returns nx, ny, nz, Lx, Ly, Lz.
    Expected format:
      Line 1: nx ny nz
      Line 2: Lx Ly Lz
    """
    with open(system_path, 'r') as f:
        line1 = f.readline().strip()
        nx, ny, nz = map(int, line1.split())
        line2 = f.readline().strip()
        Lx, Ly, Lz = map(float, line2.split())
    return nx, ny, nz, Lx, Ly, Lz

def find_latest_log(outputs_dir, RunName, freq):
    """
    Searches the outputs directory for files matching:
       output_<RunName>_freq_<freq>_*
    Returns the one with the highest log number.
    """
    pattern = os.path.join(outputs_dir, f"output_{RunName}_freq_{freq:1.2f}_*")
    log_files = glob.glob(pattern)
    if not log_files:
        return None
    def extract_log_number(fname):
        base = os.path.basename(fname)
        try:
            return int(base.split('_')[-1])
        except ValueError:
            return -1
    log_files.sort(key=extract_log_number)
    return log_files[-1]

def read_log_file(log_path, sub_n):
    """
    Reads the log file and splits it into records.
    Each record: one simulation time followed by sub_n numbers.
    Returns a list of (time, data_array) pairs.
    """
    with open(log_path, 'r') as f:
        tokens = f.read().split()
    tokens = [t for t in tokens if t.strip() != ""]
    record_size = sub_n + 1
    if len(tokens) % record_size != 0:
        print("Warning: Total tokens not a multiple of expected record size.")
    nrecords = len(tokens) // record_size
    records = []
    for i in range(nrecords):
        chunk = tokens[i*record_size:(i+1)*record_size]
        try:
            t = float(chunk[0])
        except ValueError:
            t = None
        data = np.array(chunk[1:], dtype=float)
        records.append((t, data))
    return records

def effective_coordinates(start_idx, end_idx, d):
    """
    Returns a 1D array of physical coordinates (in mm) for cells from start_idx to end_idx.
    (Assumes Fortran 1-indexing; converts to Python 0-indexing.)
    """
    n = end_idx - start_idx + 1
    return np.arange(n)*d + (start_idx - 1)*d

#############################
# Functions to draw plots on an axis
#############################
def draw_heatmap(ax, temp_subset, plane, fixed_val,
                 x_coords, y_coords, z_coords, dx, dy, dz,
                 fixed_as_index=False, time_step=1.0, record_time=0.0, record_idx=0):
    """
    Draws a 2D heatmap on ax.
      - For plane 'xy': fixed in z.
      - For plane 'xz': fixed in y.
      - For plane 'yz': fixed in x.
    If fixed_as_index is True, fixed_val is treated as an index.
    If an axis has only one cell, its extent is set from 0 to the cell length.
    The plot title is appended with the simulation time (record_time * time_step)
    and the record index.
    Returns the plotted 2D array, the extent used, and the title.
    """
    # Determine extents.
    x_extent = [0, dx] if len(x_coords)==1 else [x_coords[0], x_coords[-1]]
    y_extent = [0, dy] if len(y_coords)==1 else [y_coords[0], y_coords[-1]]
    z_extent = [0, dz] if len(z_coords)==1 else [z_coords[0], z_coords[-1]]
    
    if plane == 'xy':
        if fixed_as_index:
            idx = int(fixed_val)
        else:
            if fixed_val < z_extent[0] or fixed_val > z_extent[1]:
                QtWidgets.QMessageBox.warning(None, "Fixed Value Warning",
                    f"Fixed value for z must be between {z_extent[0]:.3f} and {z_extent[1]:.3f} mm.\nUsing nearest valid value.")
            idx = int(np.abs(np.array(z_coords) - fixed_val).argmin())
        im_data = temp_subset[:, :, idx]
        xlabel, ylabel = 'X (mm)', 'Y (mm)'
        title = f"XY Heatmap at z index {idx} (z ≃ {z_coords[idx]:.3f} mm)"
        extent = [x_extent[0], x_extent[1], y_extent[0], y_extent[1]]
    elif plane == 'xz':
        if fixed_as_index:
            idx = int(fixed_val)
        else:
            if fixed_val < y_extent[0] or fixed_val > y_extent[1]:
                QtWidgets.QMessageBox.warning(None, "Fixed Value Warning",
                    f"Fixed value for y must be between {y_extent[0]:.3f} and {y_extent[1]:.3f} mm.\nUsing nearest valid value.")
            idx = int(np.abs(np.array(y_coords) - fixed_val).argmin())
        im_data = temp_subset[:, idx, :]
        xlabel, ylabel = 'X (mm)', 'Z (mm)'
        title = f"XZ Heatmap at y index {idx} (y ≃ {y_coords[idx]:.3f} mm)"
        extent = [x_extent[0], x_extent[1], z_extent[0], z_extent[1]]
    elif plane == 'yz':
        if fixed_as_index:
            idx = int(fixed_val)
        else:
            if fixed_val < x_extent[0] or fixed_val > x_extent[1]:
                QtWidgets.QMessageBox.warning(None, "Fixed Value Warning",
                    f"Fixed value for x must be between {x_extent[0]:.3f} and {x_extent[1]:.3f} mm.\nUsing nearest valid value.")
            idx = int(np.abs(np.array(x_coords) - fixed_val).argmin())
        im_data = temp_subset[idx, :, :]
        xlabel, ylabel = 'Y (mm)', 'Z (mm)'
        title = f"YZ Heatmap at x index {idx} (x ≃ {x_coords[idx]:.3f} mm)"
        extent = [y_extent[0], y_extent[1], z_extent[0], z_extent[1]]
    else:
        raise ValueError("Plane must be 'xy', 'xz', or 'yz'.")
    
    # Append time info: actual time = record_time * time_step.
    actual_time = record_time * time_step
    title += f", at time {actual_time:.4g} (timestep {record_idx})"
    
    ax.clear()
    im = ax.imshow(im_data.T, origin='lower', cmap='hot', extent=extent, aspect='auto')
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    plt.colorbar(im, ax=ax)
    return im_data, extent, title

def draw_line(ax, temp_subset, dimension, fixed1, fixed2,
              x_coords, y_coords, z_coords, dx, dy, dz,
              fixed_as_index=False, time_step=1.0, record_time=0.0, record_idx=0):
    """
    Draws a 1D line plot on ax.
      - For dimension 'x': vary x; fix y and z.
      - For 'y': vary y; fix x and z.
      - For 'z': vary z; fix x and y.
    If fixed_as_index is True, the fixed values are treated as indices.
    The plot title is appended with the simulation time and record index.
    Returns the x-axis array, line data, and the title.
    """
    x_extent = [0, dx] if len(x_coords)==1 else [x_coords[0], x_coords[-1]]
    y_extent = [0, dy] if len(y_coords)==1 else [y_coords[0], y_coords[-1]]
    z_extent = [0, dz] if len(z_coords)==1 else [z_coords[0], z_coords[-1]]
        
    if dimension == 'x':
        if fixed_as_index:
            y_idx = int(fixed1)
            z_idx = int(fixed2)
        else:
            if fixed1 < y_extent[0] or fixed1 > y_extent[1]:
                QtWidgets.QMessageBox.warning(None, "Fixed Value Warning",
                    f"Fixed value for y must be between {y_extent[0]:.3f} and {y_extent[1]:.3f} mm.\nUsing nearest valid value.")
            if fixed2 < z_extent[0] or fixed2 > z_extent[1]:
                QtWidgets.QMessageBox.warning(None, "Fixed Value Warning",
                    f"Fixed value for z must be between {z_extent[0]:.3f} and {z_extent[1]:.3f} mm.\nUsing nearest valid value.")
            y_idx = int(np.abs(np.array(y_coords) - fixed1).argmin())
            z_idx = int(np.abs(np.array(z_coords) - fixed2).argmin())
        line_data = temp_subset[:, y_idx, z_idx]
        x_axis = x_coords if len(x_coords) > 1 else np.linspace(x_extent[0], x_extent[1], 2)
        xlabel = 'X (mm)'
        title = f"Temperature vs X at y index {y_idx}, z index {z_idx}"
    elif dimension == 'y':
        if fixed_as_index:
            x_idx = int(fixed1)
            z_idx = int(fixed2)
        else:
            if fixed1 < x_extent[0] or fixed1 > x_extent[1]:
                QtWidgets.QMessageBox.warning(None, "Fixed Value Warning",
                    f"Fixed value for x must be between {x_extent[0]:.3f} and {x_extent[1]:.3f} mm.\nUsing nearest valid value.")
            if fixed2 < z_extent[0] or fixed2 > z_extent[1]:
                QtWidgets.QMessageBox.warning(None, "Fixed Value Warning",
                    f"Fixed value for z must be between {z_extent[0]:.3f} and {z_extent[1]:.3f} mm.\nUsing nearest valid value.")
            x_idx = int(np.abs(np.array(x_coords) - fixed1).argmin())
            z_idx = int(np.abs(np.array(z_coords) - fixed2).argmin())
        line_data = temp_subset[x_idx, :, z_idx]
        x_axis = y_coords if len(y_coords) > 1 else np.linspace(y_extent[0], y_extent[1], 2)
        xlabel = 'Y (mm)'
        title = f"Temperature vs Y at x index {x_idx}, z index {z_idx}"
    elif dimension == 'z':
        if fixed_as_index:
            x_idx = int(fixed1)
            y_idx = int(fixed2)
        else:
            if fixed1 < x_extent[0] or fixed1 > x_extent[1]:
                QtWidgets.QMessageBox.warning(None, "Fixed Value Warning",
                    f"Fixed value for x must be between {x_extent[0]:.3f} and {x_extent[1]:.3f} mm.\nUsing nearest valid value.")
            if fixed2 < y_extent[0] or fixed2 > y_extent[1]:
                QtWidgets.QMessageBox.warning(None, "Fixed Value Warning",
                    f"Fixed value for y must be between {y_extent[0]:.3f} and {y_extent[1]:.3f} mm.\nUsing nearest valid value.")
            x_idx = int(np.abs(np.array(x_coords) - fixed1).argmin())
            y_idx = int(np.abs(np.array(y_coords) - fixed2).argmin())
        line_data = temp_subset[x_idx, y_idx, :]
        x_axis = z_coords if len(z_coords) > 1 else np.linspace(z_extent[0], z_extent[1], 2)
        xlabel = 'Z (mm)'
        title = f"Temperature vs Z at x index {x_idx}, y index {y_idx}"
    else:
        raise ValueError("Dimension must be 'x', 'y', or 'z'.")
    
    # Append time information.
    actual_time = record_time * time_step
    title += f", at time {actual_time:.4g} (timestep {record_idx})"
    
    ax.clear()
    ax.plot(x_axis, line_data, marker='o')
    ax.set_xlabel(xlabel)
    ax.set_ylabel("Temperature")
    ax.set_title(title)
    ax.grid(True)
    return x_axis, line_data, title

def get_case_insensitive(d, key, default=None):
    key_lower = key.lower()
    for k, v in d.items():
        if k.lower() == key_lower:
            return v
    return default

#############################
# PyQt5 GUI Application
#############################
class MainWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Simulation Output Analyzer")
        self.last_fig = None  # Will hold the matplotlib Figure

        # Default directories.
        default_input = os.path.join(os.getcwd(), "inputs")
        if not os.path.exists(default_input):
            default_input = os.getcwd()
        default_output = os.path.join(os.getcwd(), "outputs")
        if not os.path.exists(default_output):
            default_output = os.getcwd()
        self.input_dir = default_input
        self.output_dir = default_output

        # Read parameters and system file.
        try:
            self.params = read_param_file(os.path.join(self.input_dir, "param.in"))
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Error", f"Failed to read param.in: {e}")
            sys.exit(1)
        try:
            self.full_nx, self.full_ny, self.full_nz, self.Lx, self.Ly, self.Lz = read_system_file(os.path.join(self.input_dir, "system.in"))
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Error", f"Failed to read system.in: {e}")
            sys.exit(1)
        self.dx = self.Lx / self.full_nx
        self.dy = self.Ly / self.full_ny
        self.dz = self.Lz / self.full_nz

        try:
            self.start_ix = self.params["start_ix"]
            self.end_ix   = self.params["end_ix"]
            self.start_iy = self.params["start_iy"]
            self.end_iy   = self.params["end_iy"]
            self.start_iz = self.params["start_iz"]
            self.end_iz   = self.params["end_iz"]
        except KeyError as e:
            QtWidgets.QMessageBox.critical(self, "Parameter Error", f"Missing parameter {e} in param.in")
            sys.exit(1)
        self.eff_nx = self.end_ix - self.start_ix + 1
        self.eff_ny = self.end_iy - self.start_iy + 1
        self.eff_nz = self.end_iz - self.start_iz + 1
        self.sub_n = self.eff_nx * self.eff_ny * self.eff_nz

        self.x_coords = effective_coordinates(self.start_ix, self.end_ix, self.dx)
        self.y_coords = effective_coordinates(self.start_iy, self.end_iy, self.dy)
        self.z_coords = effective_coordinates(self.start_iz, self.end_iz, self.dz)

        self.RunName = self.params.get("RunName", "run1")
        self.freq = self.params.get("freq", 1.0)
        self.time_step = get_case_insensitive(self.params, "time_step", 1.0)

        # Automatically determine the latest log file.
        self.latest_log = find_latest_log(self.output_dir, self.RunName, self.freq)
        if self.latest_log is None:
            QtWidgets.QMessageBox.critical(self, "File Error", "No log file found matching the expected pattern.")
            sys.exit(1)
        self.records = read_log_file(self.latest_log, self.sub_n)
        if not self.records:
            QtWidgets.QMessageBox.critical(self, "File Error", "No records found in the log file.")
            sys.exit(1)

        self.build_ui()

    def build_ui(self):
        central = QtWidgets.QWidget()
        main_layout = QtWidgets.QVBoxLayout()

        # Directory selection.
        dir_layout = QtWidgets.QHBoxLayout()
        in_label = QtWidgets.QLabel("Input Dir:")
        self.inDirLineEdit = QtWidgets.QLineEdit(self.input_dir)
        self.inBrowseButton = QtWidgets.QPushButton("Browse")
        self.inBrowseButton.clicked.connect(self.browse_input_dir)
        out_label = QtWidgets.QLabel("Output Dir:")
        self.outDirLineEdit = QtWidgets.QLineEdit(self.output_dir)
        self.outBrowseButton = QtWidgets.QPushButton("Browse")
        self.outBrowseButton.clicked.connect(self.browse_output_dir)
        dir_layout.addWidget(in_label)
        dir_layout.addWidget(self.inDirLineEdit)
        dir_layout.addWidget(self.inBrowseButton)
        dir_layout.addWidget(out_label)
        dir_layout.addWidget(self.outDirLineEdit)
        dir_layout.addWidget(self.outBrowseButton)
        main_layout.addLayout(dir_layout)

        # Output file selection drop-down.
        file_layout = QtWidgets.QHBoxLayout()
        file_label = QtWidgets.QLabel("Output file:")
        self.outputFileCombo = QtWidgets.QComboBox()
        self.update_output_files_list()
        file_layout.addWidget(file_label)
        file_layout.addWidget(self.outputFileCombo)
        main_layout.addLayout(file_layout)

        # Mode selection.
        mode_layout = QtWidgets.QHBoxLayout()
        mode_label = QtWidgets.QLabel("Mode:")
        self.modeCombo = QtWidgets.QComboBox()
        self.modeCombo.addItems(["heatmap", "line"])
        self.modeCombo.currentTextChanged.connect(self.mode_changed)
        mode_layout.addWidget(mode_label)
        mode_layout.addWidget(self.modeCombo)
        main_layout.addLayout(mode_layout)

        # Heatmap-specific.
        self.heatmap_widget = QtWidgets.QWidget()
        hm_layout = QtWidgets.QHBoxLayout()
        plane_label = QtWidgets.QLabel("Plane:")
        self.planeCombo = QtWidgets.QComboBox()
        self.planeCombo.addItems(["xy", "xz", "yz"])
        fixed_label = QtWidgets.QLabel("Fixed (mm/index):")
        self.fixedLineEdit = QtWidgets.QLineEdit("0.0")
        self.fixedToggle = QtWidgets.QCheckBox("Index")
        hm_layout.addWidget(plane_label)
        hm_layout.addWidget(self.planeCombo)
        hm_layout.addWidget(fixed_label)
        hm_layout.addWidget(self.fixedLineEdit)
        hm_layout.addWidget(self.fixedToggle)
        self.heatmap_widget.setLayout(hm_layout)
        main_layout.addWidget(self.heatmap_widget)

        # Line-specific.
        self.line_widget = QtWidgets.QWidget()
        line_layout = QtWidgets.QHBoxLayout()
        dimension_label = QtWidgets.QLabel("Dimension:")
        self.dimensionCombo = QtWidgets.QComboBox()
        self.dimensionCombo.addItems(["x", "y", "z"])
        fixed1_label = QtWidgets.QLabel("Fixed1 (mm/index):")
        self.fixed1LineEdit = QtWidgets.QLineEdit("0.0")
        self.fixed1Toggle = QtWidgets.QCheckBox("Index")
        fixed2_label = QtWidgets.QLabel("Fixed2 (mm/index):")
        self.fixed2LineEdit = QtWidgets.QLineEdit("0.0")
        self.fixed2Toggle = QtWidgets.QCheckBox("Index")
        line_layout.addWidget(dimension_label)
        line_layout.addWidget(self.dimensionCombo)
        line_layout.addWidget(fixed1_label)
        line_layout.addWidget(self.fixed1LineEdit)
        line_layout.addWidget(self.fixed1Toggle)
        line_layout.addWidget(fixed2_label)
        line_layout.addWidget(self.fixed2LineEdit)
        line_layout.addWidget(self.fixed2Toggle)
        self.line_widget.setLayout(line_layout)
        main_layout.addWidget(self.line_widget)

        # Timestep: slider and line edit.
        ts_layout = QtWidgets.QHBoxLayout()
        ts_label = QtWidgets.QLabel("Timestep (record index):")
        self.timestepLineEdit = QtWidgets.QLineEdit(str(len(self.records)-1))
        self.timestepSlider = QtWidgets.QSlider(QtCore.Qt.Horizontal)
        self.timestepSlider.setMinimum(0)
        self.timestepSlider.setMaximum(len(self.records)-1)
        self.timestepSlider.setValue(len(self.records)-1)
        self.timestepSlider.valueChanged.connect(self.slider_changed)
        self.timestepLineEdit.editingFinished.connect(self.lineedit_changed)
        ts_layout.addWidget(ts_label)
        ts_layout.addWidget(self.timestepSlider)
        ts_layout.addWidget(self.timestepLineEdit)
        main_layout.addLayout(ts_layout)

        # Buttons: Plot, Save Plot, Export Data.
        btn_layout = QtWidgets.QHBoxLayout()
        self.plotButton = QtWidgets.QPushButton("Plot")
        self.plotButton.clicked.connect(self.do_plot)
        self.saveButton = QtWidgets.QPushButton("Save Plot")
        self.saveButton.clicked.connect(self.save_plot)
        self.exportButton = QtWidgets.QPushButton("Export Data")
        self.exportButton.clicked.connect(self.export_data)
        btn_layout.addWidget(self.plotButton)
        btn_layout.addWidget(self.saveButton)
        btn_layout.addWidget(self.exportButton)
        main_layout.addLayout(btn_layout)

        # Info label.
        info = (f"Subset ranges: X: {self.x_coords[0]:.3f} to {self.x_coords[-1]:.3f} mm, "
                f"Y: {self.y_coords[0]:.3f} to {self.y_coords[-1]:.3f} mm, "
                f"Z: {self.z_coords[0]:.3f} to {self.z_coords[-1]:.3f} mm")
        self.infoLabel = QtWidgets.QLabel(info)
        main_layout.addWidget(self.infoLabel)

        # Embedded matplotlib FigureCanvas.
        self.figure = Figure(figsize=(5,4))
        self.canvas = FigureCanvas(self.figure)
        main_layout.addWidget(self.canvas)

        central.setLayout(main_layout)
        self.setCentralWidget(central)

        self.mode_changed(self.modeCombo.currentText())

    def browse_input_dir(self):
        dirname = QtWidgets.QFileDialog.getExistingDirectory(self, "Select Input Directory", self.input_dir)
        if dirname:
            self.input_dir = dirname
            self.inDirLineEdit.setText(dirname)
            try:
                self.params = read_param_file(os.path.join(self.input_dir, "param.in"))
                self.full_nx, self.full_ny, self.full_nz, self.Lx, self.Ly, self.Lz = read_system_file(os.path.join(self.input_dir, "system.in"))
                self.dx = self.Lx / self.full_nx
                self.dy = self.Ly / self.full_ny
                self.dz = self.Lz / self.full_nz
                self.start_ix = self.params["start_ix"]
                self.end_ix   = self.params["end_ix"]
                self.start_iy = self.params["start_iy"]
                self.end_iy   = self.params["end_iy"]
                self.start_iz = self.params["start_iz"]
                self.end_iz   = self.params["end_iz"]
                self.eff_nx = self.end_ix - self.start_ix + 1
                self.eff_ny = self.end_iy - self.start_iy + 1
                self.eff_nz = self.end_iz - self.start_iz + 1
                self.sub_n = self.eff_nx * self.eff_ny * self.eff_nz
                self.x_coords = effective_coordinates(self.start_ix, self.end_ix, self.dx)
                self.y_coords = effective_coordinates(self.start_iy, self.end_iy, self.dy)
                self.z_coords = effective_coordinates(self.start_iz, self.end_iz, self.dz)
                self.infoLabel.setText(
                    f"Subset ranges: X: {self.x_coords[0]:.3f} to {self.x_coords[-1]:.3f} mm, "
                    f"Y: {self.y_coords[0]:.3f} to {self.y_coords[-1]:.3f} mm, "
                    f"Z: {self.z_coords[0]:.3f} to {self.z_coords[-1]:.3f} mm")
            except Exception as e:
                QtWidgets.QMessageBox.critical(self, "Error", f"Error reloading input files: {e}")

    def browse_output_dir(self):
        dirname = QtWidgets.QFileDialog.getExistingDirectory(self, "Select Output Directory", self.output_dir)
        if dirname:
            self.output_dir = dirname
            self.outDirLineEdit.setText(dirname)
            self.update_output_files_list()

    def update_output_files_list(self):
        pattern = os.path.join(self.output_dir, f"output_{self.RunName}_freq_{self.freq:1.2f}_*")
        files = glob.glob(pattern)
        files.sort()
        self.outputFileCombo.clear()
        if files:
            self.outputFileCombo.addItems(files)
            if self.latest_log in files:
                index = files.index(self.latest_log)
                self.outputFileCombo.setCurrentIndex(index)
        else:
            self.outputFileCombo.addItem("")

    def mode_changed(self, mode):
        if mode == "heatmap":
            self.heatmap_widget.show()
            self.line_widget.hide()
        else:
            self.heatmap_widget.hide()
            self.line_widget.show()

    def slider_changed(self, value):
        self.timestepLineEdit.setText(str(value))

    def lineedit_changed(self):
        try:
            val = int(self.timestepLineEdit.text())
        except ValueError:
            val = len(self.records)-1
        if val < 0 or val >= len(self.records):
            val = len(self.records)-1
        self.timestepSlider.setValue(val)

    def do_plot(self):
        selected_file = self.outputFileCombo.currentText().strip()
        if selected_file:
            log_file = selected_file
        else:
            log_file = self.latest_log
        try:
            self.records = read_log_file(log_file, self.sub_n)
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Error", f"Error reading log file: {e}")
            return

        try:
            ts = int(self.timestepLineEdit.text())
        except ValueError:
            ts = len(self.records)-1
        if ts < 0 or ts >= len(self.records):
            ts = len(self.records)-1
            QtWidgets.QMessageBox.warning(self, "Timestep Warning",
                                          f"Timestep index out of range. Using latest record (index {ts}).")
        rec_time, data = self.records[ts]
        temp_subset = data.reshape((self.eff_nx, self.eff_ny, self.eff_nz), order='F')

        self.figure.clf()
        ax = self.figure.add_subplot(111)
        mode = self.modeCombo.currentText()
        if mode == "heatmap":
            plane = self.planeCombo.currentText()
            try:
                fixed_val = float(self.fixedLineEdit.text())
            except ValueError:
                fixed_val = 0.0
            use_index = self.fixedToggle.isChecked()
            self.current_data, self.current_extent, self.current_title = draw_heatmap(
                ax, temp_subset, plane, fixed_val,
                self.x_coords, self.y_coords, self.z_coords,
                self.dx, self.dy, self.dz, fixed_as_index=use_index,
                time_step=self.time_step, record_time=rec_time, record_idx=ts)
        else:
            dimension = self.dimensionCombo.currentText()
            try:
                fixed1_val = float(self.fixed1LineEdit.text())
                fixed2_val = float(self.fixed2LineEdit.text())
            except ValueError:
                fixed1_val = fixed2_val = 0.0
            use_index_flag = self.fixed1Toggle.isChecked() or self.fixed2Toggle.isChecked()
            self.current_axis, self.current_line, self.current_title = draw_line(
                ax, temp_subset, dimension, fixed1_val, fixed2_val,
                self.x_coords, self.y_coords, self.z_coords,
                self.dx, self.dy, self.dz, fixed_as_index=use_index_flag,
                time_step=self.time_step, record_time=rec_time, record_idx=ts)
        self.canvas.draw()

    def save_plot(self):
        if self.figure is None:
            QtWidgets.QMessageBox.warning(self, "Save Plot", "No plot available. Please plot first.")
            return
        fname, _ = QtWidgets.QFileDialog.getSaveFileName(self, "Save Plot", "", "PNG Image (*.png);;All Files (*)")
        if fname:
            self.figure.savefig(fname)
            QtWidgets.QMessageBox.information(self, "Save Plot", f"Plot saved to {fname}")

    def export_data(self):
        mode = self.modeCombo.currentText()
        try:
            ts = int(self.timestepLineEdit.text())
        except ValueError:
            ts = len(self.records)-1
        rec_time, data = self.records[ts]
        temp_subset = data.reshape((self.eff_nx, self.eff_ny, self.eff_nz), order='F')
        meta_lines = [
            f"# Log file: {self.outputFileCombo.currentText().strip() or self.latest_log}",
            f"# Time step: {self.time_step}",
        ]
        if mode == "heatmap":
            plane = self.planeCombo.currentText()
            try:
                fixed_val = float(self.fixedLineEdit.text())
            except ValueError:
                fixed_val = 0.0
            use_index = self.fixedToggle.isChecked()
            if plane == 'xy':
                idx = int(fixed_val) if use_index else int(np.abs(np.array(self.z_coords)-fixed_val).argmin())
                data_to_export = temp_subset[:, :, idx]
                grid_x, grid_y = np.meshgrid(self.x_coords, self.y_coords, indexing='ij')
                out_data = np.column_stack((grid_x.flatten(), grid_y.flatten(), data_to_export.flatten()))
                meta_lines.append(f"# Plane: xy, Fixed (z): {fixed_val} {'(index)' if use_index else '(mm)'}")
                header = "\n".join(meta_lines) + f"\n# Heatmap (xy) at z index {idx} (z ≃ {self.z_coords[idx]:.3f} mm), at time {(rec_time*self.time_step):.4g} (timestep {ts})\n# Columns: X (mm), Y (mm), Temperature"
            elif plane == 'xz':
                idx = int(fixed_val) if use_index else int(np.abs(np.array(self.y_coords)-fixed_val).argmin())
                data_to_export = temp_subset[:, idx, :]
                grid_x, grid_y = np.meshgrid(self.x_coords, self.z_coords, indexing='ij')
                out_data = np.column_stack((grid_x.flatten(), grid_y.flatten(), data_to_export.flatten()))
                meta_lines.append(f"# Plane: xz, Fixed (y): {fixed_val} {'(index)' if use_index else '(mm)'}")
                header = "\n".join(meta_lines) + f"\n# Heatmap (xz) at y index {idx} (y ≃ {self.y_coords[idx]:.3f} mm), at time {(rec_time*self.time_step):.4g} (timestep {ts})\n# Columns: X (mm), Z (mm), Temperature"
            elif plane == 'yz':
                idx = int(fixed_val) if use_index else int(np.abs(np.array(self.x_coords)-fixed_val).argmin())
                data_to_export = temp_subset[idx, :, :]
                grid_x, grid_y = np.meshgrid(self.y_coords, self.z_coords, indexing='ij')
                out_data = np.column_stack((grid_x.flatten(), grid_y.flatten(), data_to_export.flatten()))
                meta_lines.append(f"# Plane: yz, Fixed (x): {fixed_val} {'(index)' if use_index else '(mm)'}")
                header = "\n".join(meta_lines) + f"\n# Heatmap (yz) at x index {idx} (x ≃ {self.x_coords[idx]:.3f} mm), at time {(rec_time*self.time_step):.4g} (timestep {ts})\n# Columns: Y (mm), Z (mm), Temperature"
            else:
                QtWidgets.QMessageBox.warning(self, "Export Error", "Invalid plane selected.")
                return
        else:  # line mode
            dimension = self.dimensionCombo.currentText()
            try:
                fixed1_val = float(self.fixed1LineEdit.text())
                fixed2_val = float(self.fixed2LineEdit.text())
            except ValueError:
                fixed1_val = fixed2_val = 0.0
            use_index_flag = self.fixed1Toggle.isChecked() or self.fixed2Toggle.isChecked()
            if dimension == 'x':
                if use_index_flag:
                    y_idx = int(fixed1_val)
                    z_idx = int(fixed2_val)
                else:
                    y_idx = int(np.abs(np.array(self.y_coords)-fixed1_val).argmin())
                    z_idx = int(np.abs(np.array(self.z_coords)-fixed2_val).argmin())
                line_data = temp_subset[:, y_idx, z_idx]
                x_axis = self.x_coords
                out_data = np.column_stack((x_axis, line_data))
                meta_lines.append(f"# Dimension: x, Fixed1 (y): {fixed1_val} {'(index)' if use_index_flag else '(mm)'}, Fixed2 (z): {fixed2_val} {'(index)' if use_index_flag else '(mm)'}")
                header = "\n".join(meta_lines) + f"\n# Line data for dimension x at y index {y_idx}, z index {z_idx}, at time {(rec_time*self.time_step):.4g} (timestep {ts})\n# Columns: X (mm), Temperature"
            elif dimension == 'y':
                if use_index_flag:
                    x_idx = int(fixed1_val)
                    z_idx = int(fixed2_val)
                else:
                    x_idx = int(np.abs(np.array(self.x_coords)-fixed1_val).argmin())
                    z_idx = int(np.abs(np.array(self.z_coords)-fixed2_val).argmin())
                line_data = temp_subset[x_idx, :, z_idx]
                x_axis = self.y_coords
                out_data = np.column_stack((x_axis, line_data))
                meta_lines.append(f"# Dimension: y, Fixed1 (x): {fixed1_val} {'(index)' if use_index_flag else '(mm)'}, Fixed2 (z): {fixed2_val} {'(index)' if use_index_flag else '(mm)'}")
                header = "\n".join(meta_lines) + f"\n# Line data for dimension y at x index {x_idx}, z index {z_idx}, at time {(rec_time*self.time_step):.4g} (timestep {ts})\n# Columns: Y (mm), Temperature"
            elif dimension == 'z':
                if use_index_flag:
                    x_idx = int(fixed1_val)
                    y_idx = int(fixed2_val)
                else:
                    x_idx = int(np.abs(np.array(self.x_coords)-fixed1_val).argmin())
                    y_idx = int(np.abs(np.array(self.y_coords)-fixed2_val).argmin())
                line_data = temp_subset[x_idx, y_idx, :]
                x_axis = self.z_coords
                out_data = np.column_stack((x_axis, line_data))
                meta_lines.append(f"# Dimension: z, Fixed1 (x): {fixed1_val} {'(index)' if use_index_flag else '(mm)'}, Fixed2 (y): {fixed2_val} {'(index)' if use_index_flag else '(mm)'}")
                header = "\n".join(meta_lines) + f"\n# Line data for dimension z at x index {x_idx}, y index {y_idx}, at time {(rec_time*self.time_step):.4g} (timestep {ts})\n# Columns: Z (mm), Temperature"
            else:
                QtWidgets.QMessageBox.warning(self, "Export Error", "Invalid dimension selected.")
                return
        fname, _ = QtWidgets.QFileDialog.getSaveFileName(self, "Export Data", "", "Data Files (*.dat);;Text Files (*.txt);;All Files (*)")
        if fname:
            np.savetxt(fname, out_data, header=header)
            QtWidgets.QMessageBox.information(self, "Export Data", f"Data exported to {fname}")

if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    window = MainWindow()
    window.resize(900, 700)
    window.show()
    sys.exit(app.exec_())


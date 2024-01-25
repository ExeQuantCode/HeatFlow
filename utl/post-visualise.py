#!/home/fhd205/opt/anaconda3/envs/py3.11/bin/python
import plotly.graph_objects as go
import numpy as np
from scipy import stats
import sys

# -------------------------------------------------------------
# Instructions for Using This Script:
# -------------------------------------------------------------
# Dependencies: 
# - Python (version 3.6 or higher)
# - Packages: plotly, numpy, scipy
#
# Recommended Installation Method:
# 1. Activate your Python 3.6+ environment in Conda:
#    > conda activate [your_python_3.6_or_higher_environment]
#
# 2. Install the required packages using Conda (if needed):
#    > conda install scipy
#    > conda install numpy
#    > conda install plotly
#
# Note:
# - You can find the appropriate shebang (hashbang) line for
#   your environment by executing `which python` in the terminal
#   after activating your environment.
# - Replace '[your_python_3.6_or_higher_environment]' with the
#   actual name of your Conda environment.
# -------------------------------------------------------------

###############################################################
def invert_mode(arr):
    """ Inverts the mode value in the array.

    Args:
        arr (numpy.ndarray): Input array.

    Returns:
        numpy.ndarray: Array with mode value inverted.
    """
    mode = stats.mode(arr)[0]
    arr[arr == mode] = -mode
    return arr
###############################################################
def compress_to_range(arr):
    """ Compresses the array values to a continuous range.

    Args:
        arr (numpy.ndarray): Input array.

    Returns:
        numpy.ndarray: Compressed array.
    """
    unique_values = np.unique(arr)
    mapping = {value: idx + 1 for idx, value in enumerate(unique_values)}
    compressed_arr = np.vectorize(mapping.get)(arr)
    return compressed_arr
###############################################################
def read_matrix(infile):
    """ Reads a 3D matrix from a file.

    Args:
        infile (str): Path to the input file.

    Returns:
        tuple: A tuple containing the matrix, its dimensions, and limits.
    """
    with open(infile, 'r') as f:
        x_dim, y_dim, z_dim = map(int, f.readline().split())
        matrix = np.zeros((x_dim, y_dim, z_dim))
        print(matrix.shape)
        
        x_lim, y_lim, z_lim = map(float, f.readline().split())

        f.readline()
        z = 0
        while z < z_dim:
            for y in range(y_dim):
                line = f.readline().strip()
                if line:
                    matrix[:,y,z] = list(map(int, line.split()))
            z += 1
            f.readline()  # Skip the blank line after each z block
            
    return matrix, (x_dim, y_dim, z_dim), (x_lim, y_lim, z_lim)
###############################################################
###############################################################
###############################################################

if len(sys.argv) > 1:
    matrix_file = sys.argv[1]
else:
    print("No input file provided.")
    sys.exit(1)

matrix, dim, lim = read_matrix(matrix_file)

# Create a 3D grid
x, y, z = np.indices(np.array(matrix.shape))
x, y, z = x * lim[0] / dim[0], y * lim[1] / dim[1], z * lim[2] / dim[2]
values = matrix.flatten()


###############################################################
# auto set the most useful view
values=invert_mode(values)
values=compress_to_range(values)
isomin_ = min(values)
isomax_ = max(values)
sufnum = len(np.unique(values))
###############################################################

# Define isosurface
fig = go.Figure(data=go.Isosurface(
    x=x.flatten(),
    y=y.flatten(),
    z=z.flatten(),
    value=values,
    isomin=isomin_+1,  # Minimum value to define a region
    isomax=isomax_,  # Maximum value to define a region
    opacity=0.4,  # Transparency of surfaces
    surface_count=sufnum,  # Number of isosurfaces
    caps=dict(x_show=False, y_show=False)
    ))

fig.show()


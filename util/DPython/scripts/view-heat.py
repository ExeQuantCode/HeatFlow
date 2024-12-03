#!/home/fhd205/opt/anaconda3/envs/py3.11/bin/python
import argparse
import os
import numpy as np
import plotly.graph_objects as go
import matplotlib.pyplot as plt

def parse_arguments():
    """
    Parse command line arguments for geometry and heat files.
    """
    parser = argparse.ArgumentParser(description="Process geom and heat files.")
    parser.add_argument('-g', '--geom', type=str, help="Path to the geometry file.")
    parser.add_argument('-H', '--heat', type=str, help="Path to the heat distribution file.")
    parser.add_argument('-l', '--lower', type=float, help="set lower bound on temprature for 2D plots")
    parser.add_argument('-u', '--upper', type=float, help="set upper bound on temprature for 2D plots")
    parser.add_argument('-s', '--cut', type=int, help="element to take slice at")
    return parser.parse_args()

def find_file(search_paths):
    """
    Search for a file in a list of paths and return the first match found.
    """
    for path in search_paths:
        if os.path.isfile(path):
            return path
    return None

def read_geom_file(filepath):
    """
    Read and process the geometry file.
    """
    with open(filepath, 'r') as f:
        x_dim, y_dim, z_dim = map(int, f.readline().split())
        x_lim, y_lim, z_lim = map(float, f.readline().split())
    return (x_dim, y_dim, z_dim), (x_lim, y_lim, z_lim)

def read_heat_file(filepath, dimensions):
    """
    Read and process the heat distribution file.
    """
    a=[]
    with open(filepath, 'r') as f:
        for line in f:
            a.extend([float(i) for i in line.split()])
    a_array = np.array(a)
    matrix = a_array.reshape(dimensions, order='F')
    return matrix

def generate_plot(matrix, dimensions, limits):
    """
    Generate and display a 3D plot of the data.
    """
    x, y, z = np.indices(np.array(matrix.shape))
    x, y, z = x * limits[0] / dimensions[0], y * limits[1] / dimensions[1], z * limits[2] / dimensions[2]
    values = matrix.flatten()

    isomin_ = min(values)
    isomax_ = max(values)
    sufnum = 25

    fig = go.Figure(data=go.Isosurface(
        x=x.flatten(),
        y=y.flatten(),
        z=z.flatten(),
        value=values,
        isomin=isomin_,
        isomax=isomax_,
        opacity=0.3,
        surface_count=sufnum,
        caps=dict(x_show=False, y_show=False)
    ))

    fig.show()

def plotline(matrix, axis, element1, element2):
    """
    Plots a line through a 3D matrix along a specified axis at given elements in the other two axes.
    
    Parameters:
    - matrix: 3D numpy array.
    - axis: Axis along which to plot the line (0 for x, 1 for y, 2 for z).
    - element1: Position in the first non-specified axis.
    - element2: Position in the second non-specified axis.
    """
    if axis == 0:
        line_to_plot = matrix[:, element1, element2]
        x = np.arange(line_to_plot.size)
        plt.title(f'Line along x-axis at y={element1}, z={element2}')
    elif axis == 1:
        line_to_plot = matrix[element1, :, element2]
        x = np.arange(line_to_plot.size)
        plt.title(f'Line along y-axis at x={element1}, z={element2}')
    elif axis == 2:
        line_to_plot = matrix[element1, element2, :]
        x = np.arange(line_to_plot.size)
        plt.title(f'Line along z-axis at x={element1}, y={element2}')
    else:
        raise ValueError("Axis must be 0 (x), 1 (y), or 2 (z).")

    plt.plot(x, line_to_plot)
    plt.xlabel('Position')
    plt.ylabel('Tempurature')
    plt.grid(True)
    plt.show()

    
def plot_center_slice(matrix, axis, limits, uppertemp=None, lowertemp=None, slice_cord=None):
    """
    Plots a 2D slice from the center (or specified) cell of a given axis.
    The slice is normal to the specified axis.
    
    Parameters:
    - matrix: 3D numpy array.
    - axis: Axis normal to the slice (0 for x, 1 for y, 2 for z).
    - limits: Tuple of limits for each axis.
    - uppertemp: Optional upper bound to temperature.
    - lowertemp: Optional lower bound to temperature.
    - slice_cord: Optional slice coordinate; if None, center slice is plotted.
    """
    if slice_cord is not None:
        center_index = slice_cord
    else:
        center_index = matrix.shape[axis] // 2

    if axis == 0:
        slice_to_plot = matrix[center_index, :, :]
        lim=limits[2],limits[1]
        xlab='z axis'
        ylab='y axis'
        plt.title('2D Slice at center x-axis')
    elif axis == 1:
        slice_to_plot = matrix[:, center_index, :]
        lim=limits[0],limits[2]
        xlab='x axis'
        ylab='z axis'
        plt.title('2D Slice at center y-axis')
    elif axis == 2:
        slice_to_plot = matrix[:, :, center_index]
        lim=limits[0],limits[1]
        xlab='x axis'
        ylab='y axis'
        plt.title('2D Slice at center z-axis')
    else:
        raise ValueError("Axis must be 0 (x), 1 (y), or 2 (z).")

    # Adjust the dimensions if needed (for imshow)
    if axis != 0:
        slice_to_plot = slice_to_plot.transpose()
        

    if uppertemp and lowertemp:
        adjusted_slice = np.clip(slice_to_plot, lowertemp, uppertemp)
        plt.imshow(adjusted_slice, cmap='hot', interpolation='nearest',
                   extent=[0, lim[0], 0, lim[1]])
    else:
        #plt.imshow(slice_to_plot, cmap='hot', interpolation='nearest')
        plt.imshow(slice_to_plot, cmap='hot', interpolation='nearest', extent=[0, lim[0], 0, lim[1]])

    plt.colorbar(label='Tempurature')
    plt.xlabel(xlab)
    plt.ylabel(ylab)
    plt.title(f'2D Heatmap with Temperature Range [{lowertemp}, {uppertemp}]')
    plt.show()


def plot_options(matrix, limits, uppertemp=None, lowertemp=None, cut=None):
    # If 'cut' is specified, plot slices at this specific coordinate on all axes.
    if cut is not None:
        # Iterate over each axis and plot the given slice with optional temperature bounds.
        for axis in range(3):
            plot_center_slice(matrix, axis, limits, uppertemp, lowertemp, slice_cord=cut)
    else:
        # If 'cut' is not specified, consider plotting center slices with optional temperature bounds.
        for axis in range(3):
            plot_center_slice(matrix, axis, limits, uppertemp, lowertemp)    

    
    
def main():
    args = parse_arguments()

    # Define default file search paths
    geom_paths = ["geom.in", "inputs/geom.in", "../inputs/geom.in"]
    heat_paths = ["TempDis.dat", "outputs/TempDis.dat", "../outputs/TempDis.dat"]

    # Attempt to find the geometry and heat files if not specified
    geom_file = args.geom if args.geom else find_file(geom_paths)
    heat_file = args.heat if args.heat else find_file(heat_paths)

    if not geom_file or not heat_file:
        print("Error: Required files not found.")
        return

    dimensions, limits = read_geom_file(geom_file)
    matrix = read_heat_file(heat_file, dimensions)
    #generate_plot(matrix, dimensions, limits)

    
    plot_options(matrix, limits, args.upper, args.lower, args.cut)
    
    plotline(matrix, 2, 3, 3)

if __name__ == "__main__":
    main()


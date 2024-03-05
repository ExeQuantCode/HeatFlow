#!/home/fhd205/opt/anaconda3/envs/py3.11/bin/python
import argparse
import os
import numpy as np
import plotly.graph_objects as go

def parse_arguments():
    """
    Parse command line arguments for geometry and heat files.
    """
    parser = argparse.ArgumentParser(description="Process geom and heat files.")
    parser.add_argument('-g', '--geom', type=str, help="Path to the geometry file.")
    parser.add_argument('-H', '--heat', type=str, help="Path to the heat distribution file.")
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
    with open(filepath, 'r') as f:
        line = f.readline()
        a = [float(i) for i in line.split()]
    a_array = np.array(a)
    matrix = a_array.reshape(dimensions)
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
    generate_plot(matrix, dimensions, limits)

if __name__ == "__main__":
    main()


#!/home/fhd205/opt/anaconda3/envs/py3.11/bin/python
import sys
import math
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from itertools import product, combinations


def multi_split(s, *delimiters):
    #Split the string on the first found delimiter.
    for delimiter in delimiters:
        if delimiter in s:
            return s.split(delimiter)
    raise ValueError("No delimiters found in the string: {}".format(s))

def volume_read(block_type, line, block_data):
    line_lower = line.lower()
    if block_type == "VOLUME":
        flags = ["x", "y", "z", "d"]
        if any(line_lower.startswith(flag) for flag in flags):
            key, value = multi_split(line_lower, '=', ':')
            block_data[key.strip()] = float(value.strip())
        if line_lower.startswith("grid"):
            key, value = multi_split(line_lower, '=', ':')
            v1, v2, v3 = value.split()
            v1, v2, v3 = int(v1), int(v2), int(v3)
            block_data[key.strip()] = [v1, v2, v3]
        flags = ["f", "u"]
        if any(line_lower.startswith(flag) for flag in flags):
            key, value = multi_split(line_lower, '=', ':')
            block_data[key.strip()] = str(value.strip())

def cuboid_read(block_type, line, block_data):
    line_lower = line.lower()
    if block_type == "CUBOID":
        flags = ["origin", "dim"]
        if any(line_lower.startswith(flag) for flag in flags):
            key, value = multi_split(line_lower, '=', ':')
            v1, v2, v3 = value.split()
            block_data[key.strip()] = [float(v1), float(v2), float(v3)]
        if line_lower.startswith("f"):
            key, value = multi_split(line_lower, '=', ':')
            block_data[key.strip()] = str(value.strip())

def sphere_read(block_type, line, block_data):
    line_lower = line.lower()
    if block_type == "SPHERE":
        if line_lower.startswith("center"):
            key, value = multi_split(line_lower, '=', ':')
            v1, v2, v3 = value.split()
            block_data[key.strip()] = [float(v1), float(v2), float(v3)]
        if line_lower.startswith("radius"):
            key, value = multi_split(line_lower, '=', ':')
            block_data[key.strip()] = float(value.strip())
        if line_lower.startswith("fill"):
            key, value = multi_split(line_lower, '=', ':')
            block_data[key.strip()] = str(value.strip())

def cylind_read(block_type, line, block_data):
    line_lower = line.lower()
    if block_type == "CYLINDER":
        if line_lower.startswith("start"):
            key, value = multi_split(line_lower, '=', ':')
            v1, v2, v3 = value.split()
            block_data[key.strip()] = [float(v1), float(v2), float(v3)]
        if line_lower.startswith("dir"):
            key, value = multi_split(line_lower, '=', ':')
            v1, v2, v3 = value.split()
            v1, v2, v3 = float(v1), float(v2), float(v3)
            v_mag=math.sqrt(v1**2+v2**2+v3**2)
            v1, v2, v3 =v1/v_mag, v2/v_mag, v3/v_mag
            block_data[key.strip()] = [v1, v2, v3]
        if line_lower.startswith("radius"):
            key, value = multi_split(line_lower, '=', ':')
            block_data[key.strip()] = float(value.strip())
        if line_lower.startswith("length"):
            key, value = multi_split(line_lower, '=', ':')
            block_data[key.strip()] = float(value.strip())
        if line_lower.startswith("fill"):
            key, value = multi_split(line_lower, '=', ':')
            block_data[key.strip()] = str(value.strip())

def get_blocks(filename):
    blocks = {}
    cur_block = None
    with open(filename, 'r') as file:
        for full_line in file:
            line = full_line.strip()
            if not line or line.startswith('!'):
                continue
            if not cur_block:
                cur_block = line
                blocks.setdefault(cur_block, []).append({})
                cur_block_data = blocks[cur_block][-1]
            elif line == "END":
                cur_block = None
            else:
                volume_read(cur_block, line, cur_block_data)
                cuboid_read(cur_block, line, cur_block_data)
                sphere_read(cur_block, line, cur_block_data)
                cylind_read(cur_block, line, cur_block_data)
    return blocks

###############################################################################
###############################################################################
###############################################################################
###############################################################################

if len(sys.argv) != 2:
    print("Usage: visualize.py <filename>")
    sys.exit(1)

filename = sys.argv[1]
data = get_blocks(filename)


fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')


ax.set_autoscalex_on(False)
ax.set_xlim(0, data["VOLUME"][0]["x"])
ax.set_autoscaley_on(False)
ax.set_ylim(0, data["VOLUME"][0]["y"])
ax.set_autoscalez_on(False)
ax.set_zlim(0, data["VOLUME"][0]["z"])

# Visualizing the CUBOID
if 'CUBOID' in data:
    for cuboid in data['CUBOID']:
        o = np.array(cuboid['origin'])
        d = np.array(cuboid['dimensions'])
        
        # Define the vertices of the cuboid
        vertices = [o + np.array([dx, dy, dz]) for dx in [0, d[0]] for dy in [0, d[1]] for dz in [0, d[2]]]
        print(vertices)
        # Define the vertices that compose each of the 6 faces of the cuboid
        faces = [                                #          5________7
            [vertices[i] for i in [0, 1, 3, 2]], #front    /|       /|
            [vertices[i] for i in [4, 5, 7, 6]], #back    1________3 |
            [vertices[i] for i in [0, 2, 6, 4]], #bottom  | |      | |
            [vertices[i] for i in [1, 3, 7, 5]], #top     | |      | |
            [vertices[i] for i in [0, 1, 5, 4]], #left    | 4______|_6
            [vertices[i] for i in [2, 3, 7, 6]]  #right   |/       |/
        ]                                        #        0________2

        # Create a Poly3DCollection object for the faces
        cuboid_faces = Poly3DCollection(faces, edgecolors='k', linewidths=1, alpha=0.1)
        cuboid_faces.set_facecolor((0, 1, 0, 0.1))  # RGBA for transparent green

        # Add the faces to the axes
        ax.add_collection3d(cuboid_faces)

# Visualizing the SPHERE
if 'SPHERE' in data:
    for sphere in data['SPHERE']:
        u = np.linspace(0, 2 * np.pi, 100)
        v = np.linspace(0, np.pi, 100)
        x = sphere['center'][0] + sphere['radius'] * np.outer(np.cos(u), np.sin(v))
        y = sphere['center'][1] + sphere['radius'] * np.outer(np.sin(u), np.sin(v))
        z = sphere['center'][2] + sphere['radius'] * np.outer(np.ones(np.size(u)), np.cos(v))
        ax.plot_surface(x, y, z, color='y')

# Visualizing the CYLINDER (approximation)
if 'CYLINDER' in data:
    for cylinder in data['CYLINDER']:
        u, v = np.mgrid[0:2*np.pi:30j, 0:cylinder['length']:30j]  # parametric coordinates
        direction = np.array(cylinder['direction'])
        
        # The cylinder's basis vectors (where it points)
        l = direction * cylinder['length']
        r = np.cross([0.123, 0.456, 1.789], direction)
        r = r / np.linalg.norm(r) * cylinder['radius']
        s = np.cross(l, r)
        s = s / np.linalg.norm(s) * cylinder['radius']
        
        # Parametric equation for the cylinder with the given direction
        X = (r[0]*np.cos(u) + s[0]*np.sin(u)) + cylinder['start'][0] + l[0]*v/cylinder['length']
        Y = (r[1]*np.cos(u) + s[1]*np.sin(u)) + cylinder['start'][1] + l[1]*v/cylinder['length']
        Z = (r[2]*np.cos(u) + s[2]*np.sin(u)) + cylinder['start'][2] + l[2]*v/cylinder['length']
        
        ax.plot_surface(X, Y, Z, color='b')


plt.show()

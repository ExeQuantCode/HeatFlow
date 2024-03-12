#!/home/fhd205/opt/anaconda3/envs/py3.11/bin/python
import sys
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
import numpy as np
import math

#--------------------------------------------------------------------------#
class Volume:
    def __init__(self):
        self.x = None
        self.y = None
        self.z = None
        self.x_grid = None
        self.y_grid = None
        self.z_grid = None
        self.units = None
        self.dim = None
        self.default_material = None

    def __str__(self):
        return f"Volume(x={self.x}, y={self.y}, z={self.z}, x_grid={self.x_grid}, y_grid={self.y_grid}, z_grid={self.z_grid}, units={self.units}, dim={self.dim}, default_material={self.default_material})"

    def read(self, file_path):
        with open(file_path, 'r') as file:
            for line in file:
                line = line.strip()
                if line.startswith('!') or line.startswith('#') or line == '':
                    continue
                if line == 'END':
                    break
                
                key, value = self.parse_line(line)
                if key in ['X', 'Y', 'Z', 'UNITS', 'DIM']:
                    setattr(self, key.lower(), float(value))
                elif key == 'FILL':
                    self.default_material = int(value)
                elif key == 'GRID':
                    self.x_grid, self.y_grid, self.z_grid = map(int, value.split())

    @staticmethod
    def parse_line(line):
        line = line.replace(':', '=', 1)
        parts = line.split('=')
        if len(parts) == 2:
            return parts[0].strip().upper(), parts[1].strip()
        return None, None
#--------------------------------------------------------------------------#

class Cuboid:
    def __init__(self):
        self.origin = [None, None, None]
        self.dimensions = [None, None, None]
        self.material = None

    def __str__(self):
        return f"Cuboid(origin={self.origin}, dimensions={self.dimensions}, material={self.material})"

    @staticmethod
    def parse_line(line):
        line = line.replace(':', '=', 1)
        key, _, value = line.partition('=')
        return key.strip().upper(), value.strip()

    def set_attribute(self, key, value):
        if key == 'ORIGIN':
            self.origin = list(map(float, value.split()))
        elif key == 'DIM':
            self.dimensions = list(map(float, value.split()))
        elif key == 'FILL':
            self.material = int(value)
#--------------------------------------------------------------------------#

class Sphere:
    def __init__(self):
        self.center = [None, None, None]
        self.radius = None
        self.material = None
    
    def __str__(self):
        return f"Sphere(center={self.center}, radius={self.radius}, material={self.material})"
    
    @staticmethod
    def parse_line(line):
        line = line.replace(':', '=', 1)
        key, _, value = line.partition('=')
        return key.strip().upper(), value.strip()

    def set_attribute(self, key, value):
        if key == 'CENTER':
            self.center = list(map(float, value.split()))
        elif key == 'RADIUS':
            self.radius = float(value)
        elif key == 'FILL':
            self.material = int(value)
#--------------------------------------------------------------------------#

class Cylinder:
    def __init__(self):
        self.start = [None, None, None]
        self.radius = None
        self.length = None
        self.direction = [None, None, None]
        self.material = None

    def __str__(self):
        return f"Cylinder(start={self.start}, radius={self.radius}, length={self.length}, direction={self.direction}, material={self.material})"

    @staticmethod
    def parse_line(line):
        line = line.replace(':', '=', 1)
        key, _, value = line.partition('=')
        return key.strip().upper(), value.strip()

    def set_attribute(self, key, value):
        if key == 'START':
            self.start = list(map(float, value.split()))
        elif key == 'RADIUS':
            self.radius = float(value)
        elif key == 'LENGTH':
            self.length = float(value)
        elif key == 'DIRECTION':
            self.direction = list(map(float, value.split()))
        elif key == 'FILL':
            self.material = int(value)
#--------------------------------------------------------------------------#

class Blocks:
    def __init__(self):
        self.volume = Volume()
        self.cuboids = []
        self.spheres = []  
        self.cylinders = []

    def read(self, file_path):
        self.volume.read(file_path)
        with open(file_path, 'r') as file:
            current_shape = None
            for line in file:
                line = line.strip()
                if line in ['CUBOID', 'SPHERE', 'CYLINDER']:
                    if current_shape is not None:
                        self.add_shape(current_shape)
                    current_shape = self.create_shape(line)
                elif line == 'END' and current_shape is not None:
                    self.add_shape(current_shape)
                    current_shape = None
                elif current_shape is not None:
                    key, value = current_shape.parse_line(line)
                    current_shape.set_attribute(key, value)

    def create_shape(self, shape_type):
        if shape_type == 'CUBOID':
            return Cuboid()
        elif shape_type == 'SPHERE':
            return Sphere()
        elif shape_type == 'CYLINDER':
            return Cylinder()

    def add_shape(self, shape):
        if isinstance(shape, Cuboid):
            self.cuboids.append(shape)
        elif isinstance(shape, Sphere):
            self.spheres.append(shape)
        elif isinstance(shape, Cylinder):
            self.cylinders.append(shape)
    def print_all_values(self):
        print("Volume:", str(self.volume))
        print("Cuboids:")
        for cuboid in self.cuboids:
            print(str(cuboid))
        print("Spheres:")
        for sphere in self.spheres:
            print(str(sphere))
        print("Cylinders:")
        for cylinder in self.cylinders:
            print(str(cylinder))
    
    def plot_shapes(self):
        fig = plt.figure()
        ax = fig.add_subplot(111, projection='3d')
        
        # Set plot limits according to the volume
        ax.set_xlim(0, self.volume.x)
        ax.set_ylim(0, self.volume.y)
        ax.set_zlim(0, self.volume.z)
        
        # Plot Cuboids
        for cuboid in self.cuboids:
            o = np.array(cuboid.origin)
            d = np.array(cuboid.dimensions)
            vertices = [o + np.array([dx, dy, dz]) for dx in [0, d[0]] for dy in [0, d[1]] for dz in [0, d[2]]]
            faces = [
                [vertices[i] for i in [0, 1, 3, 2]],
                [vertices[i] for i in [4, 5, 7, 6]],
                [vertices[i] for i in [0, 2, 6, 4]],
                [vertices[i] for i in [1, 3, 7, 5]],
                [vertices[i] for i in [0, 1, 5, 4]],
                [vertices[i] for i in [2, 3, 7, 6]]
            ]
            cuboid_faces = Poly3DCollection(faces, edgecolors='k', linewidths=1, alpha=0.1)
            cuboid_faces.set_facecolor((0, 1, 0, 0.1))
            ax.add_collection3d(cuboid_faces)
            
        # Plot Spheres
        for sphere in self.spheres:
            u, v = np.mgrid[0:2*np.pi:100j, 0:np.pi:50j]
            x = sphere.center[0] + sphere.radius * np.outer(np.cos(u), np.sin(v))
            y = sphere.center[1] + sphere.radius * np.outer(np.sin(u), np.sin(v))
            z = sphere.center[2] + sphere.radius * np.outer(np.ones(np.size(u)), np.cos(v))
            ax.plot_surface(x, y, z, color='y')
            
        # Plot Cylinders
        for cylinder in self.cylinders:
            u, v = np.mgrid[0:2*np.pi:30j, 0:cylinder.length:30j]
            direction = np.array(cylinder.direction)
            l = direction * cylinder.length
            r = np.cross([0.123, 0.456, 1.789], direction)
            r = r / np.linalg.norm(r) * cylinder.radius
            s = np.cross(l, r)
            s = s / np.linalg.norm(s) * cylinder.radius
            X = (r[0]*np.cos(u) + s[0]*np.sin(u)) + cylinder.start[0] + l[0]*v/cylinder.length
            Y = (r[1]*np.cos(u) + s[1]*np.sin(u)) + cylinder.start[1] + l[1]*v/cylinder.length
            Z = (r[2]*np.cos(u) + s[2]*np.sin(u)) + cylinder.start[2] + l[2]*v/cylinder.length
            ax.plot_surface(X, Y, Z, color='b')
            
        plt.show()
#--------------------------------------------------------------------------#


if len(sys.argv) < 2:
    print("Usage: python_script.py <inputfile>")
    sys.exit(1)

infile = sys.argv[1]
print(infile)
blocks = Blocks()
blocks.read(infile)
blocks.print_all_values()
blocks.plot_shapes()

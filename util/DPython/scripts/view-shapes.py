#!/home/fhd205/opt/anaconda3/envs/py3.11/bin/python

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
#--------------------------------------------------------------------------#



blocks = Blocks()
blocks.read('Pillers.in')
blocks.print_all_values()


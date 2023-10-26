#!/home/links/fhd205/.conda/envs/py3.7.6/bin/python
import sys
import math
import numpy as np
import struct


###############################################################################
# read the input file and assemble the data into a dict structure
###############################################################################
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

vert = False
face = False
fi = 0
def struct_read(block_type, line, block_data):
    global vert, face, vi, fi
    if block_type == "STRUCTURE":
        #print('Here')
        line_lower = line.lower()
        if line_lower.startswith("fill"):
            key, value = multi_split(line_lower, '=', ':')
            block_data[key.strip()] = str(value.strip())
        elif line == "VERTICES":
            block_data["VERTICES"] = []
            vert = True
            face = False
        elif line == "FACES":
            block_data["FACES"] = []
            face = True
            vert = False
        else:
            if vert:
                vertex_data = {}
                key, value = multi_split(line_lower, '=', ':')
                v1, v2, v3 = value.split()
                vertex_data[key.strip()] = [float(v1), float(v2), float(v3)]
                block_data["VERTICES"].append(vertex_data)
            if face:
                key, value = multi_split(line_lower, '=', ':')
                vertices = value.split()
                for i in range(1, len(vertices) - 1):
                    face_data = {}
                    fi=fi+1
                    face_data[str(fi)] = [int(vertices[0]), int(vertices[i]), int(vertices[i+1])]
                    block_data["FACES"].append(face_data)
###############################################################################


###############################################################################
# Call to create the data structure
###############################################################################
def read_infile(filename):
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
                struct_read(cur_block, line, cur_block_data)
    return blocks


###############################################################################


###############################################################################
# For feedback this writes out the inout file again with apropreate alterations
###############################################################################
def write_data(blocks):
    for block_name, block_contents in blocks.items():
        for block_content in block_contents:
            print(block_name)
            for key, value in block_content.items():
                print("       {} = {}".format(key, value))
            print("END")
    print("---------")

###############################################################################


###############################################################################
# code to determin if shapes defined by the STRUCTURE block are sealed surfaces
###############################################################################
def get_edge(vertex1, vertex2):
    #Return a tuple representing an edge, ensuring a consistent ordering.
    return tuple(sorted([vertex1, vertex2]))

def is_closed_surface(structure):
    # each edge can only be met by 2 faces
    edge_count = {}
    # Count edges for all faces
    for face in structure['FACES']:
        vertices = list(face.values())[0]
        edge = get_edge(vertices[0], vertices[1])
        edge_count[edge] = edge_count.get(edge, 0) + 1
        edge = get_edge(vertices[0], vertices[2])
        edge_count[edge] = edge_count.get(edge, 0) + 1
        edge = get_edge(vertices[1], vertices[2])
        edge_count[edge] = edge_count.get(edge, 0) + 1
    # Check if every edge is used exactly twice
    for count in edge_count.values():
        if count != 2:
            return False
    return True

###############################################################################


###############################################################################
# functions to determin if a coordinate falls with in any defined volume
###############################################################################
def plane_intersection(ray_origin, triangle_vertices):
    # writing this was absolute hell
    # it's now working but i don't know what is different ...
    # ... that makes this work and preveous ones not work!
    ray_vector = np.array([1.0, 0.0, 0.0])
    EPSILON = 1e-7
    vertex_A, vertex_B, vertex_C = triangle_vertices
    
    edge_AB = vertex_B - vertex_A
    edge_AC = vertex_C - vertex_A
    orthogonal_vector = np.cross(ray_vector, edge_AC)
    dot_product = np.dot(edge_AB, orthogonal_vector)
    
    if -EPSILON < dot_product < EPSILON:
        return False, None  # This ray is parallel to this triangle.
    
    inverse_dot = 1.0 / dot_product
    s_vector = ray_origin - vertex_A
    param_u = inverse_dot * np.dot(s_vector, orthogonal_vector)
    
    if param_u < 0.0 or param_u > 1.0:
        return False, None
    
    q_vector = np.cross(s_vector, edge_AB)
    param_v = inverse_dot * np.dot(ray_vector, q_vector)
    
    if param_v < 0.0 or param_u + param_v > 1.0:
        return False, None
    
    # Compute t to find the intersection point on the line.
    t_param = inverse_dot * np.dot(edge_AC, q_vector)
    
    if t_param > EPSILON:  # ray intersection
        intersection_point = ray_origin + ray_vector * t_param
        return True, intersection_point
    else:  # Line intersection but not a ray intersection.
        return False, None


def point_in_poly(origin,struct):
    intersection_count = 0
    for face_entry in struct['FACES']:
        face = list(face_entry.values())[0]
        vertices = [np.array(list(struct['VERTICES'][v-1].values())[0]) for v in face]
        intersects, intersection_location = plane_intersection(origin, vertices)
        if intersects:
            intersection_count += 1
    return intersection_count


def point_in_structures(point,Structure):
    origin = np.array(point["coord"])
    material = point["fill"]
    count=0
    for struct in Structure:
        count=point_in_poly(origin,struct)
        if count % 2 == 0:
            if verbose:
                print("Point is outside the structure.",point["fill"])
        else:
            point["fill"]=struct["fill"]
            if verbose:
                print("Point is inside the structure.",point["fill"])


def point_in_cube(cord,struct):
    origin=np.array(struct["origin"])
    dim=np.array(struct["dimensions"])
    if origin[0] <= cord[0] <= origin[0]+dim[0] and\
       origin[1] <= cord[1] <= origin[1]+dim[1] and\
       origin[2] <= cord[2] <= origin[2]+dim[2]: 
        return True
    else:
        return False


def point_in_cuboids(point,Structure):
    origin = np.array(point["coord"])
    material = point["fill"]
    count=0
    for struct in Structure:
        found=point_in_cube(origin,struct)
        if found:
            point["fill"]=struct["fill"]
            if verbose:
                print("Point is inside the cuboid.",point["fill"])
        else:
            if verbose:
                print("Point is outside the cuboid.",point["fill"])


def point_in_sphere(cord,struct):
    center=np.array(struct["center"])
    rad=np.array(struct["radius"])
    dist=math.sqrt( (cord[0]-center[0])**2+(cord[1]-center[1])**2+(cord[2]-center[2])**2 )
    if dist <= rad:
        return True
    else:
        return False


def point_in_spheres(point,Structure):
    origin = np.array(point["coord"])
    material = point["fill"]
    count=0
    for struct in Structure:
        found=point_in_sphere(origin,struct)
        if found:
            point["fill"]=struct["fill"]
            if verbose:
                print("Point is inside the sphere.",point["fill"])
        else:
            if verbose:
                print("Point is outside the sphere.",point["fill"])


def point_in_cylinder(cord,struct):
    start=np.array(struct["start"])
    radius=np.array(struct["radius"])
    length=np.array(struct["length"])
    direction=np.array(struct["direction"])

    h = cord-start
    dist_to_line = abs(np.dot(direction,h))
    dist_on_line = np.dot(np.cross(direction,np.cross(direction,h)),direction)
    if 0 <= dist_on_line <= length and dist_to_line <= radius:
        return True
    else:
        return False


def point_in_cylinders(point,Structure):
    global verbose
    origin = np.array(point["coord"])
    material = point["fill"]
    count=0
    for struct in Structure:
        found=point_in_cylinder(origin,struct)
        if found:
            point["fill"]=struct["fill"]
            if verbose:
                print("Point is inside the cylinder.",point["fill"])
        else:
            if verbose:
                print("Point is outside the cylinder.",point["fill"])


def set_points_material(point,data_structure):
    struct=data_structure['STRUCTURE']
    point_in_structures(point,struct)
    
    cube=data_structure['CUBOID']
    point_in_cuboids(point,cube)
    
    sphere=data_structure['SPHERE']
    point_in_spheres(point,sphere)
    
    cylinder=data_structure['CYLINDER']
    point_in_cylinders(point,cylinder)

###############################################################################


###############################################################################
# make a gird of points out of the data from VOLUME
###############################################################################
def create_grid(X, Y, Z, grid__dims):
    # Determine the step size for each dimension
    dx = X / (grid_dims[0])
    dy = Y / (grid_dims[1])
    dz = Z / (grid_dims[2])

    grid_points = []

    for i in range(grid_dims[0]):
        x_coord = (0.5+i)*dx
        layer = []

        for j in range(grid_dims[1]):
            y_coord = (0.5+j)*dy
            row = []

            for k in range(grid_dims[2]):
                z_coord = (0.5+k)*dz
                point = {
                    "coord": [x_coord, y_coord, z_coord],
                    "fill":  blocks["VOLUME"][0]["fill"]
                }
                row.append(point)

            layer.append(row)
        grid_points.append(layer)

    return grid_points

###############################################################################


###############################################################################
###############################################################################
###############################################################################

###############################################################################
# this is the main of the code
###############################################################################
if len(sys.argv) != 2:
    print("Usage: geom-grid.py <filename>")
    sys.exit(1)


# set the verbosity flag
verbose=False


# read the input file 
filename = sys.argv[1]
blocks = read_infile(filename)


# uncoment line to output the input (dev test)
write_data(blocks);print("")


# checks if defined structures form a closed surface
for c, structure in enumerate(blocks['STRUCTURE']):
    if not is_closed_surface(structure):
        print("Error: Structure {} is not closed!".format(c+1))
        exit(1)
print("All structures are closed!")


# ceate the point mesh. each coordinate is at the center of it's respective cell
X = blocks["VOLUME"][0]["x"]
Y = blocks["VOLUME"][0]["y"]
Z = blocks["VOLUME"][0]["z"]
grid_dims = blocks["VOLUME"][0]["grid"]
point_mesh = create_grid(X, Y, Z, grid_dims)


# assine a material to each cell/point
i=int(0)
j=int(0)
k=int(0)
for i in range(grid_dims[0]):
    for j in range(grid_dims[1]):
        for k in range(grid_dims[2]):
            point=point_mesh[k][j][i]
            set_points_material(point,blocks)
            coordinate = point_mesh[k][j][i]['coord']
            fill = point_mesh[k][j][i]['fill']


with open("GridMaterial.dat", "w") as f:
    i=int(0)
    j=int(0)
    k=int(0)
    for i in range(grid_dims[0]):
        for j in range(grid_dims[1]):
            line_values = []
            for k in range(grid_dims[2]):
                coordinate = point_mesh[k][j][i]['coord']
                fill = int(point_mesh[k][j][i]['fill'])
                line_values.append(str(fill))
            f.write(" ".join(line_values) + "\n")
        f.write("\n")

with open("GridMaterial.bin", "wb") as f:
    i=int(0)
    j=int(0)
    k=int(0)
    for i in range(grid_dims[0]):
        for j in range(grid_dims[1]):
            line_values = []
            for k in range(grid_dims[2]):
                coordinate = point_mesh[k][j][i]['coord']
                fill = int(point_mesh[k][j][i]['fill'])
                line_values.append(str(fill))
                f.write(struct.pack('i', fill))



# print out an input file in a readable format for HEATFOLW


#!/home/fhd205/opt/anaconda3/envs/py3.11/bin/python
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
        flags = ["x", "y", "z", "d","units"]
        if any(line_lower.startswith(flag) for flag in flags):
            key, value = multi_split(line_lower, '=', ':')
            block_data[key.strip()] = float(value.strip())
        if line_lower.startswith("grid"):
            key, value = multi_split(line_lower, '=', ':')
            v1, v2, v3 = value.split()
            v1, v2, v3 = int(v1), int(v2), int(v3)
            block_data[key.strip()] = [v1, v2, v3]
        flags = ["f"]
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
    direction = direction / np.linalg.norm(direction)

    h = cord-start
    dist_on_line = np.dot(direction,h)
    dist_to_line = np.sqrt(np.dot(h,h)-np.dot(direction,h)**2)
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
    if 'CUBOID' in data_structure:
        cube=data_structure['CUBOID']
        point_in_cuboids(point,cube)
    
    if 'SPHERE' in data_structure:
        sphere=data_structure['SPHERE']
        point_in_spheres(point,sphere)
    
    if 'CYLINDER' in data_structure:
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
    
    #print("dx ="+str(X)+"/"+str(grid_dims[0])+" = "+str(dx))

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
                #print(point["coord"])
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
print(blocks)


# uncoment line to output the input (dev test)
write_data(blocks);print("")


# ceate the point mesh. each coordinate is at the center of it's respective cell
X = blocks["VOLUME"][0]["x"]
Y = blocks["VOLUME"][0]["y"]
Z = blocks["VOLUME"][0]["z"]
grid_dims = blocks["VOLUME"][0]["grid"]
point_mesh = create_grid(X, Y, Z, grid_dims)
print("mesh built");print("")

# assine a material to each cell/point
i=int(0)
j=int(0)
k=int(0)
for i in range(grid_dims[0]):
    for j in range(grid_dims[1]):
        for k in range(grid_dims[2]):
            point=point_mesh[i][j][k] 
            set_points_material(point,blocks) 
            coordinate = point['coord']
            fill = point['fill'] 


print(" Writing to: geom.in")
with open("geom.in", "w") as f:
    s=blocks["VOLUME"][0]["units"]
    x=str(blocks["VOLUME"][0]["x"]*s)
    y=str(blocks["VOLUME"][0]["y"]*s)
    z=str(blocks["VOLUME"][0]["z"]*s)
    i=str(grid_dims[0])
    j=str(grid_dims[1])
    k=str(grid_dims[2])
    f.write(i+' '+j+' '+k+'\n')
    f.write(x+' '+y+' '+z+'\n\n')
    i=int(0)
    j=int(0)
    k=int(0)
    for k in range(grid_dims[2]):
        for j in range(grid_dims[1]):
            line_values = []
            for i in range(grid_dims[0]):
                #coordinate = point_mesh[i][j][k]['coord']
                fill = int(point_mesh[i][j][k]['fill'])
                line_values.append(str(fill))
            f.write(" ".join(line_values) + "\n")
        f.write("\n")


# print out an input file in a readable format for HEATFOLW


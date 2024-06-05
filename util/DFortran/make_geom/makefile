# Makefile for a Fortran project using gfortran

# Directories
SRC_DIR = src
BIN_DIR = bin
OBJ_DIR = obj
MOD_DIR = mod  

# Manually specify the order of the source files
SOURCES = $(SRC_DIR)/shapes.f90 \
          $(SRC_DIR)/shape_checking.f90 \
          $(SRC_DIR)/read.f90 \
          $(SRC_DIR)/mkgeom.f90

# Convert source file names to object file names
OBJECTS = $(patsubst $(SRC_DIR)/%.f90, $(OBJ_DIR)/%.o, $(SOURCES))

# Compiler
FC = gfortran
# Compiler flags
FFLAGS = -O2 -fcheck=all -J$(MOD_DIR) 

# Name of the executable
EXECUTABLE = $(BIN_DIR)/mkgeom.x

# Default rule
all: $(BIN_DIR) $(OBJ_DIR) $(MOD_DIR) $(EXECUTABLE)

# Rule for creating directories
$(BIN_DIR) $(OBJ_DIR) $(MOD_DIR):
	mkdir -p $@

# Rule for making the executable
$(EXECUTABLE): $(OBJECTS)
	$(FC) $(FFLAGS) -o $@ $^

# Rule for making the object files
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Clean rule
clean:
	rm -rf $(BIN_DIR) $(OBJ_DIR) $(MOD_DIR)

.PHONY: all clean

####################################################################
#  LAPACK make include file.                                       #
#  LAPACK, Version 3.0                                             #
#  June 30, 1999                                                  #
####################################################################
#
SHELL = /bin/sh
#
#  The machine (platform) identifier to append to the library names
#
PLAT = _linux
#
#  Modify the FORTRAN and OPTS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader and
#  desired load options for your machine.
#
FORTRAN  = g77
OPTS     = -funroll-all-loo ps -fno-f2c -O3
DRVOPTS  = $(OPTS)
NOOPT    =
LOADER   = g77
LOADOPTS = $(OPTS)
#
#  The archiver and the flag(s) to use when building archive (library)
#  If you system has no ranlib, set RANLIB = echo.
#
ARCH     = ar
ARCHFLAGS= cr
RANLIB   = ranlib
#
#  The location of the libraries to which you will link.  (The
#  machine-specific, optimized BLAS library should be used whenever
#  possible.)
#
BLASLIB      =  /home/links/hm556/.local/lapack/libblas.a
LAPACKLIB    =  /home/links/hm556/.local/lapack/liblapack.a
#BLASLIB      =  $(MKLROOT)/lib/intel64_lin/libmkl_blas95_ilp64.a   #lapack-3.11.0/librefblas.a
#LAPACKLIB    =  $(MKLROOT)/lib/intel64_lin/libmkl_lapack95_ilp64.a
# TMGLIB       = tmglib$(PLAT).a
# EIGSRCLIB    = eigsrc$(PLAT).a
# LINSRCLIB    = linsrc$(PLAT).a

##########################################
# CODE DIRECTORIES AND FILES
##########################################
mkfile_path := $(abspath $(firstword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))
BIN_DIR := ./bin
SRC_DIR := ./src
LIB_DIR := ./lib
BUILD_DIR = ./obj
# LIBS := mod_constants.f90 \
# 	mod_misc.f90 \
# 	mod_misc_linalg.f90 \
# 	mod_rw_geom.f90 \
# 	mod_edit_kpoints.f90 \
# 	mod_rw_dat.f90
#OBJS := $(addprefix $(LIB_DIR)/,$(LIBS))
SRCS := constants.f90 \
		ReadTxt.f90 \
		inputs.f90 \
        constructions.f90 \
	heating.f90 \
	setup.f90 \
        material.f90 \
        delta_ave.f90 \
	matinv.f90 \
        matrix4.f90 \
        output.f90 \
        main.f90
OBJS := $(addprefix $(SRC_DIR)/,$(SRCS))


FFLAGS = -O2 
MODULEFLAGS = -J
FC = gfortran
# LIB= libchesspexsi.a #-l?

##########################################
# LIBRARY SECTION
##########################################
MKLROOT?="/usr/local/intel/parallel_studio_xe_2017/compilers_and_libraries_2017/linux/mkl/lib/intel64_lin"
LLAPACK = $(MKLROOT)/lib/intel64_lin/libmkl_lapack95_lp64.a \
	-Wl,--start-group \
	$(MKLROOT)/lib/intel64_lin/libmkl_intel_lp64.a \
	$(MKLROOT)/lib/intel64_lin/libmkl_sequential.a \
	$(MKLROOT)/lib/intel64_lin/libmkl_core.a \
	-Wl,--end-group \
	-lpthread

#$(MKLROOT)/libmkl_scalapack_lp64.a \
#$(MKLROOT)/libmkl_solver_lp64_sequential.a \

NAME = ThermalFLow
programs = $(BIN_DIR)/$(NAME)
all: $(programs)

$(BIN_DIR):
	mkdir -p $@

$(BUILD_DIR):
	mkdir -p $@

$(programs) : $(OBJS) | $(BIN_DIR) $(BUILD_DIR)
	$(FC) -O3 $(MODULEFLAGS) $(BUILD_DIR) $(OBJS) $(LAPACKLIB) $(BLASLIB) -o $@

debug :  $(OBJS)
	$(FC) -O3 -mcmodel=large -fbacktrace -fcheck=all -fbounds-check $(MODULEFLAGS) $(BUILD_DIR) $(LAPACKLIB) $(BLASLIB) -o $(programs)

[![License workflow](https://img.shields.io/badge/License-GPLv3-yellow.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html "View GPLv3 license")
[![Latest Release](https://img.shields.io/github/v/release/nedtaylor/RAFFLE?sort=semver)](https://github.com/ExeQuantCode/HeatFlow/releases "View on GitHub")
[![GCC compatibility](https://img.shields.io/badge/gcc-14.1.0-green)](https://gcc.gnu.org/gcc-14/ "View GCC")


# HeatFlow

by Harry Mclean, Francis Huw Davies, Ned Thaddeus Taylor, and Steven Paul Hepplestone

HeatFlow is Fortran-based a software package for modelling dynamical heat transport in systems using finite difference methods.
The software is primarily designed to utilise the Cattaneo method.
However, the Fourier method can be used instead.

## Requirements

- Fortran compiler supporting Fortran 2003 standard or later

The library bas been developed and tested using the following Fortran compilers:
- gfortran -- gcc 13.2.0
- gfortran -- gcc 14.1.0

## Installation

To install HeatFlow, the source must be obtained from the git repository. Use the following commands to get started:
```
 git clone https://github.com/ExeQuantCode/HeatFlow.git
 cd raffle
```

Then, within the repository, run the following command:

```
make install
```

This will build the library and install the executable in the following directory:
```
bin/ThermalFlow.x
```

This executable can now be called to run the HeatFlow software package and simulate heat transport.
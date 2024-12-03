!!!#################################################################################################
!!! Contains all the constants used in the code
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module constants
  implicit none
  !!!Specifies accuracy of variables (Can be reduced to save memory)
  integer, parameter :: real12 = Selected_real_kind(12,200) !(12,200)
  integer, parameter :: int12  = Selected_int_kind(12)
  !!!Defines necessary constants for the heat flow process
  real(real12), parameter :: k_b = 1.3806503e-23_real12 !Boltzmann constant
  real(real12), parameter :: pi = 3.141592653589793_real12 !Pi
  real(real12), parameter :: hbar = 1.05457148e-34_real12 !Reduced Planck constant
  real(real12), parameter :: atomic_mass=1.67262158e-27_real12 !Atomic mass of hydrogen
  real(real12), parameter :: avogadros=6.022e23_real12 !Avogadro's number
  real(real12), parameter :: StefBoltz = 5.670373e-8_real12 !Stefan-Boltzmann constant
  real(real12), parameter :: TINY=1.0e-12_real12 !Tiny number to avoid division by zero
  integer(int12) , parameter :: fields=6_int12 !Number of fields in the system
end module constants

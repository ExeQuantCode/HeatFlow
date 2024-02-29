module constants
  implicit none
  !!!Specifies accuracy of variables (Can be reduced to save memory)
  integer, parameter :: real12 = Selected_real_kind(12,200) !(12,200)
  integer, parameter :: int12  = Selected_int_kind(12)
  !!!Defines necessary constants for the heat flow process
  real(real12), parameter :: k_b = 1.3806503e-23_real12
  real(real12), parameter :: pi = 3.141592653589793_real12
  real(real12), parameter :: hbar = 1.05457148e-34_real12
  !real, parameter :: h = 6.626068e-34
  real(real12), parameter :: atomic_mass=1.67262158e-27_real12
  real(real12), parameter :: avogadros=6.022e23_real12
  
  real(real12), parameter :: TINY=1.0e-9_real12
  integer(int12) , parameter :: fields=6_int12
end module constants

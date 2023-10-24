MODULE Constants
  implicit none
  !!!Defines necessary constants for the heat flow process
  real, parameter :: k_b = 1.3806503e-23
  real, parameter :: pi = 3.14159265
  real, parameter :: hbar = 1.05457148e-34
  real, parameter :: h = 6.626068e-34
  real, parameter :: atomic_mass=1.67262158e-27
  real, parameter :: avogadros=6.022e23
  !!!Specifies accuracy of variables (Can be reduced to save memory)
  integer, parameter :: real12 = Selected_real_kind(12,200) !(12,200)
  integer, parameter :: int12  = Selected_int_kind(12)
  real(real12), parameter :: room = 293.0
end MODULE Constants

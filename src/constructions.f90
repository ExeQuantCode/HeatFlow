MODULE CONSTRUCTIONS
!!!These serve as classes or structures in f90
  use constants, only: real12, int12
  !!!Defines the heatblock to be used in the simulation
  type heatblock
     sequence
     integer(int12) :: imaterial_type
     !!real(real12) :: Temperature
     real(real12) :: volume
     real(real12), dimension(3) :: Length
     real(real12), dimension(3) :: area
  end type heatblock
end MODULE CONSTRUCTIONS
  

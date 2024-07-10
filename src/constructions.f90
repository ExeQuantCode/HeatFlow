!!!#################################################################################################
!!!This module contains the type definitions for the heatblock and material types
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
!The heatblock type represents a block of material in a heat simulation. It contains several fields:
!imaterial_type: An integer that represents the type of material the block is made of.
!volume: The volume of the block.
!Length: A 3-element array representing the length of the block in three dimensions.
!kappa, rho, heat_capacity, tau: These are physical properties of the material,...
!... such as thermal conductivity (kappa), density (rho), heat capacity, ...
!...and a time relaxation (tau).
!
!The material type represents a type of material used in the simulation. It contains several fields:
!index: An integer that serves as an identifier for the material.
!heat_capacity, h_conv, kappa, kappa3D, rho, sound_speed, tau: ...
!...These are physical properties of the material.
! h_conv represent convective heat transfer coefficient, ...
!... and kappa3D represent three-dimensional thermal conductivity.
!source: A logical (boolean) value that indicate whether this material is a source of heat.
!####################################################
module constructions
!!These serve as classes or structures in f90
  use constants, only: real12, int12
  !!Defines the heatblock to be used in the simulation
  type heatblock
     integer(int12) :: imaterial_type !what type of material it is in the mat.in
     real(real12) :: volume !volume of the block
     real(real12), dimension(3) :: Length !length of the block in 3 dimensions
     integer(int12) :: iheater !whether the block is a heater
     real(real12) :: kappa, rho, heat_capacity, tau, em !physical properties of the material
  end type heatblock
!!Defines the material to be used in the simulation
  type material
     integer(int12) :: index !identifier for the material
     real(real12) :: heat_capacity !heat capacity of the material
     real(real12) :: h_conv !convective heat transfer coefficient
     real(real12) :: kappa !thermal conductivity
     real(real12) :: kappa3D !three-dimensional thermal conductivity
     real(real12) :: rho !density of the material
     real(real12) :: sound_speed   !speed of sound in the material
     real(real12) :: tau !relaxation time
     real(real12) :: em !emissivity of the material
     logical :: source !whether the material is a source of heat ??
  end type material
end module constructions
  

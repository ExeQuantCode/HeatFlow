!!!#################################################################################################
!!! This is the module for the global variables used in the code
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module globe_data
  use sptype, only: sprs2_dp, diag_sprs_dp
  use constants, only: real12
  implicit none
  real(real12), dimension(:,:,:), allocatable :: Temp_cur ! Current temperature
  real(real12), dimension(:), allocatable :: Temp_p, Temp_pp ! Previous and previous previous temperature
  real(real12), dimension(:), allocatable :: Q_P ! Previous heat flux
  real(real12) :: inverse_time, heat, heated_volume, heated_temp ! Inverse time step, heat input, heated volume
  real(real12), dimension(:), allocatable :: lin_rhoc ! 1D array for HeatCapacity*Rho
  TYPE(sprs2_dp) :: ra !Techniqually rH
  TYPE(diag_sprs_dp) :: da !Techniqually dH
   
end module globe_data
 

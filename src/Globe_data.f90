module globe_data
  use sptype, only: sprs2_dp, diag_sprs_dp
  use constants, only: real12
  implicit none
  real(real12), dimension(:,:,:), allocatable :: TN
  real(real12), dimension(:), allocatable :: TPD, TPPD, heat
  real(real12) :: inverse_time
  real(real12), dimension(:), allocatable :: heatcheck
  real(real12), dimension(:), allocatable :: Grid1DHR ! 1D array for HeatCapacity*Rho
  TYPE(sprs2_dp) :: ra !Techniqually rH
  TYPE(diag_sprs_dp) :: da !Techniqually dH
   
end module globe_data
 

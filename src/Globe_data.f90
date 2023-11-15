module globe_data
  use sptype, only: sprs2_dp, diag_sprs_dp
  use constants, only: real12
  implicit none
  real(real12), dimension(:,:,:), allocatable :: T, TN, Told
  real(real12), dimension(:), allocatable :: TD, TPD, TPPD
  TYPE(sprs2_dp) :: ra !Techniqually rH
  TYPE(diag_sprs_dp) :: da !Techniqually dH
   
end module globe_data
 

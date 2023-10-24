module globe_data
  use sptype
  implicit none
  double precision, dimension(:,:), allocatable :: H
  TYPE(sprs2_dp) :: ra !Techniqually rH
  TYPE(diag_sprs_dp) :: da !Techniqually dH
end module globe_data
 

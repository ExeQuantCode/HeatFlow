module sptype
  use constants, only: int12
  !Symbolic names for kind types of 4, 2, and 1byte integers:
  integer, parameter :: I4B = SELECTED_INT_KIND(9)
  integer, parameter :: I2B = int12 !SELECTED_INT_KIND(4)
  integer, parameter :: I1B = int12 !SELECTED_INT_KIND(2)
  !Symbolic names for kind types of single- and double-precision reals:
  integer, parameter :: SP = KIND(1.0)
  integer, parameter :: DP = KIND(1.0D0)
  !Symbolic names for kind types of single- and double-precision complex:
  integer, parameter :: SPC = KIND((1.0,1.0))
  integer, parameter :: DPC = KIND((1.0D0,1.0D0))
  !Symbolic name for kind type of default logical:
  integer, parameter :: LGT = KIND(.true.)
  TYPE sprs2_dp
     integer(I4B) :: n,len
     real(DP),     dimension(:), pointer :: val
     integer(I4B), dimension(:), pointer :: irow
     integer(I4B), dimension(:), pointer :: jcol
  END TYPE sprs2_dp
  TYPE diag_sprs_dp
     integer(I4B) :: n          ! Size of the matrix (assuming it's square n by n)
     integer(I4B) :: num_diags  ! Number of diagonals stored
     real(DP), dimension(:,:), pointer :: vals  ! Values of the diagonals
     integer(I4B), dimension(:), pointer :: diag_offsets ! Offsets of each diagonal from the main diagonal
     ! element diag_vals(x,1) will be the xth diagonal and will be in the first row
     ! diag_offsets=jcol-irow 
     ! hence for diag_vals(x,y), irow = y & jcol = diag_offsets(x)+y
     ! can be converted for operations
  END TYPE diag_sprs_dp
end module sptype

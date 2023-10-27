MODULE INITIAL
  use constants, only: real12, int12
  use inputs, only: NA, T_Bath
  use globe_data, only: TD,TPD
  implicit none

  
contains
  
  subroutine INIT_EVOLVE(it)
    integer, intent(in) :: iT
    
    !Current version just sets all previous time steps temperatures = heat bath

    if (it.eq.1) then
       TD=T_bath
       TPD=T_bath
    else if (it.eq.2) then
       TPD=T_Bath
    end if
    
    
  end subroutine INIT_EVOLVE
end MODULE INITIAL

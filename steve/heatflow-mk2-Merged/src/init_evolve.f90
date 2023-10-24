MODULE INITIAL
  use constants
  use inputs
  implicit none

  
contains
  
  subroutine INIT_EVOLVE(it,TP,TPP)
    integer, intent(in) :: iT
    real(real12), dimension(NA), intent(out) :: TP, TPP
    
    !Current version just sets all previous time steps temperatures = heat bath

    if (it.eq.1) then
       TP=T_bath
       TPP=T_bath
    else if (it.eq.2) then
       TPP=T_Bath
    end if
    
    
  end subroutine INIT_EVOLVE
end MODULE INITIAL

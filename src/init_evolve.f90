MODULE INITIAL
  use constants, only: real12, int12
  use inputs, only: NA, T_Bath
  use globe_data, only: TD,TPD
  implicit none

  
contains
  
  subroutine initial_evolve()
    
    !Current version just sets all previous time steps temperatures = heat bath

  
    TD=T_bath
    TPD=T_bath
    
    
  end subroutine initial_evolve
end MODULE INITIAL

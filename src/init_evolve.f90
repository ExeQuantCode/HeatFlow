module initial 
  use constants, only: real12, int12
  use inputs, only: NA, T_Bath
  use globe_data, only: TPD,TPPD
  implicit none

  
contains
  
  subroutine initial_evolve()
    
    !Current version just sets all previous time steps temperatures = heat bath

    TPPD= 0
    TPD=T_bath
    
    
  end subroutine initial_evolve
end module initial
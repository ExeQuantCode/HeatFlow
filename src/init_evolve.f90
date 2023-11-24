module initial 
  use constants, only: real12, int12
  use inputs, only: NA, T_Bath
  use globe_data, only: TPD,TPPD
  implicit none

  
contains
  
  subroutine initial_evolve()
    
    !Current version just sets all previous time steps temperatures = heat bath

    TPPD= 400._real12
    TPD= 400._real12
    ! print*,'Initial Temperature = ',TPD
    
  end subroutine initial_evolve
end module initial
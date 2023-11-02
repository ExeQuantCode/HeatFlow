module cattaneo
use inputs, only: NA, time_step
use globe_data, only: TD,TPD
use constants, only: real12, int12

implicit none
contains
subroutine S_catS(s_cat)
    real(real12), dimension(NA), intent(inout) :: S_cat
    real(real12) :: tau, rho, heat_capacity
        !** call material??
    S_cat = (tau/(rho*heat_capacity))*(TPD-2*TD)/(time_step**2) 
end subroutine S_catS

end module cattaneo
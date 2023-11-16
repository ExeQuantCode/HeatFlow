module cattaneo
use inputs, only: NA, time_step, nx, ny, nz, grid
use globe_data, only: TPD,TPPD
use constants, only: real12, int12
implicit none
contains
subroutine S_catS(s_cat)
    real(real12), dimension(NA), intent(inout) :: S_cat
    real(real12) :: heat_capacity,kappa,rho,tau
    integer(int12) :: i, x, y, z
        !** call material??
    i = 1 
    do z = 1, nz
        do y = 1, ny
            do x = 1, nx
                grid(x,y,z)%rho = rho
                grid(x,y,z)%tau = tau
                grid(x,y,z)%heat_capacity = heat_capacity

                S_cat(i) = (tau/(rho*heat_capacity))*(TPPD(i)-2*TPD(i))/(time_step**2) 
                i = i+1
            end do
        end do
    end do
end subroutine S_catS

end module cattaneo
module cattaneo
use inputs, only: NA, time_step, nx, ny, nz, grid
use globe_data, only: TPD,TPPD
use constants, only: real12, int12
use  materials, only: material
implicit none
contains
subroutine S_catS(s_cat)
    real(real12), dimension(NA), intent(inout) :: S_cat
    real(real12) :: heat_capacity,TC,kappa,kappa3D,h_conv,rho,sound_speed,tau
    integer(int12) :: i, ix, iy, iz
        !** call material??
    i = 1 
    do iz = 1, nz
        do iy = 1, ny
            do ix = 1, nx
                call material(grid(ix,iy,iz)%imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
                S_cat(i) = (tau/(rho*heat_capacity))*(TPPD(i)-2*TPD(i))/(time_step**2) 
                i = i+1
            end do
        end do
    end do
end subroutine S_catS

end module cattaneo
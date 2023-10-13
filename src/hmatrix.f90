module heatermatrix
use delta_ave
use materials
use heater
use inputs

contains

subroutine hmatrix(i,j,H)
  integer, intent(in) :: i,j,ix,iy,iz
  real(real12), intent(out) :: H

!Calculates Hmatrix element
!does stuff
!LOOP THROUGH THE CELLS Issues with this its not averaging cells that are different and has issue with infinity
    do ix=1,nx
       do iy=1,ny
          do iz=1,nz
             !Delta ave returns boundary averaged values and temperature gradients                 
             ! It doesn't change anything
             !print*, ix,iy,iz
             CALL DELTA_ave(T,Kap,l_c,H_con,ix,iy,iz,grid)
             call material(grid(ix,iy,iz)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
             call heater(ix,iy,iz,it,grid(ix,iy,iz)%imaterial_type,QQ)
               !write(*,*) 'QQ', QQ
             !delta_ave needs to be converted from 1st order to second order
             ! make matrices for gamma !

             !gammax(ix) = (kappa*time_step*time_step)/(rho*heat_capacity*cellengthx(ix)*cellengthx(ix)*(tau+time_step))

             alpha(ix,iy,iz) = (tau+time_step)/(time_step*time_step)

             tdx = (kap(1,1)+kap(1,2)/2)/(rho*heat_capacity)
             tdy = (kap(2,1)+kap(2,2)/2)/(rho*heat_capacity)
             tdz = (kap(3,1)+kap(3,2)/2)/(rho*heat_capacity)
             
             gammax(ix) = (((kap(1,1)+kap(1,2))/2)*time_step*time_step)/(rho*heat_capacity*cellengthx(ix) &
                  *cellengthx(ix)*(tau+time_step))
             gammay(iy) = (((kap(2,1)+kap(2,2))/2)*time_step*time_step)/(rho*heat_capacity*cellengthy(iy) &
                  *cellengthy(iy)*(tau+time_step))
             gammaz(iz) = (((kap(3,1)+kap(3,2))/2)*time_step*time_step)/(rho*heat_capacity*cellengthz(iz) &
                  *cellengthz(iz)*(tau+time_step))
             !print*, rho*heat_capacity*cellengthz(iz)*cellengthz(iz)*(tau+time_step) 
             !gammaz(iz) = (tdz/(cellengthz(iz)*cellengthz(iz)))/alpha(ix,iy,iz)
             !print*, ix,iy,iz
             !print*, tau
             !print*, time_step
             
             !print*, 'ALPHA =', alpha
             beta(ix,iy,iz) = ((tau)/(time_step*time_step))!/alpha(ix,iy,iz)
             !print*, 'BETA =', beta

             !Change qq to include material properties
             !QQ(ix,iy,iz) = QQ(ix,iy,iz) *(cellengthx(ix)*cellengthy(iy)*cellengthz(iz))!/ &
             !     (sum(cellengthx)*sum(cellengthy)*sum(cellengthz))
             QQ(ix,iy,iz) = QQ(ix,iy,iz)/(rho*heat_capacity)
 
          end do
       end do
    end do




end subroutine hmatrix

end module heatermatrix

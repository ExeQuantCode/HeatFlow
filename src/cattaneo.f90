!!!#################################################################################################
!!! This module contains the subroutine that calculates the Cattaneo term for the S vector.
!!! The subroutines in this module:
!!! - S_catS, This subroutine calculates the Cattaneo term for the S vector.
!!! The variables used in this module are:
!!! - S_cat, This is the Cattaneo term for the S vector.
!!! Author: Harry Mclean, Frank Davis, Steven Hepplestone
!!!#################################################################################################
module cattaneo
use inputs, only: NA, time_step, nx, ny, nz, grid
use globe_data, only: Temp_p,Temp_pp, lin_rhoc
use constants, only: real12, int12, TINY
implicit none
contains
subroutine S_catS(s_cat)
    real(real12), dimension(NA), intent(inout) :: S_cat

    integer(int12) :: i, ix, iy, iz
    i = 0
    S_cat = 0._real12
    do iz=1,nz
        do iy = 1, ny
            do ix = 1, nx
                i = i+1
                S_cat(i) = ( Temp_pp(i) - 2._real12 * Temp_p(i) ) &
                * ( grid(ix,iy,iz)%tau * lin_rhoc(i) )  !

            end do
        end do
    end do

end subroutine S_catS

end module cattaneo
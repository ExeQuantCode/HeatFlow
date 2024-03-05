!!!#################################################################################################
!!! This module contains the subroutine that calculates the Cattaneo term for the S vector.
!!! The subroutines in this module:
!!! - S_catS, This subroutine calculates the Cattaneo term for the S vector.
!!! The variables used in this module are:
!!! - S_cat, This is the Cattaneo term for the S vector.
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module cattaneo
use inputs, only: NA, time_step, nx, ny, nz, grid
use globe_data, only: Temp_p,Temp_pp, lin_rhoc
use constants, only: real12, int12, TINY
implicit none
contains
pure subroutine S_catS(s_cat)
    real(real12), dimension(NA), intent(inout) :: S_cat
    integer(int12) :: index, ix, iy, iz

    index = 0
    do iz=1,nz
        do iy = 1, ny
            do ix = 1, nx
                index = index+1
                S_cat(index) = ( Temp_pp(index) - 2.0_real12 * Temp_p(index) ) &
                * ( grid(ix,iy,iz)%tau * lin_rhoc(index) )  !

            end do
        end do
    end do

end subroutine S_catS

end module cattaneo
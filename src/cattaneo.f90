module cattaneo
use inputs, only: NA, time_step, nx, ny, nz, grid
use globe_data, only: TPD,TPPD
use constants, only: real12, int12, TINY
implicit none
contains
subroutine S_catS(s_cat)
    real(real12), dimension(NA), intent(inout) :: S_cat

    integer(int12) :: i, x, y, z
    real(real12) :: r_tmp1
    i = 0
    S_cat = 0._real12
    do z=1,nz
        do y = 1, ny
            do x = 1, nx
                i = i+1
                S_cat(i) = ( TPPD(i) - 2._real12 * TPD(i) ) * ( grid(x,y,z)%tau )  !

            end do
        end do
    end do

end subroutine S_catS

end module cattaneo
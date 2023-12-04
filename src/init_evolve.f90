module initial 
  use constants, only: real12, int12
  use inputs, only: NA, T_Bath, nx, ny, nz
  use globe_data, only: TPD,TPPD
  implicit none

  
contains
  
  subroutine initial_evolve()
    implicit none
    integer(int12) :: i,j,k, index

    !Current version just sets all previous time steps temperatures = heat bath
    ! Gaussian distribution of temperatures about T_Bath+100
    ! index = 0
    ! do k = 1, nz
    !   do j = 1, ny
    !     do i = 1, nx
    !       index = index + 1
    !       TPD(index) = (T_Bath + 100._real12) * exp(-((i - nx/2)**2 + (j - ny/2)**2 + (k - nz/2)**2) / 1000._real12)+T_Bath
    !     end do
    !   end do
    ! end do
    ! TPPD=TPD
    ! TPPD= 310._real12
    ! TPD= 310._real12
    TPD = T_Bath
    TPPD = T_Bath
    ! print*,'Initial Temperature = ',TPD
    
  end subroutine initial_evolve
end module initial
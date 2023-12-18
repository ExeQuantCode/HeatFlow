module initial 
  use constants, only: real12, int12
  use inputs, only: NA, T_Bath, nx, ny, nz, InputTempDis
  use globe_data, only: TPD,TPPD
  implicit none

  
contains
  
  subroutine initial_evolve()
    implicit none
    integer(int12) :: i,j,k, index, reason
    character :: buffer
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
    if (InputTempDis) then 
      open(unit=10,file='./outputs/TempDis.dat',status='unknown')
      read(10,'(A)', iostat = reason) buffer
      if (Reason .ne. 0) then
        write(10,*) 'Error: Unexpected EOF TempDis.in'
        call exit
      end if
      read(buffer,*) (TPD(index), index = 1, NA)
      TPPD = TPD
      close(10)
    else 
      TPD = T_Bath
      TPPD = T_Bath
    end if

    ! print*,'Initial Temperature = ',TPD
    
  end subroutine initial_evolve
end module initial
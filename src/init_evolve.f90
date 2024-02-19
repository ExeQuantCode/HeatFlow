module initial 
  use constants, only: real12, int12
  use inputs, only: NA, nx, ny, nz, InputTempDis, FullRestart, T_System
  use globe_data, only: TPD,TPPD
  implicit none

  
contains
  
  subroutine initial_evolve()
    implicit none
    integer(int12) :: i,j,k, index, reason
    logical :: file_TPD_exists, file_TPPD_exists
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

      if (FullRestart) then
        inquire(file='./outputs/TempDisTPD.dat', exist=file_TPD_exists)
        inquire(file='./outputs/TempDisTPPD.dat', exist=file_TPPD_exists)
        if (.not. file_TPD_exists .or. .not. file_TPPD_exists) then
          write(*,*) 'Error: TempDisTPD.dat or TempDisTPPD.dat does not exist'
          call exit
        end if
        open(unit=11, file='./outputs/TempDisTPD.dat', status='unknown')
        read(11,'(A)', iostat = reason) buffer
        read(buffer,*) (TPD(index), index = 1, NA)
        close(11)
        open(unit=12, file='./outputs/TempDisTPPD.dat', status='unknown')
        read(12,'(A)', iostat = reason) buffer
        read(buffer,*) (TPPD(index), index = 1, NA)
        close(12)
      end if
    if (InputTempDis) then
      inquire(file='./outputs/TempDis.dat', exist=file_TPD_exists)
      if (.not. file_TPD_exists) then
        write(*,*) 'Error: TempDis.dat does not exist'
        call exit
      end if
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
      TPD = T_System
      TPPD = T_System
    end if

    
  end subroutine initial_evolve
end module initial
module output
  use constants, only: real12, int12, TINY
  use inputs, only: nx,ny,nz, time_step, zpos, grid, NA, Check_Steady_State, ntime
  use constructions, only: heatblock
  use globe_data, only: TPD,TPPD
  implicit none
  
contains
  subroutine plot(it)
    implicit none
    ! real(real12), dimension(nx, ny,nz) :: T
    integer :: new, newunit
   !  real(real12), dimension(e) :: T_matrix
    real(real12), dimension(nx) :: R
    real(real12) :: xlen
    integer(int12) :: flag, index
    !integer :: zpos = 508
    integer(int12), intent(in) :: it
    real(real12) :: CT(nx,ny,nz), TN(nx,ny,nz)
    integer(int12) :: i,j,k,ix

    
    xlen= 1.0*0.333
    flag=0
    if (it.eq.1) then
       open(unit=30,file='./outputs/Temperature.txt')
    end if
    
    r(1)=grid(1,ny/2,zpos)%length(1)
    do ix=2,nx
       r(ix)=grid(ix,ny/2,zpos)%length(1)+r(ix-1)
    end do

    index=1
    do k = 1, nz
      do j = 1, ny
         do i = 1, nx
            TN(i,j,k) = TPPD(index)
            index = index+1
         end do
      end do
    end do 
    ! print*, 'Writing Temperature to file'
    !write(30,*) REAL(it)*time_step, ((T_matrix(i)-T_Bath),i=1,e)
    if (Check_Steady_State) then
      if (it.gt.1) then
        print*, 'Checking for steady state'
        open(unit=32,file='./Tests/outputs/Temperature_Steady.txt')
        read(32,*) CT
        close(32)
        if (any(abs(TN-CT).gt.TINY)) then
          print*, TN(1,1,1), CT(1,1,1)
          print*, 'Steady state not reached'
          stop
        else
          print*, 'Steady state reached, Test passed'
        end if
      end if
    end if 
    write(30,*) real((it-1)*(time_step)),(TN(nx/2,ny/2,:))   !-293.0
    if (it == ntime) then
        close(30)
        ! print*, 'TH after ', real((it-1)*(time_step)), ' seconds is ', TN(6,6,6)
        ! print*, 'TM after ', real((it-1)*(time_step)), ' seconds is ', TN(9,9,9)
    end if

    call PlotdeltaT(it)
    205 format(5f12.6)

  end subroutine plot
  !!---------------------------------------------------------------------
  !To plot difference in temperature between two time_steps
  !!---------------------------------------------------------------------
  subroutine PlotdeltaT(it)
    real(real12) :: DT(NA)
    real(real12) :: TO(nx,ny,nz)
    integer(int12) :: i,j,k,it
    integer(int12) :: index
    
    DT=TPD-TPPD
    index=1
    do k = 1, nz
      do j = 1, ny
         do i = 1, nx
            TO(i,j,k) = DT(index)
            index = index+1
         end do
      end do
    end do
    if (it == 1) then
       open(unit=31,file='./outputs/DTemperature.txt')
    end if
    write(31,*) REAL(it)*time_step, TO(:,ny/2,ny/2)
  end subroutine PlotdeltaT
end module output

module output
  use constants, only: real12, int12
  use inputs, only: nx,ny,nz, time_step, zpos, grid, NA
  use constructions, only: heatblock
  use globe_data, only: TN,TPD,TPPD
  implicit none
  
contains
  subroutine plot(it)
    
    ! real(real12), dimension(nx, ny,nz) :: T
    integer :: new, newunit
   !  real(real12), dimension(e) :: T_matrix
    real(real12), dimension(nx) :: R
    real(real12) :: xlen
    integer(int12) :: flag, index
    !integer :: zpos = 508
    integer(int12), intent(in) :: it

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

    !write(30,*) REAL(it)*time_step, ((T_matrix(i)-T_Bath),i=1,e)
    write(30,*) REAL(it)*time_step, TN(:,ny/2,ny/2)! (TN(nx/2,ny/2,nz/2))   !-293.0

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

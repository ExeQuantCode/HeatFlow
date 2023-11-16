module output
  use constants, only: real12, int12
  use inputs, only: nx,ny,nz, time_step, zpos, grid
  use constructions, only: heatblock
  use globe_data, only: TN,TPD
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
            TN(i,j,k) = TPD(index)
            index = index+1
         end do
      end do
    end do 

    !write(30,*) REAL(it)*time_step, ((T_matrix(i)-T_Bath),i=1,e)
    write(30,*) REAL(it)*time_step, TN! (TN(nx/2,ny/2,nz/2))   !-293.0

    
    205 format(5f12.6)

  end subroutine plot
end module output

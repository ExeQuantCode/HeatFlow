MODULE OUTPUT
  use constants, only: real12, int12
  use inputs, only: nx,ny,nz, time_step, zpos, grid
  use constructions, only: heatblock
  implicit none
  
contains
  SUBROUTINE PLOT(it,TN)
    
    ! real(real12), dimension(nx, ny,nz) :: T
    integer :: new, newunit
    real(real12), dimension(nx,ny,nz) :: TN
  !  real(real12), dimension(e) :: T_matrix
    real(real12), dimension(nx) :: R
    real(real12) :: xlen
    integer(int12) flag
    !integer :: zpos = 508
    
    integer(int12) :: i,j,k,it,ix

    
    xlen= 1.0*0.333
    flag=0
    if (it.eq.1) then
       open(unit=30,file='Temperature.txt')
    end if
    
    r(1)=grid(1,ny/2,zpos)%length(1)
    do ix=2,nx
       r(ix)=grid(ix,ny/2,zpos)%length(1)+r(ix-1)
    end do

    !write(30,*) REAL(it)*time_step, ((T(i,j,nz/2)-T_Bath),i=1,nx,j=1,ny)

    !write(30,*) REAL(it)*time_step, ((T(i,ny/2,zpos)-T_Bath),i=1,nx)



    !write(30,*) REAL(it)*time_step, ((T_matrix(i)-T_Bath),i=1,e)
    write(30,*) REAL(it)*time_step, TN! (TN(nx/2,ny/2,nz/2))   !-293.0

    !prints a x-section to the output file, for ix=1
  !  write(30,*) (TN(2,2,2))
    
    205 format(5f12.6)
   ! write(31,*) REAL(it)*time_step, ((T_matrix(i)-T_Bath))
  !  do ix=1,nx
  !     if (r(ix).gt.xlen.and.flag.eq.0) then
  !        flag=1
    !      write(32,*) REAL(it)*time_step, ((T(ix,ny/2,zpos)-T_Bath))
   !    end if
   ! end do
   ! print*,zpos
  end SUBROUTINE PLOT
end MODULE OUTPUT

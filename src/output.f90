
!##############################################################################################################
! This codes primary function is to output the results of the simulation to txt file
!##############################################################################################################
MODULE OUTPUT
  use constants
  use constructions
  use inputs
  implicit none
  
contains
  SUBROUTINE PLOT(it,TN,grid)
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    real(real12), dimension(nx,ny,nz):: TN
    real(real12), dimension(nx):: r
    real(real12) :: xlen
    integer(int12) :: flag


    integer(int12) :: i,j,k,it,ix


    
    
    
    xlen= 1.0*0.333
    flag=0
    if (it.eq.1) then
       open(unit=30,file='Temperature.txt', status='old')
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

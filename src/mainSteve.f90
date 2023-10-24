PROGRAM HEATFLOW_V0_1
  
  use constants
  use inputs
  use constructions
  use setup
  use simulator
  use matrix_inversion
  use output
  
  implicit none
  
TYPE(heatblock), dimension(nx,ny,nz) :: grid
real(real12), dimension(nx, ny,nz) :: T,T0, T00
real(real12), dimension(NA):: T, TP, TPP
integer(int12) :: i,it,ix,iy,iz
real(real12), dimension(nx) :: cellengthx
real(real12), dimension(ny) :: cellengthy
real(real12), dimension(nz) :: cellengthz

real(real12) :: rstart, rend, rprogress

!write(*,*) "I WILL END"
!stop 0

call cpu_time(rstart)

! call fspak90('factor',ija)

Print*, 'Setup initialising'
grid%imaterial_type=1
grid%length=dx
grid%area=dx*dx
grid%volume=dx*dx*dx
grid%heater=0
grid%heater(nx/2,ny/2,nz/2)=1

Print*, 'Setup complete, running simulation'
do it=1,ntime
   if (iverb.eq.1) then
      print*, 'Evolving system, timestep = ', it
   end if
  
   CALL evolve(grid,T, TP, TPP)

   CALL plot(it,TN_matrix,grid)
   
   
end do

call cpu_time(rend)

print*, 'time=', rend-rstart

print*, 'all done'





end PROGRAM HEATFLOW_V0_1


  

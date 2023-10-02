PROGRAM HEATFLOW_V0_1
  
  use constants
  use parameters
  use constructions
  use setup
  use simulator
  use matrix_inversion
  use output
  
  implicit none
  
TYPE(heatblock), dimension(nx,ny,nz) :: grid
real(real12), dimension(nx, ny,nz) :: T,TN, Told
integer(int12), parameter :: e = nx*ny*nz
real(real12), dimension(e):: T_matrix, TN_matrix, Told_matrix
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


Print*, 'Setup complete, running simulation'
do it=1,ntime
   if (iverb.eq.1) then
      print*, 'Evolving system, timestep = ', it
   end if
  
   CALL evolve(grid,T_matrix, TN_matrix, Told_matrix, it)

   CALL plot(it,TN_matrix,grid)
   
   
end do

call cpu_time(rend)

print*, 'time=', rend-rstart

print*, 'all done'





end PROGRAM HEATFLOW_V0_1


  

!##############################################################################################################
! This is the main program for HeatFlow
!##############################################################################################################
PROGRAM HEATFLOW_V0_1
  
  use constants, only: real12, int12
  use constructions, only: heatblock
  use output, only: plot
  use inputs, only: readINPUT, nx, ny, nz, NA, iverb, ntime
  use evolution, only: evolve
  use setup, only: Initiate, set_global_variables
  implicit none
   real(real12) :: rstart, rend, rprogress
   integer(int12) :: it
   integer :: newunit, unit
   real(real12), allocatable :: T(:,:,:), TN(:,:,:), Told(:,:,:)
   !real(real12), dimension(nx, ny,nz) :: T,T0, T00
   real(real12), allocatable :: TD(:), TPD(:) !1D x and b respectively
   TYPE(heatblock), allocatable :: grid(:, :, :)
   call cpu_time(rstart) ! starts timer 


   Print*, 'Setup initialising' ! indication that the model is runnning
   
   ! Read parameters from inputs file, located in inputs.f90
   open(unit = newunit, file = 'Inputs.txt')
   call readINPUT(unit)
   close(unit)
   allocate(grid(nx, ny, nz))
   allocate(TN(nx, ny, nz))
   allocate(T(nx, ny, nz))
   allocate(Told(nx, ny, nz))
   allocate(TD(NA))
   allocate(TPD(NA))

   call Initiate(grid)
   call set_global_variables()
   Print*, 'Setup complete, running simulation' ! indication that inputs have been read

   do it=1,ntime ! run simulation for 'ntime' time steps
      if (iverb.eq.1) then
         print*, 'Evolving system, timestep = ', it
      end if

      !call bmake(grid, T, TN, Told, TPD ,it) !Temp will be moved to evolve eventually

      CALL evolve(it,Told) !run the simulation, located in evolve.f90

      !CALL plot(it,TN,grid) ! Write result to an output file, located in output.f90

      
   end do

   call cpu_time(rend) !ends timer so we can see how long the simulation took to run

   print*, 'time=', rend-rstart

   print*, 'all done'





end PROGRAM HEATFLOW_V0_1


  

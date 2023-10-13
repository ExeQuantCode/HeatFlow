!##############################################################################################################
! This is the main program for HeatFlow
!##############################################################################################################
PROGRAM HEATFLOW_V0_1
  
  use constants
  use constructions
  use setup
  use simulator
  use matrix_inversion
  use output
  use inputs

  implicit none
   real(real12) :: rstart, rend, rprogress
   integer(int12) :: it
   real(real12), allocatable :: T(:,:,:), TN(:,:,:), Told(:,:,:) 
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

   Print*, 'Setup complete, running simulation' ! indication that inputs have been read
   do it=1,ntime ! run simulation for 'ntime' time steps
      if (iverb.eq.1) then
         print*, 'Evolving system, timestep = ', it
      end if

      CALL evolve(grid, T, TN, Told, it) !run the simulation, located in Matrix4.f90

      CALL plot(it,TN,grid) ! Write result to an output file, located in output.f90

      
   end do

   call cpu_time(rend) !ends timer so we can see how long the simulation took to run

   print*, 'time=', rend-rstart

   print*, 'all done'





end PROGRAM HEATFLOW_V0_1


  

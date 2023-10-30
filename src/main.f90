!##############################################################################################################
! This is the main program for HeatFlow
!##############################################################################################################
PROGRAM HEATFLOW_V0_1
  
  use constants, only: real12, int12
  use constructions, only: heatblock
  use output, only: plot
  use inputs, only: read_all_files, nx, ny, nz, NA, iverb, ntime, grid
  use evolution, only: evolve
  use setup, only: Initiate, set_global_variables

  implicit none
   real(real12) :: rstart, rend, rprogress
   integer(int12) :: it
   integer :: newunit, unit
   !real(real12), dimension(nx, ny,nz) :: T,T0, T00
   call cpu_time(rstart) ! starts timer 


   Print*, 'Setup initialising' ! indication that the model is runnning
   
   ! Read parameters from inputs file, located in inputs.f90
   call read_all_files()



   call Initiate()
   call set_global_variables()
   Print*, 'Setup complete, running simulation' ! indication that inputs have been read

   do it=1,ntime ! run simulation for 'ntime' time steps
      if (iverb.eq.1) then
         print*, 'Evolving system, timestep = ', it
      end if

      !call bmake(grid, T, TN, Told, TPD ,it) !Temp will be moved to evolve eventually

      CALL evolve(it) !run the simulation, located in evolve.f90

      CALL plot(it) ! Write result to an output file, located in output.f90

      
   end do

   call cpu_time(rend) !ends timer so we can see how long the simulation took to run

   print*, 'time=', rend-rstart

   print*, 'all done'





end PROGRAM HEATFLOW_V0_1


  

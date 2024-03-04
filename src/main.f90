!!!#################################################################################################
!!! This is the main program for HeatFlow
!!! Author: Harry Mclean, Frank Davis, Steven Hepplestone.
!!!#################################################################################################
!!!The program defines variables for timing the execution of the simulation ...
!!!...(cpustart, cpuend, cpustart2) and variables for controlling the simulation...
!!!... (itime, newunit, unit).
!!!The code reads input parameters from files using the read_all_files subroutine.
!!!It sets global variables and arrays using the set_global_variables subroutine.
!!!The program runs an initial evolution step using the initial_evolve subroutine.
!!!It enters a loop that runs the simulation for a specified number of time steps (ntime).
!!!Inside the loop, it checks the verbosity level (iverb) and prints progress...
!!!... information to the console.
!!!It calls the evolve subroutine to perform the time evolution of the system.
!!!It calls the data_write and file_print subroutine to write and show the results.
!!!After the loop, it calculates and prints the total execution time.
!!!Finally, it outputs a message indicating that the simulation is complete.
!!!#################################################################################################
!!!verboisity 0 - no feedback, 1 user needs details, 2, developer needs details, 3 everything
!!!#################################################################################################
program HEATFLOW_V0_2
  
  use constants, only: real12, int12
  use constructions, only: heatblock
  use output, only: data_write, final_print
  use inputs, only: read_all_files, nx, ny, nz, NA, iverb, ntime, grid, LPercentage
  use inputs, only: IVERB
  use evolution, only: simulate
  use setup, only: set_global_variables
  use INITIAL, only: initial_evolve

  implicit none
   real(real12) :: cpustart, cpuend, rprogress, cpustart2, progress
   integer(int12) :: itime
   integer :: newunit, unit

   !-------------------------------------------------------------!
   ! calculate the time to run full simulation                   !
   !-------------------------------------------------------------!
   CALL cpu_time(cpustart)                                         !
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

   ! give feedback to user that code has begun
   write(*,*) 'Setup initialising' 
   
   !-------------------------------------------------------------!
   ! Read parameters from input file and set global variables ...!
   ! ... and arrays                                              !
   !-------------------------------------------------------------!
   CALL read_all_files()                                         !
   
   CALL cpu_time(cpustart2)                                         !   !
   CALL set_global_variables() 
   CALL cpu_time(cpuend)
   if (IVERB.ge.1) write(*,'(A,F12.6)') &
   ' time to complete set_global_variables=', cpuend-cpustart2                    ! 

 
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!


   !-------------------------------------------------------------!
   ! run initial evolve step                                     !
   !-------------------------------------------------------------!
   CALL initial_evolve()                                         !
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

   ! give feedback to user that main simulation is begining
   write(*,*) 'Setup complete, running simulation' 

   !-------------------------------------------------------------!
   ! run simulation for 'ntime' time steps                       !
   !-------------------------------------------------------------!
   do itime=1,ntime 

      if (iverb.eq.0) then
         if (Lpercentage) then 
            progress = real(itime)/real(ntime)*100.0
            write(*,'(A,A,F12.4,A)', advance = 'no') achar(13)&
            , 'Evolving system, timestep = ', progress, '%'
         end if 
         if ((mod(itime,10000) .eq.0) .and. (.not.Lpercentage)) &
            write(*,'(A,A,I12)', advance = 'no') achar(13), 'Evolving system, timestep = ', itime
      end if                                                           !


      if (itime .eq. 1) CALL initial_evolve                        !
      
      ! run the time evolution                                   !
      CALL simulate(itime)
                                                 !
                                                                 !
      ! Write results                           !
      CALL data_write(itime) 
      if (IVERB.ge.1) CALL final_print                                         !
                                                                 !
   end do                                                        !
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

   !-------------------------------------------------------------!
   ! calculate end time and print to user                        !
   !-------------------------------------------------------------!
   CALL cpu_time(cpuend)                                           !
   write(*,'(A,F12.6)') ' time=', cpuend-cpustart                    !
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

   ! give feedback to user that code has ended
   write(*,*) 'all done'

end program HEATFLOW_V0_2


  

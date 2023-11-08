!##############################################################################################################
! This is the main program for HeatFlow
!##############################################################################################################
program HEATFLOW_V0_1
  
  use constants, only: real12, int12
  use constructions, only: heatblock
  use output, only: plot
  use inputs, only: read_all_files, nx, ny, nz, NA, iverb, ntime, grid
  use evolution, only: evolve
  use setup, only: set_global_variables
  use INITIAL, only: initial_evolve

  implicit none
   real(real12) :: rstart, rend, rprogress
   integer(int12) :: it
   integer :: newunit, unit
   !real(real12), dimension(nx, ny,nz) :: T,T0, T00

   !-------------------------------------------------------------!
   ! calculate the time to run full simulation                   !
   !-------------------------------------------------------------!
   call cpu_time(rstart)                                         !
   !-------------------------------------------------------------!

   ! give feedback to user that code has begun
   write(*,*) 'Setup initialising' 
   
   !-------------------------------------------------------------!
   ! Read parameters from input file and set global variables ...!
   ! ... and arrays                                              !
   !-------------------------------------------------------------!
   call read_all_files()                                         !
                                                                 !
   call set_global_variables()                                   !
   !-------------------------------------------------------------!


   !-------------------------------------------------------------!
   ! run initial evolve step                                     !
   !-------------------------------------------------------------!
   call initial_evolve()                                         !
   !-------------------------------------------------------------!

   ! give feedback to user that main simulation is begining
   write(*,*) 'Setup complete, running simulation' 

   !-------------------------------------------------------------!
   ! run simulation for 'ntime' time steps                       !
   !-------------------------------------------------------------!
   do it=1,ntime                                                 !
      if (iverb.eq.1) then                                       !
         print*, 'Evolving system, timestep = ', it              !
      end if                                                     !
                                                                 !
      ! Temp will be moved to evolve eventually                  !
      !call bmake(grid, T, TN, Told, TPD ,it)                    !
                                                                 !
      ! run the time evolution                                   !
      CALL evolve(it)                                            !
                                                                 !
      ! Write results                                            !
      CALL plot(it)                                              !
                                                                 !
   end do                                                        !
   !-------------------------------------------------------------!

   !-------------------------------------------------------------!
   ! calculate end time and print to user                        !
   !-------------------------------------------------------------!
   call cpu_time(rend)                                           !
   write(*,'(A,F12.6)') ' time=', rend-rstart                    !
   !-------------------------------------------------------------!

   ! give feedback to user that code has ended
   write(*,*) 'all done'

end program HEATFLOW_V0_1


  

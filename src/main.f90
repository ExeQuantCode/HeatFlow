!##############################################################################################################
! This is the main program for HeatFlow
!##############################################################################################################
program HEATFLOW_V0_1
  
  use constants, only: real12, int12
  use constructions, only: heatblock
  use output, only: plot
  use inputs, only: read_all_files, nx, ny, nz, NA, iverb, ntime, grid, Percentage
  use evolution, only: evolve
  use setup, only: set_global_variables
  use INITIAL, only: initial_evolve

  implicit none
   real(real12) :: rstart, rend, rprogress, rstart2, progress
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
   
   call cpu_time(rstart2)                                         !   !
   call set_global_variables() 
   call cpu_time(rend)                                           !
   write(*,'(A,F12.6)') ' time to complete HSparce=', rend-rstart2                    ! 
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
   do it=1,ntime 

      if (iverb.eq.1) then
         if (percentage) then 
            progress = real(it)/real(ntime)*100.0
            write(*,'(A,A,F12.4,A)', advance = 'no') achar(13), 'Evolving system, timestep = ', progress, '%'
         end if 
         if ((mod(it,10000) .eq.0).and. (percentage .neqv. .True. )) &
            write(*,'(A,A,I12)', advance = 'no') achar(13), 'Evolving system, timestep = ', it
      end if                                                           !

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


  

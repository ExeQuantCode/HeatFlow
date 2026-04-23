!!!#################################################################################################
!!! This is the main program for HeatFlow
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone.
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
program HEATFLOW_V0_3
  
  use constants, only: real12, int12
  use constructions, only: heatblock
  use output, only: data_write, final_print
  use inputs, only: read_all_files, iverb, ntime, LPercentage
  use inputs, only: IVERB
  use evolution, only: simulate
  use setup, only: set_global_variables
  use INITIAL, only: initial_evolve
   use petsc_solver, only: petsc_init, petsc_finalize, petsc_is_root

  implicit none
   real(real12) :: cpustart, cpuend, cpustart2, progress
   integer(int12) :: itime

   !-------------------------------------------------------------!
   ! Initialize PETSc FIRST (before any other operations)        !
   !-------------------------------------------------------------!
   CALL petsc_init()
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

   !-------------------------------------------------------------!
   ! calculate the time to run full simulation                   !
   !-------------------------------------------------------------!
   CALL cpu_time(cpustart)                                         
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

   ! give feedback to user that code has begun
   if(petsc_is_root()) write(*,*) 'Setup initialising'

   !-------------------------------------------------------------!
   ! handle command line arguments on master/root node only      !
   !-------------------------------------------------------------!
   if(petsc_is_root())then
      call handle_command_line_arguments()
   end if

   !-------------------------------------------------------------!
   ! Read parameters from input file and set global variables ...!
   ! ... and arrays                                              !
   !-------------------------------------------------------------!
   if(petsc_is_root())then
      CALL read_all_files()                                         
      
      CALL cpu_time(cpustart2)                                      
      CALL set_global_variables() 
      CALL cpu_time(cpuend)
      if (IVERB.ge.1) write(*,'(A,F12.6)') &
      ' time to complete set_global_variables=', cpuend-cpustart2   
   end if

 
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!


   ! give feedback to user that main simulation is begining
   if (petsc_is_root()) write(*,*) 'Setup complete, running simulation'

   !-------------------------------------------------------------!
   ! run simulation for 'ntime' time steps                       !
   !-------------------------------------------------------------!

   do itime=1,ntime 

      if (petsc_is_root() .and. iverb.eq.0) then
         if (Lpercentage) then 
            progress = real(itime)/real(ntime)*100.0
            write(*,'(A,A,F12.4,A)', advance = 'no') achar(13)&
            , 'Evolving system, timestep = ', progress, '%'
         end if 
         if ((mod(itime,10000) .eq.0) .and. (.not.Lpercentage)) &
            write(*,'(A,A,I12)', advance = 'no') achar(13), 'Evolving system, timestep = ', itime
      end if                                                     

      ! CALL initial_evolve to set systems initial Temperature conditions      
      if (itime .eq. 1) CALL initial_evolve                      
      
      ! run the time evolution  
      CALL simulate(itime)

                                                
                             
      ! Write results                           
      if (petsc_is_root()) CALL data_write(itime)
      if (petsc_is_root() .and. IVERB.ge.3) CALL final_print
                                                                 
   end do  
   CALL petsc_finalize()
                                                      
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

   !-------------------------------------------------------------!
   ! calculate end time and print to user                        !
   !-------------------------------------------------------------!
   CALL cpu_time(cpuend)
   if (petsc_is_root()) write(*,'(A,F12.6)') ' time=', cpuend-cpustart
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

   ! give feedback to user that code has ended
   if (petsc_is_root()) write(*,*) 'all done'

contains

   !-------------------------------------------------------------!
   ! Handle command line arguments for directory specification   !
   !-------------------------------------------------------------!
   subroutine handle_command_line_arguments()
     implicit none
     integer :: nargs, i, stat
     character(len=1024) :: arg, directory
     logical :: dir_exists
     character(len=*), parameter :: directory_flag = '--directory'
     character(len=*), parameter :: directory_prefix = '--directory='
      
     nargs = command_argument_count()
     directory = ''
      
     write(*,*) 'Number of command line arguments: ', nargs
     do i = 1, nargs
        call get_command_argument(i, arg)
        write(*,*) 'Received command line argument: ', trim(arg)
        if (trim(arg) .eq. directory_flag) then
           if (i .eq. nargs) then
              if (petsc_is_root()) write(*,*) 'Error: Missing value for --directory'
              call exit(1)
           end if
           call get_command_argument(i + 1, directory)
           exit
        else if (index(trim(arg), directory_prefix) .eq. 1) then
           directory = trim(arg(len(directory_prefix) + 1:))
           exit
        end if
     end do
      
        write(*,*) 'Changing directory to: ', trim(directory)
     if(len_trim(directory) .gt. 0) then
        inquire(file=trim(directory)//'/.' , exist=dir_exists)
        if(.not. dir_exists) then
           if (petsc_is_root()) write(*,*) 'Error: Directory does not exist: ', trim(directory)
           call exit(1)
        end if
        call chdir(trim(directory), stat)
        if(stat .ne. 0) then
           if(petsc_is_root()) write(*,*) 'Error: Failed to change directory to: ', trim(directory)
           call exit(1)
        end if
        if(petsc_is_root()) write(*,*) 'Changed directory to: ', trim(directory)
     end if

   end subroutine handle_command_line_arguments
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

end program HEATFLOW_V0_3


  

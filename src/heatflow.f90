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
   use inputs, only: IVERB, input_directory, output_directory, restart_directory
   use inputs, only: set_io_directories, join_path
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
   call handle_command_line_arguments()

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
     integer :: nargs, i
     character(len=1024) :: arg, directory, cli_input_directory, cli_output_directory
     logical :: dir_exists
     character(len=*), parameter :: directory_flag = '--directory'
     character(len=*), parameter :: directory_prefix = '--directory='
     character(len=*), parameter :: input_directory_flag = '--input-directory'
     character(len=*), parameter :: input_directory_prefix = '--input-directory='
     character(len=*), parameter :: output_directory_flag = '--output-directory'
     character(len=*), parameter :: output_directory_prefix = '--output-directory='
      
     nargs = command_argument_count()
     directory = ''
     cli_input_directory = ''
     cli_output_directory = ''
      
     if (petsc_is_root()) write(*,*) 'Number of command line arguments: ', nargs
     i = 1
     do while (i .le. nargs)
        call get_command_argument(i, arg)
        if (petsc_is_root()) write(*,*) 'Received command line argument: ', trim(arg)
        if (trim(arg) .eq. directory_flag) then
           call require_argument_value(i, nargs, directory_flag, directory)
           i = i + 2
           cycle
        else if (index(trim(arg), directory_prefix) .eq. 1) then
           directory = trim(arg(len(directory_prefix) + 1:))
        else if (trim(arg) .eq. input_directory_flag) then
           call require_argument_value(i, nargs, input_directory_flag, cli_input_directory)
           i = i + 2
           cycle
        else if (index(trim(arg), input_directory_prefix) .eq. 1) then
           cli_input_directory = trim(arg(len(input_directory_prefix) + 1:))
        else if (trim(arg) .eq. output_directory_flag) then
           call require_argument_value(i, nargs, output_directory_flag, cli_output_directory)
           i = i + 2
           cycle
        else if (index(trim(arg), output_directory_prefix) .eq. 1) then
           cli_output_directory = trim(arg(len(output_directory_prefix) + 1:))
        end if
        i = i + 1
     end do

     if(len_trim(directory) .gt. 0) then
        inquire(file=trim(directory)//'/.' , exist=dir_exists)
        if(.not. dir_exists) then
           if (petsc_is_root()) write(*,*) 'Error: Directory does not exist: ', trim(directory)
           call exit(1)
        end if
        call set_io_directories(input_dir=join_path(directory, 'inputs'), &
             output_dir=join_path(directory, 'outputs'), restart_dir=join_path(directory, 'restart'))
     end if

     if(len_trim(cli_input_directory) .gt. 0) then
        inquire(file=trim(cli_input_directory)//'/.' , exist=dir_exists)
        if(.not. dir_exists) then
           if (petsc_is_root()) write(*,*) 'Error: Input directory does not exist: ', trim(cli_input_directory)
           call exit(1)
        end if
        call set_io_directories(input_dir=cli_input_directory)
     end if

     if(len_trim(cli_output_directory) .gt. 0) then
        call set_io_directories(output_dir=cli_output_directory)
     end if

     if (petsc_is_root()) then
        write(*,*) 'Input directory: ', trim(input_directory)
        write(*,*) 'Output directory: ', trim(output_directory)
        write(*,*) 'Restart directory: ', trim(restart_directory)
     end if

   end subroutine handle_command_line_arguments
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!

     subroutine require_argument_value(index, count, flag_name, value)
       implicit none
       integer, intent(in) :: index, count
       character(len=*), intent(in) :: flag_name
       character(len=*), intent(out) :: value

       if (index .eq. count) then
          if (petsc_is_root()) write(*,*) 'Error: Missing value for ', trim(flag_name)
          call exit(1)
       end if

       call get_command_argument(index + 1, value)
     end subroutine require_argument_value

end program HEATFLOW_V0_3


  

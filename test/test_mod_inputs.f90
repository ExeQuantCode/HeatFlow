module test_mod_inputs
    use constants, only: real12, int12
    use inputs, only: read_param, read_mat, read_system
    use inputs, only: solver_type, T_decimals, input_dir, read_power_files, power_lookup
    use inputs, only: power_profiles
    use inputs, only: nx, ny, nz, grid
    use initial, only: read_temp_file
    ! use TempDep, only: ReadTempDepTable    
    implicit none

contains

    subroutine test_read_param(success)
        implicit none
        logical :: success
        character(len=1024) :: param_infile
        integer :: newunit, reason, unit
        logical :: file_exists
        character(len=1024) :: cwdstring, fullpath
    !-----------------------------------------------
    ! get data from param.in
    !-----------------------------------------------
        call getcwd(cwdstring)

    ! name infile
        param_infile = "/test/data/param.in" ! file name
        fullpath = trim(cwdstring)//param_infile ! full path
        print*, "fullpath = ", fullpath
        param_infile = fullpath
        ! check if file is there
        inquire(file=param_infile, exist=file_exists) ! check if file exists
    
        ! give error and exit code
        if (.not.file_exists) then
            write(0,*) 'Error cannot find file: param.in'
            stop 1
        end if
        
        ! open, read and close param.in
        open(newunit=unit, file=param_infile, iostat=reason)
        CALL read_param(unit)
        close(unit)

        !-----------------------------------------------
        ! check the solver and T_decimals parameters
        !-----------------------------------------------
        if (trim(solver_type) .ne. 'ilu') then
            write(0,*) 'Error: solver_type = "', trim(solver_type), '", expected "ilu"'
            stop 1
        end if
        if (T_decimals .ne. 3) then
            write(0,*) 'Error: T_decimals = ', T_decimals, ', expected 3'
            stop 1
        end if
        write(*,*) "test_read_param (solver, T_decimals): PASSED"
        
    
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !         !-----------------------------------------------
    ! ! get data from param_error.in
    ! !-----------------------------------------------
    !     call getcwd(cwdstring)

    ! ! name infile
    !     param_infile = "/test/data/param_error.in" ! file name
    !     fullpath = trim(cwdstring)//param_infile ! full path
    !     print*, "fullpath = ", fullpath
    !     param_infile = fullpath
    !     ! check if file is there
    !     inquire(file=param_infile, exist=file_exists) ! check if file exists
    
    !     ! give error and exit code
    !     if (.not.file_exists) then
    !         write(0,*) 'Error cannot find file: param_error.in'
    !         stop 1
    !     end if
        
    !     ! open, read and close param.in
    !     open(newunit=unit, file=param_infile, iostat=reason)
    !     CALL read_param(unit)
    !     close(unit)
        
    
    !     !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    end subroutine test_read_param

    subroutine test_read_mat(success)
        implicit none
        logical :: success
        character(len=1024) :: mat_infile
        integer :: newunit, reason, unit
        logical :: file_exists
        character(len=1024) :: cwdstring, fullpath


        call getcwd(cwdstring)

        !-----------------------------------------------
        ! get data from mat.in
        !-----------------------------------------------
        ! name infile
        mat_infile = "/test/data/mat.in" ! file name
        fullpath = trim(cwdstring)//mat_infile ! full path
        print*, "fullpath = ", fullpath
        mat_infile = fullpath

        ! check if file is there
        inquire(file=mat_infile, exist=file_exists)
    
        ! give error and exit code
        if (.not.file_exists) then
            write(0,*) 'Error cannot find file: mat.in'
            stop 1
        end if
        
        ! open, read and close mat.in
        open(newunit=unit, file=mat_infile, iostat=reason)
        CALL read_mat(unit)
        close(unit)
            !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! !-----------------------------------------------
        ! ! get data from mat_error.in
        ! !-----------------------------------------------
        ! ! name infile
        ! mat_infile = "/test/data/mat_error.in" ! file name
        ! fullpath = trim(cwdstring)//mat_infile ! full path
        ! print*, "fullpath = ", fullpath
        ! mat_infile = fullpath

        ! ! check if file is there
        ! inquire(file=mat_infile, exist=file_exists)
    
        ! ! give error and exit code
        ! if (.not.file_exists) then
        !     write(0,*) 'Error cannot find file: mat_error.in'
        !     stop 1
        ! end if
        
        ! ! open, read and close mat.in
        ! open(newunit=unit, file=mat_infile, iostat=reason)
        ! CALL read_mat(unit)
        ! close(unit)
        !     !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    end subroutine test_read_mat

    subroutine test_read_mesh(success)
        implicit none
        logical :: success
        character(len=1024) :: system_infile
        integer :: newunit, reason, unit
        logical :: file_exists
        character(len=1024) :: cwdstring, fullpath


        call getcwd(cwdstring)

        !-----------------------------------------------
        ! get data from system.in
        !-----------------------------------------------
        ! name infile
        system_infile = "/test/data/system.in" ! file name
        fullpath = trim(cwdstring)//system_infile ! full path
        print*, "fullpath = ", fullpath
        system_infile = fullpath

        ! check if file is there
        inquire(file=system_infile, exist=file_exists)
    
        ! give error and exit code
        if (.not.file_exists) then
            write(0,*) 'Error cannot find file: system_error.in'
            stop 1
        end if
        
        ! open, read and close 
        open(newunit=unit, file=system_infile, iostat=reason)
        CALL read_system(unit)
        close(unit)
            !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! !-----------------------------------------------
        ! ! get data from system_error.in
        ! !-----------------------------------------------
        ! ! name infile
        ! system_infile = "/test/data/system_error.in" ! file name
        ! fullpath = trim(cwdstring)//system_infile ! full path
        ! print*, "fullpath = ", fullpath
        ! system_infile = fullpath

        ! ! check if file is there
        ! inquire(file=system_infile, exist=file_exists)
    
        ! ! give error and exit code
        ! if (.not.file_exists) then
        !     write(0,*) 'Error cannot find file: system.in'
        !     stop 1
        ! end if
        
        ! ! open, read and close
        ! open(newunit=unit, file=system_infile, iostat=reason)
        ! CALL read_system(unit)
        ! close(unit)
        !     !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    end subroutine test_read_mesh

    subroutine test_read_tempdep_table(success)
        implicit none
        logical :: success
        character(len=1024) :: tempdep_infile
        integer :: newunit, reason, unit
        logical :: file_exists
        character(len=1024) :: cwdstring, fullpath
        integer(int12) :: ix, iy,iz, index

        ix =1
        iy =1
        iz =1
        index = 1
        call getcwd(cwdstring)

        !-----------------------------------------------
        ! get data from tempdep.in
        !-----------------------------------------------
        ! name infile
        tempdep_infile = "/test/data/tempdep.in" ! file name
        fullpath = trim(cwdstring)//tempdep_infile ! full path
        print*, "fullpath = ", fullpath
        tempdep_infile = fullpath

        ! check if file is there
        inquire(file=tempdep_infile, exist=file_exists)
    
        ! give error and exit code
        if (.not.file_exists) then
            write(0,*) 'Error cannot find file: tempdep.in'
            stop 1
        end if
        

        ! open, read and close 
        open(newunit=unit, file=tempdep_infile, iostat=reason)
        ! CALL ReadTempDepTable(tempdep_infile, ix, iy, iz, index)
        close(unit)
            !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

       
    end subroutine test_read_tempdep_table

    subroutine test_read_temp_file(success)
        implicit none
        logical :: success
        character(len=1024) :: dat_infile
        integer :: newunit, reason, unit
        logical :: file_exists
        character(len=1024) :: cwdstring, fullpath
        real(real12), dimension(:), allocatable :: T

        allocate(T(625))
    !-----------------------------------------------
    ! get data from param.in
    !-----------------------------------------------
        call getcwd(cwdstring)

    ! name infile
        dat_infile = "/test/data/TempDis.dat" ! file name
        fullpath = trim(cwdstring)//dat_infile ! full path
        print*, "fullpath = ", fullpath
        dat_infile = fullpath
        ! check if file is there
        inquire(file=dat_infile, exist=file_exists) ! check if file exists
    
        ! give error and exit code
        if (.not.file_exists) then
            write(0,*) 'Error cannot find file: TempDis.dat'
            stop 1
        end if
        
        ! open, read and close param.in
        open(newunit=unit, file=dat_infile, iostat=reason)
        CALL read_temp_file(dat_infile, T)
        close(unit)
        
    
    end subroutine test_read_temp_file

    subroutine test_power_lookup(success)
        implicit none
        logical :: success
        real(real12) :: tol, p
        integer(int12) :: icase

        tol = 1.0e-12_real12
        icase = 100_int12

        ! Set up a minimal grid that uses functional case 100
        nx = 1; ny = 1; nz = 1
        if (allocated(grid)) deallocate(grid)
        allocate(grid(nx,ny,nz))
        grid%iheater = 100

        ! Load power_100.in from the test data directory
        if (allocated(power_profiles)) deallocate(power_profiles)
        input_dir = './test/data'
        call read_power_files()

        ! On the first tabulated point (now at t=-1)
        p = power_lookup(icase, -1.0_real12)
        if (abs(p - 0.0_real12) .gt. tol) then
            write(0,*) 'Error: power_lookup(t=-1) = ', p, ', expected 0.0'
            stop 1
        end if

        ! Extrapolate before the first segment: slope = 10, so t=-2 -> -10
        p = power_lookup(icase, -2.0_real12)
        if (abs(p + 10.0_real12) .gt. tol) then
            write(0,*) 'Error: power_lookup(t=-2) = ', p, ', expected -10.0'
            stop 1
        end if

        ! On the second tabulated point (t=0)
        p = power_lookup(icase, 0.0_real12)
        if (abs(p - 10.0_real12) .gt. tol) then
            write(0,*) 'Error: power_lookup(t=0) = ', p, ', expected 10.0'
            stop 1
        end if

        ! Initial gradient at t=0: (P(0)-P(-1)) / (0 - (-1)) = 10.0
        p = (power_lookup(icase, 0.0_real12) - power_lookup(icase, -1.0_real12)) / &
            (0.0_real12 - (-1.0_real12))
        if (abs(p - 10.0_real12) .gt. tol) then
            write(0,*) 'Error: initial gradient dP/dt at t=0 = ', p, ', expected 10.0'
            stop 1
        end if

        ! Mid-interval interpolation
        p = power_lookup(icase, 0.5_real12)
        if (abs(p - 15.0_real12) .gt. tol) then
            write(0,*) 'Error: power_lookup(t=0.5) = ', p, ', expected 15.0'
            stop 1
        end if

        p = power_lookup(icase, 2.0_real12)
        if (abs(p - 10.0_real12) .gt. tol) then
            write(0,*) 'Error: power_lookup(t=2) = ', p, ', expected 10.0'
            stop 1
        end if

        ! On last point
        p = power_lookup(icase, 3.0_real12)
        if (abs(p - 0.0_real12) .gt. tol) then
            write(0,*) 'Error: power_lookup(t=3) = ', p, ', expected 0.0'
            stop 1
        end if

        ! After the last point -> heater off
        p = power_lookup(icase, 5.0_real12)
        if (abs(p - 0.0_real12) .gt. tol) then
            write(0,*) 'Error: power_lookup(t=5) = ', p, ', expected 0.0'
            stop 1
        end if

        write(*,*) "test_power_lookup: PASSED"
    end subroutine test_power_lookup

    subroutine run_all_tests(success)
        implicit none
        logical :: success
        
        write(*,*) "Starting mod_inputs tests..."
        write(*,*) "-----------------------------"
        
        call test_read_param(success)
        call test_read_mat(success)
        call test_read_mesh(success)
        call test_read_temp_file(success)
        call test_power_lookup(success)
        ! call test_read_tempdep_table(success)
        
    end subroutine run_all_tests

end module test_mod_inputs

program main
    use test_mod_inputs
    use mpi
    implicit none
    logical :: success
    integer :: ierr

    success = .true.
    call MPI_Init(ierr)
    call run_all_tests(success)
    call MPI_Finalize(ierr)
    if (success) then
        write(*,*) "All tests passed"
    else
        write(0,*) "Some tests failed"
        stop 1
    end if
end program main
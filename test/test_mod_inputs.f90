module test_mod_inputs
    use constants, only: real12, int12
    use inputs, only: read_param, read_mat, read_system
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
        
    
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

            !-----------------------------------------------
    ! get data from param_error.in
    !-----------------------------------------------
        call getcwd(cwdstring)

    ! name infile
        param_infile = "/test/data/param_error.in" ! file name
        fullpath = trim(cwdstring)//param_infile ! full path
        print*, "fullpath = ", fullpath
        param_infile = fullpath
        ! check if file is there
        inquire(file=param_infile, exist=file_exists) ! check if file exists
    
        ! give error and exit code
        if (.not.file_exists) then
            write(0,*) 'Error cannot find file: param_error.in'
            stop 1
        end if
        
        ! open, read and close param.in
        open(newunit=unit, file=param_infile, iostat=reason)
        CALL read_param(unit)
        close(unit)
        
    
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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

        !-----------------------------------------------
        ! get data from mat_error.in
        !-----------------------------------------------
        ! name infile
        mat_infile = "/test/data/mat_error.in" ! file name
        fullpath = trim(cwdstring)//mat_infile ! full path
        print*, "fullpath = ", fullpath
        mat_infile = fullpath

        ! check if file is there
        inquire(file=mat_infile, exist=file_exists)
    
        ! give error and exit code
        if (.not.file_exists) then
            write(0,*) 'Error cannot find file: mat_error.in'
            stop 1
        end if
        
        ! open, read and close mat.in
        open(newunit=unit, file=mat_infile, iostat=reason)
        CALL read_mat(unit)
        close(unit)
            !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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

        !-----------------------------------------------
        ! get data from system_error.in
        !-----------------------------------------------
        ! name infile
        system_infile = "/test/data/system_error.in" ! file name
        fullpath = trim(cwdstring)//system_infile ! full path
        print*, "fullpath = ", fullpath
        system_infile = fullpath

        ! check if file is there
        inquire(file=system_infile, exist=file_exists)
    
        ! give error and exit code
        if (.not.file_exists) then
            write(0,*) 'Error cannot find file: system.in'
            stop 1
        end if
        
        ! open, read and close
        open(newunit=unit, file=system_infile, iostat=reason)
        CALL read_system(unit)
        close(unit)
            !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    end subroutine test_read_mesh


    subroutine run_all_tests(success)
        implicit none
        logical :: success
        
        write(*,*) "Starting mod_inputs tests..."
        write(*,*) "-----------------------------"
        
        call test_read_param(success)
        call test_read_mat(success)
        call test_read_mesh(success)
        
    end subroutine run_all_tests

end module test_mod_inputs

program main
    use test_mod_inputs
    implicit none
    logical :: success
    
    call run_all_tests(success)
    if (success) then
        write(*,*) "All tests passed"
    else
        write(0,*) "Some tests failed"
        stop 1
    end if
end program main
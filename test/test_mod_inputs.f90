module test_mod_inputs
    use constants, only: real12, int12
    use inputs, only: read_param, read_mat, read_system
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

    subroutine run_all_tests(success)
        implicit none
        logical :: success
        
        write(*,*) "Starting mod_inputs tests..."
        write(*,*) "-----------------------------"
        
        call test_read_param(success)
        call test_read_mat(success)
        call test_read_mesh(success)
        call test_read_temp_file(success)
        ! call test_read_tempdep_table(success)
        
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

program test_mod_output
    use output, only: last_log
    use constants, only: real12, int12, TINY, fields
    use inputs, only: nx,ny,nz, time_step, grid, NA, Check_Steady_State, ntime, WriteToTxt
    use inputs, only: Test_Run, freq, RunName, FullRestart, IVERB, write_every
    use inputs, only: start_ix, end_ix, start_iy, end_iy, start_iz, end_iz
    use globe_data, only: Temp_p,Temp_pp, heat, heated_volume
    implicit none

    ! Test variables
    integer(int12) :: test_itime, newunit
    character(len=1024) :: test_logname, test_outdir, cwdstring, full_path
    logical :: file_exists, success

    ! Initialize test values
    nx = 2
    ny = 2  
    nz = 2
    NA = nx*ny*nz
    time_step = 0.1_real12
    Test_Run = .false.
    Check_Steady_State = .true.
    write_every = 1
    WriteToTxt = .true.
    ntime = 10
    freq = 0.0
    RunName = "test"
    IVERB = 4
    
    start_ix = 1
    end_ix = 2
    start_iy = 1 
    end_iy = 2
    start_iz = 1
    end_iz = 2

    allocate(Temp_p(NA))
    allocate(Temp_pp(NA))
    Temp_p = 1.0_real12
    Temp_pp = 0.0_real12
    heat = 100.0_real12
    heated_volume = 1.0_real12

    success = .true.

    ! Test last_log subroutine
    call getcwd(cwdstring)
    write(*,*) "Current working directory: ", trim(cwdstring)
    test_outdir = "/test_outputs/"
    test_outdir = trim(adjustl(test_outdir))
    print *, "Test outdir: ", test_outdir
    cwdstring = trim(adjustl(cwdstring))
    full_path = trim(cwdstring) // trim(test_outdir)
    test_outdir = full_path
    call system("mkdir -p " // trim(test_outdir))
    
    !Check directory creation
    inquire(file=test_outdir, exist=file_exists)
    if (.not. file_exists) then
        write(0,*) "Test last_log, dir creation: FAILED"
        success = .false.
    else
        write(*,*) "Test last_log, dir creation: PASSED"
    endif

    !Check file creation

    call last_log(test_logname, test_outdir)
    print *, "Test logname: ", test_logname
    if (test_logname == "") then
        write(0,*) "Test last_log: FAILED"
        success = .false.
    else
        write(*,*) "Test last_log: PASSED"
    endif


    ! Cleanup
    call system("rm -rf " // trim(test_outdir))

    deallocate(Temp_p)
    deallocate(Temp_pp)

    if (success) then
        write(*,*) "PASS: test_mod_output"
    else
        write(0,*) "FAIL: test_mod_output"
        stop 1 
    endif

end program test_mod_output

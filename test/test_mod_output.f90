program test_mod_output
    use output, only: last_log, data_write
    use constants, only: real12, int12
    use inputs, only: nx, ny, nz, NA, time_step, Test_Run, Check_Steady_State, &
              write_every, WriteToTxt, ntime, freq, RunName, IVERB, start_ix, end_ix, &
                start_iy, end_iy, start_iz, end_iz
    use globe_data, only: Temp_p, Temp_pp, heat, heated_volume
    implicit none

    ! Test variables
    integer(int12) :: test_itime
    character(len=1024) :: test_logname, test_outdir
    logical :: file_exists

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
    freq = 1.0
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

    ! Test last_log subroutine
    test_outdir = "./test_outputs/"
    call system("mkdir -p " // trim(test_outdir))
    call last_log(test_logname, test_outdir)
    inquire(file=test_logname, exist=file_exists)
    if (.not. file_exists) then
        write(*,*) "Test last_log: FAILED"
    else
        write(*,*) "Test last_log: PASSED"
        stop 1  
    endif

    ! Test data_write subroutine
    test_itime = 1
    call system("mkdir -p ./outputs/")
    call data_write(test_itime)
    inquire(file="./outputs/TempDis.dat", exist=file_exists)
    if (file_exists) then
        write(*,*) "Test data_write outputs: PASSED"
    else
        write(*,*) "Test data_write outputs: FAILED"
        stop 1
    endif

    ! Cleanup
    call system("rm -rf ./test_outputs/")
    call system("rm -rf ./outputs/*")
    call system("rm -rf ./outputs/")
    deallocate(Temp_p)
    deallocate(Temp_pp)

    write(*,*) "PASS: test_mod_output"

end program test_mod_output
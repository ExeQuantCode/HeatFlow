program test_mod_evolve
    use constants, only: real12, int12, TINY
    use evolution, only: simulate
    use inputs, only: NA, icattaneo, isteady, nx, ny, nz, IVERB, T_System, time_step, grid, power_in
    use globe_data, only: Temp_p, Temp_pp, inverse_time, heat, lin_rhoc
    implicit none

    ! Test variables
    integer(int12) :: itime
    logical :: test_passed

    ! Initialize test environment
    call setup_test()
    
    ! Test 1: Basic simulation step
    write(*,*) "Test 1: Basic simulation step"
    itime = 1
    test_passed = .true.
    
    call simulate(itime)
    
    ! Check results are not NaN
    if (any(isnan(Temp_p))) then
        write(*,*) "Test 1 FAILED: NaN values in temperature"
        test_passed = .false.
        stop 1
    endif
    
    ! Check temperature is within physical bounds
    if (any(Temp_p < 0.0_real12)) then
        write(*,*) "Test 1 FAILED: Negative temperature values"
        test_passed = .false.
        stop 1
    endif
    
    if (test_passed) then
        write(*,*) "Test 1 PASSED"
    endif

    ! Cleanup test environment
    call cleanup_test()

contains

    subroutine setup_test()
        ! Initialize required variables and arrays
        NA = 100
        nx = 10
        ny = 10 
        nz = 1
        IVERB = 0
        icattaneo = 0
        isteady = 0
        
        allocate(Temp_p(NA))
        allocate(Temp_pp(NA))
        allocate(lin_rhoc(NA))
        
        Temp_p = 300.0_real12  ! Room temperature
        Temp_pp = 300.0_real12
        lin_rhoc = 1.0_real12
        inverse_time = 1.0_real12/time_step
        heat = 0.0_real12
    end subroutine setup_test
    
    subroutine cleanup_test()
        ! Deallocate arrays
        if (allocated(Temp_p)) deallocate(Temp_p)
        if (allocated(Temp_pp)) deallocate(Temp_pp)
        if (allocated(lin_rhoc)) deallocate(lin_rhoc)
    end subroutine cleanup_test

end program test_mod_evolve
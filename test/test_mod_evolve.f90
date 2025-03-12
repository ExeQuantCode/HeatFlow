program test_mod_evolve
    use constants, only: real12, int12, TINY
    use evolution, only: simulate
    use inputs, only: NA, icattaneo, isteady, nx, ny, nz, IVERB, T_System, time_step, grid, power_in
    use inputs, only: TempDepProp, Periodicx, Periodicy, Periodicz
    use inputs, only: kappaBoundx1, kappaBoundy1, kappaBoundz1, kappaBoundNx, kappaBoundNy, kappaBoundNz
    use inputs, only: T_Bathx1, T_Bathx2, T_Bathy1, T_Bathy2, T_Bathz1, T_Bathz2, T_BathCG
    use heating, only: heater
    use globe_data, only: Temp_p, Temp_pp, inverse_time, heat, lin_rhoc
    use setup, only: sparse_Hmatrix
    implicit none

    ! Test variables
    integer(int12) :: itime
    logical :: success

    ! Initialize test environment
    call setup_test()
    
    ! Test 1: Basic simulation step
    write(*,*) "Test 1: Basic simulation step"
    itime = 1
    success = .true.
    
    call simulate(itime)

    ! Check results are not NaN
    if (any(isnan(Temp_p))) then
        write(0,*) "Test 1 FAILED: NaN values in temperature"
        success = .false.
    endif
    
    ! Check temperature is within physical bounds
    if (any(Temp_p .lt. 0.0_real12)) then
        write(0,*) "Test 1 FAILED: Negative temperature values"
        success = .false.
    endif
    
    if (success) then
        write(0,*) "Test 1 PASSED"
    endif

    ! Cleanup test environment
    call cleanup_test()

contains

    subroutine setup_test()
        implicit none

        ! Initialize required variables and arrays
        NA = 100
        nx = 10
        ny = 10 
        nz = 1
        IVERB = 6
        icattaneo = 1
        isteady = 1
        
        allocate(Temp_p(NA))
        allocate(Temp_pp(NA))
        allocate(lin_rhoc(NA))
        allocate(grid(nx, ny, nz))

        Temp_p = 300.0_real12  ! Room temperature
        Temp_pp = 300.0_real12
        lin_rhoc = 1.0_real12
        time_step = 1.0_real12
        inverse_time = 1.0_real12/time_step
        heat = 0.0_real12

        Periodicx = .false.
        Periodicy = .false.
        Periodicz = .false.

        kappaBoundx1 = 1.0_real12
        kappaBoundy1 = 1.0_real12
        kappaBoundz1 = 1.0_real12
        kappaBoundNx = 1.0_real12
        kappaBoundNy = 1.0_real12
        kappaBoundNz = 1.0_real12

        grid(:,:,:)%kappa = 1.0_real12
        grid(:,:,:)%Length(1) = 1.0_real12
        grid(:,:,:)%Length(2) = 1.0_real12
        grid(:,:,:)%Length(3) = 1.0_real12
        grid(:,:,:)%iheater = 1
        power_in = 1.0_real12

        T_Bathx1 = 300.0_real12
        T_Bathx2 = 300.0_real12
        T_Bathy1 = 300.0_real12
        T_Bathy2 = 300.0_real12
        T_Bathz1 = 300.0_real12
        T_Bathz2 = 300.0_real12
        T_BathCG = 0.0_real12

        TempDepProp = 1

        call sparse_Hmatrix()
    end subroutine setup_test
    
    subroutine cleanup_test()
        ! Deallocate arrays
        if (allocated(Temp_p)) deallocate(Temp_p)
        if (allocated(Temp_pp)) deallocate(Temp_pp)
        if (allocated(lin_rhoc)) deallocate(lin_rhoc)
    end subroutine cleanup_test

end program test_mod_evolve
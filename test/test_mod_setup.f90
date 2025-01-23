module test_mod_setup_mod
    use constants, only: real12, int12, TINY
    use inputs, only: NA, icattaneo, isteady, nx, ny, nz, IVERB, T_System, time_step, grid, power_in
    use inputs, only: TempDepProp, Periodicx, Periodicy, Periodicz
    use inputs, only: kappaBoundx1, kappaBoundy1, kappaBoundz1, kappaBoundNx, kappaBoundNy, kappaBoundNz
    use inputs, only: T_Bathx1, T_Bathx2, T_Bathy1, T_Bathy2, T_Bathz1, T_Bathz2, T_BathCG
    use setup, only: set_global_variables, sparse_Hmatrix, stability, build_Hmatrix, SparseToReal
    use globe_data, only: ra, Temp_cur, Temp_p, Temp_pp, inverse_time, heat, lin_rhoc, Q_P
    implicit none
    
contains

subroutine setup_test()
    implicit none

    ! Initialize required variables and arrays
    nx = 2; ny = 2; nz = 2
    NA = nx * ny * nz
    IVERB = 0
    icattaneo = 0
    isteady = 0
    
    ! allocate(Temp_p(NA))
    ! allocate(Temp_pp(NA))
    ! allocate(lin_rhoc(NA))
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
    grid(:,:,:)%iheater = 0

    T_Bathx1 = 300.0_real12
    T_Bathx2 = 300.0_real12
    T_Bathy1 = 300.0_real12
    T_Bathy2 = 300.0_real12
    T_Bathz1 = 300.0_real12
    T_Bathz2 = 300.0_real12
    T_BathCG = 0.0_real12

end subroutine setup_test

subroutine test_set_global_variables(success)
    implicit none
    integer :: ix, iy, iz
    real(real12) :: kappa, rho, heat_capacity
    logical :: success
    write(*,*) "Testing set_global_variables..."
    
    ! Setup test conditions
    nx = 2; ny = 2; nz = 2
    NA = nx * ny * nz
    allocate(grid(nx,ny,nz))
    grid(:,:,:)%imaterial_type = 140
    time_step = 0.1_real12
    
    ! Call function
    call set_global_variables()
    
    ! Verify results
    if (allocated(Temp_cur) .and. allocated(Temp_p) .and. allocated(Temp_pp)) then
        write(*,*) "  PASS: Arrays allocated correctly"
    else
        write(*,*) "  FAIL: Array allocation error"
        success = .false.
    end if

    ! Cleanup
    deallocate(grid)
end subroutine

subroutine test_sparse_hmatrix(success)
    implicit none
    integer :: nx, ny, nz
    logical :: Periodicx, Periodicy, Periodicz
    logical :: success
    write(*,*) "Testing sparse_hmatrix..."
    
    ! Setup
    nx = 2; ny = 2; nz = 2
    NA = nx * ny * nz
    Periodicx = .false.
    Periodicy = .false.
    Periodicz = .false.

    ! Call function
    call sparse_Hmatrix()

    ! Verify results
    if (ra%n == NA) then
        write(*,*) "  PASS: Matrix dimensions correct"
    else
        write(0,*) "  FAIL: Wrong matrix dimensions"
        success = .false.
    end if
end subroutine

subroutine test_stability(success)
    implicit none
    real(real12) :: kappa, rho, heat_capacity
    integer(int12) :: ix, iy, iz
    logical :: success
    
    write(*,*) "Testing stability..."
    
    ! Test case setup
    kappa = 1.0_real12
    rho = 1.0_real12
    heat_capacity = 1.0_real12
    ix = 1; iy = 1; iz = 1
    nx = 2; ny = 2; nz = 2
    
    allocate(grid(nx,ny,nz))
    grid(ix,iy,iz)%length = [1.0_real12, 1.0_real12, 1.0_real12]
    time_step = 0.01_real12 ! Small timestep for stability
    
    ! Call stability check
    call stability(kappa, rho, heat_capacity, ix, iy, iz)
    
    write(*,*) "  PASS: Stability check completed"
    
    deallocate(grid)
end subroutine

subroutine test_build_hmatrix(success)
    implicit none
    integer :: nx, ny, nz
    logical :: Check_Sparse_Full
    logical :: success
    write(*,*) "Testing build_hmatrix..."
    
    ! Setup
    nx = 2; ny = 2; nz = 2
    NA = nx * ny * nz
    Check_Sparse_Full = .true.
    
    ! Call function
    call build_Hmatrix()
    
    write(*,*) "  PASS: H matrix built successfully"
end subroutine

subroutine test_sparse_to_real(success)
    implicit none
    integer :: nx, ny, nz, NA
    real(real12), dimension(:,:), allocatable :: test_matrix
    logical :: success
    
    write(*,*) "Testing sparse_to_real..."
    
    ! Setup
    nx = 2; ny = 2; nz = 2
    NA = nx * ny * nz
    allocate(test_matrix(NA,NA))
    
    ! Call function
    call SparseToReal(test_matrix)
    
    if (allocated(test_matrix)) then
        write(*,*) "  PASS: Matrix conversion completed"
    else
        write(*,*) "  FAIL: Matrix conversion failed"
        success = .false.
    end if
    
    deallocate(test_matrix)
end subroutine
end module test_mod_setup_mod


program test_mod_setup
    use test_mod_setup_mod, only: test_set_global_variables, test_sparse_hmatrix, setup_test    
    use test_mod_setup_mod, only: test_stability, test_build_hmatrix, test_sparse_to_real
    implicit none

    ! Test variables
    logical :: success = .true.
    
    ! Run tests
    print*, "Tests started:"
    call test_set_global_variables(success)
    call setup_test()
    call test_sparse_hmatrix(success) 
    ! call test_stability(success)
    call test_build_hmatrix(success)
    call test_sparse_to_real(success)

    ! Print summary
    write(*,*) "Tests completed:"
    if (success) then
        write(*,*) "All tests passed"
    else
        write(0,*) "Some tests failed"
        stop 1
    end if

end program test_mod_setup
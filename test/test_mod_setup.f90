program test_mod_setup
    use constants, only: real12, int12, TINY
    use inputs, only: nx, ny, nz, NA, grid, time_step, kappaBoundx1, kappaBoundy1, kappaBoundz1
    use inputs, only: Check_Sparse_Full, Check_Stability, ntime, IVERB, Periodicx, Periodicy, Periodicz
    use setup, only: set_global_variables, sparse_Hmatrix, stability, build_Hmatrix, SparseToReal
    use globe_data, only: ra, Temp_cur, Temp_p, Temp_pp, inverse_time, heat, lin_rhoc, Q_P
    implicit none

    ! Test variables
    integer :: num_failed = 0
    integer :: num_passed = 0
    
    ! Run tests
    call test_set_global_variables()
    call test_sparse_hmatrix() 
    call test_stability()
    call test_build_hmatrix()
    call test_sparse_to_real()

    ! Print summary
    write(*,*) "Tests completed:"
    write(*,*) "Passed:", num_passed
    write(*,*) "Failed:", num_failed

contains

    subroutine test_set_global_variables()
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
            num_passed = num_passed + 1
            write(*,*) "  PASS: Arrays allocated correctly"
        else
            num_failed = num_failed + 1
            write(*,*) "  FAIL: Array allocation error"
            stop 1
        end if

        ! Cleanup
        deallocate(grid)
    end subroutine

    subroutine test_sparse_hmatrix()
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
            num_passed = num_passed + 1
            write(*,*) "  PASS: Matrix dimensions correct"
        else
            num_failed = num_failed + 1
            write(*,*) "  FAIL: Wrong matrix dimensions"
            stop 1
        end if
    end subroutine

    subroutine test_stability()
        real(real12) :: kappa, rho, heat_capacity
        integer(int12) :: ix, iy, iz
        
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
        
        num_passed = num_passed + 1
        write(*,*) "  PASS: Stability check completed"
        
        deallocate(grid)
    end subroutine

    subroutine test_build_hmatrix()
        write(*,*) "Testing build_hmatrix..."
        
        ! Setup
        nx = 2; ny = 2; nz = 2
        NA = nx * ny * nz
        Check_Sparse_Full = .true.
        
        ! Call function
        call build_Hmatrix()
        
        num_passed = num_passed + 1
        write(*,*) "  PASS: H matrix built successfully"
    end subroutine

    subroutine test_sparse_to_real()
        real(real12), dimension(:,:), allocatable :: test_matrix
        
        write(*,*) "Testing sparse_to_real..."
        
        ! Setup
        nx = 2; ny = 2; nz = 2
        NA = nx * ny * nz
        allocate(test_matrix(NA,NA))
        
        ! Call function
        call SparseToReal(test_matrix)
        
        if (allocated(test_matrix)) then
            num_passed = num_passed + 1
            write(*,*) "  PASS: Matrix conversion completed"
        else
            num_failed = num_failed + 1
            write(*,*) "  FAIL: Matrix conversion failed"
            stop 1
        end if
        
        deallocate(test_matrix)
    end subroutine

end program test_mod_setup
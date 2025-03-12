module test_boundary_vector
    use constants, only: real12, int12
    use inputs, only: NA,nx,ny,nz, grid, kappaBoundx1, kappaBoundy1, kappaBoundz1
    use inputs, only: kappaBoundNx, kappaBoundNy, kappaBoundNz
    use inputs, only: Periodicx, Periodicy, Periodicz
    use inputs, only: T_Bathx1, T_Bathx2, T_Bathy1, T_Bathy2, T_Bathz1, T_Bathz2, T_BathCG
    use globe_data, only: Temp_p
    use boundary_vector, only: boundary, constantboundarytempgrad
    implicit none

contains
    subroutine test_boundary_terms(success)
        implicit none   
        real(real12), dimension(:), allocatable :: B
        real(real12) :: expected
        integer :: i, j, k
        logical :: success

        ! Initialize test data
        NA = 27
        nx = 3
        ny = 3 
        nz = 3
        allocate(B(NA))
        allocate(grid(nx,ny,nz))
        allocate(Temp_p(NA))
        
        B = 0.0_real12
        Temp_p = 1.0_real12
        
        ! Set test grid properties
        do i = 1,nx
            do j = 1,ny
                do k = 1,nz
                    grid(i,j,k)%kappa = 1.0_real12
                    grid(i,j,k)%Length = [1.0_real12, 1.0_real12, 1.0_real12]
                end do
            end do
        end do

        ! Test non-periodic boundaries
        Periodicx = .false.
        Periodicy = .false.
        Periodicz = .false.
        
        kappaBoundx1 = 1.0_real12
        kappaBoundy1 = 1.0_real12
        kappaBoundz1 = 1.0_real12
        kappaBoundNx = 1.0_real12
        kappaBoundNy = 1.0_real12
        kappaBoundNz = 1.0_real12
        
        T_Bathx1 = 300.0_real12
        T_Bathx2 = 300.0_real12
        T_Bathy1 = 300.0_real12
        T_Bathy2 = 300.0_real12  
        T_Bathz1 = 300.0_real12
        T_Bathz2 = 300.0_real12

        ! Call boundary subroutine
        call boundary(B)

        ! Test boundary values
        expected = 900.0_real12 ! For corner point Bx1+By1+Bz1 = 300+300+300 = 900
        if (abs(B(1) - expected) > 1e-10) then
            write(0,*) "Test failed: Corner boundary value incorrect"
            write(0,*) "Expected: ", expected
            write(0,*) "Actual: ", B(1)
            success = .false.
        else
            write(*,*) "Test passed: Corner boundary value correct"
        end if

        deallocate(B)
        deallocate(grid)
        deallocate(Temp_p)

    end subroutine test_boundary_terms

    subroutine test_constantboundarytempgrad(success)
        implicit none
        real(real12) :: result
        logical :: success

        ! Test constant gradient calculation
        NA = 1
        allocate(Temp_p(NA))
        Temp_p(1) = 100.0_real12
        T_BathCG = 2.0_real12
        
        result = constantboundarytempgrad(1_int12)
        
        if (abs(result - 50.0_real12) > 1e-10) then
            write(0,*) "Test failed: Constant gradient calculation incorrect"
            write(0,*) "Expected: ", 50.0_real12
            write(0,*) "Actual: ", result
            success = .false.
        else
            write(*,*) "Test passed: Constant gradient calculation correct"
        end if
        
        deallocate(Temp_p)
        
    end subroutine test_constantboundarytempgrad

end module test_boundary_vector

program run_tests
    use test_boundary_vector
    implicit none
    logical :: success = .true.

    
    call test_boundary_terms(success)
    call test_constantboundarytempgrad(success)

    if (success) then
        write(*,*) "All tests passed"
    else
        write(0,*) "One or more tests failed"
        stop 1
    end if
    
end program run_tests
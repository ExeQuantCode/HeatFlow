module test_boundary_vector
    use constants, only: real12, int12
    use boundary_vector, only: boundary, constantboundarytempgrad
    use inputs, only: Periodicx, Periodicy, Periodicz, NA, nx, ny, nz, grid, Temp_p, &
              kappaBoundx1, kappaBoundy1, kappaBoundz1, kappaBoundNx, kappaBoundNy, kappaBoundNz, &
                       T_Bathx1, T_Bathx2, T_Bathy1, T_Bathy2, T_Bathz1, T_Bathz2, T_BathCG
    use globe_data, only: Temp_p
    implicit none

contains
    subroutine test_boundary_terms()
        real(real12), dimension(:), allocatable :: B
        real(real12) :: expected
        integer :: i, j, k

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
        expected = 300.0_real12 ! For corner point
        if (abs(B(1) - expected) > 1e-10) then
            write(*,*) "Test failed: Corner boundary value incorrect"
            write(*,*) "Expected: ", expected
            write(*,*) "Actual: ", B(1)
            stop 1
        else
            write(*,*) "Test passed: Corner boundary value correct"
        end if

        deallocate(B)
        deallocate(grid)
        deallocate(Temp_p)

    end subroutine test_boundary_terms

    subroutine test_constantboundarytempgrad()
        real(real12) :: result
        
        ! Test constant gradient calculation
        NA = 1
        allocate(Temp_p(NA))
        Temp_p(1) = 100.0_real12
        T_BathCG = 2.0_real12
        
        result = constantboundarytempgrad(1_int12)
        
        if (abs(result - 50.0_real12) > 1e-10) then
            write(*,*) "Test failed: Constant gradient calculation incorrect"
        else
            write(*,*) "Test passed: Constant gradient calculation correct"
        end if
        
        deallocate(Temp_p)
        
    end subroutine test_constantboundarytempgrad

end module test_boundary_vector

program run_tests
    use test_boundary_vector
    implicit none
    
    call test_boundary_terms()
    call test_constantboundarytempgrad()
    
end program run_tests
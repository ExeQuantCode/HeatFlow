module test_hmatrixmod
    use constants, only: real12, int12
    use inputs, only: time_step, grid
    use inputs, only: isteady, icattaneo, kappaBoundx1, kappaBoundy1, kappaBoundz1
    use inputs, only: kappaBoundNx, kappaBoundNy, kappaBoundNz, Periodicx, Periodicy, Periodicz
    use globe_data, only: inverse_time, lin_rhoc
    use hmatrixmod, only: altmod, calculate_alpha, calculate_conductivity
    implicit none

contains

    subroutine test_altmod(success)
        implicit none
        integer(int12) :: result
        logical :: success
        
        ! Test case 1: Normal modulo operation
        result = altmod(7_int12, 3_int12)
        if (result /= 1) then
            write(0,*) "FAIL: altmod(7,3) expected 1, got", result
            success = .false.
        else
            write(*,*) "PASS: altmod(7,3) test"
        endif

        ! Test case 2: When modulo is zero
        result = altmod(6_int12, 3_int12)
        if (result /= 3) then
            write(0,*) "FAIL: altmod(6,3) expected 3, got", result
            success = .false.
        else
            write(*,*) "PASS: altmod(6,3) test"
        endif
    end subroutine test_altmod

    subroutine test_calculate_alpha(success)
        implicit none
        real(real12) :: result
        integer(int12) :: test_x, test_y, test_z, test_i
        logical :: success

        
        ! Setup test conditions
        isteady = 0
        icattaneo = 0
        test_x = 1
        test_y = 1
        test_z = 1
        test_i = 1
        allocate(grid(test_x, test_y, test_z))
        allocate(lin_rhoc(1))

        grid(1,1,1)%tau = 0.0_real12  
        lin_rhoc(1) = 1.0_real12

        result = calculate_alpha(test_x, test_y, test_z, test_i)

        ! Basic test - with isteady = 0 and icattaneo = 0
        if (abs(result - inverse_time*lin_rhoc(test_i)) > 1.0e-10_real12) then
            write(0,*) "FAIL: calculate_alpha basic test"
            success = .false.
        else
            write(*,*) "PASS: calculate_alpha basic test"
        endif
    end subroutine test_calculate_alpha

    subroutine test_calculate_conductivity(success)
        implicit none
        real(real12) :: result
        integer(int12) :: x1, y1, z1, x2, y2, z2
        logical :: success
        
        ! Test interior points
        x1 = 2
        y1 = 2
        z1 = 2
        x2 = 2
        y2 = 2
        z2 = 3

        result = calculate_conductivity(x1, y1, z1, x2, y2, z2)
        
        ! Basic test - should return non-zero conductivity for adjacent interior points
        if (result <= 0.0_real12) then
            write(0,*) "FAIL: calculate_conductivity interior test"
            success = .false.
        else
            write(*,*) "PASS: calculate_conductivity interior test"
        endif
    end subroutine test_calculate_conductivity

    subroutine run_all_tests(success)
        implicit none
        logical :: success
        
        write(*,*) "Starting hmatrixmod tests..."
        write(*,*) "-----------------------------"
        
        call test_altmod(success)
        call test_calculate_alpha(success)
        call test_calculate_conductivity(success)
        
    end subroutine run_all_tests

end module test_hmatrixmod

program main
    use test_hmatrixmod
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
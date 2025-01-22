module test_hmatrixmod
    use constants, only: real12, int12
    use hmatrixmod
    implicit none

contains

    subroutine test_altmod()
        implicit none
        integer(int12) :: result
        
        ! Test case 1: Normal modulo operation
        result = altmod(7_int12, 3_int12)
        if (result /= 1) then
            print *, "FAIL: altmod(7,3) expected 1, got", result
            stop 1
        else
            print *, "PASS: altmod(7,3) test"
        endif

        ! Test case 2: When modulo is zero
        result = altmod(6_int12, 3_int12)
        if (result /= 3) then
            print *, "FAIL: altmod(6,3) expected 3, got", result
            stop 1
        else
            print *, "PASS: altmod(6,3) test"
        endif
    end subroutine test_altmod

    subroutine test_calculate_alpha()
        implicit none
        real(real12) :: result
        integer(int12) :: test_x, test_y, test_z, test_i
        
        ! Setup test conditions
        isteady = 0
        icattaneo = 0
        test_x = 1
        test_y = 1
        test_z = 1
        test_i = 1

        result = calculate_alpha(test_x, test_y, test_z, test_i)
        
        ! Basic test - with isteady = 0 and icattaneo = 0
        if (abs(result - inverse_time*lin_rhoc(test_i)) > 1.0e-10_real12) then
            print *, "FAIL: calculate_alpha basic test"
            stop 1
        else
            print *, "PASS: calculate_alpha basic test"
        endif
    end subroutine test_calculate_alpha

    subroutine test_calculate_conductivity()
        implicit none
        real(real12) :: result
        integer(int12) :: x1, y1, z1, x2, y2, z2
        
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
            print *, "FAIL: calculate_conductivity interior test"
            stop 1
        else
            print *, "PASS: calculate_conductivity interior test"
        endif
    end subroutine test_calculate_conductivity

    subroutine run_all_tests()
        implicit none
        
        print *, "Starting hmatrixmod tests..."
        print *, "-----------------------------"
        
        call test_altmod()
        call test_calculate_alpha()
        call test_calculate_conductivity()
        
        print *, "-----------------------------"
        print *, "Finished hmatrixmod tests."
    end subroutine run_all_tests

end module test_hmatrixmod

program main
    use test_hmatrixmod
    implicit none
    
    call run_all_tests()
end program main
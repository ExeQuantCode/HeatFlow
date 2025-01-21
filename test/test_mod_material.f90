program test_mod_material
    use constants, only: real12, int12
    use materials, only: material
    implicit none

    integer(int12) :: imaterial_type
    real(real12) :: kappa, kappa3D, h_conv, heat_capacity, rho, sound_speed, tau, em
    logical :: test_passed

    ! Test case 1: Silicon (140)
    imaterial_type = 140
    call material(imaterial_type, kappa, kappa3D, h_conv, heat_capacity, rho, sound_speed, tau, em)
    
    test_passed = abs(kappa - 130.0_real12) < 1e-6 .and. &
                                abs(kappa3D - 130.0_real12) < 1e-6 .and. &
                                abs(heat_capacity - 4200.0_real12) < 1e-6 .and. &
                                abs(rho - 2328.0_real12) < 1e-6 .and. &
                                abs(sound_speed - 8433.0_real12) < 1e-6
                                
    if (test_passed) then
        write(*,*) "Test case 1 (Silicon): PASSED"
    else
        write(*,*) "Test case 1 (Silicon): FAILED"
    endif

    ! Test case 2: Invalid material type
    imaterial_type = -1
    write(*,*) "Testing invalid material type (-1)..."
    call material(imaterial_type, kappa, kappa3D, h_conv, heat_capacity, rho, sound_speed, tau, em)
    ! Program should exit with error message

end program test_mod_material
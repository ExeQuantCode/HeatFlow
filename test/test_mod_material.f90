program test_mod_material
    use constants, only: real12, int12
    use materials, only: material
    use inputs, only: read_mat
    implicit none

    integer(int12) :: imaterial_type
    real(real12) :: kappa, kappa3D, h_conv, heat_capacity, rho, sound_speed, tau, em
    logical :: test_passed
    integer :: unit, reason, i
    logical :: file_exists
    character(len=1024) :: cwdstring, fullpath, mat_infile
    integer(int12), dimension(13) :: test_mats

    test_mats = [100, 3001, 3002, 3003, 3004, 9001, 9002, 9003, 310, 320, 330, 340, 350]
    call getcwd(cwdstring)
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
        stop 1
    endif


    ! Program should exit with error message

    ! Test case 2: All materials
    do i = 1, size(test_mats)
        imaterial_type = test_mats(i)
        call material(imaterial_type, kappa, kappa3D, h_conv, heat_capacity, rho, sound_speed, tau, em)
        ! Program should exit with error message
    end do


    ! Test case 3: Non-existing material from file
    !-----------------------------------------------
    ! get data from mat.in
    !-----------------------------------------------
    ! name infile
    mat_infile = "/test/data/mat.in" ! file name
    fullpath = trim(cwdstring)//mat_infile ! full path
    print*, "fullpath = ", fullpath
    mat_infile = fullpath

    ! check if file is there
    inquire(file=mat_infile, exist=file_exists)

    ! give error and exit code
    if (.not.file_exists) then
        write(0,*) 'Error cannot find file: mat.in'
        stop 1
    end if
    
    ! open, read and close mat.in
    open(newunit=unit, file=mat_infile, iostat=reason)
    CALL read_mat(unit)
    close(unit)
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! Test case 4: Invalid material type
    imaterial_type = -1
    write(*,*) "Testing invalid material type (-1)..."
    call material(imaterial_type, kappa, kappa3D, h_conv, heat_capacity, rho, sound_speed, tau, em)
end program test_mod_material
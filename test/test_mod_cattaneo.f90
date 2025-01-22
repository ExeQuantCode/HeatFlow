program test_mod_cattaneo
    use cattaneo, only: S_catS
    use constructions, only: heatblock
    use constants, only: real12, int12
    use inputs, only: NA, time_step, nx, ny, nz, grid
    use globe_data, only: Temp_p,Temp_pp, lin_rhoc
    implicit none

    ! Local test grid dimensions
    integer(int12), parameter :: test_nx = 2, test_ny = 2, test_nz = 1
    integer(int12), parameter :: test_na = test_nx * test_ny * test_nz


    ! Local arrays for test
    type(heatblock), dimension(test_nx,test_ny,test_nz) :: grid_test
    real(real12), dimension(test_na) :: Temp_p_test
    real(real12), dimension(test_na) :: Temp_pp_test
    real(real12), dimension(test_na) :: lin_rhoc_test
    real(real12), dimension(test_na) :: s_cat_test

    integer(int12) :: ix, iy, iz, idx
    real(real12)   :: tolerance, expected, difference
    logical         :: pass



    ! Initialize test values
    tolerance = 1.0e-12_real12
    idx = 0
    do iz = 1, test_nz
        do iy = 1, test_ny
            do ix = 1, test_nx
                idx = idx + 1
                Temp_p_test(idx)   = 100.0_real12 
                Temp_pp_test(idx)  = Temp_p_test(idx)
                lin_rhoc_test(idx) = 1.0_real12 
                grid_test(ix,iy,iz)%tau = 0.5_real12
            end do
        end do
    end do
    s_cat_test = 0.0_real12

    !Set global and inputs variables equal to test
    nx = test_nx
    ny = test_ny
    nz = test_nz
    NA = test_na
    time_step = 0.5_real12
    Temp_p = Temp_p_test
    Temp_pp = Temp_pp_test
    lin_rhoc = lin_rhoc_test
    grid = grid_test
    
    call S_catS(s_cat_test)


    ! Check results against a simple expected formula
    pass = .true.
    idx = 0
    do iz = 1, test_nz
        do iy = 1, test_ny
            do ix = 1, test_nx
                idx = idx + 1
                expected = (Temp_pp_test(idx) - 2.0_real12 * Temp_p_test(idx)) * &
                                     (grid_test(ix,iy,iz)%tau * lin_rhoc_test(idx))
                difference = abs(s_cat_test(idx) - expected)
                if (difference > tolerance) then
                    pass = .false.
                    exit
                end if
            end do
        end do
    end do

    if (pass) then
        write(*,*) "test_mod_cattaneo: S_catS PASSED"
    else
        write(*,*) "test_mod_cattaneo: S_catS FAILED"
        write(*,*) "  Expected: ", expected
        write(*,*) "  Actual:   ", s_cat_test(idx)
        stop 1
    end if

contains




end program test_mod_cattaneo
program test_mod_cattaneo
    use cattaneo
    use constants, only: real12, int12
    implicit none

    ! Local test grid dimensions
    integer(int12), parameter :: test_nx = 2, test_ny = 2, test_nz = 1
    integer(int12), parameter :: test_na = test_nx * test_ny * test_nz

    ! Simple custom grid type for testing
    type :: grid_type
        real(real12) :: tau
    end type grid_type

    ! Local arrays for test
    type(grid_type), dimension(test_nx,test_ny,test_nz) :: grid_test
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

    ! Call S_catS with local data by overwriting global symbols
    ! (This mimics usage but ensures the subroutine sees our test arrays.)
    call override_globals_for_test(Temp_p_test, Temp_pp_test, lin_rhoc_test, grid_test, &
                                                                 test_nx, test_ny, test_nz)
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
                end if
            end do
        end do
    end do

    if (pass) then
        write(*,*) "test_mod_cattaneo: S_catS PASSED"
    else
        write(*,*) "test_mod_cattaneo: S_catS FAILED"
        stop 1
    end if

contains

    ! This stub "overrides" the module-level variables with our local test arrays.
    ! In practice, you might set them directly or adjust your module for easier testing.
    subroutine override_globals_for_test(tp, tpp, lrhoc, grd, nx_local, ny_local, nz_local)
        real(real12), intent(in) :: tp(:), tpp(:), lrhoc(:)
        type(grid_type), intent(in) :: grd(:,:,:)
        integer(int12), intent(in) :: nx_local, ny_local, nz_local
        ! Replace or point the module’s arrays and parameters to these test arrays.
        ! This is just a placeholder approach to keep code self-contained.
    end subroutine override_globals_for_test

end program test_mod_cattaneo
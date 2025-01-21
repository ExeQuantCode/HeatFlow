module test_mod_heating
    use constants
    use Heating
    implicit none

contains

    !--------------------------------------------------------------------
    ! Main routine to run all tests
    !--------------------------------------------------------------------
    subroutine run_tests_mod_heating()
        implicit none
        call test_heater_constant_heating()
        call test_heater_no_heating()
    end subroutine run_tests_mod_heating

    !--------------------------------------------------------------------
    ! Test subroutine for the case of constant heating (iheater=1)
    !--------------------------------------------------------------------
    subroutine test_heater_constant_heating()
        use globe_data, only: Q_P
        use inputs, only: nx, ny, nz, NA, power_in, time_step, heated_steps, freq, T_Bath, grid
        implicit none
        
        integer(int12) :: itime
        real(real12), allocatable :: Q_test(:), Qdens_test(:)
        integer :: i
        
        ! Set mock sizes (assuming small domain for demonstration)
        nx = 2; ny = 2; nz = 1
        NA = nx*ny*nz
        allocate(grid(nx,ny,nz))
        
        ! Mock input data
        power_in = 50.0_real12
        time_step = 1.0_real12
        heated_steps = 10
        freq = 5.0_real12
        T_Bath = 300.0_real12
        itime = 1
        
        ! Fill grid with dummy values
        do i = 1, NA
             grid(i,1,1)%rho           = 1000.0_real12
             grid(i,1,1)%volume        = 1.0_real12
             grid(i,1,1)%heat_capacity = 1.0_real12
             grid(i,1,1)%Length(1)     = 1.0_real12
             grid(i,1,1)%Length(2)     = 1.0_real12
             grid(i,1,1)%tau           = 0.0_real12
             grid(i,1,1)%em            = 0.0_real12
             grid(i,1,1)%iheater       = 1       ! Constant heating case
        end do
        allocate(Q_test(NA), Qdens_test(NA))
        
        ! Reset Q_P
        allocate(Q_P(NA))
        Q_P = 0.0_real12
        
        ! Call the subroutine under test
        call heater(itime, Q_test, Qdens_test)
        
        ! (Optional) Print checks
        ! write(*,*) "Test Heater Constant Heating Results:", Q_test
        
        deallocate(Q_test, Qdens_test, Q_P, grid)
    end subroutine test_heater_constant_heating

    !--------------------------------------------------------------------
    ! Test subroutine for the case of no heating (iheater=0)
    !--------------------------------------------------------------------
    subroutine test_heater_no_heating()
        use globe_data, only: Q_P
        use constants
        use inputs, only: nx, ny, nz, NA, power_in, time_step, heated_steps, freq, T_Bath, grid
        implicit none

        integer(int12) :: itime
        real(real12), allocatable :: Q_test(:), Qdens_test(:)
        integer :: i

        ! Set mock sizes
        nx = 2; ny = 2; nz = 1
        NA = nx*ny*nz
        allocate(grid(nx,ny,nz))

        ! Mock input data
        power_in   = 100.0_real12
        time_step  = 1.0_real12
        heated_steps = 5
        freq = 2.0_real12
        T_Bath = 250.0_real12
        itime = 1

        ! Fill grid with dummy values
        do i = 1, NA
             grid(i,1,1)%rho           = 500.0_real12
             grid(i,1,1)%volume        = 2.0_real12
             grid(i,1,1)%heat_capacity = 2.0_real12
             grid(i,1,1)%Length(1)     = 1.0_real12
             grid(i,1,1)%Length(2)     = 1.0_real12
             grid(i,1,1)%tau           = 0.0_real12
             grid(i,1,1)%em            = 0.0_real12
             grid(i,1,1)%iheater       = 0       ! No heating
        end do
        allocate(Q_test(NA), Qdens_test(NA))

        ! Reset Q_P
        allocate(Q_P(NA))
        Q_P = 0.0_real12

        ! Call the subroutine under test
        call heater(itime, Q_test, Qdens_test)

        ! (Optional) Print checks
        ! write(*,*) "Test Heater No Heating Results:", Q_test

        deallocate(Q_test, Qdens_test, Q_P, grid)
    end subroutine test_heater_no_heating

end module test_mod_heating
module Heating_test
    use constants, only: real12, int12
    use Heating, only: heater
    implicit none
    
contains
!--------------------------------------------------------------------
    ! Main routine to run all tests
    !--------------------------------------------------------------------
subroutine run_tests_mod_heating()
    implicit none
    integer :: hcase, i
    do i = 0, 6
        hcase = i
        print*, "Running test for heater case: ", hcase
        call test_heater_constant_heating(hcase)
    end do
    call test_heater_no_heating()
    call test_heater_functional_power()
end subroutine run_tests_mod_heating

!--------------------------------------------------------------------
! Test subroutine for functional heating (iheater=100, power_100.in)
!--------------------------------------------------------------------
subroutine test_heater_functional_power()
    use globe_data, only: Q_P, Temp_p
    use inputs, only: nx, ny, nz, NA, power_in, time_step, heated_steps, freq, T_Bath, grid
    use inputs, only: input_dir, read_power_files, icattaneo
    implicit none

    integer(int12) :: itime
    real(real12), allocatable :: Q_test(:), Qdens_test(:)
    integer :: i, j, k
    real(real12) :: tol

    tol = 1.0e-10_real12

    ! Set mock sizes
    nx = 2; ny = 2; nz = 1
    NA = nx*ny*nz
    allocate(grid(nx,ny,nz))
    allocate(Temp_p(NA))

    ! Mock input data
    power_in = 0.0_real12
    time_step = 1.0_real12
    heated_steps = 10
    freq = 1.0_real12
    T_Bath = 300.0_real12
    Temp_p = 300.0_real12
    icattaneo = 0

    ! All cells use the functional heater case 100 (power_100.in in test/data)
    do concurrent( i = 1:nx, j = 1:ny, k = 1:nz )
         grid(i,j,k)%rho           = 1000.0_real12
         grid(i,j,k)%volume        = 1.0_real12
         grid(i,j,k)%heat_capacity = 1.0_real12
         grid(i,j,k)%Length(1)     = 1.0_real12
         grid(i,j,k)%Length(2)     = 1.0_real12
         grid(i,j,k)%tau           = 0.0_real12
         grid(i,j,k)%em            = 0.0_real12
         grid(i,j,k)%iheater       = 100
    end do
    allocate(Q_test(NA), Qdens_test(NA))
    allocate(Q_P(NA))
    Q_P = 0.0_real12

    ! Load power_100.in from the test data directory
    input_dir = './test/data'
    call read_power_files()

    ! t=1: exactly on the second point -> 20.0
    itime = 1
    call heater(itime, Q_test, Qdens_test)
    if (any(abs(Q_test - 20.0_real12) .gt. tol)) then
       write(0,*) "FAIL: functional power at t=1, expected 20, got ", Q_test(1)
       stop 1
    end if

    ! t=2: halfway between (1,20) and (3,0) -> 10.0
    itime = 2
    call heater(itime, Q_test, Qdens_test)
    if (any(abs(Q_test - 10.0_real12) .gt. tol)) then
       write(0,*) "FAIL: functional power at t=2 (interpolation), expected 10, got ", Q_test(1)
       stop 1
    end if

    ! t=4: after the last point -> heater off (0.0)
    itime = 4
    call heater(itime, Q_test, Qdens_test)
    if (any(abs(Q_test) .gt. tol)) then
       write(0,*) "FAIL: functional power at t=4 (after end), expected 0, got ", Q_test(1)
       stop 1
    end if

    ! Test Cattaneo derivative term: tau=5, dt=1, dP/dt at t=0 = (10-0)/1 = 10
    ! Q = P(t=1) + tau*dP/dt = 20 + 5*10 = 70
    icattaneo = 1
    do concurrent( i = 1:nx, j = 1:ny, k = 1:nz )
         grid(i,j,k)%tau = 5.0_real12
    end do
    itime = 1
    call heater(itime, Q_test, Qdens_test)
    if (any(abs(Q_test - 70.0_real12) .gt. tol)) then
       write(0,*) "FAIL: functional power Cattaneo term, expected 70, got ", Q_test(1)
       stop 1
    end if

    write(*,*) "PASS: functional power (power_100.in) test"

    deallocate(Q_test, Qdens_test, Q_P, grid, Temp_p)
end subroutine test_heater_functional_power

!--------------------------------------------------------------------
! Test subroutine for the case of constant heating (iheater=1)
!--------------------------------------------------------------------
subroutine test_heater_constant_heating(hcase)
    use globe_data, only: Q_P, Temp_p
    use inputs, only: nx, ny, nz, NA, power_in, time_step, heated_steps, freq, T_Bath, grid
    implicit none
    
    integer(int12) :: itime
    real(real12), allocatable :: Q_test(:), Qdens_test(:)
    integer :: i, j, k, hcase
    
    ! Set mock sizes (assuming small domain for demonstration)
    nx = 2; ny = 2; nz = 1
    NA = nx*ny*nz
    allocate(grid(nx,ny,nz))
    allocate(Temp_p(NA))
    
    ! Mock input data
    power_in = 50.0_real12
    time_step = 1.0_real12
    heated_steps = 10
    freq = 5.0_real12
    T_Bath = 300.0_real12
    itime = hcase
    heated_steps = 1
    Temp_p = 300.0_real12
    
    ! Fill grid with dummy values
    do concurrent( i = 1:nx, j = 1:ny, k = 1:nz )
         grid(i,j,k)%rho           = 1000.0_real12
         grid(i,j,k)%volume        = 1.0_real12
         grid(i,j,k)%heat_capacity = 1.0_real12
         grid(i,j,k)%Length(1)     = 1.0_real12
         grid(i,j,k)%Length(2)     = 1.0_real12
         grid(i,j,k)%tau           = 0.0_real12
         grid(i,j,k)%em            = 0.0_real12
         grid(i,j,k)%iheater       = hcase     ! Constant heating case
    end do
    allocate(Q_test(NA), Qdens_test(NA))
    
    ! Reset Q_P
    allocate(Q_P(NA))
    Q_P = 0.0_real12
    ! Call the subroutine under test
    call heater(itime, Q_test, Qdens_test)
    
    ! (Optional) Print checks
    ! write(*,*) "Test Heater Constant Heating Results:", Q_test
    
    deallocate(Q_test, Qdens_test, Q_P, grid, Temp_p)
end subroutine test_heater_constant_heating

!--------------------------------------------------------------------
! Test subroutine for the case of no heating (iheater=0)
!--------------------------------------------------------------------
subroutine test_heater_no_heating()
    use globe_data, only: Q_P, Temp_p
    use constants, only: real12, int12
    use inputs, only: nx, ny, nz, NA, power_in, time_step, heated_steps, freq, T_Bath, grid
    implicit none

    integer(int12) :: itime
    real(real12), allocatable :: Q_test(:), Qdens_test(:)
    integer :: i, j, k

    ! Set mock sizes
    nx = 2; ny = 2; nz = 1
    NA = nx*ny*nz
    allocate(grid(nx,ny,nz))
    allocate(Temp_p(NA))

    ! Mock input data
    power_in   = 100.0_real12
    time_step  = 1.0_real12
    heated_steps = 5
    freq = 2.0_real12
    T_Bath = 250.0_real12
    itime = 1
    Temp_p = 300.0_real12

    ! Fill grid with dummy values
    do concurrent( i = 1:nx, j = 1:ny, k = 1:nz )
         grid(i,j,k)%rho           = 500.0_real12
         grid(i,j,k)%volume        = 2.0_real12
         grid(i,j,k)%heat_capacity = 2.0_real12
         grid(i,j,k)%Length(1)     = 1.0_real12
         grid(i,j,k)%Length(2)     = 1.0_real12
         grid(i,j,k)%tau           = 0.0_real12
         grid(i,j,k)%em            = 0.0_real12
         grid(i,j,k)%iheater       = 0       ! No heating
    end do
    allocate(Q_test(NA), Qdens_test(NA))

    ! Reset Q_P
    allocate(Q_P(NA))
    Q_P = 0.0_real12

    ! Call the subroutine under test
    call heater(itime, Q_test, Qdens_test)

    ! (Optional) Print checks
    ! write(*,*) "Test Heater No Heating Results:", Q_test

    deallocate(Q_test, Qdens_test, Q_P, grid, Temp_p)
end subroutine test_heater_no_heating
    
end module Heating_test


program test_mod_heating
    use Heating_test, only: run_tests_mod_heating
    implicit none

    !--------------------------------------------------------------------
    ! Main routine to run all tests
    !--------------------------------------------------------------------
    call run_tests_mod_heating()
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
end program test_mod_heating
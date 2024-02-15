module Heating
  use constants, only: real12, int12, pi
  use constructions, only: heatblock
  use globe_data, only: TPD, TPPD, Heat
  use inputs, only: nx,ny,nz, grid, NA, iheater, power_in, time_step, T_System, freq, ntime
  use materials, only: material
  implicit none
contains


!!!##############################################################################
!!! This subroutine calculates the heat source for each cell in a computational 
!!! grid based on different heating modes.
!!!##############################################################################
  subroutine heater(it, Q, Qdens)
    integer(int12), intent(in) :: it
    real(real12), dimension(NA), intent(out) :: Q, Qdens
    integer(int12) :: i, j, k, IA ,heated_num
    real(real12) :: time, dt, POWER, time_pulse, x, x2
    real(real12) :: rho, volume, heat_capacity, area, heated_volume

    ! Initialize variables
    IA = 0
    Q = 0._real12
    dt = time_step
    POWER = power_in
    time = dt * real(it,real12)
    time_pulse = 0.5_real12
    heated_volume=0.0
    heated_num=0

    ! Iterate over all cells in the grid
    do k = 1, nz
       do j = 1, ny
          do i = 1, nx
             IA = IA + 1
             ! get cell properties
             rho = grid(i,j,k)%rho
             volume = grid(i,j,k)%volume
             heat_capacity = grid(i,j,k)%heat_capacity
             area = grid(i,j,k)%area(1)

             ! select heater case
             select case(iheater(i,j,k))
             case(0)
                !------------------------------
                ! No heating
                !------------------------------
                Q(IA) = 0.0_real12
                !------------------------------
             case(1)
                !------------------------------
                ! Constant heating
                !------------------------------
                Q(IA) = POWER
                
                !------------------------------
             case(2)
                !------------------------------
                 ! Heater on for a time period
                !------------------------------
                if (time <= time_pulse) then
                   Q(IA) = POWER
                else
                   Q(IA) = 0.0_real12
                end if
                !------------------------------
             case(3)
                !------------------------------
                ! AC oscillatory heating
                !------------------------------
                x = time * 2.0_real12 * PI * freq
                x2 = dt * 2.0_real12 * PI * freq
                Q(IA) = POWER * 0.5_real12 * ((x2) - sin(x + x2) * cos(x + x2) + sin(x) * cos(x)) / x2
                !------------------------------
             case(4)
                !------------------------------
                ! AC oscillatory heating raw
                !------------------------------
                Q(IA) = POWER * (sin(time * 2 * PI * freq)**2)
                !------------------------------
             case(5)
                !------------------------------
                ! Radiative heating 
                !------------------------------
                ! Q(IA)= e*eps* T**4
                !------------------------------
             case(6)
                !------------------------------
                ! Step one heating
                !------------------------------
                if (it == 1) then
                   Q(IA) = POWER
                else
                   Q(IA) = 0.0
                end if
                !------------------------------
             end select

             !------------------------------
             ! convert from Q~density to Q
             !------------------------------
             !Qdens(IA)=Q(IA)
             Q(IA)=Q(IA)
             !------------------------------

             
             
             !------------------------------
             ! count heated volume
             !------------------------------
             if (iheater(i,j,k) .gt. 0) then
                heated_volume = heated_volume + volume
                heated_num = heated_num + 1
             end if
             !------------------------------
          end do
       end do
    end do

    !write(*,*) ""
    !write(*,*) "==============================="
    !write(*,*) "sum(Q(:))=",sum(Q(:))
    !write(*,*) "sum(Qdens(:))=",sum(Qdens(:))
    !write(*,*) "power",power_in
    !write(*,*) "sum(Q(:))/one_cell_volume=",sum(Q(:))/grid(1,1,1)%volume
    !write(*,*) "one_cell_volume", grid(1,1,1)%volume
    !write(*,*) "heated_volume", heated_volume
    !write(*,*) "sum(Q(:))/heated_volume",sum(Q(:))/heated_volume
    !write(*,*) "sum(Qdens(:))/heated_volume",sum(Qdens(:))/heated_volume
    !write(*,*) "heated_num = ",heated_num
    !write(*,*) "==============================="

    ! Normalize all heat sources by the heated volume
    if (heated_volume .gt. 0.0) Qdens(:) = Q(:) / heated_volume
    

  end subroutine heater


end module Heating

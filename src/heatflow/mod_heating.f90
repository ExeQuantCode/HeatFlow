!!!#################################################################################################
!!! Module for calculating the heat density vector for each cell in the computational grid ...
!!! ... based on different heating modes.
!!! This module contains the subroutines: 
!!!   - heater, calculates the heat source for each cell in a computational grid based on ...
!!! ... different heating modes.
!!! This module contains the variables:
!!!   - Q, the heat source for each cell in the computational grid.
!!!   - Qdens, the heat source density for each cell in the computational grid.
!!!  
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module Heating
  use constants, only: real12, int12, pi, StefBoltz, TINY
  use globe_data, only: Temp_p, Temp_pp, Heat, heated_volume, Q_P
  use inputs, only: nx,ny,nz, grid, NA, power_in, time_step, heated_steps, T_System, freq, ntime, &
                     T_Bath, LSPOWER
  use materials, only: material
  implicit none
contains


!!!##############################################################################
!!! This subroutine calculates the heat source for each cell in a computational 
!!! grid based on different heating modes.
!!!##############################################################################
  subroutine heater(itime, Q, Qdens)
    integer(int12), intent(in) :: itime
    real(real12), dimension(NA), intent(out) :: Q, Qdens
    integer(int12) :: ix, iy, iz, IA ,heated_num
    real(real12) :: time, POWER, time_pulse, x, x2
    real(real12) :: rho, volume, heat_capacity, area, tau

    ! Ensure Initialize variables
    IA = 0
    Q = 0.0_real12
    Qdens = 0.0_real12
    POWER = power_in
    time = time_step * real(itime,real12)
    time_pulse = heated_steps  * time_step
    heated_volume=0.0
    heated_num=0
    
    ! Iterate over all cells in the grid
    do iz = 1, nz
       do iy = 1, ny
          do ix = 1, nx
             IA = IA + 1
             ! get cell properties
             rho = grid(ix,iy,iz)%rho
             volume = grid(ix,iy,iz)%volume
             heat_capacity = grid(ix,iy,iz)%heat_capacity
             area = grid(ix,iy,iz)%Length(1)*grid(ix,iy,iz)%Length(2) !???
             !tau divided by time_step squared in setup.f90
             tau = grid(ix,iy,iz)%tau*(time_step**2.0_real12) 
             ! select heater case

             select case(grid(ix,iy,iz)%iheater)

             case(0)
                !------------------------------
                ! No heating
                !------------------------------
                Q(IA) = 0.0_real12

                !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             case(1)
                !------------------------------
                ! Constant heating
                !------------------------------
                Q(IA) = POWER
                
                !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             case(2)
                !------------------------------
                 ! Heater on for a time period
                !------------------------------
                if ( time .le. time_pulse ) then
                   Q(IA) = POWER
                else
                   Q(IA) = 0.0_real12
                end if
                !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             case(3)
                !------------------------------
                ! AC oscillatory heating
                !------------------------------
                x = time * 2.0_real12 * PI * freq
                x2 = time_step * 2.0_real12 * PI * freq
                Q(IA) = POWER * 0.5_real12 * &
                  ((x2) - sin(x + x2) * cos(x + x2) + sin(x) * cos(x)) / x2
                !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             case(4)
                !------------------------------
                ! AC oscillatory heating raw, with power correction
                !------------------------------
                Q(IA) = POWER * (sin(time * 2.0_real12 * PI * freq)**2.0_real12)
                !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             case(5)
                !------------------------------
                ! AC oscillatory heating raw, with power correction
                !------------------------------
                Q(IA) = POWER * (sin(time * 2.0_real12 * PI * freq)**2.0_real12)&
                  +POWER*2.0_real12*PI*freq*tau*sin(2.0_real12*time*2.0_real12*PI*freq)
                !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             case(6)
                !------------------------------
                ! Step one heating
                !------------------------------
                if (itime == 1) then
                   Q(IA) = POWER
                else
                   Q(IA) = 0.0
                end if
                !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             end select
             !------------------------------
             ! If emissitivity is not zero, then calculate the radiative heating
             !------------------------------
             if (grid(ix,iy,iz)%em .gt. TINY) then
               Q(IA) = Q(IA) - grid(ix,iy,iz)%em * grid(ix,iy,iz)%length(1)*&
                       grid(ix,iy,iz)%length(2)*StefBoltz &
                       * ((Temp_p(IA)**4.0_real12) - (T_Bath**4.0_real12))
             end if
             !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

             !------------------------------
             ! Additional PowerTerm FD
             !------------------------------
              ! Q(IA) = Q(IA) +(tau/time_step)*(Q(IA)-Q_P(IA))
            
             !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             
             !------------------------------
             ! count heated volume
             !------------------------------
             if (LSPOWER .and. (grid(ix,iy,iz)%iheater .gt. 0)) then
                heated_volume = heated_volume + volume
                heated_num = heated_num + 1
             end if
             !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            
          end do
       end do
    end do
   !  Q_P = Q

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
    if (LSPOWER) then
      if (heated_volume .gt. 0.0) Qdens(:) = Q(:) / heated_volume
    else
      Qdens(:) = Q(:) / volume
    end if 

  end subroutine heater


end module Heating

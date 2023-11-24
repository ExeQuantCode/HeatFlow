module Heating
  use constants, only: real12, int12
  use constructions, only: heatblock
  use globe_data, only: TPD, TPPD
  use inputs, only: nx,ny,nz, grid, NA, iheater, power_in, time_step, T_Bath, freq
  use materials, only: material
  implicit none
contains


  !!Simple heat source implemented
  subroutine heater(it,Q)
    integer :: IA
    integer(int12), intent(in) :: it 
    integer(int12) :: i,j,k
    real(real12) :: time, TC,heat_capacity,rho,m
    real(real12) :: PI, dt, POWER, time_pulse
    real(real12), dimension(NA), intent(out) :: Q
    !real(real12), dimension(NA) :: Q
    IA=0
    Q = 0 
    PI = 3.14
    dt= time_step
    POWER = power_in
    time=dt*real(it)
    time_pulse = 0.5
    do i=1,nx
       do j=1,ny
          do k=1,nz
             IA=IA+1
               rho = grid(i,j,k)%rho
               m= rho*grid(i,j,k)%volume
               heat_capacity = grid(i,j,k)%heat_capacity
             select case(iheater(i,j,k))
                
                !NO HEATING
             CASE(0)
               
		          Q(IA) = 0.0
                !Q(IA)=0.0
                
             CASE(1)
                !heater permanently on
                Q(IA)=POWER
                !Q(IA)=POWER
                
             CASE(2)
                !heater on for a time period
                if (time.le.time_pulse) then
		            Q(IA)=POWER
                else
                  Q(IA)=0.0
                end if
                
             CASE(3)
                !OSCCILATORY HEATING
                Q(IA)=POWER*abs(SIN(time*2*PI*freq))
                !Q(IA)=POWER*SIN(time*2*PI/PARAM_HEAT_PERIOD)
               
               CASE(4)
                  !sin^2 heating
                  Q(IA)=POWER*SIN(time*2*PI*freq)**2

               CASE(5)
                   ! Radiative
                   !Q(IA)= e*eps* T**4
               CASE(6)
                  if (it.eq.1) then
                     Q(IA)=POWER
                  else
                     Q(IA)=0.0
                  end if
               case(7)
                  !Fixed temperature
                  Q(IA) =0
                  TPPD = T_Bath+100
                  TPD(IA) = T_Bath+100
             end select
             Q(IA) = Q(IA)/(m*heat_capacity)
          end do
       end do
    end do
   
    
  end subroutine heater
end module Heating


             
             





module Heating
  use constants, only: real12, int12, pi
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
    real(real12) :: dt, POWER, time_pulse, x
    real(real12), dimension(NA), intent(out) :: Q
    !real(real12), dimension(NA) :: Q
    IA = 0
    Q = 0._real12
    dt = time_step
    POWER = power_in
    time = dt * real(it,real12)
    time_pulse = 0.5_real12
    do i=1,nx
       do j=1,ny
          do k=1,nz
             IA=IA+1
               rho = grid(i,j,k)%rho
               m   = rho*grid(i,j,k)%volume
               heat_capacity = grid(i,j,k)%heat_capacity
             select case(iheater(i,j,k))
             case(0)
               !NO HEATING
               
		          Q(IA) = 0.0_real12
                !Q(IA)=0.0
                
             case(1)
                !heater permanently on
                Q(IA) = POWER
                !Q(IA)=POWER
                
             case(2)
                !heater on for a time period
                if (time.le.time_pulse) then
		            Q(IA)=POWER
                else
                  Q(IA)=0.0_real12
                end if
                
             case(3)
                !OSCCILATORY HEATING
               !sin^2(x)
               x = 2._real12 * pi * freq
               Q(IA) = POWER*(sin(x*time))**2
               ! Q(IA)= 0.5_real12*((x*(time))-sin(x*(time))*cos(x*(time)))
               ! Q(IA)= 0.5_real12*((x*(time)-x*time)-sin(x*(time+dt))*cos(x*(time+dt))+sin(x*time)*cos(x*time))
               !  if (x.ge.0) then
               !    Q(IA)=POWER*((-1*cos((time+dt)*2.0_real12*pi*freq))+cos((time)*2.0_real12*pi*freq))
               !  else
               !    Q(IA)=POWER*(cos((time+dt)*2.0_real12*pi*freq)-cos((time)*2.0_real12*pi*freq))
               !  end if
               !Integral of power between time steps
               ! x = 2*PI*freq
               ! Q(IA)=POWER*(abs(-cos((time+dt)*2*PI*freq)+cos((time)*2*PI*freq)))
               ! Q(IA) = POWER*(2/pi)*(x*time-x*(time-dt)- cos(x*time)+cos(x*(time-dt)))
               
             case(4)


             case(5)
                  ! Radiative
                  !Q(IA)= e*eps* T**4
             case(6)
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


             
             





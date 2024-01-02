module Heating
  use constants, only: real12, int12, pi
  use constructions, only: heatblock
  use globe_data, only: TPD, TPPD, Heat
  use inputs, only: nx,ny,nz, grid, NA, iheater, power_in, time_step, T_System, freq, ntime
  use materials, only: material
  implicit none
contains


  !!Simple heat source implemented
  subroutine heater(it,Q)
    integer :: IA
    integer(int12), intent(in) :: it 
    integer(int12) :: i,j,k,ierr
    real(real12) :: time, TC,heat_capacity,rho,m, totaltime
    real(real12) ::  dt, POWER, time_pulse, x, x2
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
                
             CASE(3)
               !  AC OSCCILATORY HEATING
               !  Q(IA)=POWER*(SIN(time*2*PI*freq)**2)
               !  Q(IA)= POWER*cos(time*2*PI*freq)**2
               x = time*2.0_real12*PI*freq
               x2 = (dt)*2.0_real12*PI*freq
               Q(IA) = POWER*0.5_real12*((x2)-sin(x+x2)*cos(x+x2)+sin((x))*cos((x)))/x2
               Heat(IA) = Q(IA)
               
               ! Q(IA)=POWER*0.5*((2*PI*freq)*(dt)-SIN((time+dt)*2*PI*freq)*cos((time+dt)*2*PI*freq) &
               !    +SIN((time)*2*PI*freq)*cos((time)*2*PI*freq))/dt
               ! Q(IA)=POWER*SIN(time*2*PI/PARAM_HEAT_PERIOD)
               
             case(4)
              !  AC OSCCILATORY HEATING raw
            
              Q(IA)=POWER*(SIN(time*2*PI*freq)**2)


             case(5)
                  ! Radiative
                  !Q(IA)= e*eps* T**4
             case(6)
               if (it.eq.1) then
                  Q(IA)=POWER
               else
                  Q(IA)=0.0
               end if
            !  case(7)
            !    !Fixed temperature
            !    Q(IA) =0
            !    TPPD = T_Bath+100
            !    TPD(IA) = T_Bath+100
             end select
             Q(IA) = Q(IA)/(m*heat_capacity)
            !  if (Q(IA) .gt. 0) print*, "Q(IA)=",Q(IA) 
          
          end do
       end do
    end do
   
    
  end subroutine heater
end module Heating


             
             





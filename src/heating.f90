module Heating
  use constants, only: real12, int12
  use constructions, only: heatblock
  use inputs, only: nx,ny,nz, grid, NA, iheater, power_in, time_step
  use materials, only: material
  implicit none
contains


  !!Simple heat source implemented
  subroutine heater(it,Q)
    integer :: IA
    integer(int12), intent(in) :: it 
    integer(int12) :: i,j,k
    real(real12) :: time, TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau
    real(real12) :: PI, dt, Heating_f, POWER, PARAM_time_pulse
    real(real12), dimension(NA), intent(out) :: Q
    !real(real12), dimension(NA) :: Q
    IA=0
    Q = 0 
    PI = 3.14
    dt= time_step
    POWER = power_in
    time=dt*real(it)

    do i=1,nx
       do j=1,ny
          do k=1,nz
             IA=IA+1
             call material(grid(i,j,k)%imaterial_type, TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)

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
                if (time.le.PARAM_time_pulse) then
		            Q(IA)=POWER
                else
                  Q(IA)=0.0
                end if
                
             CASE(3)
                !OSCCILATORY HEATING
                Q=POWER*SIN(time*2*PI*Heating_f)
                !Q(IA)=POWER*SIN(time*2*PI/PARAM_HEAT_PERIOD)
               
               CASE(4)
                  !sin^2 heating
                  Q(IA)=POWER*SIN(time*2*PI*Heating_f)**2


             end select
             Q(IA) = Q(IA)/(rho*heat_capacity)
          end do
       end do
    end do
   
    
  end subroutine heater
end module Heating


             
             





module Heating
  use constants, only: real12, int12
  use constructions, only: heatblock
  use inputs, only: nx,ny,nz, grid, NA, iheater


contains


  !!Simple heat source implemented
  subroutine heater(it,Q)
    integer :: IA
    integer(int12), intent(in) :: it 
    real(real12) :: time
    real(real12), dimension(NA), intent(out) :: Q
    !real(real12), dimension(NA) :: Q
    time=dt*REAL(iT)
    IA=0
    POWER = 1000
    do i=1,NX
       do j=1,NY
          do k=1,NZ
             IA=IA+1
             
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
                   Q=POWER
		   !Q(IA)=POWER
                else
		            Q=0.0
                   !Q(IA)=0.0
                end if
                
             CASE(3)
                !OSCCILATORY HEATING
                Q=POWER*SIN(time*2*PI/PARAM_HEAT_PERIOD)
                !Q(IA)=POWER*SIN(time*2*PI/PARAM_HEAT_PERIOD)
                
             !case(0)
             !   print(*,*) 'Undefined heater, setting block ', &
             !        i, j, k, ' to zero'
             !   Q(IA)=0.0
             end select
          end do
       end do
    end do
    
    
  end subroutine heater
end module Heating


             
             





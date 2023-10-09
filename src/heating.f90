module Heating
  use constants
  use parameters


contains


  !!Simple heat source implemented
  subroutine heater(it,Q)
    
    integer :: IA,iheater
    real(rea12) :: time
    time=dt*REAL(iT)
    IA=0
    
    do i=1,NX
       do j=1,NY
          do k=1,NZ
             IA=IA+1
             iheater=grid(ix,iy,iz)%heater_type
             select case(iheater)
                
                !NO HEATING
             CASE(0)
                Q(IA)=0.0
                
             CASE(1)
                !heater permanently on
                
                Q(IA)=POWER
                
             CASE(2)
                !heater on for a time period
                if (time.le.PARAM_time_pulse) then
                   Q(IA)=POWER
                else
                   Q(IA)=0.0
                end if
                
             CASE(3)
                !OSCCILATORY HEATING
                
                Q(IA)=POWER*SIN(time*2*PI/PARAM_HEAT_PERIOD)
                
             case(default)
                print(*,*) 'Undefined heater, setting block ', &
                     i, j, k, ' to zero'
                Q(IA)=0.0
             end select
          end do
       end do
    end do
    
    
  end subroutine heater
end module Heating


             
             





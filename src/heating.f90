module Heating
  use constants
  use parameters


contains


  !!Simple heat source implemented
  subroutine heater(ix,iy,iz,it,imaterial,QQ)


    integer(int12) :: i
    integer(int12), intent(in) :: ix,iy,iz,it,imaterial
    !integer(int12) :: cutoff
    integer(int12), parameter :: e = nx*ny*nz
    !  real(real12), dimension(e) :: QQ
    real(real12) :: r, R_spot,area, time, time_Switch
    
    !    real(real12), dimension(nx,ny,nz) :: QQ
    real(real12) :: QQ
    real(real12) :: AC
    !QQ = 0.0


    select case(iheater)        

    case(1)
       !        do i=1,e
       if (ix.le.(nx) .and. iy .le.(ny) .and. iz .le. nz)  then
          ! if (iy.eq.(ny/2)) then
          !    if (iz.eq.nz) then
          QQ = power_in
          print*, QQ !'heating'
       else
          QQ = 0.0

       end if
       !             end do


    case(2)
       t=real(it)*time_step*2.0*pi/T_Period
       if (ix.eq.(nx/2))  then
          if (iy.eq.(ny/2)) then
             if (iz.eq.1) then
                QQ=sin(t)*sin(t)*power_in
                !  print*, 'heating'
             end if
          end if
       end if
    case(3)
       if (it.le.10) then
          if (ix.eq.((nx+1)/2))  then
             if (iy.eq.((ny+1)/2)) then
                if (iz.eq.1) then
                   QQ=1.0
                   print*, 'heating'
                   !print*, QQ
                end if
             end if
          end if
       else 
          QQ=0.0
          !print*, QQ
       end if

       !laser of 2 mm spot size, shone on surface
    case(4)
       time_switch=300.0
       time=it*time_step

       r=REAL((ix-nx/2)**2+(iy-ny/2)**2)**(0.5)
       r=r*1e-4
       r_spot=1.e-3
       area=r_spot*r_spot*PI
       if (time.lt.time_switch) then
          if (iz.eq.(nz/4)) then

             if (r.lt.r_spot) then
                QQ=power_in/area
             end if
          end if
       end if


    case(5)
       time_switch=2.0
       time=it*time_step

       if (ix.eq.(nx/4)) then
          if (time.le.time_switch) then
             QQ=power_in
          end if
       end if


    case(10)
       if (imaterial.eq.2000) then
          QQ=1.0
       else 
          QQ=0.0
       end if
       !     if (it.gt.(ntime/2)) then
       !        QQ=0.0
       !     end if
       !print*, QQ

    case(11)
       !if (it.le.10) then
       tt = time_step*it
       if (ix.eq.(101))  then
          if (iy.le.(26) .and. iy.ge.(25)) then
             if (iz.eq.506) then
                !if (ACon = 1.0) then
                !AC = sin(om*tt)*sin(om*tt)
                !else
                !AC = 1
                !end if
                QQ=power_in !*AC
                print*, 'heating'
             end if
          end if
       end if
       !else
       !   QQ = 0.0
       !end if
    case(12)
       !if (ix.le.(202) .and. ix.ge.(201))  then !202,201
       if (ix.eq.(101)) then
          if (iy.le.(52) .and. iy.ge.(49)) then !52,49 : 26,25
             !if (iz.le.(1012) .and. iz.ge.(1011)) then !1012,1011
             if (iz.eq.(506)) then
                QQ=power_in
                print*, 'heating'
             end if
          end if
       end if

    case(13)
       if (it.le.100) then
          tt = time_step*it
          if (ix.eq.(101))  then
             if (iy.le.(26) .and. iy.ge.(25)) then
                if (iz.eq.506) then
                   !if (ACon = 1.0) then
                   AC = sin(om*tt)*sin(om*tt)
                   !else
                   !AC = 1
                   !end if
                   QQ=power_in*AC
                   print*, 'heating'
                end if
             end if
          end if
       else
          QQ = 0.0
       end if


    case(20)
       tt = time_step*it
       !print*,cutoff
       if (it.le.cutoff) then
          if (ACon.eq.1) then
             AC = sin(om*tt)*sin(om*tt)
             !print*, AC, om, tt, sin(om*tt)
          else
             AC = 1
          end if
          !if(ix.eq.nx.and.iy.eq.ny.and.iz.eq.nz) then
             !QQ(ix,iy,iz) = power_in
             QQ = power_in*AC
             !print*, QQ, ix,iy,iz
             !print*, 'heating'
          !else
             !QQ = 0.0
          !end if
       else
          QQ = 0.0
       end if



    case(21)
       tt = time_step*it
       !print*,cutoff
       if (it.le.cutoff) then
          if (ACon.eq.1) then
             AC = sin(om*tt)*sin(om*tt)
             !print*, AC, om, tt, sin(om*tt)
          else
             AC = 1
          end if
          if(ix.ge.18 .and. ix.le.22) then 
             QQ = power_in*AC
          else
             QQ = 0.0
          end if
       else
          QQ = 0.0
       end if

       


       !print*, 'QQ = ', QQ


    end select
  end subroutine heater
end module Heating

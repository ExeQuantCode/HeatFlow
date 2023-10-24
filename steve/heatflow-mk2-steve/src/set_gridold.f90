MODULE SET_GRID_UP
  use constants
  use parameters
  use constructions
  
  implicit none
  
  
  contains
  
  !!!Each grid set up follows a template to make life bearable for the user, please cut and paste subroutine0 and then adapt to your 'instance'
    !Silver on chip setup
!!!This is currently hardcoded for report, is intended for a 200x50x516 grid
    
  subroutine set_gridWAB(grid,cellengthx,cellengthy,cellengthz)
    
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    !!!Defines dummy variables
    integer(int12) :: ix, iy, iz, itime, i, it, j, k
    
    real(real12), dimension(nx, ny,nz) :: T, TN, Told
   
    real(real12), dimension(nx):: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    
    real(real12) :: pa,pb,A,Lx,Ly,Lz
  
    !!!Need to create uniform case!
    Lx = 5e-6 !0.003  !!!1cm bee case for 3x3x3
    Ly = 2e-5
    Lz = 1e-7
    A = 1e-6  !!1e-06
    cellengthx=Lx
    cellengthy=Ly
    cellengthz=Lz

!!!bulk
    
    print*, nx/2
       
    !!!3D Case
       do ix=1,nx
          do iy=1,ny
             !grid(ix,iy,1)%imaterial_type=31
             !grid(ix,iy,2)%imaterial_type=32
             do iz=1,500
                grid(ix,iy,iz)%imaterial_type=31
             end do
             do iz = 501,505
                grid(ix,iy,iz)%imaterial_type=32
             end do
             iz = 506
             grid(ix,iy,iz)%imaterial_type=0
             if (ix.le.(120) .and. ix.ge.(101)) then 
                 if (iy.le.(31) .and. iy.ge.(20)) then
                   grid(ix,iy,iz)%imaterial_type=33
                   if (ix.ge.(102)) then
                       if (iy.le.(26) .and. iy.ge.(25)) then
                          grid(ix,iy,iz)%imaterial_type=0   !Silver device
                       end if
                    end if   
                 end if 
              end if
              do iz = 507,nz
                 grid(ix,iy,iz)%imaterial_type=0
              end do
          end do
      end do
    end subroutine set_gridWAB

  subroutine set_gridWABfine(grid,cellengthx,cellengthy,cellengthz)
    
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    !!!Defines dummy variables
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
    
    real(real12), dimension(nx, ny,nz) :: T, TN
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    real(real12) :: pa,pb,A,Lx,Ly,Lz
  
    !!!Need to create uniform case!
    Lx = 5e-6 !25e-7
    Ly = 1e-5  !1e-5
    Lz = 1e-7  !5e-8
    A = 5e-7  !!1e-06
    cellengthx=Lx
    cellengthy=Ly
    cellengthz=Lz

!!!bulk
    
    print*, nx/2
       
    !!!3D Case
       do ix=1,nx
          do iy=1,ny
             !grid(ix,iy,1)%imaterial_type=31
             !grid(ix,iy,2)%imaterial_type=32
             do iz=1,500 !1000
                grid(ix,iy,iz)%imaterial_type=31
             end do
             do iz = 501,505 !1001,1010
                grid(ix,iy,iz)%imaterial_type=32
             end do
             !do iz = 1011,1012 !1011,1012
             iz = 506
             grid(ix,iy,iz)%imaterial_type=0
             if (ix.le.(120) .and. ix.ge.(101)) then !240,201 : 120,101 
                 if (iy.le.(62) .and. iy.ge.(39)) then !62,39 : 31,20
                    grid(ix,iy,iz)%imaterial_type=33
                    if (ix.ge.(102)) then !203 : 102
                       if (iy.le.(52) .and. iy.ge.(49)) then !52,49 : 26,25
                          grid(ix,iy,iz)%imaterial_type=0   !Silver device
                       end if
                    end if   
                 end if 
              end if
              !end do
              do iz = 507,nz !1013 : 507
                 grid(ix,iy,iz)%imaterial_type=0
              end do
          end do
      end do
   end subroutine set_gridWABfine
  
   subroutine set_grid00C(grid,cellengthx,cellengthy,cellengthz)
  TYPE(heatblock), dimension(nx,ny,nz) :: grid
!!!Defines dummy variables
  
    integer(int12) :: ix,iy,iz,itime,i,it,j,k                                                                                                                                            
    real(real12), dimension(nx, ny,nz) :: T, TN, Told
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
     real(real12), dimension(nz) :: cellengthz
     real(real12) :: pa,pb,A,L

     L = 1.0
     A = L*L  
     cellengthx= L/nx
     cellengthy= L/ny
     cellengthz= L/nz
     grid(:,:,:)%imaterial_type= 0
   end subroutine set_grid00C
     
  subroutine set_grid006(grid,cellengthx,cellengthy,cellengthz)
    !!Cath test case
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    !!!Defines dummy variables
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
!    integer, parameter:: e = nx*ny*nz
   ! real(real12), dimension(e) :: T, TN
    real(real12), dimension(nx, ny,nz) :: T, TN, Told
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
  ! real(real12) :: cellengthx, cellengthy, cellengthz
    real(real12) :: pa,pb,A,L
  
    !!!Need to create uniform case!
    L = 1 !0.003  !!!1cm bee case for 3x3x3
    A = L*L  !!1e-06
    cellengthx=L
    cellengthy=L
    cellengthz=L

!!!bulk
!    if (ix.eq.1) then
!       print*, nx/2,'This is nx/2, and comes from subroutine set_grid006'
 !   end if
    
    if (nz.eq.1) then !!!2D Case

       do ix=1, nx
          do iy=1, ny
             grid(ix,iy,1)%imaterial_type=0
             if (ix.le.((nx+1)/2) .and. ix.gt.((nx-1)/2)) then 
                if (iy.le.((ny+1)/2) .and. iy.gt.((ny-1)/2)) then
                   grid(ix,iy,1)%imaterial_type=14   !2d silicon bee
                   print*, 'The Bee has been Built'
                end if
             end if
          end do
       end do
       !!print*, ix, iy
    else !!!3D Case
       do ix=1,nx
          do iy=1,ny
             do iz=1,nz
                grid(ix,iy,iz)%imaterial_type=0
                if (ix.ge.(nx/4) .and. ix.le.(3*nx/4)) then
                   if (iy.ge.1 .and. iy.le.(ny/4)) then                         
                      if (iz.ge.(nz/4) .and. iz.le.(3*nz/4)) then
                         grid(ix,iy,iz)%imaterial_type=3003 !earth 2.0
                         if (ix.le.5 .or. ix.ge.(nx-5)) then
                            if (iy.le.5 .or. iy.ge.(ny-5)) then
                               if (iz.le.5 .or. iz.ge.(nz-5)) then
                                           grid(ix,iy,iz)%imaterial_type=3004 !clay 
                                           if (ix.le.((nx/2)+4) .and. ix.ge.((nx/2)-4)) then
                                              if (iy.le.((ny/4)+4) .and. iy.ge.((ny/4)-4)) then
                                                 if (iz.le.((nz/2)+4) .and. iz.ge.((nz/2)-4)) then
                                                    grid(ix,iy,iz)%imaterial_type=14 !3d silicon bee
                                                    if (ix.ge.(nx-5) .and. ix.le.nx) then
                                                       if (iy.ge.5 .and. iy.le.10) then
                                                          if (iz.ge.1 .and. iz.le.5) then
                                                             grid(ix,iy,iz)%imaterial_type=0 !!5x5x5 hole in the clay 
                                                          end if
                                                       end if
                                                    end if
                                                 end if
                                              end if
                                           end if
                                        end if
                                     end if
                                  end if
                               end if
                            end if
                         end if
                      end do
                   end do
                end do
                
             end if
           end subroutine set_grid006




  subroutine set_grid005(grid,cellengthx,cellengthy,cellengthz) !earth 2.0 with gap (e)
    
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
    real(real12), dimension(nx, ny,nz) :: T, TN
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    real(real12) :: pa,pb,A,L
    
    !!Need to create uniform case!
    L = 0.003  ! 1cm bee case for 3x3x3
    A = 1.0  !1e-06
    cellengthx=L
    cellengthy=L
    cellengthz=L
    
    !!bulk 
    if (nz.eq.1) then
       
       do ix=1, nx
          do iy=1, ny
             grid(ix,iy,1)%imaterial_type=0
             if (ix.le.((nx/2)+1) .and. ix.ge.((nx/2)-1)) then 
                if (iy.le.((nx/2)+1) .and. iy.ge.((nx/2)-1)) then
                   grid(ix,iy,1)%imaterial_type=14   !2d silicon bee
                   !print*, 'The bee has been built'
                end if
             end if
          end do
       end do
       ! print*, ix, iy
    else
       do ix=1,nx
          do iy=1,ny
             do iz=1,nz
                grid(ix,iy,iz)%imaterial_type=0
                if (ix.ge.(nx/4) .and. ix.le.(3*nx/4)) then
                   if (iy.ge.1 .and. iy.le.(ny/4)) then                         
                      if (iz.ge.(nz/4) .and. iz.le.(3*nz/4)) then
                         grid(ix,iy,iz)%imaterial_type=3003 !earth 2.0
                         if (ix.le.5 .or. ix.ge.(nx-5)) then
                            if (iy.le.5 .or. iy.ge.(ny-5)) then
                               if (iz.le.5 .or. iz.ge.(nz-5)) then
                                  grid(ix,iy,iz)%imaterial_type=3004 !clay 
                                  if (ix.le.((nx/2)+4) .and. ix.ge.((nx/2)-4)) then
                                     if (iy.le.((ny/4)+4) .and. iy.ge.((ny/4)-4)) then
                                        if (iz.le.((nz/2)+4) .and. iz.ge.((nz/2)-4)) then
                                           grid(ix,iy,iz)%imaterial_type=14 !3d silicon bee
                                           if (ix.ge.(nx-5) .and. ix.le.nx) then
                                              if (iy.ge.5 .and. iy.le.10) then
                                                 if (iz.ge.1 .and. iz.le.5) then
                                                    grid(ix,iy,iz)%imaterial_type=0 !!5x5x5 hole in the clay 
                                                 end if
                                              end if
                                           end if
                                        end if
                                     end if
                                  end if
                               end if
                            end if
                         end if
                      end if
                   end if
                end if
             end do
          end do
       end do
       
    end if
  end subroutine set_grid005
  
  
  subroutine set_grid004(grid,cellengthx,cellengthy,cellengthz) !earth 2.0 without gap (d)
    
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
    real(real12), dimension(nx, ny,nz) :: T, TN
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    real(real12) :: pa,pb,A,L
    
    !!Need to create uniform case!
    L = 0.003  ! 1cm bee case for 3x3x3
    A = 1.0  !1e-06
    cellengthx=L
    cellengthy=L
    cellengthz=L
    
    !!bulk 
    if (nz.eq.1) then
       
       do ix=1, nx
          do iy=1, ny
             grid(ix,iy,1)%imaterial_type=0
             if (ix.le.((nx/2)+1) .and. ix.ge.((nx/2)-1)) then 
                if (iy.le.((ny/2)+1) .and. iy.ge.((ny/2)-1)) then
                   grid(ix,iy,1)%imaterial_type=14   !2d silicon bee
                end if
             end if
          end do
       end do
       ! print*, ix, iy
    else
       do ix=1,nx
          do iy=1,ny
             do iz=1,nz
                grid(ix,iy,iz)%imaterial_type=0
                if (ix.ge.(nx/4) .and. ix.le.(3*nx/4)) then
                   if (iy.ge.1 .and. iy.le.(ny/4)) then                         
                      if (iz.ge.(nz/4) .and. iz.le.(3*nz/4)) then
                         grid(ix,iy,iz)%imaterial_type=3003 !earth 2.0
                         if (ix.le.5 .or. ix.ge.(nx-5)) then
                            if (iy.le.5 .or. iy.ge.(ny-5)) then
                               if (iz.le.5 .or. iz.ge.(nz-5)) then
                                  grid(ix,iy,iz)%imaterial_type=3004 !clay 
                                  if (ix.le.((nx/2)+4) .and. ix.ge.((nx/2)-4)) then
                                     if (iy.le.((ny/4)+4) .and. iy.ge.((ny/4)-4)) then
                                        if (iz.le.((nz/2)+4) .and. iz.ge.((nz/2)-4)) then
                                           grid(ix,iy,iz)%imaterial_type=14 !3d silicon bee
                                        end if
                                     end if
                                  end if
                               end if
                            end if
                         end if
                      end if
                   end if
                end if
             end do
          end do
       end do
       
    end if
  end subroutine set_grid004
  
  
  
  
  subroutine set_grid003(grid,cellengthx,cellengthy,cellengthz) !earth 1.0 without gap (c)
    
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
    real(real12), dimension(nx, ny,nz) :: T, TN
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    real(real12) :: pa,pb,A,L
    
    !!Need to create uniform case!
    L = 0.003  ! 1cm bee case for 3x3x3
    A = 1.0  !1e-06
    cellengthx=L
    cellengthy=L
    cellengthz=L
    
    !!bulk 
    if (nz.eq.1) then
       
       do ix=1, nx
          do iy=1, ny
             grid(ix,iy,1)%imaterial_type=0
             if (ix.le.((nx/2)+1) .and. ix.ge.((nx/2)-1)) then 
                if (iy.le.((nx/2)+1) .and. iy.ge.((nx/2)-1)) then
                   grid(ix,iy,1)%imaterial_type=14   !2d silicon bee
                end if
             end if
          end do
       end do
       ! print*, ix, iy
    else
       do ix=1,nx
          do iy=1,ny
             do iz=1,nz
                grid(ix,iy,iz)%imaterial_type=0
                if (ix.ge.(nx/4) .and. ix.le.(3*nx/4)) then
                   if (iy.ge.1 .and. iy.le.(ny/4)) then                         
                      if (iz.ge.(nz/4) .and. iz.le.(3*nz/4)) then
                         grid(ix,iy,iz)%imaterial_type=3002 !earth 1.0
                         if (ix.le.5 .or. ix.ge.(nx-5)) then
                            if (iy.le.5 .or. iy.ge.(ny-5)) then
                               if (iz.le.5 .or. iz.ge.(nz-5)) then
                                  grid(ix,iy,iz)%imaterial_type=3004 !clay 
                                  if (ix.le.((nx/2)+4) .and. ix.ge.((nx/2)-4)) then
                                     if (iy.le.((ny/4)+4) .and. iy.ge.((ny/4)-4)) then
                                        if (iz.le.((nz/2)+4) .and. iz.ge.((nz/2)-4)) then
                                           grid(ix,iy,iz)%imaterial_type=14 !3d silicon bee, this is 9x [3,3,3] bee, no gap
                                        end if
                                     end if
                                  end if
                               end if
                            end if
                         end if
                      end if
                   end if
                end if
             end do
          end do
       end do
                
    end if
  end subroutine set_grid003
  
  
  subroutine set_grid002(grid,cellengthx,cellengthy,cellengthz) !earth 1.0 1 bee no gap (b)
    
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
    real(real12), dimension(nx, ny,nz) :: T, TN
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    real(real12) :: pa,pb,A,L
    
    !!Need to create uniform case!
    L = 0.003  ! 1cm bee case for 3x3x3
    A = 1.0  !1e-06
    cellengthx=L
    cellengthy=L
    cellengthz=L
    
    !!bulk 
    if (nz.eq.1) then

       do ix=1, nx
          do iy=1, ny
             grid(ix,iy,1)%imaterial_type=002
             if (ix.le.((nx/2)+1) .and. ix.ge.((nx/2)-1)) then 
                if (iy.le.((nx/2)+1) .and. iy.ge.((nx/2)-1)) then
                   grid(ix,iy,1)%imaterial_type=14   !2d silicon bee
                end if
             end if
          end do
       end do
       ! print*, ix, iy
    else
       do ix=1,nx
          do iy=1,ny
             do iz=1,nz
                grid(ix,iy,iz)%imaterial_type=0
                if (ix.ge.(nx/4) .and. ix.le.(3*nx/4)) then
                   if (iy.ge.1 .and. iy.le.(ny/4)) then                         
                      if (iz.ge.(nz/4) .and. iz.le.(3*nz/4)) then
                         grid(ix,iy,iz)%imaterial_type=3002 !earth 1.0
                         if (ix.le.5 .or. ix.ge.(nx-5)) then
                            if (iy.le.5 .or. iy.ge.(ny-5)) then
                               if (iz.le.5 .or. iz.ge.(nz-5)) then
                                  grid(ix,iy,iz)%imaterial_type=3004 !clay 
                                  if (ix.le.((nx/2)+1) .and. ix.ge.((nx/2)-1)) then
                                     if (iy.le.((ny/4)+1) .and. iy.ge.((ny/4)-1)) then
                                        if (iz.le.((nz/2)+1) .and. iz.ge.((nz/2)-1)) then
                                           grid(ix,iy,iz)%imaterial_type=14 !3d silicon bee
                                        end if
                                     end if
                                  end if
                               end if
                            end if
                         end if
                      end if
                   end if
                end if
             end do
          end do
       end do
       
    end if
  end subroutine set_grid002
  
  
  subroutine set_grid001(grid,cellengthx,cellengthy,cellengthz) !bee in plastic box (a)
    
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
    real(real12), dimension(nx, ny,nz) :: T, TN
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    real(real12) :: pa,pb,A,L
    
    !!Need to create uniform case!
    L = 0.003  ! 1cm bee case for 3x3x3
    A = 1.0  !1e-06
    cellengthx=L
    cellengthy=L
    cellengthz=L

    !!bulk 
    if (nz.eq.1) then

       do ix=1, nx
          do iy=1, ny
             grid(ix,iy,1)%imaterial_type=0
             if (ix.le.((nx/2)+1) .and. ix.ge.((nx/2)-1)) then 
                if (iy.le.((nx/2)+1) .and. iy.ge.((nx/2)-1)) then
                   grid(ix,iy,1)%imaterial_type=14   !2d silicon bee
                end if
             end if
          end do
       end do
       print*, ix, iy
    else
       
       do ix=1,nx
          do iy=1,ny
             do iz=1,nz
                grid(ix,iy,iz)%imaterial_type=0
                if (ix.eq.1 .or. ix.eq.nx) then
                   if (iy.eq.1 .or. iy.eq.ny) then
                      if (iz.eq.1 .or. iz.eq.nz) then
                         grid(ix,iy,iz)%imaterial_type=3001 !plastic coating of depth 3mm
                         if (ix.le.((nx/2)+1) .and. ix.ge.((nx/2)-1)) then
                            if (iy.le.((ny/2)+1) .and. iy.ge.((ny/2)-1)) then
                               if (iz.le.((nz/2)+1) .and. iz.ge.((nz/2)-1)) then
                                  grid(ix,iy,iz)%imaterial_type=14 !3d silicon bee
                               end if
                            end if
                         end if
                      end if
                   end if
                end if
             end do
          end do
       end do
       
    end if
  end subroutine set_grid001
  
  subroutine set_grid000(grid,cellengthx,cellengthy,cellengthz) !single bee 
    
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
    real(real12), dimension(nx, ny,nz) :: T, TN
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    real(real12) :: pa,pb,A,L
  
    !!Need to create uniform case!
    L = 0.003  ! 1cm bee case for 3x3x3
    A = 1.0  !1e-06
    cellengthx=L
    cellengthy=L
    cellengthz=L

    !!bulk 
    if (nz.eq.1) then

       do ix=1, nx
          do iy=1, ny
             grid(ix,iy,1)%imaterial_type=0
             if (ix.le.((nx/2)+1) .and. ix.ge.((nx/2)-1)) then 
                if (iy.le.((nx/2)+1) .and. iy.ge.((nx/2)-1)) then
                   grid(ix,iy,1)%imaterial_type=14   !2d silicon bee
                end if
             end if
          end do
       end do
       !       print*, ix, iy
    else
       
       do ix=1,nx
          do iy=1,ny
             do iz=1,nz
                grid(ix,iy,iz)%imaterial_type=14
                if (ix.le.((nx/2)+1) .and. ix.ge.((nx/2)-1)) then
                   if (iy.le.((ny/2)+1) .and. iy.ge.((ny/2)-1)) then
                      if (iz.le.((nz/2)+1) .and. iz.ge.((nz/2)-1)) then
                         grid(ix,iy,iz)%imaterial_type=14 !3d silicon bee
                      end if
                   end if
                end if
             end do
          end do
       end do
       
    end if
  end subroutine set_grid000
  
  subroutine set_grid10(grid,cellengthx,cellengthy,cellengthz)
    
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
    real(real12), dimension(nx, ny,nz) :: T, TN
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    real(real12) :: LT,AT,Lz
    
    !!Need to create uniform case!
    LT = 1.0e-2
    Lz=1.0e-2
    cellengthx=LT/REAL(nx)
    cellengthy=LT/real(ny)
    cellengthz=Lz/real(nz)
    !!bulk 
    do ix=1,nx
       do iy=1,ny
          do iz=1,nz
             !             if (nx.gt.nx/4) then
             !                cellengthx(ix)=L*1.0
             !             end if
             grid(ix,iy,iz)%imaterial_type=32
             
          end do
       end do
    end do
    
    
  end subroutine set_grid10
  
  
  subroutine set_grid1(grid,cellengthx,cellengthy,cellengthz)
    
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
    real(real12), dimension(nx, ny,nz) :: T, TN
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    real(real12) :: pa,pb,A,L
    
    !For syscfg = 1
    L = 1e-04
    A = 1e-08
    
    !x axis
    do ix=1,nx
       cellengthx(ix)=L
    end do
    
    !y axis 
    do ix=1,ny
       if (iy.eq.(ny/2)) then
          cellengthy(ix)=0.35e-09
       else if ((iy.eq.((ny/2)+1).or.(iy.eq.((ny/2)-1)))) then
          cellengthy(ix)=0.35e-07
       else
          cellengthy(ix)=L
       end if
  end do
  
  
  
  !z axis
  do ix=1,nz
     cellengthz(ix)=L
  end do
  
  
  do ix=1,nx
     do iy=1,ny
        do iz=1,nz
           
           
           !!APPROXIMATE THERMOACOUSTIC TRANSDUCER, SET SYSTEM PARAMETER TO 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           !!Parameters Input (Defines material composition of the simulated volume)
           !!Silicon
           if (iy.lt.(ny/2)) then
              grid(ix,iy,iz)%imaterial_type=2
              
              !!Graphene
           else if (iy.eq.(ny/2)) then
              grid(ix,iy,iz)%imaterial_type=3
              
              !!Gold
           else if (((ix.le.((nx/10)+1)).or.(ix.gt.(nx * 9.0/10.0))).and.(((iy.gt.(ny/2))).and.(iy.le.((ny/2)+2)))) then
              grid(ix,iy,iz)%imaterial_type=4
              
              !!Air
           else
              grid(ix,iy,iz)%imaterial_type=1
           end if
        end do
     end do
  end do
  
end subroutine set_grid1


subroutine set_grid2(grid,cellengthx,cellengthy,cellengthz)
  !!SH notes he is not sure if this is the same as case10
  TYPE(heatblock), dimension(nx,ny,nz) :: grid
  integer(int12) :: ix,iy,iz,itime,i,it,j,k
  real(real12), dimension(nx, ny,nz) :: T, TN
  real(real12), dimension(nx) :: cellengthx
  real(real12), dimension(ny) :: cellengthy
  real(real12), dimension(nz) :: cellengthz
  real(real12) :: pa,pb,A,L
  L = 1e-04
  A = 1e-08
  cellengthx=L
  cellengthy=L
  cellengthz=L
  do ix=1,nx
     do iy=1,ny
        do iz=1,nz
           grid(ix,iy,iz)%imaterial_type=2
        end do
     end do
  end do
  
end subroutine set_grid2

subroutine set_grid_pig(grid,cellengthx,cellengthy,cellengthz)
  !!SH notes that this may need fixing
  TYPE(heatblock), dimension(nx,ny,nz) :: grid
  integer(int12) :: ix,iy,iz,itime,i,it,j,k
  real(real12), dimension(nx, ny,nz) :: T, TN
  real(real12), dimension(nx) :: cellengthx
  real(real12), dimension(ny) :: cellengthy
  real(real12), dimension(nz) :: cellengthz
  real(real12) :: r_skin, r_fat,r_air,A,L,rZ
  
  !!air/epdermis/fat/muscle          
  L = 2e-04
  cellengthx=L
  cellengthy=L
  cellengthz=L                                           
  
  print*, 'total sim size is'
  print*, 'x - ', REAL(nx)*L
  print*, 'y - ', REAL(ny)*L
  print*, 'z - ', REAL(nz)*L+200.0*L
  R_skin=2.e-3
  r_fat=8.e-3
  
  r_air=REAL(nz/4-100)*L+100.0*2.0*L
  do ix=1,nx
     do iy=1,nx
        do iz=1,nz
           if (iz.lt.100) then
              cellengthz(iz)=2.0*L
           end if
           if (iz.gt.(nz-100)) then
              cellengthz(iz)=2.0*L
           end if
           if (iz.ge.nz/4) then
              rz=REAL(iz-nz/4)*L
           end if
           if (iz.lt.nz/4) then
              grid(ix,iy,iz)%imaterial_type=3001 !air  h 50
           else if (rz.lt.R_skin) then
              grid(ix,iy,iz)%imaterial_type=9002 !epidermis
           else if (rz.ge.R_skin.and.rz.le.(r_skin+r_fat)) then
              grid(ix,iy,iz)%imaterial_type=9003 !fat
           else
              grid(ix,iy,iz)%imaterial_type=9001 !muscle
           end if
           
        end do
     end do
  end do
  
end subroutine set_grid_pig


subroutine set_grid3(grid,cellengthx,cellengthy,cellengthz)
  !!SH notes that this may need fixing
  TYPE(heatblock), dimension(nx,ny,nz) :: grid
  integer(int12) :: ix,iy,iz,itime,i,it,j,k
  real(real12), dimension(nx, ny,nz) :: T, TN
  real(real12), dimension(nx) :: cellengthx
  real(real12), dimension(ny) :: cellengthy
  real(real12), dimension(nz) :: cellengthz
  real(real12) :: pa,pb,A,L
  
  !!Silicon/air           
  L = 1e-04
  A = 1e-08
  cellengthx=L
  cellengthy=L
  cellengthz=L                                           
  do ix=1,nx
     do iy=1,nx
        do iz=1,nz
           
           if (iy.le.(ny/2)) then
              grid(ix,iy,iz)%imaterial_type=2
           else
              grid(ix,iy,iz)%imaterial_type=1
           end if
        end do
     end do
  end do
  
end subroutine set_grid3


subroutine set_grid_bee(grid,cellengthx,cellengthy,cellengthz)
  !!SH notes that this may need fixing
  TYPE(heatblock), dimension(nx,ny,nz) :: grid
  integer(int12) :: ix,iy,iz,itime,i,it,j,k
  real(real12), dimension(nx, ny,nz) :: T, TN
  real(real12), dimension(nx) :: cellengthx
  real(real12), dimension(ny) :: cellengthy
  real(real12), dimension(nz) :: cellengthz
  integer, dimension(nx,ny) :: bee
  real(real12) :: pa,pb,A,L
  
  read(12,*) 
  do i=1,nx
     read(12,*) (bee(i,j),j=1,ny)
     !print*, i,bee(i,40)
  end do
  
  !!Silicon/air           
  L = 1e-04
  A = 1e-08
  cellengthx=L
  cellengthy=L
  cellengthz=L                                           
  do ix=1,nx
     do iy=1,ny
        do iz=1,nz
           
           if (bee(ix,iy).eq.1) then
              grid(ix,iy,iz)%imaterial_type=2000
           else
              grid(ix,iy,iz)%imaterial_type=1000
           end if
        end do
     end do
  end do
  
end subroutine set_grid_bee

subroutine set_grid_2mat(grid,cellengthx,cellengthy,cellengthz)
  !!SH notes that this may need fixing
  TYPE(heatblock), dimension(nx,ny,nz) :: grid
  integer(int12) :: ix,iy,iz,itime,i,it,j,k
  real(real12), dimension(nx, ny,nz) :: T, TN
  real(real12), dimension(nx) :: cellengthx
  real(real12), dimension(ny) :: cellengthy
  real(real12), dimension(nz) :: cellengthz
  integer, dimension(nx,ny) :: bee
  real(real12) :: pa,pb,A,L
  
  !!Silicon/air           
  L = 1e-02
  cellengthx=L/nx
  cellengthy=L/ny
  cellengthz=L/nz                                           
  do ix=1,nx
     do iy=1,ny
        do iz=1,nz
           if (iz .le. nz/2) then
              grid(ix,iy,iz)%imaterial_type=32 
           else
              grid(ix,iy,iz)%imaterial_type=32
           end if
        end do
     end do
  end do
  
end subroutine set_grid_2mat

subroutine set_grid_thermophone(grid,cellengthx,cellengthy,cellengthz)
  !!SH notes that this may need fixing
  TYPE(heatblock), dimension(nx,ny,nz) :: grid
  integer(int12) :: ix,iy,iz,itime,i,it,j,k
  real(real12), dimension(nx, ny,nz) :: T, TN
  real(real12), dimension(nx) :: cellengthx
  real(real12), dimension(ny) :: cellengthy
  real(real12), dimension(nz) :: cellengthz
  integer, dimension(nx,ny) :: bee
  real(real12) :: pa,pb,A,L
  
  if (nx*ny*nz .ne. 27000) then
     print*, "incorrect grid size for chosen set_grid"
     stop
    end if

  !!Silicon/air           
  L = 1e-01
  cellengthx=L/nx
  cellengthy=L/ny
  cellengthz=L/nz                                           
  do ix=1,nx
     do iy=1,ny
        do iz=1,nz
           if (iz .le. 10) then
              grid(ix,iy,iz)%imaterial_type=32 !SiO2 
           else if (iz .le. 17) then
              grid(ix,iy,iz)%imaterial_type=34 !SU-8
           else if (iz .le. 22) then
              grid(ix,iy,iz)%imaterial_type=35 !ITO
           else
              grid(ix,iy,iz)%imaterial_type=0 !Air
           end if
        end do
     end do
  end do
  
end subroutine set_grid_thermophone

end MODULE SET_GRID_UP




!##############################################################################################################
! This calculates the temperature gradient between a system and its boundary.
! The boundary conditions can be selected by assigning correct value to iboundary in inputs.txt
!##############################################################################################################
MODULE DeT
  use constants
  use parameters
  use constructions
  use heating
  implicit none
contains

  subroutine Delta_T(T,DeT,ix,iy,iz)
    !this subroutine does two things:
    !1)averages properties over adjacent cells 
    !2) It selects the boundary conditions (three types, discussed in entry)
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    real(real12), dimension(nx,ny,nz) :: T
    real(real12), dimension(3,2) :: DeT, kap, h_con, L_c
    !DeT = array of delta t in direction_i (i)
    ! and (j) between cells ix-1 and ix, or ix and ix+1
    integer :: ix,iy,iz
    
    
    select case(iboundary)
    case(1)
       !Boundary type 1, all surroundings are connected to heat bath at 
       !temperature=T_bath
       
       
       if (ix.gt.1) then
          DeT(1,1)=T(ix,iy,iz)-T(ix-1,iy,iz)
       else 
          
          DeT(1,1)=T(ix,iy,iz)-T_bath
       end if
       if (ix.lt.nx) then
          DeT(1,2)=T(ix,iy,iz)-T(ix+1,iy,iz)
          !print*, DeT(1,2)
       else
          DeT(1,2)=T(ix,iy,iz)-T_bath
       end if
       
       if (iy.gt.1) then
          DeT(2,1)=T(ix,iy,iz)-T(ix,iy-1,iz)
       else
          DeT(2,1)=T(ix,iy,iz)-T_bath
          
       end if
       
       if (iy.lt.ny) then
          DeT(2,2)=T(ix,iy,iz)-T(ix,iy+1,iz)
       else
          DeT(2,2)=T(ix,iy,iz)-T_bath
       end if
       
       if (iz.gt.1) then
          DeT(3,1)=T(ix,iy,iz)-T(ix,iy,iz-1)
       else
          DeT(3,1)=T(ix,iy,iz)-T_bath
       end if
       
       if (iz.lt.nz) then
          DeT(3,2)=T(ix,iy,iz)-T(ix,iy,iz+1)
       else
          DeT(3,2)=T(ix,iy,iz)-T_bath
       end if
    case(2)
       !Case not implemented yet, need to introduce seperate T_baths for various edges.
       !intended to be so one edge can be hot, the other cold etc.
    case(3)
       
       !Boundary type 3, outer cells are the heat bath
       !temperature=T_bath
       
       
       if (ix.gt.1) then
          DeT(1,1)=T(ix,iy,iz)-T(ix-1,iy,iz)
       else 
          
          DeT(1,1)=0.0
       end if
       if (ix.lt.nx) then
          DeT(1,2)=T(ix,iy,iz)-T(ix+1,iy,iz)
       else
          DeT(1,2)=0.0
       end if
       
       if (iy.gt.1) then
          DeT(2,1)=T(ix,iy,iz)-T(ix,iy-1,iz)
       else
          DeT(2,1)=0.0
          
       end if
       
       if (iy.lt.ny) then
          DeT(2,2)=T(ix,iy,iz)-T(ix,iy+1,iz)
       else
          DeT(2,2)=0.0
       end if
       
       if (iz.gt.1) then
          DeT(3,1)=T(ix,iy,iz)-T(ix,iy,iz-1)
       else
          DeT(3,1)=0.0
       end if
       
       if (iz.lt.nz) then
          DeT(3,2)=T(ix,iy,iz)-T(ix,iy,iz+1)
       else
          DeT(3,2)=0.0
       end if
       
    case(101)
       !Boundary type 101, Periodic over Z axis
       !temperature=T_bath
       
       
       if (ix.gt.1.and.ix.lt.nx) then
          DeT(1,1)=T(ix,iy,iz)-T(ix-1,iy,iz)
          DeT(1,2)=T(ix,iy,iz)-T(ix+1,iy,iz)
       else if (ix.eq.1) then
          DeT(1,1)=T(ix,iy,iz)-T_bath
          DeT(1,2)=T(ix,iy,iz)-T(ix+1,iy,iz)
       else if (ix.eq.nx) then
          DeT(1,1)=T(ix,iy,iz)-T(ix-1,iy,iz)
          DeT(1,2)=T(ix,iy,iz)-T_bath
       end if
       
       if (iy.gt.1.and.iy.lt.ny) then
          DeT(2,1)=T(ix,iy,iz)-T(ix,iy-1,iz)
          DeT(2,2)=T(ix,iy,iz)-T(ix,iy+1,iz)
       else if (iy.eq.1) then
          DeT(2,1)=T(ix,iy,iz)-T_bath
          DeT(2,2)=T(ix,iy,iz)-T(ix,iy+1,iz)
       else if (iy.eq.ny) then
          DeT(2,1)=T(ix,iy,iz)-T(ix,iy-1,iz)
          DeT(2,2)=T(ix,iy,iz)-T_bath
       end if
       
       
       !Following Josh Nelsons implementation
       if (iz.gt.1) then
          DeT(3,1)=T(ix,iy,iz)-T(ix,iy,iz-1)
       else
          DeT(3,1)=T(ix,iy,iz)-T(ix,iy,nz)
       end if
       
       if (iz.lt.nz) then
          DeT(3,2)=T(ix,iy,iz)-T(ix,iy,iz+1)
       else
          DeT(3,2)=T(ix,iy,iz)-T(ix,iy,1)
       end if
       
    end select
    
    
    
    
  end subroutine Delta_T
  
end MODULE DeT

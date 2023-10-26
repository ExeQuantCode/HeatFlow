MODULE DeA
  use constants, only: real12, int12
  use inputs, only: nx,ny,nz, icell_mix, grid
  use constructions, only: heatblock
  use materials, only: material
  implicit none
contains

  subroutine Delta_Ave(T,Kap,l_c,H_con,ix,iy,iz)
!this subroutine does two things:
!1)averages properties over adjacent cells 
!2) It selects the boundary conditions (three types, discussed in entry)
!3)
   real(real12), dimension(nx,ny,nz) :: T
  ! integer, parameter :: e = nx*ny*nz
  ! real(real12), dimension(e) = T
 real(real12), dimension(3,2) :: DeT, kap, h_con, L_c
  !real(real12), dimension(3,3) :: kappa3D
   real(real12) :: kappa, kappa3D
!convective current!
   real(real12) :: h_conv
   real(real12) :: heat_capacity
   real(real12) :: soundspeed
   real(real12) :: rho
   real(real12) :: volume
   real(real12) :: tau
!DeT = array of delta t in direction_i (i)
! and (j) between cells ix-1 and ix, or ix and ix+1
 integer(int12), intent(in) :: ix,iy,iz
 

!General averaging block


  


!We do edges, corners are not included and should technically have a seperate block, ERROR for corners is assumed to be small?!?

!!average in x-direction, on "left" side of cell
 if (ix.gt.1) then
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix, iy, iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(1,1)=kappa
    h_con(1,1)=h_conv
    CALL material(grid(ix-1,iy,iz)%imaterial_type, T(ix-1,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    
    select case(icell_mix)
    case(1)
       kap(1,1)=min(kap(1,1),kappa)
       h_con(1,1)=min(h_con(1,1),h_conv)
    case(2)
       kap(1,1)=(kap(1,1)+kappa)/2.0
       h_con(1,1)=(h_con(1,1)+h_conv)/2.0
    end select
    L_c(1,1)=grid(ix-1,iy,iz)%length(1)+grid(ix,iy,iz)%length(1)
   
  !  PRINT*, L_c(1,1), grid(ix-1,iy,iz)%length(1),grid(ix,iy,iz)%length(1)
 else 
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(1,1)=kappa
    h_con(1,1)=h_conv
    L_c(1,1)=grid(ix,iy,iz)%length(1)*2.0
 end if

!!average in x-direction, on "right" side of cell
 if (ix.lt.nx) then
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(1,2)=kappa
    h_con(1,2)=h_conv
    CALL material(grid(ix+1,iy,iz)%imaterial_type, T(ix+1,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    
    select case(icell_mix)
    case(1)
       kap(1,2)=min(kap(1,2),kappa)
       h_con(1,2)=min(h_con(1,2),h_conv)
    case(2)
       kap(1,2)=(kap(1,2)+kappa)/2.0
       h_con(1,2)=(h_con(1,2)+h_conv)/2.0
    end select
    L_c(1,2)=grid(ix+1,iy,iz)%length(1)+grid(ix,iy,iz)%length(1)
 else 
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(1,2)=kappa
    h_con(1,2)=h_conv
    L_c(1,2)=grid(ix,iy,iz)%length(1)*2.0
 end if

!!average in y direction on "front" side of cell
 if (iy.gt.1) then
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(2,1)=kappa
    h_con(2,1)=h_conv
    CALL material(grid(ix,iy-1,iz)%imaterial_type, T(ix,iy-1,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
  
    select case(icell_mix)
    case(1)
       kap(2,1)=min(kap(2,1),kappa)
       h_con(2,1)=min(h_con(2,1),h_conv)
    case(2)
       kap(2,1)=(kap(2,1)+kappa)/2.0
       h_con(2,1)=(h_con(2,1)+h_conv)/2.0
    end select
    L_c(2,1)=grid(ix,iy-1,iz)%length(2)+grid(ix,iy,iz)%length(2)
 else 
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(2,1)=kappa
    h_con(2,1)=h_conv
    L_c(2,1)=grid(ix,iy,iz)%length(2)*2.0
 end if

!!average in y direction on "rear" side of cell
 if (iy.lt.ny) then
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(2,2)=kappa
    h_con(2,2)=h_conv
    CALL material(grid(ix,iy+1,iz)%imaterial_type, T(ix,iy+1,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    
    select case(icell_mix)
    case(1)
       kap(2,2)=min(kap(2,2),kappa)
       h_con(2,2)=min(h_con(2,2),h_conv)
    case(2)
       kap(2,2)=(kap(2,2)+kappa)/2.0
       h_con(2,2)=(h_con(2,2)+h_conv)/2.0
    end select
    L_c(2,2)=grid(ix,iy+1,iz)%length(2)+grid(ix,iy,iz)%length(2)
 else 
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(2,2)=kappa
    h_con(2,2)=h_conv
    L_c(2,2)=grid(ix,iy,iz)%length(2)*2.0
 end if
 


!!average in z direction on "bottom" side of cell
 if (iz.gt.1) then
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(3,1)=kappa
    h_con(3,1)=h_conv
    CALL material(grid(ix,iy,iz-1)%imaterial_type, T(ix,iy,iz-1), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    
    select case(icell_mix)
    case(1)
       kap(3,1)=min(kap(3,1),kappa)
       h_con(3,1)=min(h_con(3,1),h_conv)
    case(2)
       kap(3,1)=(kap(3,1)+kappa)/2.0
       h_con(3,1)=(h_con(3,1)+h_conv)/2.0
    end select
    L_c(3,1)=grid(ix,iy,iz-1)%length(3)+grid(ix,iy,iz)%length(3)
 else 
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(3,1)=kappa
    h_con(3,1)=h_conv
    L_c(3,1)=grid(ix,iy,iz)%length(3)*2.0
 end if

!!average in z direction on "top" side of cell
 if (iz.lt.nz) then
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(3,2)=kappa
    h_con(3,2)=h_conv
    CALL material(grid(ix,iy,iz+1)%imaterial_type, T(ix,iy,iz+1), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    
    select case(icell_mix)
    case(1)
       kap(3,2)=min(kap(3,2),kappa)
       h_con(3,2)=min(h_con(3,2),h_conv)
    case(2)
       kap(3,2)=(kap(3,2)+kappa)/2.0
       h_con(3,2)=(h_con(3,2)+h_conv)/2.0
    end select
    L_c(3,2)=grid(ix,iy,iz+1)%length(3)+grid(ix,iy,iz)%length(3)
 else 
    CALL material(grid(ix,iy,iz)%imaterial_type, T(ix,iy,iz), kappa, kappa3D, h_conv, heat_capacity, rho, soundspeed,tau)
    Kap(3,2)=kappa
    h_con(3,2)=h_conv
    L_c(3,2)=grid(ix,iy,iz)%length(3)*2.0
 end if

 

L_c=L_c/2.0

  end subroutine Delta_Ave

end MODULE DeA

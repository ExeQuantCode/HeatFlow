module heatermatrix
use dea
use materials
use heater
use inputs

contains

subroutine hmatrix(i,j,H)
  integer, intent(in) :: i, j, x, y, z, n
  real(real12), intent(in) :: alpha, A, B, D, E, F, G, kappa1
  real(real12), intent(out) :: H


  ! For ease atm
  n = nx

   
  alpha = (tau+time_step)/(time_step*time_step)


   ! x y and z of self 
    x = j - int(j/n)*n - int(j/(n**2))*(n**2)!Structure coordinate; int(j/n)*n refers the y row in the structure; int(j/(n**2))*(n**2) refers to the plane i.e z
    y = int(j/n) - int(j/(n**2))*(n**2) !Structure coordinate
    z = int(j/(n**2))!Structure coordinate

    if (x .gt. 1) then
     call material(grid(x,y,z)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
     kappa1 = kappa
   4
    call material(grid(x-1,y,z)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
     A = (kappa1 + kappa)/2 !kappa x left 
    else 
      A=0
    end if
    if (x.lt.nx) then
      call material(grid(x,y,z)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      kappa1 = kappa
      call material(grid(x+1,y,z)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      B = (kappa1 + kappa)/2 !kappa x right
    else
      B = 0
    end if

    if (y.gt.1) then
     call material(grid(x,y,z)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
     kappa1 = kappa
     call material(grid(x,y-1,z)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
     D = (kappa1+kappa)/2 !y down
    else
      D = 0
    end if
    if (y.lt.ny) then
      call material(grid(x,y,z)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      kappa1 = kappa
      call material(grid(x,y+1,z)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      E = (kappa1+kappa)/2 !y up
    else
      E = 0
    end if
    if (z.gt.1) then
     call material(grid(x,y,z)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
     kappa1 = kappa
     call material(grid(x,y,z-1)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
     F = (kappa1+kappa)/2 !z in
    else
      F = 0
    end if
    if (z.lt.nz) then
     call material(grid(x,y,z)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
     kappa1 = kappa
     call material(grid(x,y,z+1)%imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
     G = (kappa1+kappa)/2 !z out
    else
      G = 0
    end if

   ! Finding what the Hij element is connected to in system
   if (i .eq. j) then ! return the value of the self if self is in argument
      H = -((A+B+D+E+F+G)-alpha)
      return
   end if    
 
   if (i+1 .eq. j) then
      H = B
      return
   
   else if (i-1 .eq. j) then !X left and right neighbour of self respectivily 
      H = A 
      return

   else if (i+n .eq. j ) then 
      H = E 
      return
   
   else if (i-n .eq. j) then !Y down and up neighbour of self respectivily
      H = D 
      return
   
   else if (i+n**2 .eq. j) then
      H = G 
      return 
   
   else if (i-n**2 .eq. j) then !Z out and in neighbour of self respectivily
      H = F
      return
   else 
     H = 0
     return
   end if









end subroutine hmatrix

end module heatermatrix

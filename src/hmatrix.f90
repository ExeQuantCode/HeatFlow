!!!#################################################################################################
!!! This is the hmatrixmod module, which contains the procedures for the construction of ...
!!! ... the H matrix.
!!! This module contains the subroutines:
!!!   - hmatrixfunc, This function calculates the value of the H matrix based on the relationship...
!!!     ... between the indices i and j.
!!!   - calculate_alpha, This function calculates the value of alpha based on the indices ...
!!!     ... x, y, and z.
!!!   - calculate_conductivity, This function calculates the value of the conductivity based on ...
!!!     ...  the indices x_in, y_in, z_in, x_out, y_out, and z_out.
!!!   - altmod, This function calculates the modulus of a and b, and returns b if the modulus is 0.
!!!   - boundry_diag_term, This subroutine calculates the value of the diagonal term of ...
!!!     ... the H matrix based on the indices x_b, y_b, z_b, x, y, and z.
!!! This module contains the variables:
!!!   - H, The value of the H matrix.
!!!   - alpha, The value of alpha.
!!!   - conductivity, The value of the conductivity.
!!!   - kappa_ab, The value of kappa_ab.
!!!   - x, y, z: The indices of the grid.
!!!   - A, B, D, E, F, G, The values of the conductivities of the neighbors of the grid point.
!!!   - i, j, The indices of the grid points.
!!!   - x_in, y_in, z_in, x_out, y_out, z_out, The indices of the grid points.
!!!   - x_b, y_b, z_b, The indices of the grid points.
!!!   - kappa, The value of the conductivity.
!!!   - tau, The value of tau.
!!!   - rho, The value of rho.
!!!   - heat_capacity, The value of the heat capacity.
!!!   - kappa_in, kappa_out, The values of the conductivities of the grid points.
!!!   - kappaBoundx, kappaBoundy, kappaBoundz, The values of the boundary conductivities.
!!!   - inverse_time, The value of the inverse of the time.
!!!   - isteady, icattaneo, The values of the flags for the steady state and Cattaneo's model.
!!!   - nx, ny, nz, The number of grid points in the x, y, and z directions.
!!!   - time_step, The value of the time step.
!!!   - grid, The grid.

!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module hmatrixmod
  use constants, only: real12, int12, TINY
  use inputs, only: nx, ny, nz, time_step, grid
  use inputs, only: isteady, icattaneo, kappaBoundx1, kappaBoundy1, kappaBoundz1
  use inputs, only: kappaBoundNx, kappaBoundNy, kappaBoundNz
  use globe_data, only: inverse_time, lin_rhoc
  implicit none

contains

  function hmatrixfunc(i, j) result(H)
    implicit none
    integer(int12), intent(in) :: i, j
    integer(int12) :: x, y, z
    real(real12) :: alpha, A, B, D, E, F, G
    real(real12) :: H


    ! Calculate x, y, and z based on the 1D index j
    x = altmod(i,nx)
    y = mod((i-altmod(i,nx))/nx,ny)+1
    z = (i-altmod(i,nx*ny))/(nx*ny)+1

    alpha = calculate_alpha(x,y,z, i)
    A = calculate_conductivity(x - 1, y, z, x, y, z)
    B = calculate_conductivity(x + 1, y, z, x, y, z)
    D = calculate_conductivity(x, y - 1, z, x, y, z)
    E = calculate_conductivity(x, y + 1, z, x, y, z)
    F = calculate_conductivity(x, y, z - 1, x, y, z)
    G = calculate_conductivity(x, y, z + 1, x, y, z)


    ! Determine the value of H based on the relationship between i and j
    H=0.0_real12
    if ((i-j) .eq. 0)  then

      H = -(A + B + D + E + F + G ) - alpha ! Diagonal term (self interaction)
    end if 
    
    if ((i-j) .eq. 1) then
      if (x .eq. 1) then
        H=0.0_real12
      else
        H = A ! X left neighbor (left cell interaction)
      end if
    end if 

    if ((i-j) .eq. -1) then
      if (x .eq. nx) then
        H=0.0_real12 
      else
        H = B  ! X right neighbor (right cell interaction)
      end if
    end if

    if ((i-j) .eq. nx) then
      if (y.eq.1) then
        H=0.0_real12
      else
        H = D ! Y down neighbor (down cell interaction)
      end if 
    end if 
    if ((i-j) .eq. -nx) then
      if (y.eq.ny) then
        H=0.0_real12
      else
        H = E ! Y up neighbor (up cell interaction)
      end if 
    end if 

    if ((i-j) .eq. (nx*ny)) then
      if (z.eq. 1) then
        H=0.0_real12
      else
         H = F ! Z in neighbor (forward cell interaction)
      end if
    end if 

    if ((i-j) .eq. -(nx*ny)) then
      if (z .eq. nz) then
        H=0.0_real12
      else  
        H = G ! Z out neighbor (backward cell interaction)
      end if
    end if 
  end function hmatrixfunc

  function calculate_alpha(x, y, z, i) result(alpha)
    integer(int12), intent(in) :: x, y, z, i
    real(real12) :: tau, alpha

    alpha = 0.0_real12
    tau = grid(x,y,z)%tau
    if (isteady .eq. 0) then
      if (icattaneo .eq. 0) tau = 0.0_real12
      !tau is already divided by time_step**2
      alpha = (tau*lin_rhoc(i)) + (inverse_time*lin_rhoc(i)) 
    else
      alpha = 0.0_real12
    end if 
    
  end function calculate_alpha

  function calculate_conductivity(x_in, y_in, z_in, x_out, y_out, z_out) result(conductivity)
    integer(int12), intent(in) :: x_in, y_in, z_in, x_out, y_out, z_out
    real(real12) :: kappa_out, kappa_in, kappa_ab
    real(real12) :: conductivity

    kappa_ab=0
    
    ! if not an edge element
    if ((x_in .ge. 1) .and. (x_in .le. nx) .and. (y_in .ge. 1) .and. &
         (y_in .le. ny) .and. (z_in .ge. 1) .and. (z_in .le. nz)) then
       kappa_in = grid(x_in,y_in,z_in)%kappa
       kappa_out = grid(x_out,y_out,z_out)%kappa


       if(x_in .ne. x_out) then
          kappa_ab = (grid(x_in, y_in, z_in)%Length(1) + &
               grid(x_out, y_out, z_out)%Length(1))*kappa_in*kappa_out/&
               (grid(x_in, y_in, z_in)%Length(1)*kappa_out + &
               grid(x_out, y_out, z_out)%Length(1)*kappa_in)
               
          kappa_ab = kappa_ab/(grid(x_out, y_out, z_out)%Length(1))**2

       else if (y_in .ne. y_out) then
          kappa_ab = (grid(x_in, y_in, z_in)%Length(2) + &
              grid(x_out, y_out, z_out)%Length(2))*kappa_in*kappa_out/&
              (grid(x_in, y_in, z_in)%Length(2)*kappa_out + &
              grid(x_out, y_out, z_out)%Length(2)*kappa_in)
        
          kappa_ab = kappa_ab/(grid(x_out, y_out, z_out)%Length(2))**2

       else if (z_in .ne. z_out) then
          kappa_ab = (grid(x_in, y_in, z_in)%Length(3) + &
              grid(x_out, y_out, z_out)%Length(3))*kappa_in*kappa_out/&
              (grid(x_in, y_in, z_in)%Length(3)*kappa_out + &
              grid(x_out, y_out, z_out)%Length(3)*kappa_in)
        
          kappa_ab = kappa_ab/(grid(x_out, y_out, z_out)%Length(3))**2
       end if
       conductivity = (kappa_ab) 
    else
       CALL boundry_diag_term(x_in, y_in, z_in, x_out, y_out, z_out, kappa_ab)
       conductivity = kappa_ab 
    end if
  end function calculate_conductivity

  function altmod(a,b) result(c)
    integer(int12) :: a, b, c
    c=mod(a,b)
    if(c .eq. 0) c = b
  end function altmod

  subroutine boundry_diag_term(x_b, y_b, z_b, x, y, z, kappa_ab)
    integer(int12), intent(in) :: x_b, y_b, z_b, x, y, z
    real(real12) :: kappa, kappa_ab

    kappa = grid(x,y,z)%kappa

    
    if (x_b .ne. x) then
      if (x_b .lt. 1) then
       kappa_ab = (2*kappaBoundx1*kappa)/(kappaBoundx1+kappa)
      else if (x_b .gt. nx) then
        kappa_ab = (2*kappaBoundNx*kappa)/(kappaBoundNx+kappa)
      end if 
       kappa_ab = kappa_ab/(grid(x, y, z)%Length(1))**2

    else if (y_b .ne. y) then
      if (y_b .lt. 1) then
        kappa_ab = ((2)*kappaBoundy1*kappa)/(kappaBoundy1+kappa)
      else if (y_b .gt. ny) then
        kappa_ab = ((2)*kappaBoundNy*kappa)/(kappaBoundNy+kappa)
      end if
       kappa_ab = kappa_ab/(grid(x, y, z)%Length(2))**2

    else if (z_b .ne. z) then
      if (z_b .lt. 1) then
        kappa_ab = ((2)*kappaBoundz1*kappa)/(kappaBoundz1+kappa)
      else if (z_b .gt. nz) then
        kappa_ab = ((2)*kappaBoundNz*kappa)/(kappaBoundNz+kappa)
      end if
       kappa_ab = kappa_ab/(grid(x, y, z)%Length(3))**2

    end if
    
  end subroutine boundry_diag_term


end module hmatrixmod

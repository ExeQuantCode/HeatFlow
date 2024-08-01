!!!#################################################################################################
!!! This is the hmatrixmod module, which contains the procedures for the construction of ...
!!! ... the g matrix.
!!! This module contains the subroutines:
!!!   - gmatrixfunc, This function calculates the value of the g matrix based on the relationship...
!!!     ... between the indices i and j.
!!!   - calculate_conductivity, This function calculates the value of the conductivity based on ...
!!!     ...  the indices x_in, y_in, z_in, x_out, y_out, and z_out.
!!!   - altmod, This function calculates the modulus of a and b, and returns b if the modulus is 0.
!!! This module contains the variables:
!!!   - gv, The value of the g matrix.
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
module gmatrixmod
  use constants, only: real12, int12, TINY
  use inputs, only: nx, ny, nz, time_step, grid
  use inputs, only: isteady, icattaneo, kappaBoundx1, kappaBoundy1, kappaBoundz1
  use inputs, only: kappaBoundNx, kappaBoundNy, kappaBoundNz
  use globe_data, only: inverse_time, lin_rhoc
  implicit none

contains

  function gmatrixfunc(i, j) result(Gv)
    implicit none
    integer(int12), intent(in) :: i, j
    integer(int12) :: x, y, z
    real(real12) :: A, B, D, E, F, G, S
    real(real12) :: Gv


    ! Calculate x, y, and z based on the 1D index j
    x = altmod(i,nx)
    y = mod((i-altmod(i,nx))/nx,ny)+1
    z = (i-altmod(i,nx*ny))/(nx*ny)+1

    A = calculate_conductivity(x - 1, y, z, x, y, z)
    B = calculate_conductivity(x + 1, y, z, x, y, z)
    D = calculate_conductivity(x, y - 1, z, x, y, z)
    E = calculate_conductivity(x, y + 1, z, x, y, z)
    F = calculate_conductivity(x, y, z - 1, x, y, z)
    G = calculate_conductivity(x, y, z + 1, x, y, z)
    S = calculate_conductivity(x, y, z, x, y, z)
    ! Determine the value of H based on the relationship between i and j
    Gv=0.0_real12
    if ((i-j) .eq. 0)  then
      Gv = S ! Diagonal term (self interaction)
    end if 
    
    if ((i-j) .eq. 1) then
      if (x .eq. 1) then
        Gv=0.0_real12
      else
        Gv = A ! X left neighbor (left cell interaction)
      end if
    end if 

    if ((i-j) .eq. -1) then
      if (x .eq. nx) then
        Gv=0.0_real12 
      else
        Gv = B  ! X right neighbor (right cell interaction)
      end if
    end if

    if ((i-j) .eq. nx) then
      if (y.eq.1) then
        Gv=0.0_real12
      else
        Gv = D ! Y down neighbor (down cell interaction)
      end if 
    end if 
    if ((i-j) .eq. -nx) then
      if (y.eq.ny) then
        Gv=0.0_real12
      else
        Gv = E ! Y up neighbor (up cell interaction)
      end if 
    end if 

    if ((i-j) .eq. (nx*ny)) then
      if (z.eq. 1) then
        Gv=0.0_real12
      else
        Gv = F ! Z in neighbor (forward cell interaction)
      end if
    end if 

    if ((i-j) .eq. -(nx*ny)) then
      if (z .eq. nz) then
        Gv=0.0_real12
      else  
        Gv = G ! Z out neighbor (backward cell interaction)
      end if
    end if 
  end function gmatrixfunc

  function calculate_conductivity(x_in, y_in, z_in, x_out, y_out, z_out) result(conductivity)
    integer(int12), intent(in) :: x_in, y_in, z_in, x_out, y_out, z_out
    real(real12) :: kappa_ab, kappa, a, b, c, d, e, f
    real(real12) :: conductivity, lx2, ly2, lz2
    lx2 = grid(x_out, y_out, z_out)%Length(1)**2
    ly2 = grid(x_out, y_out, z_out)%Length(2)**2
    lz2 = grid(x_out, y_out, z_out)%Length(3)**2

    kappa_ab=0.0_real12
    kappa = 0.0_real12
    ! if not an edge element
    kappa = grid(x_out, y_out, z_out)%kappa

    if ((x_out .eq. 1) .and. (nx .ne. 1)) then
       a = kappaBoundx1
       b = grid(x_out+1, y_out, z_out)%kappa
    else if ((x_out .eq. nx) .and. (nx .ne. 1)) then
        b = kappaBoundNx
        a = grid(x_out-1, y_out, z_out)%kappa
    else if (nx .eq. 1) then
        a = kappaBoundx1
        b = kappaBoundNx
    else
       a = grid(x_out-1, y_out, z_out)%kappa
       b = grid(x_out+1, y_out, z_out)%kappa
    end if

    if ((y_out .eq. 1) .and. (ny .ne. 1 )) then
        c = kappaBoundy1
        d = grid(x_out, y_out+1, z_out)%kappa
    else if ((y_out .eq. ny) .and. (ny.ne.1)) then
        c = grid(x_out, y_out-1, z_out)%kappa
        d = kappaBoundNy
    else if (ny.eq.1) then
        c= kappaBoundy1
        d= kappaBoundNy
    else
        c = grid(x_out, y_out-1, z_out)%kappa
        d = grid(x_out, y_out+1, z_out)%kappa
    end if

    if ((z_out .eq. 1) .and. (nz .ne. 1)) then
        e = kappaBoundz1
        f = grid(x_out, y_out, z_out+1)%kappa
    else if ((z_out .eq. nz) .and. (nz .ne. 1)) then
        e = grid(x_out, y_out, z_out-1)%kappa
        f = kappaBoundNz
    else if (nz .eq. 1) then
        e = kappaBoundz1
        f = kappaBoundNz
    else
        e = grid(x_out, y_out, z_out-1)%kappa
        f = grid(x_out, y_out, z_out+1)%kappa
    end if

    if ((x_in .eq. x_out) .and. (y_in .eq. y_out) .and. (z_in .eq. z_out)) kappa_ab = 2*(kappa/lx2)+2*(kappa/ly2)+2*(kappa/lz2) - &
          ((a + b)/(lx2)) - ((c + d)/ly2) - ((e + f)/lz2) 

    if(x_in .ne. x_out) then
       if (x_in .lt. x_out) kappa_ab = a - kappa
       if (x_in .gt. x_out) kappa_ab = b - kappa 
            
       kappa_ab = kappa_ab/lx2

    else if (y_in .ne. y_out) then
        if (y_in .lt. y_out) kappa_ab = c - kappa
        if (y_in .gt. y_out) kappa_ab = d - kappa 

       kappa_ab = kappa_ab/ly2

    else if (z_in .ne. z_out) then
      if (z_in .lt. z_out) kappa_ab = e - kappa
      if (z_in .gt. z_out) kappa_ab = f - kappa
      kappa_ab = kappa_ab/lz2
    end if

           
    conductivity = (kappa_ab) 
  end function calculate_conductivity

  function altmod(a,b) result(c)
    integer(int12) :: a, b, c
    c=mod(a,b)
    if(c .eq. 0) c = b
  end function altmod

  

end module gmatrixmod

module hmatrixmod
  use constants, only: real12, int12
  use inputs, only: nx, ny, nz, time_step, grid, isteady, icattaneo, kappaBoundx, kappaBoundy, kappaBoundz, mixing
  use globe_data, only: inverse_time
  implicit none

contains

  function hmatrixfunc(i, j) result(H)
    implicit none
    integer(int12), intent(in) :: i, j
    integer(int12) :: x, y, z
    real(real12) :: alpha, A, B, D, E, F, G 
    real(real12) ::  tau
    real(real12) :: conductivity
    real(real12) :: H


    ! Calculate x, y, and z based on the 1D index j
    x = altmod(i,nx)
    y = mod((i-altmod(i,nx))/nx,ny)+1
    z = (i-altmod(i,nx*ny))/(nx*ny)+1

    Alpha = calculate_alpha(x,y,z)
    A = calculate_conductivity(x - 1, y, z, x, y, z)
    B = calculate_conductivity(x + 1, y, z, x, y, z)
    D = calculate_conductivity(x, y - 1, z, x, y, z)
    E = calculate_conductivity(x, y + 1, z, x, y, z)
    F = calculate_conductivity(x, y, z - 1, x, y, z)
    G = calculate_conductivity(x, y, z + 1, x, y, z)

   
    ! Determine the value of H based on the relationship between i and j
    H=0.0_real12
    if ((i-j) .eq. 0)  then

      H = -(A + B + D + E + F + G ) - alpha  ! Diagonal
    end if 
    
    if ((i-j) .eq. 1) then
      if (x .eq. 1) then
        H=0.0_real12
      else
         H = A  ! X left neighbor
      end if
    end if 

    if ((i-j) .eq. -1) then
      if (x .eq. nx) then
        H=0.0_real12
      else
        H = B  ! X right neighbor
      end if
    end if

    if ((i-j) .eq. nx) then
      if (y.eq.1) then
        H=0.0_real12
      else
        H = D  ! Y down neighbor
      end if 
    end if 
    if ((i-j) .eq. -nx) then
      if (y.eq.ny) then
        H=0.0_real12
      else
        H = E  ! Y up neighbor
      end if 
    end if 

    if ((i-j) .eq. (nx*ny)) then
      if (z.eq. 1) then
        H=0.0_real12
      else
         H = F  ! Z in neighbor
      end if
    end if 

    if ((i-j) .eq. -(nx*ny)) then
      if (z .eq. nz) then
        H=0.0_real12
      else  
        H = G  ! Z out neighbor
      end if
    end if 
  end function hmatrixfunc

  function calculate_alpha(x, y, z) result(alpha)
    integer(int12), intent(in) :: x, y, z
    real(real12) :: tau, alpha
    tau = grid(x,y,z)%tau
    if (isteady .eq. 0) then
      if (icattaneo .eq. 0) tau = 0.0_real12
      alpha = (tau) + (1+mixing)*(inverse_time*grid(x,y,z)%rho*grid(x,y,z)%heat_capacity/(2.0_real12))
    end if 
    
  end function calculate_alpha

  function calculate_conductivity(x_in, y_in, z_in, x_out, y_out, z_out) result(conductivity)
    integer(int12), intent(in) :: x_in, y_in, z_in, x_out, y_out, z_out
    real(real12) :: kappa_out, kappa_in, tau, kappa_ab
    real(real12) :: conductivity
    real(real12) :: T ! Dummy T value; as it seems unused in the original

    if ((x_in .ge. 1) .and. (x_in .le. nx) .and. (y_in .ge. 1) .and. &
         (y_in .le. ny) .and. (z_in .ge. 1) .and. (z_in .le. nz)) then
          kappa_in = grid(x_in,y_in,z_in)%kappa
          kappa_out = grid(x_out,y_out,z_out)%kappa


       if(x_in .ne. x_out) then
          kappa_ab = (grid(x_in, y_in, z_in)%Length(1) + grid(x_out, y_out, z_out)%Length(1))*kappa_in*kappa_out/&
               (grid(x_in, y_in, z_in)%Length(1)*kappa_out + grid(x_out, y_out, z_out)%Length(1)*kappa_in)
               kappa_ab = kappa_ab/(grid(x_out, y_out, z_out)%Length(1))**2

       else if (y_in .ne. y_out) then
          kappa_ab = (grid(x_in, y_in, z_in)%Length(2) + grid(x_out, y_out, z_out)%Length(2))*kappa_in*kappa_out/&
               (grid(x_in, y_in, z_in)%Length(2)*kappa_out + grid(x_out, y_out, z_out)%Length(2)*kappa_in)
               kappa_ab = kappa_ab/(grid(x_out, y_out, z_out)%Length(2))**2

       else if (z_in .ne. z_out) then
          kappa_ab = (grid(x_in, y_in, z_in)%Length(3) + grid(x_out, y_out, z_out)%Length(3))*kappa_in*kappa_out/&
               (grid(x_in, y_in, z_in)%Length(3)*kappa_out + grid(x_out, y_out, z_out)%Length(3)*kappa_in)
               kappa_ab = kappa_ab/(grid(x_out, y_out, z_out)%Length(3))**2

       end if
       conductivity = (kappa_ab) 
    else
       call boundry_diag_term(x_in, y_in, z_in,x_out, y_out, z_out,kappa_ab)
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
    real(real12), intent(out) :: kappa_ab
    real(real12) :: kappa

    kappa = grid(x,y,z)%kappa

    
    if(x_b .ne. x) then
       kappa_ab = (2*kappaBoundx*kappa)/(kappaBoundx+kappa)
       kappa_ab = kappa_ab/(grid(x, y, z)%Length(1))**2

    else if (y_b .ne. y) then
       kappa_ab = ((2)*kappaBoundy*kappa)/(kappaBoundy+kappa)
       kappa_ab = kappa_ab/(grid(x, y, z)%Length(2))**2

    else if (z_b .ne. z) then
       kappa_ab = ((2)*kappaBoundz*kappa)/(kappaBoundz+kappa)
       kappa_ab = kappa_ab/(grid(x, y, z)%Length(3))**2

    end if
    
  end subroutine boundry_diag_term

end module hmatrixmod

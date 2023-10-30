module hmatrixmod
  use constants, only: real12, int12
  use materials, only: material
  use inputs, only: nx, ny, nz, time_step, grid
  use globe_data, only: T

contains

  subroutine hmatrix(i, j, H)
    integer(int12), intent(in) :: i, j
    integer(int12) :: x, y, z
    real(real12) :: alpha, A, B, D, E, F, G
    real(real12), intent(out) :: H

    ! For ease atm
    alpha = (tau + time_step) / (time_step * time_step)

    ! Calculate x, y, and z based on the 1D index j
    x = mod(j, nx) 
    y = mod(j / nx, ny) + 1
    z = j / (nx * ny) + 1

    A = calculate_conductivity(x - 1, y, z, x, y, z)
    B = calculate_conductivity(x + 1, y, z, x, y, z)
    D = calculate_conductivity(x, y - 1, z, x, y, z)
    E = calculate_conductivity(x, y + 1, z, x, y, z)
    F = calculate_conductivity(x, y, z - 1, x, y, z)
    G = calculate_conductivity(x, y, z + 1, x, y, z)

    ! Determine the value of H based on the relationship between i and j
    H = 0
    if (i-j .eq. 0)     H = -((A + B + D + E + F + G) - alpha)  ! Diagonal
    if (i-j .eq. 1)     H = A  ! X left neighbor
    if (i-j .eq. -1)    H = B  ! X right neighbor
    if (i-j .eq. nx)    H = D  ! Y down neighbor
    if (i-j .eq. -nx)   H = E  ! Y up neighbor
    if (i-j .eq. nx*ny) H = F  ! Z in neighbor
    if (i-j .eq. -nx*ny) H = G ! Z out neighbor
    
  end subroutine hmatrix

  function calculate_conductivity(x_in, y_in, z_in, x, y, z) result(conductivity)
    integer(int12), intent(in) :: x_in, y_in, z_in, x, y, z
    real(real12) :: kappa1, kappa, kappa3D, h_conv, heat_capacity, rho, sound_speed, tau
    real(real12) :: conductivity
    real(real12) :: T ! Dummy T value; as it seems unused in the original

    if (x_in .ge. 1 .and. x_in .le. nx .and. y_in .ge. 1 .and. &
         y_in .le. ny .and. z_in .ge. 1 .and. z_in .le. nz) then
       call material(grid(x_in, y_in, z_in)%imaterial_type, T, kappa1, &
            kappa3D, h_conv, heat_capacity, rho, sound_speed, tau)
       call material(grid(x   , y   , z   )%imaterial_type, T, kappa, &
            kappa3D, h_conv, heat_capacity, rho, sound_speed, tau)
       conductivity = (kappa1 + kappa) / (2 * (rho * heat_capacity))
    else
       conductivity = 0
    end if
  end function calculate_conductivity

end module hmatrixmod

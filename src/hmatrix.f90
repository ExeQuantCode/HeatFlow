module hmatrixmod
  use constants, only: real12, int12
  use materials, only: material
  use inputs, only: nx, ny, nz, time_step, grid, isteady, kappaBoundx, kappaBoundy, kappaBoundz
  use globe_data, only: T

contains

  subroutine hmatrix(i, j, H)
    integer(int12), intent(in) :: i, j
    integer(int12) :: x, y, z
    real(real12) :: alpha, A, B, D, E, F, G 
    real(real12), intent(out) :: H

    ! For ease atm
    if (isteady .eq. 1) then
      alpha = 0
    else
      alpha = (tau + time_step) / (time_step * time_step)
    end if   
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
    write(*,*) A,B,D
    write(*,*) E,F,G

   
    ! Determine the value of H based on the relationship between i and j
    H = 0
    if ((i-j) .eq. 0)  H = -((A + B + D + E + F + G) - alpha)  ! Diagonal
    if ((i-j) .eq. 1)        H = A  ! X left neighbor
    if ((i-j) .eq. -1)       H = B  ! X right neighbor
    if ((i-j) .eq. nx)       H = D  ! Y down neighbor
    if ((i-j) .eq. -nx)      H = E  ! Y up neighbor
    if ((i-j) .eq. (nx*ny))  H = F  ! Z in neighbor
    if ((i-j) .eq. -(nx*ny)) H = G  ! Z out neighbor
  end subroutine hmatrix

  subroutine hmatrixB(i,j,B)
    integer(int12), intent(in) :: i, j
    integer(int12) :: x, y, z
    real(real12) :: alpha, A, C, D, E, F, G 
    real(real12), intent(out) :: B
  
    x = altmod(i,nx)
    y = mod((i-altmod(i,nx))/nx,ny)+1
    z = (i-altmod(i,nx*ny))/(nx*ny)+1

    A = calculate_conductivity(x - 1, y, z, x, y, z)
    C = calculate_conductivity(x + 1, y, z, x, y, z)
    D = calculate_conductivity(x, y - 1, z, x, y, z)
    E = calculate_conductivity(x, y + 1, z, x, y, z)
    F = calculate_conductivity(x, y, z - 1, x, y, z)
    G = calculate_conductivity(x, y, z + 1, x, y, z)
   
    ! Determine the value of H based on the relationship between i and j
    B = 0
    if (i.eq.j) then
      if ((x) .eq. 1) B = A  
      if ((x) .eq. nx) B = C
      ! if ((y) .eq. 1) B = D  
      ! if ((y) .eq. ny) B = E 
      ! if ((z) .eq. 1) B = F
      ! if ((z) .eq. nz) B = G 
    else
      !print*, i,j 
    end if 

  end subroutine hmatrixB


  function calculate_conductivity(x_in, y_in, z_in, x_out, y_out, z_out) result(conductivity)
    integer(int12), intent(in) :: x_in, y_in, z_in, x_out, y_out, z_out
    real(real12) :: kappa_out, kappa_in, kappa3D, h_conv, heat_capacity, rho, sound_speed, tau, kappa_ab
    real(real12) :: conductivity
    real(real12) :: T ! Dummy T value; as it seems unused in the original

    if ((x_in .ge. 1) .and. (x_in .le. nx) .and. (y_in .ge. 1) .and. &
         (y_in .le. ny) .and. (z_in .ge. 1) .and. (z_in .le. nz)) then
       call material(grid(x_in, y_in, z_in)%imaterial_type, T, kappa_in, &
            kappa3D, h_conv, heat_capacity, rho, sound_speed, tau)
       call material(grid(x_out, y_out, z_out)%imaterial_type, T, kappa_out, &
            kappa3D, h_conv, heat_capacity, rho, sound_speed, tau)
        !** Check implmentation of rho and heat_capacity
       write(*,*) 'x_out, x_in', x_out, x_in
       write(*,*) ''
       write(*,'(A,I1,A,I1,A)') '    L_in(',x_in,')                     L_out(',x_out,')'
       if(x_in .ne. x) then
          write(*,*) grid(x_in, y_in, z_in)%Length(1), grid(x_out, y_out, z_out)%Length(1)
          kappa_ab = (grid(x_in, y_in, z_in)%Length(1) + grid(x_out, y_out, z_out)%Length(1))/&
               (grid(x_in, y_in, z_in)%Length(1)*kappa_out + grid(x_out, y_out, z_out)%Length(1)*kappa_in)
       else if (y_in .ne. y) then
          kappa_ab = (grid(x_in, y_in, z_in)%Length(2) + grid(x_out, y_out, z_out)%Length(2))/&
               (grid(x_in, y_in, z_in)%Length(2)*kappa_out + grid(x_out, y_out, z_out)%Length(2)*kappa_in)
       else if (z_in .ne. z) then
          kappa_ab = (grid(x_in, y_in, z_in)%Length(3) + grid(x_out, y_out, z_out)%Length(3))/&
               (grid(x_in, y_in, z_in)%Length(3)*kappa_out + grid(x_out, y_out, z_out)%Length(3)*kappa_in)
       end if
       conductivity = (kappa_ab) / (2 * (rho * heat_capacity))
    else
       conductivity = 0
    end if
  end function calculate_conductivity

  function altmod(a,b) result(c)
    integer(int12) :: a, b, c
    c=mod(a,b)
    if(c .eq. 0) c = b
  end function altmod

end module hmatrixmod

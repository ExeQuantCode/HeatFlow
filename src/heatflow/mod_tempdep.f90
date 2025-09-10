!###################################################################################################
! Module: TempDep
! Description: This module reads the temperature dependent properties from the Material table file ...
!        ... for each grid point and constructs the sparse matrix
! Authors: Harry Mclean
! Variable descriptions:
!  index: Keeps track of the current grid point index
!  filename: Name of the Material table file associated with the current grid point
!  ix, iy, iz: Indices for the current grid point in the x, y, and z directions, respectively
!  i, j, k: General purpose loop variables
!  num_rows, num_cols: Number of rows and columns in the temperature table
!  iostat: I/O status for file operations
!  temp_table: 2D array to hold the temperature dependent properties
!
! Subroutine descriptions:
!  ChangeProp: Main subroutine that loops over all grid points, ...
!        ... reads the temperature dependent properties, and constructs the sparse matrix
!  ReadTempDepTable: Reads the temperature dependent properties ... 
!         ... from the Material table file for a given grid point
!###################################################################################################
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!                       THIS FEATURE IS A WORK IN PROGRESS. DO NOT USE.                    !!!!!
!!!!!                                                                                          !!!!!
!!!!!                                                                                          !!!!!     
!!!!!                                                                                          !!!!!      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module TempDep
    use inputs, only: Grid, TempDepProp, Nz, Ny, Nx, NA, time_step, grid
    ! use setup, only: sparse_Hmatrix
    use globe_data, only:  Temp_p, lin_rhoc, Temp_pp
    use constants, only: real12, int12
    use hmatrixmod, only: hmatrixfunc
    
    implicit none
    
    contains

    subroutine G_A(TP)
        implicit none
        integer(int12) :: index, ix, iy, iz
        real(real12), dimension(NA), intent(in) :: TP
        real(real12), dimension(:) :: T, CV 
            !G = G*(2/dt)
            ! A = A/dt

        

    end subroutine

    function Phi_func(G) result(Phi)
    implicit none
    integer(int12) :: i, j, k, indx
    real(real12), dimension(NA), intent(in) :: G
    real(real12), dimension(NA), intent(out) :: Phi
    real(real12) :: tau2

    indx = 1

    do k = 1, NA
        do j = 1, Ny
            do i = 1, Nx
                tau2 = grid(i,j,k)%tau * time_step*time_step ! tau = tau/time_step**2
                Phi(indx) = G(indx)+ 2.0_real12 * G(indx) * tau2
                indx = indx + 1
            end do
        end do
    end do


    end function

    function Gamma_func(G, A) result(Gamma)
    implicit none
    integer(int12) :: i, j, k, indx
    real(real12), dimension(NA), intent(in) :: G,A
    real(real12), dimension(NA), intent(out) :: Gamma
    real(real12) :: tau2
    indx = 1
    Gamma(:) = 0.0_real12
    do k = 1, Nz
        do j = 1, Ny
            do i = 1, Nx
                tau2 = grid(i,j,k)%tau * time_step*time_step ! tau = tau/time_step**2
                Gamma(indx) = A(indx) - (G(indx)*Temp_p(indx)) - ((4.0_real12*grid(i,j,k)%tau2*G(indx)*Temp_p(indx))/time_step) + &
                             ((G(indx)*grid(i,j,k)%tau2*Temp_pp(indx))/time_step) + A(indx)*(grid(i,j,k)%tau2/time_step)
                indx = indx + 1 
            end do
        end do
    end do
    end function

    function Omega_func() result(Omega)
    implicit none
    integer(int12) :: i, j, k, indx
    real(real12), dimension(Nx,Ny,Nz), intent(in) :: G,A
    real(real12), dimension(Nx,Ny,Nz), intent(out) :: Omega
    real(real12) :: tau2

    indx = 1
    do k = 1, Nz
        do j = 1, Ny
            do i = 1, Nx
                tau2 = grid(i,j,k)%tau * time_step*time_step ! tau = tau/time_step**2
                Omega(indx) = (-A(indx)*Temp_p(indx)) + ((G(indx)*Temp_p(indx)*Temp_p(indx)*grid(i,j,k)%tau2)/time_step) + &
                             ((grid(i,j,k)%tau2/time_step)*((-2.0_real12*A(indx)*Temp_p(indx)) + (A(indx)*Temp_pp(indx))))
                indx = indx + 1
            end do
        end do
    end do
    end function

    function nl_F_Cat(T,phi, A, G, B, H) result(f_val)
    implicit none
    real(real12), dimension(NA) :: TS, phi, omega, gamma, f_val
    integer(int12) :: i,j,k, indx
    
    TS(:) = T(:)*T(:)
    !phi.dot(TS) + (gamma-H).dot(T) + omega - B
    indx = 1
    do indx = 1, NA
        f_val(indx) = phi(indx)*(TS(indx)) + (gamma(indx)-H(i,j,k))*T(indx) + omega(indx) - B(indx)
    end do
    end function

    function Jac_nl_F_Cat(T, phi, gamma, H) result(jac_val)
    implicit none
    real(real12) :: jac_val
    real(real12), dimension(NA) :: T, phi, gamma, H
    integer(int12) :: i,j,k, indx

    !2*(phi.dot(T)) + (gamma-H)
    do indx = 1, NA
        jac_val(indx) = 2*phi(indx)*T(indx) + (gamma(indx)-H(i,j,k))

    end do
    end function
end module TempDep
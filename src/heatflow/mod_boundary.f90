!!!#################################################################################################
!!! This module contains the subroutine that calculates the Boundary term for the S vector.
!!! The module contains the following subroutines:
!!! - boundary, The boundary subroutine calculates the boundary term for the S vector.
!!! The variables in this module are:
!!! - B, The boundary term for the S vector.
!!! - kappa, The thermal conductivity of the material.
!!! - I, The index of the grid point.
!!! - ix, iy, iz, The indices of the grid point in the x, y, and z directions.
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module boundary_vector
  use constants, only: real12, int12, TINY
  use inputs, only: NA,nx,ny,nz, grid, kappaBoundx1, kappaBoundy1, kappaBoundz1
  use inputs, only: kappaBoundNx, kappaBoundNy, kappaBoundNz, BR
  use inputs, only: Periodicx, Periodicy, Periodicz
  use inputs, only: T_Bathx1, T_Bathx2, T_Bathy1, T_Bathy2, T_Bathz1, T_Bathz2, T_BathCG
  use inputs, only: CG_x_m, CG_x_p, CG_y_m, CG_y_p, CG_z_m, CG_z_p
  use globe_data, only: Temp_p
  implicit none
contains

  !!!###############################################################################################
  subroutine boundary(B)
    real(real12), dimension(NA), intent(out) :: B
    real(real12) :: kappa, temp, kappaHarm
    integer(int12) :: I,ix,iy,iz
    real(real12) :: x1_power_dens, xn_power_dens, y1_power_dens, yn_power_dens, &
        z1_power_dens, zn_power_dens
    real(real12) :: x1_edge_vol, xn_edge_vol, y1_edge_vol, yn_edge_vol, &
        z1_edge_vol, zn_edge_vol


    !Bound term has a correction for the ix,iy,iz edges of our grid (first vector) ...
    !...and this can be both edges (second vector)
    !for example, the ix-axis has a left and right boundary,
    !these correspond to  Bound_Term(1,1) and Bound_Term(1,2)
    
    !-----------------------------------------------------------------------
    ! This is the boundary von nuemann power density
    !-----------------------------------------------------------------------
    ! This assumes all cells are the same size
    if (CG_z_m) then
        z1_edge_vol = grid(1,1,1)%volume * real(nx,real12) * real(ny,real12)
        z1_power_dens = T_BathCG / z1_edge_vol
    end if
    if (CG_z_p) then
        zn_edge_vol = grid(1,1,nz)%volume * real(nx,real12) * real(ny,real12)
        zn_power_dens = T_BathCG / zn_edge_vol
    end if
    if (CG_y_m) then
        y1_edge_vol = grid(1,1,1)%volume * real(nx,real12) * real(nz,real12)
        y1_power_dens = T_BathCG / y1_edge_vol
    end if
    if (CG_y_p) then
        yn_edge_vol = grid(1,ny,1)%volume * real(nx,real12) * real(nz,real12)
        yn_power_dens = T_BathCG / yn_edge_vol
    end if
    if (CG_x_m) then
        x1_edge_vol = grid(1,1,1)%volume * real(ny,real12) * real(nz,real12)
        x1_power_dens = T_BathCG / x1_edge_vol
    end if
    if (CG_x_p) then
        xn_edge_vol = grid(nx,1,1)%volume * real(ny,real12) * real(nz,real12)
        xn_power_dens = T_BathCG / xn_edge_vol
    end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !-----------------------------------------------------------------------------------------------
    !The boundary term is calculated for each grid point in the domain.
    !-----------------------------------------------------------------------------------------------
    I = 0
    do iz = 1, nz
        do iy = 1, ny
            do ix = 1, nx
                I = I + 1
                kappa = grid(ix, iy, iz)%kappa
    
                if (.not. Periodicx) then
                    if (ix .eq. 1) then
                        if (CG_x_m) then
                            B(I) = x1_power_dens
                        else
                            kappaHarm = (2*kappa*kappaBoundx1/(kappa+kappaBoundx1)) / &
                            (grid(ix, iy, iz)%Length(1)**2)
                            if (kappa .ne. kappaBoundx1) kappaHarm = kappaHarm*BR
                            B(I) = B(I) + (kappaHarm) * T_Bathx1
                        end if
                    end if
                    if (ix .eq. nx) then
                        if (CG_x_p) then
                            B(I) = xn_power_dens
                        else
                            kappaHarm = (2*kappa*kappaBoundNx/(kappa+kappaBoundNx)) / &
                            (grid(ix, iy, iz)%Length(1)**2)
                            if (kappa .ne. kappaBoundNx) kappaHarm = kappaHarm*BR
                            B(I) = B(I) + (kappaHarm) * T_Bathx2
                        end if
                    end if
                end if
    
                if (.not. Periodicy) then
                    if (iy .eq. 1) then
                        if (CG_y_m) then
                            B(I) = y1_power_dens
                        else
                            kappaHarm = (2*kappa*kappaBoundy1/(kappa+kappaBoundy1)) / &
                            (grid(ix, iy, iz)%Length(2)**2)
                            if (kappa .ne. kappaBoundy1) kappaHarm = kappaHarm*BR
                            B(I) = B(I) + (kappaHarm) * T_Bathy1
                        end if
                    end if
                    if (iy .eq. ny) then
                        if (CG_y_p) then
                            B(I) = yn_power_dens
                        else
                            kappaHarm = (2*kappa*kappaBoundNy/(kappa+kappaBoundNy)) / &
                            (grid(ix, iy, iz)%Length(2)**2)
                            if (kappa .ne. kappaBoundNy) kappaHarm = kappaHarm*BR
                            B(I) = B(I) + (kappaHarm) * T_Bathy2
                        end if
                    end if
                end if
    
                if (.not. Periodicz) then
                    if (iz .eq. 1) then
                        if (CG_z_m) then
                            B(I) = z1_power_dens
                        else
                            kappaHarm = (2*kappa*kappaBoundz1/(kappa+kappaBoundz1)) / &
                            (grid(ix, iy, iz)%Length(3)**2)
                            if (kappa .ne. kappaBoundz1) kappaHarm = kappaHarm*BR
                            B(I) = B(I) + (kappaHarm) * T_Bathz1
                        end if
                    end if
                    if (iz .eq. nz) then
                        if (CG_z_p) then
                            B(I) = zn_power_dens
                        else
                            kappaHarm = (2*kappa*kappaBoundNz/(kappa+kappaBoundNz)) / &
                            (grid(ix, iy, iz)%Length(3)**2)
                            if (kappa .ne. kappaBoundNz) kappaHarm = kappaHarm*BR
                            B(I) = B(I) + (kappaHarm) * T_Bathz2
                        end if
                    end if
                end if
            
            end do
        end do
    end do
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  end subroutine boundary
  !!!###############################################################################################



  !!!###############################################################################################
  !!! This function calculates the boundary temperature required to keep a constant gradient ...
  !!! ... across the boundary.
  !!!###############################################################################################
  function constantboundarytempgrad(I) result(temp)
    implicit none
    integer(int12), intent(in) :: I
    real(real12) :: temp

    !temp = Temp_p(I)/T_BathCG
    temp = Temp_p(I) + T_BathCG
    ! Dont we need to add T_BathCG to Temp_p(I)???

    ! y=mx+c m is the gradient T_BathCG
    ! x in our cell position x=N is the boundary x=N+1 is the ghost cell beyond the boundary
    ! Y is our temperature at cell x 
    ! so Y_N = T_BathCG * x_N + c
    ! and Y_N+1 = T_BathCG * x_N+1 + c
    ! c = Y_N - T_BathCG * x_N 
    ! Y_N+1 = T_BathCG * x_N+1 - T_BathCG * x_N +Y_N
    ! Y_N+1 = T_BathCG * (x_N+1 - x_N) + Y_N
    ! x_N+1 - x_N is unity since we bundle distance into kappa
    ! so Y_N+1 = T_BathCG + Y_N
    ! so we need to add T_BathCG to the temperature at the boundary 
  end function constantboundarytempgrad
  !!!###############################################################################################
  
end module boundary_vector

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
  use constants, only: real12, int12
  use inputs, only: NA,nx,ny,nz, grid, kappaBoundx1, kappaBoundy1, kappaBoundz1
  use inputs, only: kappaBoundNx, kappaBoundNy, kappaBoundNz
  use inputs, only: Periodicx, Periodicy, Periodicz
  use inputs, only: T_Bathx1, T_Bathx2, T_Bathy1, T_Bathy2, T_Bathz1, T_Bathz2, T_BathCG
  use globe_data, only: Temp_p
  implicit none
contains

  subroutine boundary(B)
    real(real12), dimension(NA), intent(out) :: B
    real(real12) :: kappa, temp
    integer(int12) :: I,ix,iy,iz


    !Bound term has a correction for the ix,iy,iz edges of our grid (first vector) ...
    !...and this can be both edges (second vector)
    !for example, the ix-axis has a left and right boundary,
    !these correspond to  Bound_Term(1,1) and Bound_Term(1,2)
    
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
                        if (T_BathCG .gt. 0) T_Bathx1 = constantboundarytempgrad(I)
                        B(I) = B(I) + ((2 * kappaBoundx1 * kappa) / (kappaBoundx1 + kappa)) / &
                               (grid(ix, iy, iz)%Length(1)**2) * T_Bathx1
                    end if
                    if (ix .eq. nx) then
                        if (T_BathCG .gt. 0) T_Bathx2 = constantboundarytempgrad(I)
                        B(I) = B(I) + ((2 * kappaBoundNx * kappa) / (kappaBoundNx + kappa)) / &
                               (grid(ix, iy, iz)%Length(1)**2) * T_Bathx2
                    end if
                end if
    
                if (.not. Periodicy) then
                    if (iy .eq. 1) then
                        if (T_BathCG .gt. 0) T_Bathy1 = constantboundarytempgrad(I)
                        B(I) = B(I) + ((2 * kappaBoundy1 * kappa) / (kappaBoundy1 + kappa)) / &
                               (grid(ix, iy, iz)%Length(2)**2) * T_Bathy1
                    end if
                    if (iy .eq. ny) then
                        if (T_BathCG .gt. 0) T_Bathy2 = constantboundarytempgrad(I)
                        B(I) = B(I) + ((2 * kappaBoundNy * kappa) / (kappaBoundNy + kappa)) / &
                               (grid(ix, iy, iz)%Length(2)**2) * T_Bathy2
                    end if
                end if
    
                if (.not. Periodicz) then
                    if (iz .eq. 1) then
                        if (T_BathCG .gt. 0) T_Bathz1 = constantboundarytempgrad(I)
                        B(I) = B(I) + ((2 * kappaBoundz1 * kappa) / (kappaBoundz1 + kappa)) / &
                               (grid(ix, iy, iz)%Length(3)**2) * T_Bathz1
                    end if
                    if (iz .eq. nz) then
                        if (T_BathCG .gt. 0) T_Bathz2 = constantboundarytempgrad(I)
                        B(I) = B(I) + ((2 * kappaBoundNz * kappa) / (kappaBoundNz + kappa)) / &
                               (grid(ix, iy, iz)%Length(3)**2) * T_Bathz2
                    end if
                end if
    
            end do
        end do
    end do
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  end subroutine boundary

  !!!###############################################################################################
  !!! This function calculates the boundary temperature required to keep a constant gradient ...
  !!! ... across the boundary.
  !!!###############################################################################################
  function constantboundarytempgrad(I) result(temp)
    implicit none
    integer(int12), intent(in) :: I
    real(real12) :: temp

    temp = Temp_p(I)/T_BathCG
    

  end function constantboundarytempgrad
  !!!###############################################################################################
  
end module boundary_vector

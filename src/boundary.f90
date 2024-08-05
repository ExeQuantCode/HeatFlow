!!!#################################################################################################
!!! This module contains the subroutine that calculates the Boundary term for the S vector.
!!! The module contains the following subroutines:
!!! - boundary, The boundary subroutine calculates the boundary term for the S vector.
!!! The variables in this module are:
!!! - B, The boundary term for the S vector.
!!! - kappa, The thermal conductivity of the material.
!!! - kappa2, The thermal conductivity of the material at the neighbouring grid point.
!!! - I, The index of the grid point.
!!! - ix, iy, iz, The indices of the grid point in the x, y, and z directions.
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module boundary_vector
  use constants, only: real12, int12, TINY
  use inputs, only: NA,nx,ny,nz, grid, kappaBoundx1, kappaBoundy1, kappaBoundz1
  use inputs, only: kappaBoundNx, kappaBoundNy, kappaBoundNz
  use inputs, only: T_Bathx1, T_Bathx2, T_Bathy1, T_Bathy2, T_Bathz1, T_Bathz2
  implicit none
contains

  subroutine boundary(B)
    real(real12), dimension(NA), intent(out) :: B
    real(real12) :: kappa
    integer(int12) :: I,ix,iy,iz

    I = 0
      do iz= 1,nz
         do iy= 1,ny
            do ix= 1,nx
              I = I+1
            kappa = grid(ix,iy,iz)%kappa
            
             select case (ix) 
               case (1)
                B(I)=B(I)+((2*kappaBoundx1*kappa)/(kappaBoundx1+kappa))/(&
                    (grid(ix,iy,iz)%Length(1)**2))*T_Bathx1

               end select

              select case(nx-ix) 
               case(0)
               B(I)=B(I)+((2*kappaBoundNx*kappa)/(kappaBoundNx+kappa))/(&
                    (grid(ix,iy,iz)%Length(1)**2))*T_Bathx2

               end select

             select case (iy)
             case (1)
                B(I)=B(I)+((2*kappaBoundy1*kappa)/(kappaBoundy1+kappa))/(&
                    (grid(ix,iy,iz)%Length(2)**2))*T_Bathy1

            end select

             select case(ny-iy)
             case(0)
                B(I)=B(I)+((2*kappaBoundNy*kappa)/(kappaBoundNy+kappa))/(&
                    (grid(ix,iy,iz)%Length(2)**2))*T_Bathy2

             end select

             select case (iz) 
             case (1)
                B(I)=B(I)+((2*kappaBoundz1*kappa)/(kappaBoundz1+kappa))/(&
                    (grid(ix,iy,iz)%Length(3)**2))*T_Bathz1
               end select

            select case(nz-iz)  
             case(0)
                B(I)=B(I)+((2*kappaBoundNz*kappa)/(kappaBoundNz+kappa))/(&
                    (grid(ix,iy,iz)%Length(3)**2))*T_Bathz2
            end select
          end do
      end do
   end do

  end subroutine boundary

  
end module boundary_vector

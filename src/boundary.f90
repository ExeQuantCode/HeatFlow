module boundary_vector
  use constants, only: real12, int12
  use inputs, only: NA,nx,ny,nz, T_Bath, grid, kappaBoundx, kappaBoundy, kappaBoundz
  implicit none
contains

  pure subroutine boundary(B)
    real(real12), dimension(NA), intent(out) :: B
    real(real12) :: kappa,heat_capacity,rho
    integer(int12) :: i,x,y,z


    !Bound term has a correction for the x,y,z edges of our grid (first vector) and this can be both edges (second vector)
    !for example, the x-axis has a left and right boundary,
    !these correspond to  Bound_Term(1,1) and Bound_Term(1,2)

    i = 0
      do z= 1,nz
         do y= 1,ny
            do x= 1,nx
         i = i+1

            heat_capacity = grid(x,y,z)%heat_capacity
            rho = grid(x,y,z)%rho
            kappa = grid(x,y,z)%kappa

             select case (x) 
               case (1) 
                B(I)=B(I)+((2*kappaBoundx*kappa)/(kappaBoundx+kappa))/(rho*heat_capacity*&
                (grid(x,y,z)%Length(1)**2))*T_Bath
               end select

              select case(nx-x) 
               case(0) 
               B(I)=B(I)+((2*kappaBoundx*kappa)/(kappaBoundx+kappa))/(rho*heat_capacity*&
                (grid(x,y,z)%Length(1)**2))*T_Bath
               end select

             select case (y)
             case (1)
                B(I)=B(I)+((2*kappaBoundy*kappa)/(kappaBoundy+kappa))/(rho*heat_capacity*&
                (grid(x,y,z)%Length(2)**2))*T_Bath
            end select

             select case(ny-y)
             case(0)
               B(I)=B(I)+((2*kappaBoundy*kappa)/(kappaBoundy+kappa))/(rho*heat_capacity*&
                (grid(x,y,z)%Length(2)**2))*T_Bath
             end select

             select case (z) 
             case (1)
                B(I)=B(I)+((2*kappaBoundz*kappa)/(kappaBoundz+kappa))/(rho*heat_capacity*&
                (grid(x,y,z)%Length(3)**2))*T_Bath
               end select

            select case(nz-z)  
             case(0)
                B(I)=B(I)+((2*kappaBoundz*kappa)/(kappaBoundz+kappa))/(rho*heat_capacity*&
                 (grid(x,y,z)%Length(3)**2))*T_Bath
            end select
          end do
      end do
   end do
  end subroutine boundary

  
end module boundary_vector

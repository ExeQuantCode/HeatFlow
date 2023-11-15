module boundary_vector
  use constants, only: real12, int12
  use inputs, only: NA,nx,ny,nz, T_Bath, grid, kappaBoundx, kappaBoundy, kappaBoundz
  use materials, only: material

  implicit none
contains

  subroutine boundary(B)
    real(real12), dimension(NA), intent(out) :: B

    integer(int12) :: i,j,k, IA


    !Bound term has a correction for the x,y,z edges of our grid (first vector) and this can be both edges (second vector)
    !for example, the x-axis has a left and right boundary,
    !these correspond to  Bound_Term(1,1) and Bound_Term(1,2)

    real(real12), dimension(3,2) :: BoundTerm
    IA=0
    BoundTerm = 0
    do k=1,nz
       do j=1,ny
          do i=1,nx

             IA=IA+1
             
             if (i.eq.1) then 
               call BoundaryTerms(BoundTerm, i, j, k)
                B(IA)=B(IA)+BoundTerm(1,1)*T_Bath
             end if 
             if (i.eq.nx) then
               call BoundaryTerms(BoundTerm, i, j, k)
                B(IA)=B(IA)+BoundTerm(1,2)*T_Bath
             end if

             if (j.eq.1) then 
               call BoundaryTerms(BoundTerm, i, j, k)
                B(IA)=B(IA)+BoundTerm(2,1)*T_Bath
             end if 
             if (j.eq.ny) then
               call BoundaryTerms(BoundTerm, i, j, k)
                B(IA)=B(IA)+BoundTerm(2,2)*T_Bath
             end if

             if (k.eq.1) then 
               call BoundaryTerms(BoundTerm, i, j, k)
                B(IA)=B(IA)+BoundTerm(3,1)*T_Bath
             end if 
             if (k.eq.nz) then
               call BoundaryTerms(BoundTerm, i, j, k)
                B(IA)=B(IA)+BoundTerm(3,2)*T_Bath
             end if

            end do
       end do
    end do
  end subroutine boundary

  subroutine BoundaryTerms(BoundTerm, x, y, z)
      real(real12), dimension(3,2), intent(out) :: BoundTerm
      integer(int12), intent(in) :: x, y, z
      real(real12) :: TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau

      


      call material(grid(x,y,z)%imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      
      if (x.eq.1) BoundTerm(1,1) = ((2*kappaBoundx*kappa)/(kappaBoundx+kappa))/(rho*heat_capacity*(grid(x,y,z)%Length(1)**2))
      if (x.eq.nx) BoundTerm(1,2) = ((2*kappaBoundx*kappa)/(kappaBoundx+kappa))/(rho*heat_capacity*(grid(x,y,z)%Length(1)**2))
      if (y.eq.1) BoundTerm(2,1) = ((2*kappaBoundy*kappa)/(kappaBoundy+kappa))/(rho*heat_capacity*(grid(x,y,z)%Length(2)**2))
      if (y.eq.ny) BoundTerm(2,2) = ((2*kappaBoundy*kappa)/(kappaBoundy+kappa))/(rho*heat_capacity*(grid(x,y,z)%Length(2)**2))
      if (z.eq.1) BoundTerm(3,1) = ((2*kappaBoundz*kappa)/(kappaBoundz+kappa))/(rho*heat_capacity*(grid(x,y,z)%Length(3)**2))
      if (z.eq.nz) BoundTerm(3,2) = ((2*kappaBoundz*kappa)/(kappaBoundz+kappa))/(rho*heat_capacity*(grid(x,y,z)%Length(3)**2))
   end subroutine BoundaryTerms   
end module boundary_vector

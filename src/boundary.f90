module boundary_vector
  use constants, only: real12, int12
  use inputs, only: NA,nx,ny,nz, T_Bath, grid, kappaBoundx, kappaBoundy, kappaBoundz
  use materials, only: material
contains

  subroutine boundary(B)
    real(real12), dimension(NA), intent(out) :: B
    real(real12), dimension(NA) :: BA

    integer(int12) :: i,j,k, IA


    !Bound term has a correction for the x,y,z edges of our grid (first vector) and this can be both edges (second vector)
    !for example, the x-axis has a left and right boundary,
    !these correspond to  Bound_Term(1,1) and Bound_Term(1,2)

    real(real12), dimension(3,2) :: BoundTerm
    B=0
    IA=0

    
    do i=1,nx
       do j=1,ny
          do k=1,nz
             IA=IA+1
             
             if (i.eq.1) then 
               call BoundaryTerms(BoundTerm, i, j, k)
                B(IA)=B(IA)+BoundTerm(1,1)*T_Bath
             else if (i.eq.NA) then
               call BoundaryTerms(BoundTerm, i, j, k)
                BA(IA)=B(IA)+BoundTerm(1,2)*T_Bath
             end if

             if (j.eq.1) then 
               call BoundaryTerms(BoundTerm, i, j, k)
                B(IA)=B(IA)+BoundTerm(2,1)*T_Bath
             else if (j.eq.NA) then
               call BoundaryTerms(BoundTerm, i, j, k)
                BA(IA)=B(IA)+BoundTerm(2,2)*T_Bath
             end if
             if (k.eq.1) then 
               call BoundaryTerms(BoundTerm, i, j, k)
                B(IA)=B(IA)+BoundTerm(3,1)*T_Bath
             else if (k.eq.NA) then
               call BoundaryTerms(BoundTerm, i, j, k)
                BA(IA)=B(IA)+BoundTerm(3,2)*T_Bath
             end if
          end do
       end do
    end do

  end subroutine boundary

  subroutine BoundaryTerms(BoundTerm, i, j, k)
      real(real12), dimension(3,2), intent(out) :: BoundTerm
      integer(int12), intent(in) :: i,j,k
      real(real12) :: TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau

      call material(grid(1,j,k)%imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      BoundTerm(1,1) = (kappa+kappaBoundx)/(2*rho*heat_capacity)

      call material(grid(nx,j,k)%imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      BoundTerm(1,2) = (kappa+kappaBoundx)/(2*rho*heat_capacity) 

      call material(grid(i,1,k)%imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      BoundTerm(2,1) = (kappa+kappaBoundy)/(2*rho*heat_capacity)

      call material(grid(i,ny,k)%imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      BoundTerm(2,2) = (kappa+kappaBoundy)/(2*rho*heat_capacity) 

      call material(grid(i,j,1)%imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      BoundTerm(3,1) = (kappa+kappaBoundz)/(2*rho*heat_capacity) 

      call material(grid(i,j,nz)%imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
      BoundTerm(3,2) = (kappa+kappaBoundz)/(2*rho*heat_capacity) 

   end subroutine BoundaryTerms   
end module boundary_vector

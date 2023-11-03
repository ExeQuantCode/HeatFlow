module boundary_vector
  use constants, only: real12, int12
  use inputs, only: NA,nx,ny,nz

contains

  subroutine boundary(B)
    real(real12), dimension(NA), intent(out) :: B
    real(real12), dimension(NA) :: BA

    integer :: i,j,k, IA


    !Bound term has a correction for the x,y,z edges of our grid (first vector) and this can be both edges (second vector)
    !for example, the x-axis has a left and right boundary,
    !these correspond to  Bound_Term(1,1) and Bound_Term(1,2)

    real(real12), dimension(3,3) :: BoundTerm
    B=0
    IA=0

    
    do i=1,nx
       do j=1,ny
          do k=1,nz
             IA=IA+1
             
             if (i.eq.1) then 
                B(IA)=B(IA)+BoundTerm(1,1)
             else if (i.eq.NA) then
                BA(IA)=B(IA)+BoundTerm(1,2)
             end if

             if (j.eq.1) then 
                B(IA)=B(IA)+BoundTerm(2,1)
             else if (i.eq.NA) then
                BA(IA)=B(IA)+BoundTerm(2,2)
             end if
             if (k.eq.1) then 
                B(IA)=B(IA)+BoundTerm(3,1)
             else if (k.eq.NA) then
                BA(IA)=B(IA)+BoundTerm(3,3)
             end if
          end do
       end do
    end do

    BoundTerm(1,1) = kappa_boundx0*T_heatbath_x/(rhox1*c_1)  
  end subroutine boundary

end module boundary_vector

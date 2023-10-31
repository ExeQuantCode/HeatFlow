
module setup
  use constants, only: real12, int12, TINY
  use inputs, only: Lx, Ly, Lz, nx, ny, nz, NA, grid
  use constructions, only: heatblock
  use hmatrixmod, only: hmatrix
  use globe_data, only: H, ra, T, TN, Told,TD,TPD,H
  use sparse, only: SRSin

  implicit none
  
   contains
    
   !------------------------------------------------
   !This sets up the HMatrix global variable so as a Sparse row storage.
   !-----------------------------------------------
   subroutine set_global_variables()
   real(real12) :: H0, hboundary
   integer(int12) :: i, j
   allocate(TN(nx, ny, nz))
   allocate(T(nx, ny, nz))
   allocate(Told(nx, ny, nz))
   allocate(TD(NA))
   allocate(TPD(NA))
   allocate(H(NA,NA))
   !------------------------------------------------
   !Make hmatrix
   !Hmatrix consists of H_ij,0 and H_ij,B (heat flow across grid and matrix correction for boundaries
   !-----------------------------------------------
   !H_ij T_i =S_j
   H=0.0
   hboundary = 0.0
   print*, NA
   do i=1,NA 
      do j=1,NA
        call hmatrix(i,j,H0)
		  
	     H(i,j)=H0
	     !if (HBoundary.eq.1) then
        !CALL HATRIX_BOUND(i,j,HB)
	     !   H(i,j)=H(i,j)+HB(i,j)
	     !end if
       end do
   end do

   ! Convert the matrix into Sparse Diagonal Storage.
   ! call SDSin(A,TINY, da)
   ! Convert the matrix into Sparse Row Storage.
   call SRSin(H,TINY, ra)
   
   end subroutine set_global_variables


subroutine Initiate()
  integer(int12) :: ix,iy,iz,itime,i,it,j,k
  real(real12), dimension(nx) :: cellengthx
  real(real12), dimension(ny) :: cellengthy
  real(real12), dimension(nz) :: cellengthz
  real(real12) :: pa,pb,A,L
  integer(int12) :: ii,jj,kk
  character(len=8192):: string

  ! calculate the cell lengths
  cellengthx(:) = Lx/real(nx)
  cellengthy(:) = Ly/real(ny)
  cellengthz(:) = Lz/real(nz)


 !!!This section calculates the cell lengths and areas based on inputs above
   ! if (it.eq.1) then  
   !   T=T_bath
   !   Told=T_bath
   !   TN=T_bath
   ! end if

      



 !!!This section calculates the cell lengths and areas based on inputs above
      do ix=1,nx
         do iy=1,ny
            do iz=1,nz
               do ii=1,3
                  if (ii.eq.1) then
                     grid(ix,iy,iz)%length(ii)=cellengthx(ix)
                  else if (ii.eq.2) then
                     grid(ix,iy,iz)%length(ii)=cellengthy(iy)
                  else if (ii.eq.3) then
                     grid(ix,iy,iz)%length(ii)=cellengthz(iz)
                     
                  end if
               end do
               do ii=1,3
                  jj=MOD(ii+1,3)
                  if (jj.eq.0) then
                     jj=3
                  endif
                  kk=MOD(ii+2,3)
                  if (kk.eq.0) then
                     kk=3
                  end if
                  grid(ix,iy,iz)%area(ii)=grid(ix,iy,iz)%length(jj)*grid(ix,iy,iz)%length(kk)
                  grid(ix,iy,iz)%volume=grid(ix,iy,iz)%area(ii)*grid(ix,iy,iz)%length(ii)
               end do
            end do
            
         end do
      end do
  end subroutine Initiate
  
  
end module setup

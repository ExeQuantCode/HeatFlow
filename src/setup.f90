
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
      real(real12), dimension(7, NA) :: Hsparse
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
      do j=1,NA 
         do i=1,NA
         call hmatrix(i,j,H0)
         
         H(i,j)=H0
         if (i.eq.1 .and. j.eq.2) print*,'Hi',H0
         if (i.eq.2 .and. j.eq.1) print*,'Hi2',H0

         !if (HBoundary.eq.1) then
         !CALL HATRIX_BOUND(i,j,HB)
         !   H(i,j)=H(i,j)+HB(i,j)
         !end if
         end do
      end do
      write(*,'(3F9.4)') H
      write(*,'(A)')
      ! H=0
      ! H(1,1)=-2
      ! H(2,1)= 1.000
      ! H(1,2)= 1.000
      ! H(2,2) = -2
      ! H(3,2) = 1.000
      ! H(2,3) = 1.000
      ! H(3,3)= -2
      ! write(*,'(3F9.4)') H
      ! Convert the matrix into Sparse Diagonal Storage.
      ! call SDSin(A,TINY, da)
      ! Convert the matrix into Sparse Row Storage.
      call SRSin(H,TINY, ra)
      do j = 1, NA
         do i = 1, NA

            if (i-j .eq. 0) then
               call hmatrix(i,j, H0)
               Hsparse(4, j) = H0  ! Diagonal, major band
            
            else if (i-j .eq. 1) then
               call hmatrix(i,j, H0)
               Hsparse(3, j) = H0 ! X left neighbor, sub_1 band

            else if (i-j .eq. -1) then 
               call hmatrix(i,j, H0)
               Hsparse(5, j) = H0 ! X right neighbor, sup_1 band
            
            else if (i-j .eq. nx)  then
               call hmatrix(i,j, H0)
               Hsparse(2, j) = H0! Y down neighbor, sub_2 band
            
            else if (i-j .eq. -nx) then
               call hmatrix(i,j, H0)
               Hsparse(6, j) = H0 ! Y up neighbor, sup_2 band 
            
            else if (i-j .eq. nx*ny) then
               call hmatrix(i,j, H0)
               Hsparse(1, j) = H0  ! Z in neighbor, sub_3 band
            
            else if (i-j .eq. -nx*ny) then
               call hmatrix(i,j, H0)
               Hsparse(7, j) = H0 ! Z out neighbor, sup_3 band
            end if 
         end do
      end do
      !print*,Hsparse
      
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


module setup
  use constants, only: real12, int12, TINY
  use inputs, only: Lx, Ly, Lz, nx, ny, nz, NA, grid
  use constructions, only: heatblock
  use hmatrixmod, only: hmatrix, hmatrixB
  use globe_data, only: H, ra, T, TN, Told,TD,TPD,H
  use sparse, only: SRSin

  implicit none
  
   contains
    

!!!##########################################################################
!!! This allocates arrays and builds the apropreate H matrix
!!!##########################################################################
   subroutine set_global_variables()

      allocate(TN(nx, ny, nz))
      allocate(T(nx, ny, nz))
      allocate(Told(nx, ny, nz))
      allocate(TD(NA))
      allocate(TPD(NA))      
      allocate(H(NA,NA))

      !** should impliment an if condition for sparse only
      ! if(sparse_only) then
         !** Never have to make the full H matrix, needs boundarys
         !call SPHM()
         !print*,Hsparse
      ! else
         call build_Hmatrix()
      ! end if

      call setup_grid()

   end subroutine set_global_variables
!!!##########################################################################


!!!#########################################################################
!!! This sets up the H Matrix and converts it into sparse row storage
!!!#########################################################################
   subroutine build_Hmatrix()
      real(real12) :: H0, HB
      integer(int12) :: i, j

      !------------------------------------------------
      !Make hmatrix
      !Hmatrix consists of H_ij,0 and H_ij,B (heat flow across grid and matrix correction for boundaries
      !-----------------------------------------------
      !H_ij T_i =S_j
      H=0.0
      HB = 0.0
      do j=1,NA 
         do i=1,NA
           call hmatrix(i,j,H0)
         
           H(i,j)=H0
        
          call hmatrixB(i,j,HB)
          print*, 'H0 =  ',H0, ', HB = ', HB
          H(i,j)=H(i,j)+HB

         end do
      end do
      
      
      write(*,'(3F9.4)') H

      ! Convert the matrix into Sparse Row Storage.
      call SRSin(H,TINY, ra)
      
    end subroutine build_Hmatrix
!!!#########################################################################


!!!#########################################################################
!!! This sets up the H Matrix directly in sparse row storage
!!!#########################################################################
   subroutine sparse_Hmatrix()
     real(real12) :: H0, HB, B
     integer(int12) :: i, j
     real(real12), dimension(7, NA) :: Hsparse

     do j = 1, NA
        do i = 1, NA
           call hmatrixB(i,j,B)
           if (i-j .eq. 0) then
              call hmatrix(i,j, H0)
              Hsparse(4, j) = H0 ! Diagonal, major band
           else if (i-j .eq. 1) then
              call hmatrix(i,j, H0)
              Hsparse(3, j) = H0 ! X left neighbor, sub_1 band
           else if (i-j .eq. -1) then 
              call hmatrix(i,j, H0)
              Hsparse(5, j) = H0 ! X right neighbor, sup_1 band
           else if (i-j .eq. nx)  then
              call hmatrix(i,j, H0)
              Hsparse(2, j) = H0 ! Y down neighbor, sub_2 band
           else if (i-j .eq. -nx) then
              call hmatrix(i,j, H0)
              Hsparse(6, j) = H0 ! Y up neighbor, sup_2 band 
           else if (i-j .eq. nx*ny) then
              call hmatrix(i,j, H0)
              Hsparse(1, j) = H0 ! Z in neighbor, sub_3 band
           else if (i-j .eq. -nx*ny) then
              call hmatrix(i,j, H0)
              Hsparse(7, j) = H0 ! Z out neighbor, sup_3 band
           end if
        end do
     end do
   end subroutine sparse_Hmatrix
!!!#########################################################################


!!!#########################################################################
!!! This sets up the remaining elements of the gird
!!!#########################################################################
   subroutine setup_grid()
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
                 jj=mod(ii+1,3)
                 if (jj.eq.0) then
                    jj=3
                 endif
                 kk=mod(ii+2,3)
                 if (kk.eq.0) then
                    kk=3
                 end if
                 grid(ix,iy,iz)%area(ii)=grid(ix,iy,iz)%length(jj)*grid(ix,iy,iz)%length(kk)
                 grid(ix,iy,iz)%volume=grid(ix,iy,iz)%area(ii)*grid(ix,iy,iz)%length(ii)
              end do
           end do

        end do
     end do
   end subroutine Setup_grid
!!!#########################################################################
  
  
end module setup

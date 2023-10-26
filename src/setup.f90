
module setup
  use constants, only: real12, int12, TINY
  use inputs, only: nx,ny,nz,NA
  use constructions, only: heatblock
  use readtxt, only: readparameters, set_gridReadFromTxt
  use hmatrixmod, only: hmatrix
  use globe_data, only: H, ra
  use sparse, only: SRSin
  implicit none
  
   contains
    
   !------------------------------------------------
   !This sets up the HMatrix global variable so as a Sparse row storage.
   !
   !-----------------------------------------------
   subroutine set_global_variables()
   real(real12) :: H0, hboundary
   integer(int12) :: i, j
   if (.not. allocated(H)) allocate(H(NA,NA))

   !------------------------------------------------
   !Make hmatrix
   !Hmatrix consists of H_ij,0 and H_ij,B (heat flow across grid and matrix correction for boundaries
   !-----------------------------------------------
   !H_ij T_i =S_j
   H=0.0
   hboundary = 0.0
   do i=1,NA 
      do j=1,NA
        CALL HMATRIX(i,j,H0)
		  
	     H(i,j)=H0
	     !if (HBoundary.eq.1) then
        !CALL HATRIX_BOUND(i,j,HB)
	     !   H(i,j)=H(i,j)+HB(i,j)
	     !end if
       end do
   end do
    !print*, H

   ! Convert the matrix into Sparse Diagonal Storage.
   ! call SDSin(A,TINY, da)
   ! Convert the matrix into Sparse Row Storage.
   call SRSin(H,TINY, ra)
   end subroutine set_global_variables


subroutine Initiate(grid)

  TYPE(heatblock), dimension(nx,ny,nz) :: grid

  integer(int12) :: ix,iy,iz,itime,i,it,j,k
 ! integer, parameter :: e = nx*ny*nz

  real(real12), dimension(nx) :: cellengthx
  real(real12), dimension(ny) :: cellengthy
  real(real12), dimension(nz) :: cellengthz
 !  real(real12) :: cellengthx, cellengthy, cellengthz

  real(real12) :: pa,pb,A,L
  integer(int12) :: ii,jj,kk
  character(len=8192):: string


 ! if (it.eq.1) then  
 !   T=T_bath
 !   Told=T_bath
 !   TN=T_bath
 ! end if


 CALL readparameters(cellengthx,cellengthy,cellengthz)
 call set_gridReadFromTxt(grid)


      



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
      !if (it.eq.1) then
      !   print*, grid(1,1,1)%length(1), 'This is the length of cell(1,1,1)'
      !end if
      
!!!Introduced by Josh, provides a 2D plot of structure, useful for checking
    !!!Open files for debugging
!      open(unit=45,file='Structure.txt')
!
!    !!!Defines formatting
!    write(string,'("( ",I0,"(",I0,"I3,/))" )') ny, nx
!    string=trim(adjustl(string))
!    !write(6,*) trim(string)
!
!    !!!Prints a grid using the material type number to show the geometry of the simulation in a 2D slice (needs work to display the 3D version)
!   ! write(45,string) grid(1:nx,1:ny,1)%imaterial_type
!   ! write(45,string) grid(1:nx,1:ny,505)%imaterial_type
!    write(45,string) grid(1:nx,1:ny,1)%imaterial_type
!   ! write(45,string) grid(1:nx,1:ny,507)%imaterial_type
!    
!    close(unit=45)
    
 
  end subroutine Initiate
  
  
end module setup

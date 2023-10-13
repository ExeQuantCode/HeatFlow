!##############################################################################################################
! This section makes the grid array based on the input, cellengths and GridMaterials.
!##############################################################################################################
module setup
  use constants
  use constructions
  use inputs
  use readtxt
  
  implicit none
  
contains

subroutine Initiate(grid, cellengthx, cellengthy, cellengthz)

  TYPE(heatblock) :: grid(nx,ny,nz)
  !define dtypes for variables
  real(real12) :: pa,pb,A,L
  integer(int12) :: ii,jj,kk
  integer(int12) :: ix,iy,iz,itime,i,it,j,k
   character(len=8192):: string

  !read arrays from txtfile
  real(real12), dimension(nx) :: cellengthx
  real(real12), dimension(ny) :: cellengthy
  real(real12), dimension(nz) :: cellengthz
   call readparameters(cellengthx,cellengthy,cellengthz)
   !Needed to deal as looped in the main, and without leads to allocation errors


   ! calls the materials txt
   CALL set_gridReadFromTxt(grid)



      



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
      open(unit=45,file='Structure.txt')

    !!!Defines formatting
    write(string,'("( ",I0,"(",I0,"I3,/))" )') ny, nx
    string=trim(adjustl(string))
    !write(6,*) trim(string)

    !!!Prints a grid using the material type number to show the geometry of the simulation in a 2D slice (needs work to display the 3D version)
   ! write(45,string) grid(1:nx,1:ny,1)%imaterial_type
   ! write(45,string) grid(1:nx,1:ny,505)%imaterial_type
    write(45,string) grid(1:nx,1:ny,1)%imaterial_type
   ! write(45,string) grid(1:nx,1:ny,507)%imaterial_type
    
    close(unit=45)
    
 
  end subroutine Initiate
  
  
end module setup

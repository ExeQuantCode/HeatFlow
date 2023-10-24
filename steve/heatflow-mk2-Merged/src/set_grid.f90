!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! subroutine written by Harry Mclean
! subroutine reads grid material structure from txt file.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MODULE SET_GRID_UP
! The aim of this version is to read data from a txt file
  use constants
  use inputs
  use constructions
  
  implicit none
  
  
  contains
  
  !!!Each grid set up follows a template to make life bearable for the user, please cut and paste subroutine0 and then adapt to your 'instance'
    !Silver on chip setup
!!!This is currently hardcoded for report, is intended for a 200x50x516 grid
  subroutine set_gridReadFromTxt(grid,cellengthx,cellengthy,cellengthz)
      TYPE(heatblock), dimension(nx,ny,nz) :: grid
    !!!Defines dummy variables
    integer(int12) :: ix,iy,iz,itime,i,it,j,k
    
    real(real12), dimension(nx, ny,nz) :: T, TN, Told
   
    real(real12), dimension(nx):: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    
    real(real12) :: pa,pb,A,Lx,Ly,Lz
  
    !!!Variables needed to read from txt
    integer, parameter :: n = 2 ! Set the size of your (n, n, n) array
    real :: WhatMat(n, n, n) ! The material used in structure from txt file
    character(255) :: filename ! txt file name 
    integer :: status, newunit, unit

    !!!Need to create uniform case!
    Lx = 5e-6 !0.003  !!!1cm bee case for 3x3x3
    Ly = 2e-5
    Lz = 1e-7
    A = 1e-6  !!1e-06
    cellengthx=Lx
    cellengthy=Ly
    cellengthz=Lz
      ! Initialize your array (replace this with your data)
      do i = 1, nx
         do j = 1, ny
            do k = 1, nz
               WhatMat(i, j, k) = 1
            end do 
         end do
      end do

      ! Specify the filename
      ! Read data from txt file
      filename = 'GridMaterial.txt'
      open(unit = newunit, file = filename, status='old')
      ! read on txt data to array
      do i = 1, nx
         do j = 1, ny
            do k = 1, nz
               read(unit, *, iostat=status) WhatMat(i, j, k)
            end do
         end do
      end do
      close(unit)
      ! save the data to the grid
   do ix=1,nx
      do iy=1,ny
         do iz=1, nz
            grid(ix,iy,iz)%imaterial_type=WhatMat(ix,iy,iz)
         end do
      end do
   end do

   end subroutine set_gridReadFromTxt


end MODULE SET_GRID_UP




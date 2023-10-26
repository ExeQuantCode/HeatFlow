!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! subroutine written by Harry Mclean
! subroutine reads grid material structure from txt file.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MODULE readtxt
! The aim of this version is to read data from a txt file
  use constants, only: real12, int12
  use constructions, only: heatblock
  use inputs, only: nx,ny,nz
  implicit none


  contains


  ! Reads Structure parameters
  subroutine readparameters(cellengthx,cellengthy,cellengthz)
    !!!Defines dummy variables
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    character(255) :: filename ! txt file name 

  
    integer :: status, newunit, unit

    ! Specify the filename
    ! Read data from txt file
    filename = 'Cellengths.txt'
    open(unit = newunit, file = filename, status='old')
    ! read on txt data to array
     

    read(unit, *, iostat=status) cellengthx
    read(unit, *, iostat=status) cellengthy
    read(unit, *, iostat=status) cellengthz
    close(unit)
    ! save the data to the grid
   


   end subroutine readparameters
  
  !Read Material array from txt and save to the grid
  subroutine set_gridReadFromTxt(grid)
      TYPE(heatblock) :: grid(nx, ny, nz)
    !!!Defines dummy variables
    integer(int12) :: i,j,k

    !!!Variables needed to read from txt
    real(real12) :: WhatMat(nx, ny, nz) ! The material used in structure from txt file
    character(255) :: filename ! txt file name 
    integer :: status, newunit, unit
    
 
    

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
   do i=1,nx
      do j=1,ny
         do k=1, nz
            grid(i,j,k)%imaterial_type=WhatMat(i,j,k)
         end do
      end do
   end do

   end subroutine set_gridReadFromTxt


end MODULE readtxt





! Main program
program geom_processor
  use shapes
  use read
  use shape_checking
  implicit none

  type(volume) :: vol
  type(cuboid), allocatable :: cuboids(:)
  type(sphere), allocatable :: spheres(:)
  type(cylinder), allocatable :: cylinders(:)
  integer, allocatable :: grid(:,:,:)
  integer :: num_cuboids, num_spheres, num_cylinders, i
  character(len=100) :: line

  call count_blocks(num_cuboids, num_spheres, num_cylinders)
  rewind(5)

  allocate(cuboids(num_cuboids))
  allocate(spheres(num_spheres))
  allocate(cylinders(num_cylinders))

  call read_input_file(vol,cuboids,spheres,cylinders)

  allocate(grid(vol%x_grid, vol%y_grid, vol%z_grid))
  grid=vol%default_material

  call assign_point(grid, cuboids, spheres, cylinders, vol)
  
  call write_output_file('geom.in', grid, vol)


contains

  subroutine write_output_file(filename, grid, vol)
    implicit none
    type(volume), intent(in) :: vol
    character(len=*), intent(in) :: filename
    integer, intent(in) :: grid(:,:,:)
    integer :: i, j, k
    integer :: io_status

    ! Open file for writing
    open(unit=20, file=filename, status='replace', iostat=io_status)
    if (io_status /= 0) then
        print *, "Error opening file for writing"
        return
    end if

    ! Write grid dimensions and size
    write(20, *) vol%x_grid, vol%y_grid, vol%z_grid
    write(20, *) vol%x, vol%y, vol%z
    write(20, *)

    ! Write grid data
    do k = 1, size(grid, 3)
       do j = 1, size(grid, 2)
          
            write(20, '( *(I2, 1X) )') (grid(i, j, k), i = 1, size(grid, 1))
        end do
        write(20, *)
    end do

    close(20)

end subroutine write_output_file
  
end program geom_processor



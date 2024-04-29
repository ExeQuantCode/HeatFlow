module shape_checking
  use shapes
  implicit none
contains
  subroutine assign_point(grid, heater, cuboids, spheres, cylinders, vol, alls)
    use shapes
    implicit none
    integer, intent(inout) :: grid(:,:,:), heater(:,:,:)
    type(volume), intent(in) :: vol
    type(cuboid), intent(in) :: cuboids(:)
    type(sphere), intent(in) :: spheres(:)
    type(cylinder), intent(in) :: cylinders(:)
    type(shape_list), intent(in) :: alls(:)
    real :: coord(3)
    integer :: material, heat, i, j, k


    do k = 1, size(grid, 3)
       do j = 1, size(grid, 2)
          do i = 1, size(grid, 1)
             material = grid(i, j, k)
             heat = heater(i, j, k)
             call get_coords(vol,i,j,k,coord)
             call check_shapes(coord, cuboids, spheres, cylinders, material, heat, alls)
             grid(i, j, k) = material
             heater(i, j, k) = heat
          end do
       end do
    end do
  end subroutine assign_point
  
  subroutine get_coords(vol, i, j, k, coord)
    implicit none
    integer, intent(in) :: i, j, k
    type(volume), intent(in) :: vol
    real, dimension(3), intent(out) :: coord
    real :: dx, dy, dz

    ! Calculate the step size for each dimension
    dx =  vol%x / vol%x_grid
    dy =  vol%y / vol%y_grid
    dz =  vol%z / vol%z_grid

    ! Calculate the coordinates
    coord(1) = (0.5 + i - 1) * dx
    coord(2) = (0.5 + j - 1) * dy
    coord(3) = (0.5 + k - 1) * dz
  end subroutine get_coords
  

  subroutine check_shapes(coord, cuboids, spheres, cylinders, material, heat, alls)
    use shapes
    implicit none
    type(cuboid), intent(in) :: cuboids(:)
    type(sphere), intent(in) :: spheres(:)
    type(cylinder), intent(in) :: cylinders(:)
    real, intent(in) :: coord(3)
    integer, intent(inout) :: material, heat
    type(shape_list), intent(in) :: alls(:)
    integer :: num_all, c
    
    num_all = size(alls)

    do c = 1, num_all
       if ( alls(c)%wshape .eq. 'cub' ) then
          call check_cuboids(coord, cuboids, material, heat, alls(c)%idx)
       else if ( alls(c)%wshape .eq. 'sph' ) then
          call check_spheres(coord, spheres, material, heat, alls(c)%idx)
       else if ( alls(c)%wshape .eq. 'cyl' ) then
          call check_cylinders(coord, cylinders, material, heat, alls(c)%idx)
       end if
    end do    
  end subroutine check_shapes

  subroutine check_cuboids(coord, cuboids, material, heat, idx)
    use shapes
    implicit none
    real, intent(in) :: coord(3)
    type(cuboid), intent(in) :: cuboids(:)
    integer, intent(inout) :: material, heat
    integer, intent(in) :: idx

    if (cuboids(idx)%origin(1) <= coord(1) .and. &
         coord(1) <= (cuboids(idx)%origin(1) + cuboids(idx)%dimensions(1)) .and. &
         cuboids(idx)%origin(2) <= coord(2) .and. &
         coord(2) <= (cuboids(idx)%origin(2) + cuboids(idx)%dimensions(2)) .and. &
         cuboids(idx)%origin(3) <= coord(3) .and. &
         coord(3) <= (cuboids(idx)%origin(3) + cuboids(idx)%dimensions(3))) then
       material = cuboids(idx)%material
       heat = cuboids(idx)%heat
       return
    end if
  end subroutine check_cuboids

  subroutine check_spheres(coord, spheres, material, heat, idx)
    use shapes
    implicit none
    real, intent(in) :: coord(3)
    type(sphere), intent(in) :: spheres(:)
    integer, intent(inout) :: material, heat
    integer, intent(in) :: idx
    real :: distance
    
    distance = sqrt((coord(1) - spheres(idx)%center(1))**2 + &
         (coord(2) - spheres(idx)%center(2))**2 + &
         (coord(3) - spheres(idx)%center(3))**2)
    if (distance <= spheres(idx)%radius) then
       material = spheres(idx)%material
       heat = spheres(idx)%heat
       return
    end if
  end subroutine check_spheres

  subroutine check_cylinders(coord, cyl, material, heat, idx)
    use shapes
    implicit none
    real, intent(in) :: coord(3)
    type(cylinder), intent(in) :: cyl(:)
    integer, intent(inout) :: material, heat
    integer, intent(in) :: idx
    real, dimension(3) :: h, direction
    real :: dist_on_line, dist_to_line

       direction = cyl(idx)%dir / norm(cyl(idx)%dir)
       h = coord - cyl(idx)%start
       
       dist_on_line = dot_product(direction, h)
       dist_to_line = sqrt(dot_product(h, h) - dist_on_line**2)
       if (dist_on_line >= 0.0 .and. dist_on_line <= cyl(idx)%length .and. &
            dist_to_line <= cyl(idx)%radius) then
          material = cyl(idx)%material
          heat = cyl(idx)%heat
          return
       end if
  end subroutine check_cylinders
  
  function norm(vector) result(norm_val)
    real, dimension(:), intent(in) :: vector
    real :: norm_val
    norm_val = sqrt(sum(vector**2))
  end function norm

  function dot_product(a, b) result(dot_val)
    real, dimension(3), intent(in) :: a, b
    real :: dot_val
    dot_val = sum(a*b)
  end function dot_product
  
end module shape_checking

module shape_checking
  use shapes
  implicit none
contains
  subroutine assign_point(grid, heater, cuboids, spheres, cylinders, vol)
    use shapes
    implicit none
    integer, intent(inout) :: grid(:,:,:), heater(:,:,:)
    type(volume), intent(in) :: vol
    type(cuboid), intent(in) :: cuboids(:)
    type(sphere), intent(in) :: spheres(:)
    type(cylinder), intent(in) :: cylinders(:)
    real :: coord(3)
    integer :: material, heat, i, j, k


    do k = 1, size(grid, 3)
       do j = 1, size(grid, 2)
          do i = 1, size(grid, 1)
             material = grid(i, j, k)
             heat = heater(i, j, k)
             call get_coords(vol,i,j,k,coord)
             call check_shapes(coord, cuboids, spheres, cylinders, material, heat)
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
  

  subroutine check_shapes(coord, cuboids, spheres, cylinders, material, heat)
    use shapes
    implicit none
    type(cuboid), intent(in) :: cuboids(:)
    type(sphere), intent(in) :: spheres(:)
    type(cylinder), intent(in) :: cylinders(:)
    real, intent(in) :: coord(3)
    integer, intent(inout) :: material, heat

    call check_cuboids(coord, cuboids, material, heat)
    call check_spheres(coord, spheres, material, heat)
    call check_cylinders(coord, cylinders, material, heat)
    
  end subroutine check_shapes

  subroutine check_cuboids(coord, cuboids, material, heat)
    use shapes
    implicit none
    real, intent(in) :: coord(3)
    type(cuboid), intent(in) :: cuboids(:)
    integer, intent(inout) :: material, heat
    integer :: num_cuboids,c

    num_cuboids = size(cuboids)

    do c = 1, num_cuboids
       if (cuboids(c)%origin(1) <= coord(1) .and. &
            coord(1) <= (cuboids(c)%origin(1) + cuboids(c)%dimensions(1)) .and. &
            cuboids(c)%origin(2) <= coord(2) .and. &
            coord(2) <= (cuboids(c)%origin(2) + cuboids(c)%dimensions(2)) .and. &
            cuboids(c)%origin(3) <= coord(3) .and. &
            coord(3) <= (cuboids(c)%origin(3) + cuboids(c)%dimensions(3))) then
          material = cuboids(c)%material
          heat = cuboids(c)%heat
          return
       end if
    end do
  end subroutine check_cuboids

  subroutine check_spheres(coord, spheres, material, heat)
    use shapes
    implicit none
    real, intent(in) :: coord(3)
    type(sphere), intent(in) :: spheres(:)
    integer, intent(inout) :: material, heat
    integer :: num_spheres, s
    real :: distance

    num_spheres = size(spheres)

    do s = 1, num_spheres
        distance = sqrt((coord(1) - spheres(s)%center(1))**2 + &
                        (coord(2) - spheres(s)%center(2))**2 + &
                        (coord(3) - spheres(s)%center(3))**2)
        if (distance <= spheres(s)%radius) then
            material = spheres(s)%material
            heat = spheres(s)%heat
            return
        end if
    end do
  end subroutine check_spheres

  subroutine check_cylinders(coord, cyl, material, heat)
    use shapes
    implicit none
    real, intent(in) :: coord(3)
    type(cylinder), intent(in) :: cyl(:)
    integer, intent(inout) :: material, heat
    integer :: num_cyl, c
    real, dimension(3) :: h, direction
    real :: dist_on_line, dist_to_line

    num_cyl = size(cyl)
    
    do c = 1, num_cyl
       !write(*,*) cyl(c)%dir, cyl(c)%start
       direction = cyl(c)%dir / norm(cyl(c)%dir)
       h = coord - cyl(c)%start
       
       dist_on_line = dot_product(direction, h)
       dist_to_line = sqrt(dot_product(h, h) - dist_on_line**2)
       if (dist_on_line >= 0.0 .and. dist_on_line <= cyl(c)%length .and. dist_to_line <= cyl(c)%radius) then
          material = cyl(c)%material
          heat = cyl(c)%heat
          return
       end if
    end do
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

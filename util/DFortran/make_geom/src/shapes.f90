! Define structures for volume, cuboid, sphere, and cylinder
module shapes
  type volume
     real :: x, y, z
     integer :: x_grid, y_grid, z_grid
     real :: units
     integer :: dim
     integer :: default_material
  end type volume

  type cuboid
     real, dimension(3) :: origin
     real, dimension(3) :: dimensions
     integer :: material
  end type cuboid

  type sphere
     real, dimension(3) :: center
     real :: radius
     integer :: material
  end type sphere

  type cylinder
     real, dimension(3) :: start
     real, dimension(3) :: dir
     real :: radius, length
     integer :: material
  end type cylinder

end module shapes
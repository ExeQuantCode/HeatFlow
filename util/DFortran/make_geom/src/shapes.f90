! Define structures for volume, cuboid, sphere, and cylinder
module shapes
  type volume
     real :: x, y, z
     integer :: x_grid, y_grid, z_grid
     real :: units
     integer :: dim
     integer :: default_heat, default_material
  end type volume

  type cuboid
     real, dimension(3) :: origin
     real, dimension(3) :: dimensions
     integer :: heat, material
  end type cuboid

  type sphere
     real, dimension(3) :: center
     real :: radius
     integer :: heat, material
  end type sphere

  type cylinder
     real, dimension(3) :: start
     real, dimension(3) :: dir
     real :: radius, length
     integer :: heat, material
  end type cylinder

  type shape_list
     character(len=3) :: wshape
     integer :: idx
  end type shape_list

end module shapes

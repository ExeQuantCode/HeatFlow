module read
  use shapes
  implicit none
contains
  subroutine count_blocks(n_cub, n_sph, n_cyl, n_all)
  implicit none
  integer, intent(out) :: n_cub, n_sph, n_cyl, n_all
  integer :: error
  character(len=100) :: line
  
  
  n_cub = 0
  n_sph = 0
  n_cyl = 0
  n_all = 0

  do
     read(*, '(A)',iostat=error) line
     if ( error /= 0) exit
     if (index(line, 'CUBOID') > 0) then
        n_cub = n_cub + 1
        n_all = n_all + 1
     else if (index(line, 'SPHERE') > 0) then
        n_sph = n_sph + 1
        n_all = n_all + 1
     else if (index(line, 'CYLINDER') > 0) then
        n_cyl = n_cyl + 1
        n_all = n_all + 1
     end if
  end do

  
end subroutine count_blocks

subroutine read_input_file(vol, cuboids, spheres, cylinders, alls)
  use shapes
  implicit none
  type(shape_list), intent(inout) :: alls(:)
  type(volume), intent(out) :: vol
  type(cuboid), intent(out) :: cuboids(*)
  type(sphere), intent(out) :: spheres(*)
  type(cylinder), intent(out) :: cylinders(*)
  character(len=1024) :: buffer
  character(len=1024) :: sline(2)
  integer :: error, i
  integer :: num_cuboids, num_spheres, num_cylinders, num_all

  num_cuboids = 0
  num_spheres = 0
  num_cylinders = 0
  num_all = 0
  
  ! Read lines from the file
  read: do
     read(5, '(A)', iostat=error) buffer
     if ( error /= 0) exit
     call filter_line(buffer)
     if (trim(adjustl(buffer)) == '') cycle read

     select case(trim(adjustl(buffer)))
     case('VOLUME')
        call read_vol(vol)
     case('CUBOID')
        num_cuboids = num_cuboids + 1
        num_all = num_all + 1
        call read_cuboid(cuboids(num_cuboids))
        alls(num_all)%idx = num_cuboids
        alls(num_all)%wshape = 'cub'
     case('SPHERE')
        num_spheres = num_spheres + 1
        num_all = num_all + 1
        call read_sphere(spheres(num_spheres))
        alls(num_all)%idx = num_spheres
        alls(num_all)%wshape = 'sph'
     case('CYLINDER')
        num_cylinders = num_cylinders + 1
        num_all = num_all + 1
        call read_cylinder(cylinders(num_cylinders))
        alls(num_all)%idx = num_cylinders
        alls(num_all)%wshape = 'cyl'
     end select

  end do read

end subroutine read_input_file


subroutine read_vol(vol)
  use shapes
  type(volume), intent(inout) :: vol
  integer :: error,dummy
  character(len=1024) :: buffer, sline(2),upsline
  block: do 
     read(5, '(A)', iostat=error) buffer
     if (error /= 0) exit block
     call filter_line(buffer)
     call split_equals(buffer,sline)
     call forcefitkeywords(sline(1),['DIM', 'DIR'])

     select case(trim(adjustl(sline(1))))
     case('END')
        exit block
     case('X')
        read(sline(2), *) vol%x
     case('Y')
        read(sline(2), *) vol%y
     case('Z')
        read(sline(2), *) vol%z
     case('FILL')
        read(sline(2), *) vol%default_material
     case('UNITS')
        read(sline(2), *) vol%units
     case('DIM')
        read(sline(2),*) vol%dim
     case('HEAT')
        read(sline(2),*) vol%default_heat
     case('GRID')
        read(sline(2),*) vol%x_grid, vol%y_grid, vol%z_grid
     case default
        write(*,'(*(A))') "Warning: keyword not found. skipping line: ", trim(adjustl(buffer))
     end select
  end do block
end subroutine read_vol


subroutine read_cuboid(cub)
  use shapes
  implicit none
  type(cuboid), intent(inout) :: cub
  integer :: i, error
  character(len=1024) :: buffer, sline(2)

  block: do
     read(5, '(A)', iostat=error) buffer
     call filter_line(buffer)
     call split_equals(buffer, sline)
     call forcefitkeywords(sline(1),['DIM', 'DIR'])

     select case(trim(adjustl(sline(1))))
     case('END')
        exit block
     case('ORIGIN')
        read(sline(2), *) cub%origin
     case('DIM')
        read(sline(2), *) cub%dimensions
     case('HEAT')
        read(sline(2), *) cub%heat
     case('FILL')
        read(sline(2), *) cub%material
     case default
        write(*,'(*(A))') "Warning: keyword not found.\n skipping line: ", trim(adjustl(buffer))
     end select
     if (error /= 0) exit block
  end do block
end subroutine read_cuboid


subroutine read_sphere(sph)
  use shapes
  implicit none
  type(sphere), intent(inout) :: sph
  integer :: i, error
  character(len=1024) :: buffer, sline(2)

  block: do
     read(5, '(A)', iostat=error) buffer
     call filter_line(buffer)
     call split_equals(buffer, sline)
     call forcefitkeywords(sline(1),['DIM', 'DIR'])

     select case(trim(adjustl(sline(1))))
     case('END')
        exit block
     case('CENTER')
        read(sline(2), *) sph%center
     case('RADIUS')
        read(sline(2), *) sph%radius
     case('HEAT')
        read(sline(2), *) sph%heat
     case('FILL')
        read(sline(2), *) sph%material
     case default
        write(*,'(*(A))') "Warning: keyword not found.\n skipping line: ", trim(adjustl(buffer))
     end select
     if (error /= 0) exit block
  end do block
end subroutine read_sphere


subroutine read_cylinder(cyl)
  use shapes
  implicit none
  type(cylinder), intent(inout) :: cyl
  integer :: i, error
  character(len=1024) :: buffer, sline(2)

  block: do
     read(5, '(A)', iostat=error) buffer
     call filter_line(buffer)
     call split_equals(buffer, sline)
     call forcefitkeywords(sline(1),['DIM', 'DIR'])
          
     select case(trim(adjustl(sline(1))))
     case('END')
        exit block
     case('START')
        read(sline(2), *) cyl%start
     case('DIR')
        read(sline(2), *) cyl%dir
     case('RADIUS')
        read(sline(2), *) cyl%radius
     case('LENGTH')
        read(sline(2), *) cyl%length
     case('HEAT')
        read(sline(2), *) cyl%heat
     case('FILL')
        read(sline(2), *) cyl%material
     case default
        write(*,'(*(A))') "Warning: keyword not found.\n skipping line: ", trim(adjustl(buffer))
     end select
     if (error /= 0) exit block
  end do block
end subroutine read_cylinder

!!!###############################################################################
!!! Truncates comments, replace : -> =. makes uppercase. remove tabs
!!!###############################################################################
subroutine filter_line(line)
  character(len=1024) :: line
  character(len=1024) :: dummy
  integer :: i

  if (index(line, '#') > 0) then
     line = line(:index(line, '#') - 1)
  end if

  if (index(line, '!') > 0) then
     line = line(:index(line, '!') - 1)
  end if

  ! Replace ':' with '='
  do i = 1, len_trim(line)
     if (line(i:i) == ':') then
        line(i:i) = '='
     end if
  end do

  call lower_upper(line,dummy)
  call remove_tabs(dummy,line)

end subroutine filter_line
!!!###############################################################################

!!!###############################################################################
!!! Converts lowercase letters in a string to uppercase.
!!!###############################################################################
subroutine lower_upper(lower, upper)
  character(len=*), intent(in) :: lower
  character(len=len(lower)),intent(out) :: upper
  integer :: i

  upper = lower

  do i = 1, len(lower)
     if (lower(i:i) >= 'a' .and. lower(i:i) <= 'z') then
        upper(i:i) = achar(iachar(lower(i:i)) - 32)
     end if
  end do
end subroutine lower_upper
!!!###############################################################################

!!!###############################################################################
!!! Removes tab characters from a string
!!!###############################################################################
subroutine remove_tabs(original_string, cleaned_string)
  character(len=*), intent(in) :: original_string
  character(len=len(original_string)), intent(out) :: cleaned_string
  integer :: i, j

  j = 1
  cleaned_string = ''  ! Initialize the cleaned string

  do i = 1, len_trim(original_string)
     if (iachar(original_string(i:i)) /= 9) then
        cleaned_string(j:j) = original_string(i:i)
        j = j + 1
     end if
  end do
end subroutine remove_tabs
!!!###############################################################################


!!!###############################################################################
!!! Splits a string at the first equals sign into a two-element array
!!!###############################################################################
subroutine split_equals(l,sl)
  character(len=*), intent(in) :: l
  character(len=*), intent(inout):: sl(2)
  integer :: pos

  sl = ''
  pos = index(l, '=')
  if (pos /= 0) then
     sl(1) = l(:pos-1)
     sl(2) = l(pos+1:)
  else
     sl(1) = l
  end if
end subroutine split_equals
!!!###############################################################################


!!!###############################################################################
!!! Sets string to the first suplied keyword that matches the start of the string
!!!###############################################################################
subroutine forcefitkeywords(str,keywords)
  character(len=1024), intent(inout) :: str
  character(len=3), dimension(:), intent(in) :: keywords
  integer :: i, pos
  
  keyloop: do i = 1, size(keywords)
     pos = index(trim(adjustl(str)), trim(adjustl(keywords(i))))
     
     ! Check if keyword is at the beginning
     if (pos .eq. 1) then
        str = keywords(i)
        exit keyloop
     end if 
  end do keyloop
end subroutine forcefitkeywords
!!!###############################################################################

end module read

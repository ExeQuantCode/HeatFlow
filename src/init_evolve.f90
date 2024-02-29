module initial 
  use constants, only: real12, int12, fields
  use inputs, only: NA, nx, ny, nz, InputTempDis, FullRestart, T_System
  use globe_data, only: TPD,TPPD
  implicit none

  
contains

  subroutine initial_evolve()
    implicit none
    integer(int12) :: i,j,k, index, reason
    logical :: file_TPD_exists, file_TPPD_exists
    character :: buffer
    
    if (FullRestart) then
       call read_temp_file('./outputs/TempDis.dat',TPD)
       call read_temp_file('./outputs/TempDisTPD.dat',TPPD)
    else if (InputTempDis) then
       call read_temp_file('./outputs/TempDis.dat',TPD)
       TPPD = TPD
    else 
       TPD = T_System
       TPPD = T_System
    end if
    
  end subroutine initial_evolve
  

!!!
subroutine read_temp_file(filepath, T)
  implicit none
  character(len=*), intent(in) :: filepath
  real(real12), intent(out) :: T(:) 
  integer(int12) :: i, reason, unit
  logical :: exists
  real(real12), dimension(6) :: line
  integer(int12) :: c, numvals

  !----------------------------
  ! check file exists
  !----------------------------
  inquire(file=filepath, exist=exists)
  if (.not. exists) then
     write(*,*) 'Error:',filepath,'does not exist'; call exit
  end if
  !----------------------------

  !----------------------
  ! Open file
  !----------------------
  open(newunit=unit, file=filepath, status='old', action='read', iostat=reason)
  if (reason .ne. 0) then
     write(*,*) "Error opening file."; call exit
  end if
  !----------------------

  !-------------------------------------------------------------
  ! Read file, "fields" values per line
  !-------------------------------------------------------------
  c = 0
  do while (c .lt. NA)
     read(unit, *, iostat=reason) line
     !----------------------------
     ! error responses
     if (reason .gt. 0) then
        write(*,*) "Error: read error."; call exit
     end if
     !----------------------------

     
     !----------------------------
     ! assinge values
     numvals = min(fields, NA - c) ! find num values left in NA
     do i = 1, numvals
        c = c + 1
        T(c) = line(i)
     end do
     !----------------------------
  end do
  !-------------------------------------------------------------

  !-------------------------------------
  ! check values are missing
  !-------------------------------------
  if (c .lt. NA) then
     write(*,*) "Error: File contains fewer values than expected (", &
          c, " out of ", NA, ")."; call exit
  end if
  !-------------------------------------

  !--------------------------------------------------------------------
  ! Attempt to read one more value to ensure there are no extra values
  !--------------------------------------------------------------------
  read(unit, *, iostat=reason) line
  if (reason .eq. 0) then
     print *, "Error: File contains more values than expected."; call exit
  end if
  !--------------------------------------------------------------------

  close(unit)

end subroutine read_temp_file

  
end module initial

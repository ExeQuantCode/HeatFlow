!!!##########################################################################
!!! This code reads inputs files found in the utl folder                  !!!
!!!##########################################################################
module inputs
  use constants, only: real12, int12
  use constructions, only: heatblock, material

  implicit none

  integer :: unit, newunit
  real(real12) :: time_step, freq, power_in, T_period, cutoff, kappaBoundx, kappaBoundy, kappaBoundz
  real(real12) :: mixing, T_Bathx1, T_Bathx2, T_Bathy1, T_Bathy2, T_Bathz1, T_Bathz2, T_System
  integer(int12) :: IVERB, icell_mix, ntime, Rel, zpos, ACon, iboundary, nx, ny, nz, icattaneo, isteady, NA
  logical :: Check_Sparse_Full, Check_Stability, Check_Steady_State, WriteToTxt, Percentage, InputTempDis
  logical :: verbose = .TRUE., Test_Run = .FALSE., FullRestart
  character(1024) :: RunName
  type(heatblock), dimension(:,:,:), allocatable :: grid
  integer(int12), dimension(:,:,:), allocatable :: iheater
  type(material), dimension(:), allocatable :: input_materials
  real(real12) :: Lx, Ly, Lz ! Volume dimensions (lenghts)




contains
!!!##########################################################################
!!! read_all_files will call the routines to read each input file
!!!##########################################################################
  subroutine read_all_files()
    integer :: unit, reason
    character(64) :: param_infile, mat_infile, mesh_infile
    logical :: file_exists


    !-----------------------------------------------
    ! get data from param.in
    !-----------------------------------------------
    ! name infile
    !path = "./inputs/param.in"
    param_infile = "./inputs/param.in"

    ! check if file is there
    inquire(file=param_infile, exist=file_exists)

    ! give error and exit code
    if (.not.file_exists) then
        write(6,*) 'Error cannot find file: param.in'
        call exit
    end if
    
    ! open, read and close
    open(newunit=unit, file=param_infile, iostat=reason)
    call read_param(unit)
    close(unit)
    if (Check_Steady_State) then
      write(6,*) '!!!Model Steady state check!!!'
      verbose=.FALSE.
      param_infile = "./Tests/inputs/param_steady.in"
      open(newunit=unit, file=param_infile, iostat=reason)
      call read_param(unit) 
      close(unit)
    end if

    !-----------------------------------------------


    !-----------------------------------------------
    ! get data from mat.in
    !-----------------------------------------------
    ! name infile
    if (Check_Steady_State) then
        mat_infile = "./Tests/inputs/mat_steady.in"
    else
        mat_infile = "./inputs/mat.in"
    end if

    ! check if file is there
    inquire(file=mat_infile, exist=file_exists)

    ! give error and exit code
    if (.not.file_exists) then
        write(6,*) 'Error cannot find file: mat.in'
        call exit
    end if
    
    ! open, read and close
    open(newunit=unit, file=mat_infile, iostat=reason)
    call read_mat(unit)
    close(unit)
    !-----------------------------------------------


    !-----------------------------------------------
    ! get data from geom.in
    !-----------------------------------------------
    ! name infile
    if (Check_Steady_State) then
        mesh_infile = "./Tests/inputs/geom_steady.in"
    else
        mesh_infile = "./inputs/geom.in"
    end if

    ! check if file is there
    inquire(file=mesh_infile, exist=file_exists)

    ! give error and exit code
    if (.not.file_exists) then
        write(6,*) 'Error cannot find file: geom.in'
        call exit
    end if
    
    ! open, read and close
    open(newunit=unit, file=mesh_infile, iostat=reason)
    call read_mesh(unit)
    close(unit)
    !-----------------------------------------------

    !-----------------------------------------------
    ! get data from heat.in
    !-----------------------------------------------
    ! name infile
    if (Check_Steady_State) then
        mesh_infile = "./Tests/inputs/heat_steady.in"
    else
        mesh_infile = "./inputs/heat.in"
    end if

    ! check if file is there
    inquire(file=mesh_infile, exist=file_exists)

    ! give error and exit code
    if (.not.file_exists) then
        write(6,*) 'Error cannot find file: heat.in'
        call exit
    end if
    
    ! open, read and close
    open(newunit=unit, file=mesh_infile, iostat=reason)
    call read_heat(unit)
    close(unit)
    !-----------------------------------------------
  end subroutine read_all_files
!!!##########################################################################


!!!##########################################################################
!!! check_param gives errors and warnings regarding the INPUT file
!!!##########################################################################
  subroutine check_param(readvar,n)
    integer::n
    integer,dimension(n)::readvar
    ! Not currently in use
    if(any(readvar.gt.1)) then
       write(6,*)
       write(6,'(A43)') '###############################'
       write(6,'(A43)') '##########   ERROR   ##########'
       write(6,'(A43)') '###############################'
       write(6,*)
       write(6,'(A)') ' ---       Error in subroutine "checkINPUT"       ---'
       write(6,'(A)') ' --- ERROR: same KEYWORD apears more than once    ---'
       call exit
    end if

    if (any(readvar.eq.0)) then
        write(6,*)
        write(6,'(A43)') '###############################'
        write(6,'(A43)') '##########   ERROR   ##########'
        write(6,'(A43)') '###############################'
        write(6,*)
        write(6,'(A)') ' ---       Error in subroutine "checkINPUT"       ---'
        write(6,'(A)') ' --- ERROR: A KEYWORD and associated value is missing    ---'
        call exit
    end if 

    if(verbose) then
       write(6,'(A)')        ' vebose printing option'
       write(6,'(A)')        ' running calculation with :'
       write(6,'(A35,I6)')   '   IVERB      = ',IVERB
       write(6,'(A35,L1)')   '   _Check_Sparse_Full  = ',Check_Sparse_Full
       write(6,'(A35,L1)')   '   _Check_Stability         = ',Check_Stability
       write(6,'(A35,L1)')   '   _Check_Steady_State      = ',Check_Steady_State
       write(6,'(A35,L1)')   '  _Percentage_Completion         = ',Percentage
       write(6,'(A35,L1)')   '   _Test_Run         = ',Test_Run
       write(6,'(A35,L1)')   '   _InputTempDis         = ',InputTempDis
       write(6,'(A35,A)')   '    _FullRestart         = ',FullRestart
       write(6,'(A35,A)')   '    _RunName         = ',trim(RunName)
       write(6,'(A35,L1)')   '   _WriteToTxt         = ',WriteToTxt
       write(6,'(A35,I6)')   '   icell_mix  = ',icell_mix
       write(6,'(A35,I12)')   '   ntime      = ',ntime
       write(6,'(A35,I6)')   '   Rel        = ', Rel
       write(6,'(A35,F12.5)')'   Mixing       = ',mixing
       write(6,'(A35,I6)')   '   ACon       = ',ACon       
       write(6,'(A35,F20.15)')'   Time_step  = ',time_step
       write(6,'(A35,F12.5)')'   frequency  = ',freq
       write(6,'(A35,I6)')   '   iboundary  = ',iboundary
       write(6,'(A35,I6)')   '   icattaneo  = ' , icattaneo
       write(6,'(A35,I6)')   '   isteady    = ', isteady
       write(6,'(A35,F12.5)')'   T_System   = ', T_System
       write(6,'(A35,F12.5)')'   T_Bathx1     = ', T_Bathx1
       write(6,'(A35,F12.5)')'   T_Bathx2     = ', T_Bathx2
       write(6,'(A35,F12.5)')'   T_Bathy1     = ', T_Bathy1
       write(6,'(A35,F12.5)')'   T_Bathy2     = ', T_Bathy2
       write(6,'(A35,F12.5)')'   T_Bathz1     = ', T_Bathz1
       write(6,'(A35,F12.5)')'   T_Bathz2     = ', T_Bathz2
       write(6,'(A35,F16.5)')'   cutoff     = ',cutoff
       write(6,'(A35,F12.5)')'   power_in   = ',power_in
       write(6,'(A35,F12.5)')'   T_period   = ',T_period
       write(6,'(A35,F12.5)')   '   kappaBoundx         = ',kappaBoundx
       write(6,'(A35,F12.5)')   '   kappaBoundy         = ',kappaBoundy
       write(6,'(A35,F12.5)')   '   kappaBoundz         = ',kappaBoundz

    end if
  end subroutine check_param
!!!##########################################################################


!!!##########################################################################
!!! reads an input file where each flag has the format "KEYWORD =" followed ...
!!! ... by the variable i.e. 1, 2, .TRUE., 5D-15
!!!##########################################################################
  subroutine read_param(unit)
    integer::unit, Reason, i
    integer,dimension(33)::readvar
    character(1024)::buffer
    logical::ex
    readvar=0
    !------------------------------------------
    ! assign defaults
    !------------------------------------------
    IVERB = 1
    Check_Sparse_Full = .FALSE.
    Check_Stability = .FALSE.
    Check_Steady_State = .FALSE.
    Percentage = .FALSE.
    Test_Run = .FALSE.
    InputTempDis = .FALSE.
    FullRestart = .FALSE.
    RunName = 'default'
    RunName = trim(adjustl(RunName))
    WriteToTxt = .FALSE.
    icell_mix = 2
    ntime = 10
    Rel = 0
    mixing = 0.5  
    ACon = 0
    time_step = 1.0
    freq = 1
    iboundary = 1
    icattaneo = 1
    isteady = 0
    T_System = 300
    T_Bathx1 = 300
    T_Bathx2 = 300
    T_Bathy1 = 300
    T_Bathy2 = 300
    T_Bathz1 = 300
    T_Bathz2 = 300
    cutoff = 1e12
    power_in = 0
    T_period = 1
    kappaBoundx = 400
    kappaBoundy = 400
    kappaBoundz = 400
    !------------------------------------------
    do
       read(unit,'(A)',iostat=Reason) buffer
       if(Reason.ne.0) exit
       ! looks for comments in each line of txt file and trims them off
       if(scan(buffer,'!').ne.0) buffer=buffer(:(scan(buffer,'!')-1)) 
       if(trim(buffer).eq.'') cycle ! removes blank spaces
       !---------------------------------------
       ! assignD works for doubles
       ! assignL for logicals
       ! assignI for integers
       !---------------------------------------
       ! looks for all the keywords relating to inputs and defines their variables
       call assignI(buffer,"IVERB",IVERB,readvar(1))
       call assignI(buffer,"icell_mix",icell_mix,readvar(2))             
       call assignI(buffer,"ntime",ntime,readvar(3))           
       call assignI(buffer,"Rel",Rel,readvar(4))       
       call assignD(buffer,"mixing",mixing,readvar(5))      
       call assignI(buffer,"ACon",ACon,readvar(6))         
       call assignD(buffer,"time_step",time_step,readvar(7))   
       call assignD(buffer,"freq",freq,readvar(8))       
       call assignI(buffer,"iboundary",iboundary,readvar(9))   
       call assignI(buffer,"icattaneo", icattaneo, readvar(10))
       call assignI(buffer,"isteady", isteady, readvar(11))
       call assignD(buffer,"T_Bathx1",T_Bathx1,readvar(12)) 
       call assignD(buffer,"cutoff",cutoff,readvar(13))    
       call assignD(buffer,"power_in",power_in,readvar(14))       
       call assignD(buffer,"T_period",T_period,readvar(15))       
       call assignD(buffer,"kappaBoundx",kappaBoundx,readvar(16))
       call assignD(buffer,"kappaBoundy",kappaBoundy,readvar(17))    
       call assignD(buffer,"kappaBoundz",kappaBoundz,readvar(18))
       call assignL(buffer,"_Check_Sparse_Full",Check_Sparse_Full,readvar(19))
       call assignL(buffer,"_Check_Stability",Check_Stability,readvar(20))
       call assignL(buffer,"_Check_Steady_State",Check_Steady_State,readvar(21))
       call assignL(buffer,"_WriteToTxt",WriteToTxt,readvar(22))
       call assignL(buffer,"_Percentage_Completion",Percentage,readvar(23))
       call assignL(buffer,"_Test_Run",Test_Run,readvar(24))
       call assignL(buffer,"_InputTempDis",InputTempDis,readvar(25))
       call assignS(buffer,"_RunName",RunName,readvar(26))
        call assignL(buffer,"_FullRestart",FullRestart,readvar(27))
        call assignD(buffer,"T_Bathx2",T_Bathx2,readvar(28))
        call assignD(buffer,"T_Bathy1",T_Bathy1,readvar(29))
        call assignD(buffer,"T_Bathy2",T_Bathy2,readvar(30))
        call assignD(buffer,"T_Bathz1",T_Bathz1,readvar(31))
        call assignD(buffer,"T_Bathz2",T_Bathz2,readvar(32))
        call assignD(buffer,"T_System",T_System,readvar(33))

    end do
    call check_param(readvar,size(readvar,1))

  end subroutine read_param
!!!##########################################################################


!!!##########################################################################
!!! assigns a DP value to variable if the line contains the right keyword
!!!##########################################################################
  subroutine read_mesh(unit)
    integer, intent(in) :: unit
    integer(int12) :: i, j, k, reason, c
    character(1024) :: buffer, array
    
    ! read mesh cell number
    read(unit,'(A)',iostat=Reason) buffer
    read(buffer,*) nx, ny, nz
    Na = nx*ny*nz

    ! Allocate Global data arrays
    allocate(grid(nx,ny,nz))
    
    ! read mesh volume dimessions
    read(unit,'(A)',iostat=Reason) buffer
    read(buffer,*) Lx, Ly, Lz

    grid(:,:,:)%Length(1)=real(Lx)/real(nx)
    grid(:,:,:)%Length(2)=real(Ly)/real(ny)
    grid(:,:,:)%Length(3)=real(Lz)/real(nz)
    grid(:,:,:)%area(1)=grid(:,:,:)%Length(2)*grid(:,:,:)%Length(3)
    grid(:,:,:)%area(2)=grid(:,:,:)%Length(1)*grid(:,:,:)%Length(3)
    grid(:,:,:)%area(3)=grid(:,:,:)%Length(1)*grid(:,:,:)%Length(2)
    grid(:,:,:)%volume=grid(:,:,:)%Length(1)*grid(:,:,:)%Length(2)*grid(:,:,:)%Length(3)

    do k = 1, nz
       read(unit, '(A)', iostat= Reason) buffer
       do j = 1, ny
          if (Reason .ne. 0) then
            write(6,*) 'Error: Unexpected EOF geom.in'
            call exit
          end if
          read(unit, '(A)', iostat=Reason) array 
          read(array,*,iostat = reason) (grid(i,j,k)%imaterial_type, i = 1,nx)
        end do
    end do
  end subroutine read_mesh
!!!##########################################################################


!!!##########################################################################
!!! The heating file
!!!##########################################################################
  subroutine read_heat(unit)
    integer, intent(in) :: unit
    integer(int12) :: i, j, k, reason, c
    character(1024) :: buffer, array

    ! Allocate Global data arrays
    allocate(iheater(nx,ny,nz))

    do k = 1, nz
       read(unit, '(A)', iostat= Reason) buffer
       do j = 1, ny
          if (Reason .ne. 0) then
            write(6,*) 'Error: Unexpected EOF heat.in'
            call exit
          end if
          read(unit, '(A)', iostat=Reason) array
          read(array,*,iostat = reason) (iheater(i,j,k), i=1,nx )
        end do
    end do
  
  end subroutine read_heat
!!!##########################################################################


!!!##########################################################################
!!! The the materials file
!!!##########################################################################
subroutine read_mat(unit)
    implicit none
    integer, intent(in) :: unit
    type(material), dimension(100) :: dum_mat
    character(1024) :: buffer
    integer :: reason, j
    integer, dimension(8) :: readvar
    integer :: i, index

    i=0
    readvar(:) = 0
    read: do
       read(unit,'(A)',iostat=Reason) buffer
       if(Reason.ne.0) exit read
       
       ! Remove comment lines
       if(scan(buffer,'!').ne.0) buffer=buffer(:(scan(buffer,'!')-1))
       if(trim(buffer).eq.'') cycle

       ! Check if the first field is a number
       read(buffer, *, iostat=reason) index

       ! If a number reason=zero
       if(reason .eq. 0) then
          ! If 0 reset readvar
          if(index .eq. 0) then
                ! Check for missing parameters after the loop
              if (any(readvar .ne. 1)) then
                write(6,*) "Error in parameters : material ", readvar
                call exit
              end if
             readvar(:) = 0
          else
             ! If other number record it and increment
             i = i + 1
             dum_mat(i)%index = index
             if(i .eq. 101) then
                write(6,*) 'Error: code does not suporrt over 100 materials'
                call exit
             end if
          end if
          cycle
       end if

    
       call assignD(buffer,"heat_capacity",dum_mat(i)%heat_capacity,readvar(1))
       call assignD(buffer,"h_conv"       ,dum_mat(i)%h_conv       ,readvar(2))
       call assignD(buffer,"kappa"        ,dum_mat(i)%kappa        ,readvar(3))
       call assignD(buffer,"kappa3D"      ,dum_mat(i)%kappa3D      ,readvar(4))
       call assignD(buffer,"rho"          ,dum_mat(i)%rho          ,readvar(5))
       call assignD(buffer,"sound_speed"  ,dum_mat(i)%sound_speed  ,readvar(6))
       call assignD(buffer,"tau"          ,dum_mat(i)%tau          ,readvar(7))
       call assignL(buffer,"source"       ,dum_mat(i)%source       ,readvar(8))
    end do read
    
    ! Check for duplicate indices
    do j = 1, i-1
       if (any(dum_mat(j)%index .eq. dum_mat(j+1:i)%index)) then  
          write(6,*) "Error: Duplicate material index ", dum_mat(j)%index
          call exit
       end if
    end do

    
    allocate(input_materials(i))
    input_materials(1:i) = dum_mat(1:i)

  end subroutine read_mat
!!!##########################################################################


!!!##########################################################################
!!! assigns a DP value to variable if the line contains the right keyword
!!!##########################################################################
  subroutine assignD(buffer,keyword,variable,found)
    ! for the line with the keyword trim everything before the = inclusively 
    ! and assign that value to the keyword
    integer::found
    character(1024)::buffer1,buffer2
    character(*)::buffer,keyword
    real(real12)::variable
    buffer1=buffer(:scan(buffer,"=")-1)
    if(scan("=",buffer).ne.0) buffer2=val(buffer)
    if(trim(adjustl(buffer1)).eq.trim(adjustl(keyword))&
         .and.trim(adjustl(buffer2)).ne.'') then
       found=found+1
       read(buffer2,*) variable
    end if
  end subroutine assignD
!!!##########################################################################


!!!##########################################################################
!!! assigns an integer
!!!##########################################################################
  subroutine assignI(buffer,keyword,variable,found)
    ! for the line with the keyword trim everything before the = inclusively 
    ! and assign that value to the keyword
    integer::found
    character(1024)::buffer1,buffer2
    character(*)::buffer,keyword
    integer(int12)::variable
    buffer1=buffer(:scan(buffer,"=")-1)
    if(scan("=",buffer).ne.0) buffer2=val(buffer)
    if(trim(adjustl(buffer1)).eq.trim(adjustl(keyword))&
         .and.trim(adjustl(buffer2)).ne.'') then
       found=found+1
       read(buffer2,*) variable
    end if
  end subroutine assignI
!!!##########################################################################


!!!##########################################################################
!!! assigns an logical
!!!##########################################################################
  subroutine assignL(buffer,keyword,variable,found)
    ! for the line with the keyword trim everything before the = inclusively 
    ! and assign that value to the keyword
    integer::found
    character(1024)::buffer1,buffer2
    character(*)::buffer,keyword
    logical::variable
    buffer1=buffer(:scan(buffer,"=")-1)
    if(scan("=",buffer).ne.0) buffer2=val(buffer)
    if(trim(adjustl(buffer1)).eq.trim(adjustl(keyword))&
         .and.trim(adjustl(buffer2)).ne.'') then
       found=found+1
       if(index(buffer2,"T").ne.0.or.index(buffer2,"t").ne.0) then
          variable=.TRUE.
       end if
       if(index(buffer2,"F").ne.0.or.index(buffer2,"f").ne.0) then
          variable=.FALSE.
       end if
    end if
  end subroutine assignL
!!!##########################################################################


!!!##########################################################################
!!! assigns a string
!!!##########################################################################
  subroutine assignS(buffer,keyword,variable,found)
    ! for the line with the keyword trim everything before the = inclusively 
    ! and assign that value to the keyword
    integer::found
    character(1024)::buffer1,buffer2
    character(*)::buffer,keyword
    character(*)::variable
    buffer1=buffer(:scan(buffer,"=")-1)
    if(scan("=",buffer).ne.0) buffer2=val(buffer)
    if(trim(adjustl(buffer1)).eq.trim(adjustl(keyword))&
         .and.trim(adjustl(buffer2)).ne.'') then
       found=found+1
       read(buffer2,*) variable
    end if
  end subroutine assignS
!!!##########################################################################


!!!##########################################################################
!!! val outouts the section of buffer that occurs after an "="
!!!##########################################################################
  function val(buffer)
    character(*) :: buffer
    character(100) :: val
    val=trim(adjustl(buffer((scan(buffer,"=",back=.true.)+1):)))
    return
  end function val
!!!##########################################################################

end module inputs

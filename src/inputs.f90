!!!#################################################################################################
!!! This code reads inputs files found in the inputs folder.
!!! The input files are:
!!!     - param.in, user inputed parameters for the simulation
!!!     - mat.in, user inputed material properties, heat capacity, thermal conductivity, etc.
!!!     - geom.in, user inputed geometry of the system, what materials are where
!!!     - heat.in, user inputed heat sources, where the heat is being applied,...
!!! ... too be removed and implemented in the geom.in file.
!!! The input files are read in the subroutine read_all_files 
!!! The subroutines are:
!!!     - read_param, reads the param
!!!     - read_mat, reads the mat
!!!     - read_mesh, reads the geom
!!!     - read_heat, reads the heat
!!!     - assignD, assigns a DP value to variable if the line contains the right keyword
!!!     - assignI, assigns an integer 
!!!     - assignL, assigns an logical
!!!     - assignS, assigns a string
!!!     - val, outouts the section of buffer that occurs after an "="
!!!     - check_param, gives errors and warnings regarding the INPUT file
!!! The module contains the following variables:
!!!     - time_step, the time step of the simulation
!!!     - freq, the frequency of the heater in simulation
!!!     - power_in, the power in of the heater in the simulation
!!!     - kappaBoundx, the boundary kappa in the x direction plane x=1
!!!     - kappaBoundy, the boundary kappa in the y direction plane y=1
!!!     - kappaBoundz, the boundary kappa in the z direction plane z=1
!!!     - kappaBoundNx, the boundary kappa in the x direction plane x=nx
!!!     - kappaBoundNy, the boundary kappa in the y direction plane y=ny
!!!     - kappaBoundNz, the boundary kappa in the z direction plane z=nz
!!!     - KappaBound, the boundary kappa
!!!     - T_Bathx1, the bath temperature in the x direction
!!!     - T_Bathx2, the bath temperature in the x direction
!!!     - T_Bathy1, the bath temperature in the y direction
!!!     - T_Bathy2, the bath temperature in the y direction
!!!     - T_Bathz1, the bath temperature in the z direction
!!!     - T_Bathz2, the bath temperature in the z direction
!!!     - T_System, the system temperature
!!!     - T_Bath, the bath temperature
!!!     - IVERB, the verbose
!!!     - ntime, the number of time steps
!!!     - iboundary, the boundary condition
!!!     - nx, the number of cells in the x direction
!!!     - ny, the number of cells in the y direction
!!!     - nz, the number of cells in the z direction
!!!     - icattaneo, switch cattaneo on or off (1, 0 respectively)
!!!     - isteady, switch steady state on or off (1,0 respectively)
!!!     - NA, the number of cells
!!!     - Check_Sparse_Full, check if the simulation is sparse or full
!!!     - Check_Stability, check if the simulation is stable
!!!     - Check_Steady_State, check if the simulation is in steady state
!!!     - WriteToTxt, write to a txt file
!!!     - LPercentage, the percentage of the simulation
!!!     - InputTempDis, the input temperature distribution
!!!     - Test_Run, the test run
!!!     - FullRestart, the full restart
!!!     - RunName, the name of the simulation run
!!!     - grid, the grid of the simulation
!!!     - input_materials, the materials of the simulation
!!!     - Lx, the length in the x direction
!!!     - Ly, the length in the y direction
!!!     - Lz, the length in the z direction

!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module inputs
  use constants, only: real12, int12
  use constructions, only: heatblock, material
  implicit none

  integer :: unit, newunit
  ! time step, frequency, power in, boundary kappa
  real(real12) :: time_step, freq, power_in, kappaBoundx1, kappaBoundy1, kappaBoundz1, KappaBound
  real(real12) :: kappaBoundNx, kappaBoundNy, kappaBoundNz 
  ! Bath temperatures
  real(real12) :: T_Bathx1, T_Bathx2, T_Bathy1, T_Bathy2, T_Bathz1, T_Bathz2, T_System, T_Bath
  ! verbose, number of time steps, boundary condition, number of cells
  integer(int12) :: IVERB, ntime, iboundary, nx, ny, nz, icattaneo, isteady, NA 
  ! flags
  logical :: Check_Sparse_Full, Check_Stability, Check_Steady_State
  logical :: WriteToTxt, LPercentage, InputTempDis
  logical ::  Test_Run = .FALSE., FullRestart = .FALSE.
  ! Name of simiulation run
  character(1024) :: RunName 
  ! Essentially it is the system that is being simulated
  type(heatblock), dimension(:,:,:), allocatable :: grid 
  type(material), dimension(:), allocatable :: input_materials ! The materials
  real(real12) :: Lx, Ly, Lz ! Volume dimensions (lenghts)




contains
!!!#################################################################################################
!!! read_all_files will call the routines to read each input file
!!!#################################################################################################
  subroutine read_all_files()
    integer :: unit, reason ! file unit and reason
    character(64) :: param_infile, mat_infile, mesh_infile ! file names
    logical :: file_exists ! check if file exists


    !-----------------------------------------------
    ! get data from param.in
    !-----------------------------------------------
    ! name infile
    param_infile = "./inputs/param.in" ! file name

    ! check if file is there
    inquire(file=param_infile, exist=file_exists) ! check if file exists

    ! give error and exit code
    if (.not.file_exists) then
        write(6,*) 'Error cannot find file: param.in'
        stop
    end if
    
    ! open, read and close param.in
    open(newunit=unit, file=param_infile, iostat=reason)
    CALL read_param(unit)
    close(unit)
    

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


    !-----------------------------------------------
    ! get data from mat.in
    !-----------------------------------------------
    ! name infile

    mat_infile = "./inputs/mat.in" ! file name

    ! check if file is there
    inquire(file=mat_infile, exist=file_exists)

    ! give error and exit code
    if (.not.file_exists) then
        write(6,*) 'Error cannot find file: mat.in'
        stop
    end if
    
    ! open, read and close mat.in
    open(newunit=unit, file=mat_infile, iostat=reason)
    CALL read_mat(unit)
    close(unit)
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


    !-----------------------------------------------
    ! get data from system.in
    !-----------------------------------------------
    ! name infile

    mesh_infile = "./inputs/system.in" ! file name

    ! check if file is there
    inquire(file=mesh_infile, exist=file_exists)

    ! give error and exit code
    if (.not.file_exists) then
        write(6,*) 'Error cannot find file: system.in'
        stop
    end if
    
    ! open, read and close geom.in
    open(newunit=unit, file=mesh_infile, iostat=reason)
    CALL read_system(unit)
    close(unit)
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


  end subroutine read_all_files
!!!#################################################################################################





!!!#################################################################################################
!!! reads an input file where each flag has the format "KEYWORD =" followed ...
!!! ... by the variable i.e. 1, 2, .TRUE., 5D-15
!!!#################################################################################################
  subroutine read_param(unit)
    integer:: unit, Reason, i
    integer,dimension(31)::readvar
    character(1024)::buffer
    logical::ex
    readvar=0
    !------------------------------------------
    ! assign defaults
    !------------------------------------------
    IVERB = 1
    Check_Sparse_Full = .FALSE.
    Check_Stability = .FALSE.
    LPercentage = .FALSE.
    Test_Run = .FALSE.
    InputTempDis = .FALSE.
    FullRestart = .FALSE.
    RunName = 'default'
    RunName = trim(adjustl(RunName))
    WriteToTxt = .FALSE.
    ntime = 10
    time_step = 1.0
    freq = 1
    iboundary = 1
    icattaneo = 1
    isteady = 0
    T_System = 300
    T_Bathx1 = T_Bath
    T_Bathx2 = T_Bath
    T_Bathy1 = T_Bath
    T_Bathy2 = T_Bath
    T_Bathz1 = T_Bath
    T_Bathz2 = T_Bath
    power_in = 0
    kappaBoundx1 = KappaBound
    kappaBoundy1 = KappaBound
    kappaBoundz1 = KappaBound
    kappaBoundNx = KappaBound
    kappaBoundNy = KappaBound
    kappaBoundNz = KappaBound
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
        ! assignS for strings
       !---------------------------------------
       ! looks for all the keywords relating to inputs and defines their variables
       CALL assignI(buffer,"IVERB",IVERB,readvar(1))
       CALL assignI(buffer,"ntime",ntime,readvar(2))           
       CALL assignD(buffer,"time_step",time_step,readvar(3))   
       CALL assignD(buffer,"freq",freq,readvar(4))       
       CALL assignI(buffer,"iboundary",iboundary,readvar(5))   
       CALL assignI(buffer,"icattaneo", icattaneo, readvar(6))
       CALL assignI(buffer,"isteady", isteady, readvar(7))
       CALL assignD(buffer,"power_in",power_in,readvar(8))       
       CALL assignD(buffer,"kappaBoundx1",kappaBoundx1,readvar(9))
       CALL assignD(buffer,"kappaBoundy1",kappaBoundy1,readvar(10))    
       CALL assignD(buffer,"kappaBoundz1",kappaBoundz1,readvar(11))
       CALL assignL(buffer,"_Check_Sparse_Full",Check_Sparse_Full,readvar(12))
       CALL assignL(buffer,"_Check_Stability",Check_Stability,readvar(13))
       CALL assignL(buffer,"_WriteToTxt",WriteToTxt,readvar(14))
       CALL assignL(buffer,"_Percentage_Completion",LPercentage,readvar(15))
       CALL assignL(buffer,"_Test_Run",Test_Run,readvar(16))
       CALL assignL(buffer,"_InputTempDis",InputTempDis,readvar(17))
       CALL assignS(buffer,"_RunName",RunName,readvar(18))
        CALL assignL(buffer,"_FullRestart",FullRestart,readvar(19))
        CALL assignD(buffer,"T_Bathx1",T_Bathx1,readvar(20)) 
        CALL assignD(buffer,"T_Bathx2",T_Bathx2,readvar(21))
        CALL assignD(buffer,"T_Bathy1",T_Bathy1,readvar(22))
        CALL assignD(buffer,"T_Bathy2",T_Bathy2,readvar(23))
        CALL assignD(buffer,"T_Bathz1",T_Bathz1,readvar(24))
        CALL assignD(buffer,"T_Bathz2",T_Bathz2,readvar(25))
        CALL assignD(buffer,"T_System",T_System,readvar(26))
        CALL assignD(buffer,"T_Bath",T_Bath,readvar(27))
        CALL assignD(buffer,"kappaBound",KappaBound,readvar(28))
        CALL assignD(buffer,"kappaBoundNx",kappaBoundNx,readvar(29))
        CALL assignD(buffer,"kappaBoundNy",kappaBoundNy,readvar(30))
        CALL assignD(buffer,"kappaBoundNz",kappaBoundNz,readvar(31))
    end do
    CALL check_param(readvar,size(readvar,1))

  end subroutine read_param
!!!#################################################################################################

!!!#################################################################################################
!!! check_param gives errors and warnings regarding the INPUT file
!!!#################################################################################################
  subroutine check_param(readvar,n)
    integer::n,i
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
       stop
    end if

    !------------------------------------------------------------------------------------
    ! Disply that T_system and T_bath are being used and not individual, i.e T_bathx,y,z
    !------------------------------------------------------------------------------------
    if ((any(readvar(9:11).eq.0)) .and. (readvar(28) .gt. 0) )then
        write(6,*)
        write(6,'(A43)') '###############################'
        write(6,'(A43)') '##########   WARNING   ##########'
        write(6,'(A43)') '###############################'
        write(6,*)
        write(6,'(A)') ' ---       Warning in subroutine "checkINPUT"       ---'
        write(6,'(A)') ' --- WARNING: KappaBoundx,y,z are not set, KappaBound will be used    ---'
    end if

    if (any(readvar(20:25).eq.0) .and. (readvar(27) .gt. 0) ) then
        write(6,*)
        write(6,'(A43)') '###############################'
        write(6,'(A43)') '##########   WARNING   ##########'
        write(6,'(A43)') '###############################'
        write(6,*)
        write(6,'(A)') ' ---       Warning in subroutine "checkINPUT"       ---'
        write(6,'(A)') ' --- WARNING: T_Bath x,y,z not set T_Bath will be used    ---'
    end if 
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !------------------------------------------------------------------------------------
    ! Check for missing parameters
    !------------------------------------------------------------------------------------

      if (any(readvar(9:11) .eq.0) .and. (readvar(28) .eq. 0) ) then
        write(6,*)
        write(6,'(A43)') '###############################'
        write(6,'(A43)') '##########   ERROR   ##########'
        write(6,'(A43)') '###############################'
        write(6,*)
        write(6,'(A)') ' ---       Error in subroutine "checkINPUT"       ---'
        write(6,'(A)') ' --- ERROR: KappaBound missing and KappaBound x,y,z not set    ---'
        write(6,*)
        stop
      end if
      if (any(readvar(20:25) .eq.0) .and. (readvar(27) .eq. 0) ) then
        write(6,*)
        write(6,'(A43)') '###############################'
        write(6,'(A43)') '##########   ERROR   ##########'
        write(6,'(A43)') '###############################'
        write(6,*)
        write(6,'(A)') ' ---       Error in subroutine "checkINPUT"       ---'
        write(6,'(A)') ' --- ERROR: T_Bath and T_Bath x,y,z are not set    ---'
        write(6,*)
        stop
      end if
      if (readvar(27) .gt. 1) then
        T_Bathx1 = T_Bath
        T_Bathx2 = T_Bath
        T_Bathy1 = T_Bath
        T_Bathy2 = T_Bath
        T_Bathz1 = T_Bath
        T_Bathz2 = T_Bath
        readvar(20:25) = 1
      end if
      if (readvar(28) .gt. 1) then
        kappaBoundx1 = KappaBound
        kappaBoundy1 = KappaBound
        kappaBoundz1 = KappaBound
        kappaBoundNx = KappaBound
        kappaBoundNy = KappaBound
        kappaBoundNz = KappaBound
        readvar(9:11) = 1
      end if 
    
     if (any(readvar.eq.0)) then
      write(6,*)
      write(6,'(A43)') '###############################'
      write(6,'(A43)') '##########   WARNING  ##########'
      write(6,'(A43)') '###############################'
      write(6,*)
      write(6,'(A)') ' ---       WARNING in subroutine "checkINPUT"       ---'
      write(6,'(A)') ' --- WARNING: Essential parameters missing    ---'
      ! Print all indices of readvar that are 0
      do i = 1, size(readvar)
        if (readvar(i) == 0) then
          write(6, '(A,I3)') 'Index ', i, ' of readvar is 0'
        end if
      end do
      write(6,*)
     end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^



    if(IVERB .ge. 1) then
       write(6,'(A)')        ' vebose printing option'
       write(6,'(A)')        ' running calculation with :'
       write(6,'(A35,I6)')   '   IVERB      = ',IVERB
       write(6,'(A35,L1)')   '   _Check_Sparse_Full  = ',Check_Sparse_Full
       write(6,'(A35,L1)')   '   _Check_Stability         = ',Check_Stability
       write(6,'(A35,L1)')   '  _Percentage_Completion         = ',LPercentage
       write(6,'(A35,L1)')   '   _Test_Run         = ',Test_Run
       write(6,'(A35,L1)')   '   _InputTempDis         = ',InputTempDis
       write(6,'(A35,L1)')   '    _FullRestart         = ',FullRestart
       write(6,'(A35,A)')   '    _RunName         = ',trim(RunName)
       write(6,'(A35,L1)')   '   _WriteToTxt         = ',WriteToTxt
       write(6,'(A35,I12)')   '   ntime      = ',ntime
       write(6,'(A35,F20.15)')'   Time_step  = ',time_step
       write(6,'(A35,F12.5)')'   frequency  = ',freq
       write(6,'(A35,I6)')   '   iboundary  = ',iboundary
       write(6,'(A35,I6)')   '   icattaneo  = ' , icattaneo
       write(6,'(A35,I6)')   '   isteady    = ', isteady
       write(6,'(A35,F12.5)')'   T_System   = ', T_System
       write(6,'(A35,F12.5)')'   T_Bath      = ', T_Bath
       write(6,'(A35,F12.5)')'   T_Bathx1     = ', T_Bathx1
       write(6,'(A35,F12.5)')'   T_Bathx2     = ', T_Bathx2
       write(6,'(A35,F12.5)')'   T_Bathy1     = ', T_Bathy1
       write(6,'(A35,F12.5)')'   T_Bathy2     = ', T_Bathy2
       write(6,'(A35,F12.5)')'   T_Bathz1     = ', T_Bathz1
       write(6,'(A35,F12.5)')'   T_Bathz2     = ', T_Bathz2
       write(6,'(A35,F12.5)')'   power_in   = ',power_in
       write(6,'(A35,F12.5)')   '   KappaBound         = ',KappaBound
       write(6,'(A35,F12.5)')   '   kappaBoundx1         = ',kappaBoundx1
       write(6,'(A35,F12.5)')   '   kappaBoundy1         = ',kappaBoundy1
       write(6,'(A35,F12.5)')   '   kappaBoundz1         = ',kappaBoundz1
       write(6,'(A35,F12.5)')   '   kappaBoundNx         = ',kappaBoundNx
       write(6,'(A35,F12.5)')   '   kappaBoundNy         = ',kappaBoundNy
       write(6,'(A35,F12.5)')   '   kappaBoundNz         = ',kappaBoundNz

    end if
  end subroutine check_param
!!!#################################################################################################  

!!!#################################################################################################
!!! The read in the system file, system.in
!!!#################################################################################################
  subroutine read_system(unit)
    integer, intent(in) :: unit
    integer(int12) :: ix, iy, iz, reason, c, pos ! counters
    character(1024) :: buffer, array,line, part1, part2 ! buffer and array
    ! read mesh cell number
    read(unit,'(A)',iostat=Reason) buffer ! read the buffer
    read(buffer,*) nx, ny, nz ! read the buffer into nx, ny, nz
    Na = nx*ny*nz ! number of cells
    ! Allocate Global data arrays
    allocate(grid(nx,ny,nz)) 
    ! read mesh volume dimessions
    read(unit,'(A)',iostat=Reason) buffer
    read(buffer,*) Lx, Ly, Lz 
    grid(:,:,:)%Length(1)=Lx/real(nx)
    grid(:,:,:)%Length(2)=Ly/real(ny)
    grid(:,:,:)%Length(3)=Lz/real(nz)
    grid(:,:,:)%volume=grid(:,:,:)%Length(1)*grid(:,:,:)%Length(2)*grid(:,:,:)%Length(3)
    ! Read the file
    do iz = 1, nz
        read(unit, '(A)', iostat= Reason) buffer 
        do iy = 1, ny
            if (Reason .ne. 0) then
                write(6,*) 'Error: Unexpected EOF system.in' ! error
                stop
            end if
            read(unit, '(A)', iostat=Reason) array 
            do ix = 1, nx
                read(array, '(A)', iostat=Reason) buffer
                read(buffer, '(A)', iostat=Reason) line
                pos = index(line, ':')
                part1 = trim(adjustl(line(:pos-1)))
                part2 = trim(adjustl(line(pos+1:)))
                read(part1, *) grid(ix,iy,iz)%imaterial_type
                read(part2, *) grid(ix,iy,iz)%iheater
            end do
        end do
    end do
    if (IVERB.gt.4) then
      print*, 'grid%imaterial = ', grid%imaterial_type
      print*, 'grid%iheater = ', grid%iheater
    end if
  end subroutine read_system

!!!#################################################################################################
!!! The materials file, mat.in
!!!#################################################################################################
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
       read(unit,'(A)',iostat=Reason) buffer ! read the buffer
       if(Reason.ne.0) exit read ! if the buffer is empty exit the read loop
       
       ! Remove comment lines
       if(scan(buffer,'!').ne.0) buffer=buffer(:(scan(buffer,'!')-1)) ! remove comments
       if(trim(buffer).eq.'') cycle ! remove blank lines

       ! Check if the first field is a number
       read(buffer, *, iostat=reason) index ! read the buffer into index

       ! If a number reason=zero and we can continue
       if(reason .eq. 0) then 
          ! If 0 reset readvar and check for missing parameters
          if(index .eq. 0) then
                ! Check for missing parameters after the loop
              if (any(readvar .ne. 1)) then 
                write(6,*) "Error in parameters : material ", readvar
                stop
              end if
             readvar(:) = 0
          else
             ! If other number record it and increment
             i = i + 1
             dum_mat(i)%index = index ! record the index
             if(i .eq. 101) then
                write(6,*) 'Error: code does not suporrt over 100 materials'
                stop
             end if
          end if
          cycle
       end if

    
       CALL assignD(buffer,"heat_capacity",dum_mat(i)%heat_capacity,readvar(1))! assign heatCapacity
       CALL assignD(buffer,"h_conv"       ,dum_mat(i)%h_conv       ,readvar(2))! assign h_conv
       CALL assignD(buffer,"kappa"        ,dum_mat(i)%kappa        ,readvar(3))! assign kappa
       CALL assignD(buffer,"kappa3D"      ,dum_mat(i)%kappa3D      ,readvar(4))! assign kappa3D
       CALL assignD(buffer,"rho"          ,dum_mat(i)%rho          ,readvar(5))! assign rho
       CALL assignD(buffer,"sound_speed"  ,dum_mat(i)%sound_speed  ,readvar(6))! assign sound_speed
       CALL assignD(buffer,"tau"          ,dum_mat(i)%tau          ,readvar(7))! assign tau
       CALL assignL(buffer,"source"       ,dum_mat(i)%source       ,readvar(8))! assign source
    end do read
    
    ! Check for duplicate indices
    do j = 1, i-1
       if (any(dum_mat(j)%index .eq. dum_mat(j+1:i)%index)) then  
          write(6,*) "Error: Duplicate material index ", dum_mat(j)%index
          stop
       end if
    end do

    
    allocate(input_materials(i))
    input_materials(1:i) = dum_mat(1:i)

  end subroutine read_mat
!!!#################################################################################################


!!!#################################################################################################
!!! assigns a DP value to variable if the line contains the right keyword
!!!#################################################################################################
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
!!!#################################################################################################


!!!#################################################################################################
!!! assigns an integer
!!!#################################################################################################
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
!!!#################################################################################################


!!!#################################################################################################
!!! assigns an logical
!!!#################################################################################################
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
!!!#################################################################################################


!!!#################################################################################################
!!! assigns a string
!!!#################################################################################################
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
!!!#################################################################################################


!!!#################################################################################################
!!! val outouts the section of buffer that occurs after an "="
!!!#################################################################################################
  function val(buffer)
    character(*) :: buffer
    character(100) :: val
    val=trim(adjustl(buffer((scan(buffer,"=",back=.true.)+1):)))
    return
  end function val
!!!#################################################################################################

end module inputs

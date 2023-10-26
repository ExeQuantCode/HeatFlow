
!##############################################################################################################
! This code reads parameter inputs from Inputs.txt. See InputsHelp.txt for help
!##############################################################################################################
module inputs
  use constants, only: real12, int12
  use constructions, only: heatblock


  implicit none
  integer :: unit, newunit
  real(real12) :: time_step, T_Bath, freq, power_in, T_period, cutoff
  integer(int12) :: IVERB, icell_mix, ntime, Rel, zpos, ACon, iheater &
	, iboundary, nx, ny, nz, icattaneo, isteady, NA
  logical, parameter :: verbose = .FALSE.




contains
!############################################################################
! checkINPUT gives errors and warnings regarding the INPUT file
!############################################################################
  subroutine checkINPUT(readvar,n)
    integer::n
    integer,dimension(n)::readvar
    ! Not currently in use
    if(any(readvar.gt.1)) then
       write(0,*)
       write(0,'(A43)') '###############################'
       write(0,'(A43)') '##########   ERROR   ##########'
       write(0,'(A43)') '###############################'
       write(0,*)
       write(0,'(A)') ' ---       Error in subroutine "checkINPUT"       ---'
       write(0,'(A)') ' --- ERROR: same KEYWORD apears more than once    ---'
       call exit
    end if

    if(verbose) then
       write(6,'(A)') ' vebose printing option'
       write(6,'(A)') ' running calculation with :'
       write(6,'(A35,I6)')  '      IVERB     = ',IVERB
       write(6,'(A35,I6)') '      icell_mix  = ',icell_mix
       write(6,'(A35,I6)')  '      ntime    = ',ntime
       write(6,'(A35,I6)')  '      Rel      = ', Rel
       write(6,'(A35,I6)')  '      zpos     = ',zpos
       write(6,'(A35,I6)')  '      ACon     = ',ACon       
       write(6,'(A35,F12.5)')  '   Time_step     = ',time_step
       write(6,'(A35,I6)')  '      iheater     = ',iheater
       write(6,'(A35,F12.5)')  '   frequency     = ',freq
       write(6,'(A35,I6)')  '      iboundary     = ',iboundary
       write(6,'(A35,I6)') ' icattaneo = ' , icattaneo
       write(6,'(A35,I6)') ' isteady = ', isteady
       write(6,'(A35,F12.5)')  '   T_Bath     = ', T_Bath
       write(6,'(A35,F12.5)')  '      cutoff     = ',cutoff
       write(6,'(A35,F12.5)')  '   power_in     = ',power_in
       write(6,'(A35,F12.5)')  '   T_period     = ',T_period
       write(6,'(A35,I6)')  '      nx     = ',nx
       write(6,'(A35,I6)')  '      ny     = ',ny
       write(6,'(A35,I6)')  '      nz     = ',nz

    end if


  end subroutine checkINPUT
!############################################################################



!############################################################################
! reads an input file where each flag has the format "KEYWORD =" followed ...
! ... by the variable i.e. 1, 2, .TRUE., 5D-15
!############################################################################
  subroutine readINPUT(unit)
    integer::unit,Reason, i
    integer,dimension(19)::readvar
    character(1024)::buffer
    logical::ex
    readvar=0
    !------------------------------------------
    ! assign defaults
    !------------------------------------------
    IVERB = 1
    icell_mix = 2
    ntime = 10
    Rel = 0
    zpos = 1
    ACon = 0
    time_step = 1.0
    iheater = 0
    freq = 1
    iboundary = 1
    icattaneo = 1
    isteady = 0
    T_Bath = 300
    cutoff = 1e12
    power_in = 0
    T_period = 1
    nx = 2
    ny = 2
    nz = 2
    Na = nx*ny*nz
    !------------------------------------------
    do
       read(unit,'(A)',iostat=Reason) buffer
       if(Reason.ne.0) exit
       if(scan(buffer,'!').ne.0) buffer=buffer(:(scan(buffer,'!')-1)) ! looks for comments in each line of txt file and trims them off
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
       call assignI(buffer,"zpos",zpos,readvar(5))      
       call assignI(buffer,"ACon",ACon,readvar(6))         
       call assignD(buffer,"time_step",time_step,readvar(7))   
       call assignI(buffer,"iheater",iheater,readvar(8))       
       call assignD(buffer,"freq",freq,readvar(9))       
       call assignI(buffer,"iboundary",iboundary,readvar(10))   
       call assignI(buffer, "icattaneo", icattaneo, readvar(11))
       call assignI(buffer, "isteady", isteady, readvar(12))
       call assignD(buffer,"T_Bath",T_Bath,readvar(13)) 
       call assignD(buffer,"cutoff",cutoff,readvar(14))    
       call assignD(buffer,"power_in",power_in,readvar(15))       
       call assignD(buffer,"T_period",T_period,readvar(16))       
       call assignI(buffer,"nx",nx,readvar(17))    
       call assignI(buffer,"ny",ny,readvar(18))    
       call assignI(buffer,"nz",nz,readvar(19))
       Na = nx*ny*nz
    end do
    close(unit)
    call checkINPUT(readvar,size(readvar,1))

  end subroutine readINPUT
!############################################################################



!############################################################################
! assigns a DP value to variable if the line contains the right keyword
!############################################################################
! KEYWORD = 5.2
  subroutine assignD(buffer,keyword,variable,found)
    !for the line with the keyword trim everything before the = inclusively and assign that value to the keyword
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
!############################################################################


!############################################################################
! assigns an integer
!############################################################################
  subroutine assignI(buffer,keyword,variable,found)
      !for the line with the keyword trim everything before the = inclusively and assign that value to the keyword
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
!############################################################################


!############################################################################
! assigns an logical
!############################################################################
  subroutine assignL(buffer,keyword,variable,found)
      !for the line with the keyword trim everything before the = inclusively and assign that value to the keyword

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
!############################################################################


!############################################################################
! assigns a string
!############################################################################
  subroutine assignS(buffer,keyword,variable,found)
      !for the line with the keyword trim everything before the = inclusively and assign that value to the keyword

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
!############################################################################


!############################################################################
! val outouts the section of buffer that occurs after an "="
!############################################################################
  function val(buffer)
    character(*) :: buffer
    character(100) :: val
    val=trim(adjustl(buffer((scan(buffer,"=",back=.true.)+1):)))
    return
  end function val
!############################################################################

END module inputs

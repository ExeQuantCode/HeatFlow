module inputs
  implicit none
  double precision::efermi,tau,kelvin,bandgap,alpha,addgap
  integer::gradcalc
  character(1024)::EIGENVAL,OUTCAR,POSCAR,SIGMAOUT,BRILNAME,EGRADNAME
  logical::ncl,BRILOUT,EGRADOUT,verbose

contains
!############################################################################
! checkINPUT gives errors and warnings regarding the INPUT file
!############################################################################
  subroutine checkINPUT(readvar,n)
    integer::n
    integer,dimension(n)::readvar

    if(readvar(16).gt.0.and.readvar(10).eq.0) then
       BRILOUT   = .TRUE.
    else if(readvar(17).gt.0.and.readvar(11).eq.0) then
       EGRADOUT  = .TRUE.
    end if

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
    if(gradcalc.ne.1.and.gradcalc.ne.2) then
       write(0,*)
       write(0,'(A43)') '###############################'
       write(0,'(A43)') '##########  WARNING  ##########'
       write(0,'(A43)') '###############################'
       write(0,*)
       write(0,'(A)') ' --- WARNING: GRAD_ORDER must be 1 or 2             ---'
       gradcalc=2
       write(0,'(A)') ' --- GRAD_ORDER has been changed to 2               ---'
    end if
    if(verbose) then
       write(6,'(A)') ' vebose printing option'
       write(6,'(A)') ' running calculation with :'
       write(6,'(A35,F12.5)')  '      EFERMI(efermi)        = ',efermi
       write(6,'(A35,ES16.5)') '      RTIME(tau)            = ',tau
       write(6,'(A35,F12.5)')  '      TEMP(kelvin)          = ',kelvin
       write(6,'(A35,F12.5)')  '      BANDGAP(bandgap)      = ',bandgap
       write(6,'(A35,F12.5)')  '      BANDSTRECH(alpha)     = ',alpha
       write(6,'(A35,F12.5)')  '      ADDGAP(addgap)        = ',addgap
       write(6,'(A35,I6)')     '      GRAD_ORDER(gradcalc)  = ',gradcalc
       write(6,'(A35,A)')      '      VERBOSE(verbose)      = ','.TRUE.'
       if(ncl) then
       write(6,'(A35,A)')      '      NONCOLINEAR(ncl)      = ','.TRUE.'
       else
       write(6,'(A35,A)')      '      NONCOLINEAR(ncl)      = ','.FALSE.'
       end if
       if(BRILOUT) then
       write(6,'(A35,A)')      '      OUTPUTBRIL(BRILOUT)   = ','.TRUE.'
       write(6,'(A35,A)')      '      BRILNAME(BRILNAME)    = ','.TRUE.'
       else
       write(6,'(A35,A)')      '      OUTPUTBRIL(BRILOUT)   = ','.FALSE.'
       end if
       if(EGRADOUT) then
       write(6,'(A35,A)')      '      OUTPUTEGRAD(EGRADOUT) = ','.TRUE.'
       write(6,'(A35,A)')      '      EGRADNAME(EGRADNAME)  = ','.TRUE.'
       else
       write(6,'(A35,A)')      '      OUTPUTEGRAD(EGRADOUT) = ','.FALSE.'
       end if
       write(6,'(A35,A1,A,A1)') '      BANDFILE(EIGENVAL)    = ',"'",trim(adjustl(EIGENVAL)),"'"
       write(6,'(A35,A1,A,A1)') '      DFTFILE(OUTCAR)       = ',"'",trim(adjustl(OUTCAR)),"'"
       write(6,'(A35,A1,A,A1)') '      POSFILE(POSCAR)       = ',"'",trim(adjustl(POSCAR)),"'"
       write(6,'(A35,A1,A,A1)') '      SIGMAOUT(SIGMAOUT)    = ',"'",trim(adjustl(SIGMAOUT)),"'"
    end if


  end subroutine checkINPUT
!############################################################################



!############################################################################
! reads an input file where each flag has the format "KEYWORD =" followed ...
! ... by the variable i.e. 1, 2, .TRUE., 5D-15
!############################################################################
  subroutine readINPUT(unit)
    integer::unit,Reason
    integer,dimension(17)::readvar
    character(1024)::buffer
    logical::ex
    readvar=0
    !------------------------------------------
    ! assine defaults
    !------------------------------------------
    efermi    = 0.D0
    tau       = 5D-15
    kelvin    = 293.15D0
    bandgap   = 0.D0
    alpha     = 1.D0
    addgap    = 0.D0
    gradcalc  = 2
    verbose   = .FALSE.
    ncl       = .FALSE.
    BRILOUT   = .FALSE.
    EGRADOUT  = .FALSE.
    EIGENVAL  = "EIGENVAL"
    OUTCAR    = "OUTCAR"
    POSCAR    = "POSCAR"
    SIGMAOUT  = "PROJ-SIGMA"
    BRILNAME  = "BRIL.vasp"
    EGRADNAME = "EGRAD.dat"
    !------------------------------------------
    do
       read(unit,'(A)',iostat=Reason) buffer
       if(Reason.ne.0) exit
       if(scan(buffer,'!').ne.0) buffer=buffer(:(scan(buffer,'!')-1))
       if(trim(buffer).eq.'') cycle
       !---------------------------------------
       ! assineD works for doubles
       ! assineL for logicals
       ! assineI for integers
       !---------------------------------------
       call assineD(buffer,"EFERMI",efermi,readvar(1))         ! needed
       call assineD(buffer,"RTIME",tau,readvar(2))             ! opt
       call assineD(buffer,"TEMP",kelvin,readvar(3))           ! opt
       call assineD(buffer,"BANDGAP",bandgap,readvar(4))       ! opt
       call assineD(buffer,"BANDSTRECH",alpha,readvar(5))      ! opt will change grp_vel!!
       call assineD(buffer,"ADDGAP",addgap,readvar(6))         ! opt
       call assineI(buffer,"GRAD_ORDER",gradcalc,readvar(7))   ! opt
       call assineL(buffer,"VERBOSE",verbose,readvar(8))       ! opt
       call assineL(buffer,"NONCOLINEAR",ncl,readvar(9))       ! opt
       call assineL(buffer,"OUTPUTBRIL",BRILOUT,readvar(10))   ! opt
       call assineL(buffer,"OUTPUTEGRAD",EGRADOUT,readvar(11)) ! opt
       call assineS(buffer,"BANDFILE",EIGENVAL,readvar(12))    ! opt
       call assineS(buffer,"DFTFILE",OUTCAR,readvar(13))       ! opt
       call assineS(buffer,"POSFILE",POSCAR,readvar(14))       ! opt
       call assineS(buffer,"SIGMAOUT",SIGMAOUT,readvar(15))    ! opt
       call assineS(buffer,"BRILNAME",BRILNAME,readvar(16))    ! opt
       call assineS(buffer,"EGRADNAME",EGRADNAME,readvar(17))  ! opt
    end do

    call checkINPUT(readvar,size(readvar,1))
  end subroutine readINPUT
!############################################################################



!############################################################################
! assines a DP value to variable if the line contains the right keyword
!############################################################################
! KEYWORD = 5.2
  subroutine assineD(buffer,keyword,variable,found)
    integer::found
    character(1024)::buffer1,buffer2
    character(*)::buffer,keyword
    double precision::variable
    buffer1=buffer(:scan(buffer,"=")-1)
    if(scan("=",buffer).ne.0) buffer2=val(buffer)
    if(trim(adjustl(buffer1)).eq.trim(adjustl(keyword))&
         .and.trim(adjustl(buffer2)).ne.'') then
       found=found+1
       read(buffer2,*) variable
    end if
  end subroutine assineD
!############################################################################


!############################################################################
! assines an integer
!############################################################################
  subroutine assineI(buffer,keyword,variable,found)
    integer::found
    character(1024)::buffer1,buffer2
    character(*)::buffer,keyword
    integer::variable
    buffer1=buffer(:scan(buffer,"=")-1)
    if(scan("=",buffer).ne.0) buffer2=val(buffer)
    if(trim(adjustl(buffer1)).eq.trim(adjustl(keyword))&
         .and.trim(adjustl(buffer2)).ne.'') then
       found=found+1
       read(buffer2,*) variable
    end if
  end subroutine assineI
!############################################################################


!############################################################################
! assines an logical
!############################################################################
  subroutine assineL(buffer,keyword,variable,found)
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
  end subroutine assineL
!############################################################################


!############################################################################
! assines a string
!############################################################################
  subroutine assineS(buffer,keyword,variable,found)
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
  end subroutine assineS
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

module output
  use constants, only: real12, int12, TINY, fields
  use inputs, only: nx,ny,nz, time_step, zpos, grid, NA, Check_Steady_State, ntime, WriteToTxt
  use inputs, only: Test_Run, freq, RunName, FullRestart, Lx, Ly, Lz
  use constructions, only: heatblock
  use globe_data, only: TPD,TPPD, heat
  implicit none
  
contains
  subroutine plot(it)
    integer(int12), intent(in) :: it
    real(real12), dimension(nx,ny,nz) :: CT, TN
    integer :: iounit, iounit_power, iounit_tempdis, iounit_tempdistpd,logunit
    integer(int12) :: i, j, k, indexA
    character(len=1024) :: filename, file_prefix, file_extension, outdir, logname

    
    file_prefix = 'Temperture_'
    outdir='./outputs/'
    file_extension = '.out'
    
    if (it .eq. 1) then
       if(Test_run) then
          !---------------------------------------!
          ! open test output files                !
          open(unit=33,file='./outputs/Power.txt')!
          !---------------------------------------!
       else
          !---------------------------------------
          ! find most recent log file and open it
          open(unit=30,file='./outputs/Test.txt')
          call last_log(logname,outdir)
          open(newunit=logunit, file = logname )
          !---------------------------------------
       end if
    end if


    !---------------------------------------
    !  make a 3d array
    !---------------------------------------
    indexA=1
    do k = 1, nz
       do j = 1, ny
          do i = 1, nx
             TN(i,j,k) = TPPD(indexA)
             indexA = indexA+1
          end do
       end do
    end do
    !---------------------------------------


    !---------------------------------------
    ! write out to log file
    !---------------------------------------
    if (.not.Test_run) then
       if (WriteToTxt) write(logunit,*) real((it-1)*(time_step)),(TN(6,6,:))  !-293.0
    end if
    !---------------------------------------


    ! print*, 'Writing Temperature to file'
    !write(30,*) REAL(it)*time_step, ((T_matrix(i)-T_Bath),i=1,e)
    if (Check_Steady_State) then
       if (it.gt.1) then
          print*, 'Checking for steady state'
          open(unit=32,file='./Tests/outputs/Temperature_Steady.txt')
          read(32,*) CT
          close(32)
          if (any(abs(TN-CT).gt.TINY)) then
             print*, TN(1,1,1), CT(1,1,1)
             print*, 'Steady state not reached'
             stop
          else
             print*, 'Steady state reached, Test passed'
          end if
       end if
    end if


    !---------------------------------------
    ! final step print and closes
    !---------------------------------------
    if (it == ntime) then
       if (.not.Test_run) close(logunit)
       call final_print()
    end if
    !---------------------------------------

  
  ! call PlotdeltaT(it)

 end subroutine plot
!!!########################################################################


 
!!!########################################################################
!!!To plot difference in temperature between two time_steps
!!!########################################################################
 subroutine PlotdeltaT(it)
   real(real12) :: DT(NA)
   real(real12) :: TO(nx,ny,nz)
   integer(int12) :: i,j,k,it
   integer(int12) :: index

   DT=TPD-TPPD
   index=1
   do k = 1, nz
      do j = 1, ny
         do i = 1, nx
            TO(i,j,k) = DT(index)
            index = index+1
         end do
      end do
   end do
   if (it == 1) then
      open(unit=31,file='./outputs/DTemperature.txt')
   end if
   write(31,*) REAL(it)*time_step, TO(:,1,1)
 end subroutine PlotdeltaT

!!!########################################################################
!!!
!!!########################################################################
 subroutine last_log(logname,outdir)
   character(len=1024), intent(out) :: logname
   character(len=1024), intent(in) :: outdir
   logical :: flag
   integer(int12) :: i
   
   i = 0
   flag=.true.
   do while (flag)
      write(logname, '(A,F0.2,A,I2.2)') trim(adjustl(outdir)) // 'output_' // &
           trim(adjustl(RunName)) // '_freq_' , freq, '_', i
      inquire(file=logname, exist=flag)
      i = i+1
   end do
 end subroutine last_log
!!!########################################################################

!!!########################################################################
!!! perform the prints on final iteration
!!!########################################################################
 subroutine final_print()
   real(real12) :: TotalPower, totaltime, vol
   integer(int12) :: unit
   character(len=64) :: form
   
   TotalPower=sum(heat)
   totaltime=real(ntime)*time_step
   vol = real(Lx*Ly*Lz,real12)/real(nx*ny*nz,real12)

   write(*,*) ''
   
   write(*,'(A,EN12.3)') 'Total Power is ', TotalPower*vol
   write(*,'(A,EN12.3)') 'Average Power is ', (TotalPower*vol/ntime)
   write(*,'(A,EN12.3)') 'Total Energy is ', (TotalPower*vol/ntime)*totaltime

    open(newunit=unit,file='./outputs/TempDis.dat')
    write(form,'(A,I0,A)') '(',fields,'(ES16.8,1X))'
    write(unit,form) TPD(:)
    write(unit,*)
    write(unit,*)
    close(unit)
    
    open(newunit=unit,file='./outputs/TempDisTPD.dat')
    write(form,'(A,I0,A)') '(',fields,'(ES16.8,1X))'
    write(unit,form) TPPD(:)
    write(unit,*)
    write(unit,*)
    close(unit)
  end subroutine final_print
!!!########################################################################

end module output

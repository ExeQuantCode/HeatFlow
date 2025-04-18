!!!#################################################################################################
!!! Module outputs the results of the simulation.
!!! This module contains the subroutines:
!!! - data_write, Writes the temperature field to a file.
!!! - PlotdeltaT, Plots the difference in temperature between two time steps.
!!! - last_log, Finds the most recent log file.
!!! - final_print, Performs the prints on the final iteration.
!!! The module contains the variables:
!!! - file_prefix, The prefix of the output file.
!!! - outdir, The directory of the output file.
!!! - file_extension, The extension of the output file.
!!! - logname, The name of the log file.
!!! - CT, The temperature field at the previous time step.
!!! - Temp_cur, The temperature field at the current time step.
!!! - iounit, The unit number of the output file.
!!! - iounit_power, The unit number of the power output file.
!!! - iounit_tempdis, The unit number of the temperature distribution output file.
!!! - iounit_tempdistpd, The unit number of the temperature distribution at the previous ...
!!!   ... time step output file.
!!! - logunit, The unit number of the log file.
!!! - ix, The x index.
!!! - iy, The y index.
!!! - iz, The z index.
!!! - indexA, The index of the 3D array.
!!! - filename, The name of the output file.
!!! - form, The format of the output file.
!!! - TotalPower, The total power.
!!! - totaltime, The total time.
!!! - vol, The volume.
!!! - unit, The unit number of the output file.
!!! - DT, The difference in temperature between two time steps.
!!! - TO, The difference in temperature between two time steps.
!!! - itime, The current time step.
!!! - flag, The flag for the most recent log file.
!!! - i, The index of the most recent log file.
!!! - Temp_p, The temperature field at the current time step.
!!! - Temp_pp, The temperature field at the previous time step.
!!! - heat, The heat.
!!! - RunName, The name of the run.
!!! - TINY, The tiny value.
!!! - fields, The fields.
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module output
  use constants, only: real12, int12, TINY, fields
  use inputs, only: nx,ny,nz, time_step, grid, NA, Check_Steady_State, ntime, WriteToTxt
  use inputs, only: Test_Run, freq, RunName, FullRestart, IVERB, write_every
  use inputs, only: start_ix, end_ix, start_iy, end_iy, start_iz, end_iz
  use globe_data, only: Temp_p,Temp_pp, heat, heated_volume
  implicit none
  
contains
  subroutine data_write(itime)
    implicit none
    integer(int12), intent(in) :: itime
    real(real12), dimension(nx,ny,nz) :: CT, Temp_cur
    integer(int12) :: ix, iy, iz, indexA, logunit
    character(len=1024) :: file_prefix, file_extension, outdir, logname
    
    file_prefix = 'Temperture_'
    outdir='./outputs/'
    file_extension = '.out'
    
    !---------------------------------------
    !  make a 3d array
    !---------------------------------------
    indexA=1
    do iz = 1, nz
       do iy = 1, ny
          do ix = 1, nx
             Temp_cur(ix,iy,iz) = Temp_pp(indexA)
             indexA = indexA+1
          end do
       end do
    end do
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


    if (itime .eq. 1) then
    ! Needs logica testing does not make sense
       if(Test_run) then
          !---------------------------------------
          ! open test output files                
          !---------------------------------------
          open(unit=33,file='./outputs/Power.txt')
          open(unit=30, file='./outputs/Test.txt')
          !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       else
          !---------------------------------------
          ! find most recent log file and open it
          !---------------------------------------
          CALL last_log(logname,outdir)
          open(newunit=logunit,file=logname)
          write(logunit,*) real((itime-1)*(time_step)), &
               (Temp_cur(start_ix:end_ix, start_iy:end_iy, start_iz:end_iz))
          !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       end if
    end if





    !---------------------------------------
    ! write out to log file
    !---------------------------------------
    if (.not. Test_run) then
      if (WriteToTxt) then
         if (mod(itime, write_every) .eq. 0) then
            write(*, *) 'Writing Temperature difference to file'
            write(logunit,*) real((itime-1)*(time_step)), &
               (Temp_cur(start_ix:end_ix, start_iy:end_iy, start_iz:end_iz))
         end if
      endif
    end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !---------------------------------------
    ! Test steady state 
    !---------------------------------------
    ! write(*,*) 'Writing Temperature to file'
    !write(30,*) REAL(itime)*time_step, ((T_matrix(i)-T_Bath),i=1,e)
    if (Check_Steady_State) then
       if (itime.gt.1) then
          write(*,*) 'Checking for steady state'
          open(unit=32,file='./Tests/outputs/Temperature_Steady.txt')
          read(32,*) CT
          close(32)
          if (any(abs(Temp_cur-CT).gt.TINY)) then
             write(*,*) Temp_cur(1,1,1), CT(1,1,1)
             write(*,*) 'Steady state not reached'
             stop
          else
             write(*,*) 'Steady state reached, Test passed'
          end if
       end if
    end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    
    !---------------------------------------
    ! final step print and closes
    !---------------------------------------
    if (itime .eq. ntime) then
       if (.not.Test_run) close(logunit)
       CALL final_print()
    end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  
  ! call PlotdeltaT(itime)

 end subroutine data_write
!!!########################################################################


 
!!!########################################################################
!!!To plot difference in temperature between two time_steps
!!!########################################################################
 subroutine PlotdeltaT(itime)
   real(real12) :: DT(NA)
   real(real12) :: T0(nx,ny,nz)
   integer(int12) :: ix,iy,iz,itime
   integer(int12) :: index

   DT=Temp_p-Temp_pp
   index=1
   do iz = 1, nz
      do iy = 1, ny
         do ix = 1, nx
            T0(ix,iy,iz) = DT(index)
            index = index+1
         end do
      end do
   end do
   if (itime .eq. 1) then
      open(unit=31,file='./outputs/DTemperature.txt')
   end if
   

   write(31,*) REAL(itime)*time_step, T0(:,1,1)
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
      write(logname, '(A,A,I2.2)') trim(adjustl(outdir)) // 'output_' // &
           trim(adjustl(RunName)),'_',  i
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
   
   if (IVERB .gt. 3) then 
      TotalPower=heat
      totaltime=real(ntime)*time_step
      vol = heated_volume 

      write(*,*) ''
   
      write(*,'(A,EN12.3)') 'Total Power is ', TotalPower*vol
      write(*,'(A,EN12.3)') 'Average Power is ', (TotalPower*vol/ntime)
      write(*,'(A,EN12.3)') 'Total Energy is ', (TotalPower*vol/ntime)*totaltime
   
   end if 

    open(newunit=unit,file='./outputs/TempDis.dat')
    write(form,'(A,I0,A)') '(',fields,'(ES16.8,1X))'
    write(unit,form) Temp_p(:)
    write(unit,*)
    write(unit,*)
    close(unit)
    
    open(newunit=unit,file='./outputs/TempDisTPD.dat')
    write(form,'(A,I0,A)') '(',fields,'(ES16.8,1X))'
    write(unit,form) Temp_pp(:)
    write(unit,*)
    write(unit,*)
    close(unit)
  end subroutine final_print
!!!########################################################################

end module output

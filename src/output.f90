module output
  use constants, only: real12, int12, TINY
  use inputs, only: nx,ny,nz, time_step, zpos, grid, NA, Check_Steady_State, ntime, WriteToTxt
  use inputs, only: Test_Run, freq, RunName
  use constructions, only: heatblock
  use globe_data, only: TPD,TPPD, heat
  implicit none
  
contains
  subroutine plot(it)
     implicit none
     ! real(real12), dimension(nx, ny,nz) :: T
     integer :: new, newunit
     ! real(real12), dimension(e) :: T_matrix
     real(real12), dimension(nx) :: R
     real(real12) :: xlen
     integer(int12) :: indexA
     ! integer :: zpos = 508
     integer(int12), intent(in) :: it
     real(real12) :: CT(nx,ny,nz), TN(nx,ny,nz)
     character(len=1024) :: filename
     integer(int12) :: i,j,k,ix
     real(real12), dimension(ntime) :: TotalPower
     real(real12) :: totaltime
     logical :: flag
     character(len=1024) :: file_prefix
     character(len=1024) :: file_extension
     character(len=1024) :: file_name
     
     totaltime=real(ntime)*time_step
     file_prefix = 'Temperture_'
     xlen= 1.0*0.333

      if (it == 1) then
       if (Test_Run .neqv. .True.) open(unit=30,file='./outputs/Test.txt')
       if (Test_Run .eqv. .False.) then
           ! Create the file name using timestep, frequency, and tau variables
           write(file_prefix, '(A, A, A, F0.2)') 'output_', trim(RunName), '_freq_', freq
           file_extension = '.txt'
           file_name = trim(file_prefix) // trim(file_extension)
            ! Check if the file already exists
            inquire(file=file_name, exist=flag)
            do while (flag .eqv. .True.)
              ! If the file exists, increment the file number
              ix = index(file_name, '_')
              write(file_prefix, '(A, A, A, I0.2, A, F0.2)') 'output_', trim(RunName), '_', freq, '_', ix+1
              file_extension = '.txt'
              file_name = trim(file_prefix) // trim(file_extension)
              inquire(file=file_name, exist=flag)
            end do
            if (flag .eqv. .False.) then
              ! If the file does not exist, create it
              open(newunit, file='./outputs/' // file_name)
            end if
         end if
       open(unit=33,file='./outputs/Power.txt')

     end if

    
    write(33,*) REAL(it)*time_step, heat(799)
    TotalPower(it)=heat(799)

    indexA=1
    do k = 1, nz
      do j = 1, ny
         do i = 1, nx
            TN(i,j,k) = TPPD(indexA)
            indexA = indexA+1
         end do
      end do
    end do 
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
    if (WriteToTxt) write(newunit,*) real((it-1)*(time_step)),(TN(6,6,:))   !-293.0
    if (it == ntime) then
        close(30)
        print*, 'TH after ', real((it-1)*(time_step)), ' seconds is ', TN(6,6,6)
        print*, 'TM after ', real((it-1)*(time_step)), ' seconds is ', TN(6,6,9)
        print*, 'Average Power is ', (-1.0_real12*sum(TotalPower)/ntime)
        print*, 'Total Energy is ', (-1.0_real12*sum(TotalPower)*totaltime)

        open(unit=34,file='./outputs/TempDis.dat')
        write(34,*) TPD(:)
        close(34)
    end if

    ! call PlotdeltaT(it)
    205 format(5f12.6)

  end subroutine plot
  !!---------------------------------------------------------------------
  !To plot difference in temperature between two time_steps
  !!---------------------------------------------------------------------
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
end module output

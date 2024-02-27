module output
  use constants, only: real12, int12, TINY
  use inputs, only: nx,ny,nz, time_step, grid, NA, Check_Steady_State, ntime, WriteToTxt
  use inputs, only: Test_Run, freq, RunName, FullRestart, Lx, Ly, Lz
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
     real(real12) :: TotalPower, vol
     real(real12) :: totaltime
     logical :: flag
     character(len=1024) :: file_prefix
     character(len=1024) :: file_extension
     character(len=2028) :: file_name
     character(len=1024) :: folder_path
     character(len=1024) :: full_file_path
     totaltime=real(ntime)*time_step
     file_prefix = 'Temperture_'
     xlen= 1.0*0.333

      if (it == 1) then
       if (Test_Run .eqv. .True.) open(unit=30,file='./outputs/Test.txt')
       if (Test_Run .eqv. .False.) then
           ! Create the file name using timestep, frequency, and tau variables
           write(file_prefix, '(A, A, A, F0.2)') 'output_', trim(RunName), '_freq_', freq
           file_extension = '.txt'
           file_name = trim(file_prefix) // trim(file_extension)
            ! Check if the file already exists

            inquire(file=file_name, exist=flag)
            ix = 0

            ! Specify the folder path where the file is located
            folder_path = './outputs/'

            ! Create the full file path by concatenating the folder path and file name
            full_file_path = trim(folder_path) // trim(file_name)

            ! Check if the file exists in the different folder
            inquire(file=full_file_path, exist=flag)
            do while (flag .eqv. .True.)
              ! If the file exists, increment the file number
              ix = ix+1
              write(file_prefix, '(A, A, A, F0.2, A, I2)') 'output_', trim(RunName), '_', freq, '_', ix+1
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

    
    ! write(33,*) REAL(it)*time_step, heat(799)
    ! TotalPower=sum(heat)

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
    if (WriteToTxt) write(newunit,*) real((it-1)*(time_step)),(TN(17,17,:))   !-293.0
    if (it == ntime) then
        vol = real(Lx*Ly*Lz,real12)/real(nx*ny*nz,real12)
        ! print*, 'TH after ', real((it-1)*(time_step)), ' seconds is ', TN(6,6,6)
        ! print*, 'TM after ', real((it-1)*(time_step)), ' seconds is ', TN(6,6,9)
        ! print*, 'Total Power is ', TotalPower*vol
        ! print*, 'Average Power is ', (TotalPower*vol/ntime)
        ! print*, 'Total Energy is ', (TotalPower*vol/ntime)*totaltime

        ! open(unit=34,file='./outputs/TempDis.dat')
        ! write(34,*) TPD(:)
        ! close(34)
          open(unit=35,file='./outputs/TempDisTPD.dat')
          write(35,*) TPD(:)
          close(35)
          open(unit=36,file='./outputs/TempDisTPPD.dat')
          write(36,*) TPPD(:)
          close(36)
    end if


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

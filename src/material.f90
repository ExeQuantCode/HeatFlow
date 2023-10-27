module materials

  use constants, only: real12, int12
  use globe_data, only: T
  use inputs, only: input_materials
  implicit none

contains

subroutine material(imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
  
   integer(int12), intent(in) ::imaterial_type
   integer(int12) :: i, tmp
   real(real12) :: volume !!!check intent
   !TC = Temperature of current cell
   real(real12), intent(inout) :: kappa3D, kappa, h_conv, heat_capacity, sound_speed, rho, tau,  TC 
   logical :: found

!!!=============================================
!!! notes ::
!!! 140 - silicon
!!! 3001 - air (convection - h 50)
!!! 3002 - air (convection - h 100)
!!! 3003 - air (convection - h 200)
!!! 3004 - air (convection - h 0)
!!! 
!!! 9001 - Muscle  !values provided by A. Ghita
!!! 9002 - epidermis !values provided by A. Ghita
!!! 9003 - fat !values provided by A. Ghita
!!!=============================================

   ! Error for unsutable imaterial_type
   if (imaterial_type .le. 0) then
      write(6,*) 'Error: imaterial_type not recognized:', imaterial_type
      write(6,*) '       imaterial_type cannot be zero or negative'
      call exit
   end if

      
   !-------------------------------------------------
   ! an if to test if material 
   !-------------------------------------------------
   mat: if (imaterial_type .le. 99) then
      found = .false.
      mat_loop: do i=1, size(input_materials%index)
         tmp=input_materials(i)%index
         if (tmp .eq. imaterial_type) then
            found = .true.
            heat_capacity = input_materials(i)%heat_capacity
            h_conv        = input_materials(i)%h_conv
            kappa         = input_materials(i)%kappa
            kappa3D       = input_materials(i)%kappa3D
            rho           = input_materials(i)%rho
            sound_speed   = input_materials(i)%sound_speed
            tau           = input_materials(i)%tau
            exit mat_loop
         end if
      end do mat_loop
      ! Error for unfound imaterial_type
      if (.not. found) then
         write(6,*) 'Error: imaterial_type not recognized:', imaterial_type
         call exit
      end if
   end if mat
   !-------------------------------------------------

      
   !-------------------------------------------------
   ! if to test if material is a default
   !-------------------------------------------------
   if (imaterial_type .gt. 99) then
      select case(imaterial_type)
!!!air
      case(100)
         heat_capacity=1.0!0.718 !730.0
         h_conv=21.4 !engineering toolbox, v~1[m/s]
         !      heat_diffusion=1.0 !9.909090909e-07
         kappa=1.0!0.02587 ! engineering toolbox at 20c
         kappa3D=0.02587
         rho=1.0!1.225
         tau= 0.0
         !!1-1000 are reserved for solids.
         !!1-120 are elemental solids
         !!1001-2000 are for liquids,
         !!2001-3000 are for gases
         !!3001+ are for miscelanneous

!!!Solids
!!! 140 - silicon
!!! Gases
!!! 3001 - air
!!!Sample case is Silicon (ai bees)
      case(140)
         heat_capacity=4200.0!! all sources
         h_conv=0 !moving bee at 5m/s
         !      heat_diffusion=1.0 !9.909090909e-07
         kappa=130.0
         kappa3D=130.0 !from www.ioffe.ru thermal properties of silicon
         rho=2328.0 ! from all sources
         sound_speed=8433.0 !...


         !plastic  
      case(3001)
         heat_capacity=840.0
         h_conv=0.0 !!ranges between 10 and 100
         kappa=0.19
         kappa3D=0.19
         rho=941.0
         sound_speed=1000.0

         !soil 1 
      case(3002)
         heat_capacity=1100.0 !this is 40% wet soil from engineering toolbox
         h_conv=0.0 !!ranges between 10 and 100
         kappa=1.42
         kappa3D=1.42
         rho=1700.0
         sound_speed=1000.0

         !soil 2 
      case(3003)
         heat_capacity=1400.0
         h_conv=0 !!ranges between 10 and 100
         kappa=1.59
         kappa3D=1.59
         rho=1800.0
         sound_speed=1000.0

         !clay 
      case(3004)
         heat_capacity=2300.0
         h_conv=0 !!ranges between 10 and 100
         kappa=1.25
         kappa3D=1.25
         rho=2300.0
         sound_speed=1000.0 !approx.

         !muscle
      case(9001) 
         heat_capacity=3421.0
         h_conv=0.0 !!ranges between 10 and 100
         kappa=0.6
         kappa3D=0.0
         rho=1090.0
         sound_speed=3.0


         !epidermis
      case(9002)
         heat_capacity=3391.0
         h_conv=0.0 !!ranges between 10 and 100
         kappa=0.209
         kappa3D=0.0
         rho=1109.0
         sound_speed=3.0


         !fat
      case(9003)
         heat_capacity=2348.0
         h_conv=0.0 
         kappa=0.317
         kappa3D=0.0
         rho=911.0
         sound_speed=3.0 !380.0

         !Si
      case(310)
         heat_capacity= 700.0
         h_conv = 0.0
         kappa = 130.0
         kappa3D = 130.0
         rho = 2329.0
         sound_speed = 8433.0

         !SiO2
      case(320)
         heat_capacity= 720.0
         h_conv = 0.0
         kappa  = 1.4
         kappa3D = 1.4
         rho = 2400.0
         sound_speed=5970.0
         tau = 1.0

         !Silver
      case(330)
         heat_capacity = 236.0
         h_conv = 0.0
         kappa = 427.0
         kappa3D = 429.0
         rho = 10524.0
         sound_speed = 3650.0

         !SU8 
      case(340)
         heat_capacity= 1500.0 !https://doi.org/10.1016/j.ijheatmasstransfer.2021.122346
         h_conv = 0.0
         kappa  = 0.2 !kayaku am data sheet for SU8 3000 series
         kappa3D = 0.2
         rho = 1153.0 !kayaku am data sheet for SU8 3000 series
         sound_speed= 8433.0 !UNKNOWN, TAKEN FROM Si
         tau = 1.0

         !ITO
      case(350)
         heat_capacity= 361.0
         h_conv = 0.0
         kappa  = 10.2
         kappa3D = 10.2
         rho = 7140.0
         sound_speed= 3650.0 !UNKNOWN, TAKEN FROM Ag
         tau = 1.0
      case default
         write(6,*) 'Error: imaterial_type not recognized:', imaterial_type
         call exit
      end select
   end if

 end subroutine material

end module materials

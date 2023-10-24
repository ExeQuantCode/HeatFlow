!##############################################################################################################
! A library of different materials cases, selected by giving desired material case value in GridMaterial.txt
!##############################################################################################################
module materials

 use constants
  use constructions
  use heating
  implicit none

contains

subroutine material(imaterial_type,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
  
   integer(int12) ::imaterial_type
   integer(int12) :: i3D
    ! real(real12), dimension(e) :: T
   !!   real(real12) :: Temperature
   real(real12), dimension(3,3) :: kappa3D
   real(real12) :: kappa
!convective current!
   real(real12) :: h_conv ! 
   real(real12) :: heat_capacity 
   real(real12) :: sound_speed
   real(real12) :: rho
   real(real12) :: volume
   real(real12) :: tau
!14 - silicon
!3001 - air (convection - h 50)
!3002 - air (convection - h 100)
!3003 - air (convection - h 200)
!3004 - air (convection - h 0)

!9001 - Muscle  !values provided by A. Ghita
!9002 - epidermis !values provided by A. Ghita
!9003 - fat !values provided by A. Ghita



select case(imaterial_type)

!air
   case(0)
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


!!Solids
! 14 - silicon
!! Gases
! 3001 - air
      !!Sample case is Silicon (ai bees)
   case(14)
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
   case(31)
      heat_capacity= 700.0
      h_conv = 0.0
      kappa = 130.0
      kappa3D = 130.0
      rho = 2329.0
      sound_speed = 8433.0

      !SiO2
   case(32)
      heat_capacity= 720.0
      h_conv = 0.0
      kappa  = 1.4
      kappa3D = 1.4
      rho = 2400.0
      sound_speed=5970.0
      tau = 1.0

      !Silver
   case(33)
      heat_capacity = 236.0
      h_conv = 0.0
      kappa = 427.0
      kappa3D = 429.0
      rho = 10524.0
      sound_speed = 3650.0

   !SU8 
   case(34)
      heat_capacity= 1500.0 !https://doi.org/10.1016/j.ijheatmasstransfer.2021.122346
      h_conv = 0.0
      kappa  = 0.2 !kayaku am data sheet for SU8 3000 series
      kappa3D = 0.2
      rho = 1153.0 !kayaku am data sheet for SU8 3000 series
      sound_speed= 8433.0 !UNKNOWN, TAKEN FROM Si
      tau = 1.0

   !ITO
   case(35)
      heat_capacity= 361.0
      h_conv = 0.0
      kappa  = 10.2
      kappa3D = 10.2
      rho = 7140.0
      sound_speed= 3650.0 !UNKNOWN, TAKEN FROM Ag
      tau = 1.0

   end select

 end subroutine material

end module materials

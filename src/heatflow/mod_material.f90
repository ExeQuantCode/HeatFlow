!!!#################################################################################################
!!!  Module for material properties.
!!! This module contains the subroutines:
!!! - material, which takes the material type and returns the material properties.
!!! This module contains the variables:
!!! - imaterial_type, the material type.
!!! - TC, the temperature of the material.
!!! - kappa, the thermal conductivity of the material.
!!! - heat_capacity, the heat capacity of the material.
!!! - rho, the density of the material.
!!! - tau, the relaxation time of the material.
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module materials

  use constants, only: real12, int12
  use inputs, only: input_materials, grid
  implicit none

contains
!!!#########################################################################
!!!  Subroutine to get the material properties
!!!  This subroutine takes the material type and returns the material properties.
!!!  The material properties are:
!!!  - kappa, the thermal conductivity of the material.
!!!  - heat_capacity, the heat capacity of the material.
!!!  - rho, the density of the material.
!!!  - tau, the relaxation time of the material.
!!!  - em, the emissivity of the material.
!!!  The material type is an integer that corresponds to a material in the input_materials array.
!!!  If the material type is less than 100, the material properties are taken from the ...
!!!   ...input_materials array.
!!!  If the material type is greater than 100, the material properties are taken from a default case.
!!!  If the material type is not found, an error message is printed and the program exits.
!!!  The material properties are returned as arguments.
!!!  Arguments:
!!!  - imaterial_type, the material type.
!!!  - kappa, the thermal conductivity of the material.
!!!  - heat_capacity, the heat capacity of the material.
!!!  - rho, the density of the material.
!!!  - tau, the relaxation time of the material.
!!!  - em, the emissivity of the material.
!!!#########################################################################

subroutine material(imaterial_type,kappa,heat_capacity,rho,tau, em)
  
   integer(int12), intent(in) ::imaterial_type
   integer(int12) :: i, tmp
   real(real12), intent(inout) :: kappa, heat_capacity, rho, tau
   real(real12), intent(inout) :: em
   logical :: found

!!!=============================================
!!! notes ::
!!! 140 - silicon
!!! 
!!! 9001 - Muscle  !values provided by A. Ghita
!!! 9002 - epidermis !values provided by A. Ghita
!!! 9003 - fat !values provided by A. Ghita
!!!=============================================
   !print*, grid%imaterial_type
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
            kappa         = input_materials(i)%kappa
            rho           = input_materials(i)%rho
            tau           = input_materials(i)%tau
            em   = input_materials(i)%em

            exit mat_loop
         end if
      end do mat_loop
      ! Error for unfound imaterial_type
      if (.not. found) then
         write(6,*) 'Error: imaterial_type not recognized:', imaterial_type
         call exit
      end if
   end if mat
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-

      
   !-------------------------------------------------
   ! if to test if material is a default
   !-------------------------------------------------
   if (imaterial_type .gt. 99) then
      select case(imaterial_type)
!!!air
      case(100)
         heat_capacity=1.0!0.718 !730.0
         !      heat_diffusion=1.0 !9.909090909e-07
         kappa=1.0!0.02587 ! engineering toolbox at 20c
         rho=1.0!1.225
         tau= 0.0
         !!1-1000 are reserved for solids.
         !!1-120 are elemental solids
         !!1001-2000 are for liquids,
         !!2001-3000 are for gases
         !!3001+ are for miscelanneous
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

!!!Solids
!!! 140 - silicon
!!! Gases
!!! 3001 - air
!!!Sample case is Silicon (ai bees)
      case(140)
         heat_capacity=4200.0!! all sources
         !      heat_diffusion=1.0 !9.909090909e-07
         kappa=130.0
         rho=2328.0 ! from all sources


         !plastic  
      case(3001)
         heat_capacity=840.0
         kappa=0.19
         rho=941.0

         !soil 1 
      case(3002)
         heat_capacity=1100.0 !this is 40% wet soil from engineering toolbox
         kappa=1.42
         rho=1700.0

         !soil 2 
      case(3003)
         heat_capacity=1400.0
         kappa=1.59
         rho=1800.0

         !clay 
      case(3004)
         heat_capacity=2300.0
         kappa=1.25
         rho=2300.0

         !muscle
      case(9001) 
         heat_capacity=3421.0
         kappa=0.6
         rho=1090.0


         !epidermis
      case(9002)
         heat_capacity=3391.0
         kappa=0.209
         rho=1109.0


         !fat
      case(9003)
         heat_capacity=2348.0
         kappa=0.317
         rho=911.0

         !Si
      case(310)
         heat_capacity= 700.0
         kappa = 130.0
         rho = 2329.0

         !SiO2
      case(320)
         heat_capacity= 720.0
         kappa  = 1.4
         rho = 2400.0
         tau = 1.0

         !Silver
      case(330)
         heat_capacity = 236.0
         kappa = 427.0
         rho = 10524.0

         !SU8 
      case(340)
         heat_capacity= 1500.0 !https://doi.org/10.1016/j.ijheatmasstransfer.2021.122346
         kappa  = 0.2 !kayaku am data sheet for SU8 3000 series
         rho = 1153.0 !kayaku am data sheet for SU8 3000 series
         tau = 1.0

         !ITO
      case(350)
         heat_capacity= 361.0
         kappa  = 10.2
         rho = 7140.0
         tau = 1.0
      case default
         write(6,*) 'Error: imaterial_type not recognized:', imaterial_type
         call exit
      end select
   end if

 end subroutine material
 !!!#########################################################################

end module materials


module setup
  use constants, only: real12, int12, TINY
  use inputs, only: Lx, Ly, Lz, nx, ny, nz, NA, grid, T_Bath
  use constructions, only: heatblock
  use hmatrixmod, only: hmatrix
  use globe_data, only:  ra, TN, TPD, TPPD
  use sparse, only: SRSin
  use materials, only: material
  implicit none
  
   contains
    

!!!##########################################################################
!!! This allocates arrays and builds the apropreate H matrix
!!!##########################################################################
   subroutine set_global_variables()
      integer(int12) :: i,j,k
      real(real12) :: TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau
      allocate(TN(nx, ny, nz))
      allocate(TPD(NA))
      allocate(TPPD(NA))           

      !---------------------------------------------------
      ! ASign material properties to the grid construction
      ! can be expanded to include more properties at a 
      ! later date
      !---------------------------------------------------
      do k = 1, nz
         do j = 1, ny
            do i = 1, nx
            call material(grid(i,j,k)%imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
            grid(i,j,k)%kappa = kappa
            grid(i,j,k)%rho = rho
            grid(i,j,k)%heat_capacity = heat_capacity
            grid(i,j,k)%tau = tau


            end do               
         end do
      end do


      !---------------------------------------------------
      TPPD = 0
      TPD = T_Bath
      !** should impliment an if condition for sparse only
      ! if(sparse_only) then
         !** Never have to make the full H matrix, needs boundarys
         !call SPHM()
         !print*,Hsparse
      ! else
         call build_Hmatrix()
      ! end if

      !call setup_grid()

   end subroutine set_global_variables
!!!##########################################################################


!!!#########################################################################
!!! This sets up the H Matrix and converts it into sparse row storage
!!!#########################################################################
   subroutine build_Hmatrix()

      
      call sparse_Hmatrix()
    
   end subroutine build_Hmatrix
!!!#########################################################################


!!!#########################################################################
!!! This sets up the H Matrix directly in sparse row storage
!!!#########################################################################
   subroutine sparse_Hmatrix()
     real(real12) :: H0
     integer(int12) :: i, j, len
      integer(int12), allocatable :: rar(:), rac(:)
      real(real12), allocatable :: rav(:)
      len = 0 
     do j = 1, NA
        do i = 1, NA
           call hmatrix(i,j, H0)
           if (abs(H0).gt. TINY) len=len+1
        end do
     end do
   !   print*, len
   !   print*, (7*nx*ny*nz)-2*(((ny*nx)+(nx*nz)+(ny*nz)))
     allocate(rar(len))
     allocate(rac(len))
     allocate(rav(len))
     allocate(ra%val(len),ra%irow(len),ra%jcol(len))

     len = 0
      do i = 1, NA
         do j = 1, NA
            call hmatrix(i,j,H0)
            if (abs(H0) .gt. TINY) then
               len = len+1
               rar(len) = j
               rac(len) = i
               rav(len) = H0
            end if 
         end do
      end do 
      ra%n = NA
      ra%len = len
      ra%val = rav
      ra%irow = rar
      ra%jcol = rac   
   end subroutine sparse_Hmatrix
!!!#########################################################################



  
  
end module setup

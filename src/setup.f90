
module setup
  use constants, only: real12, int12, TINY
  use inputs, only: Lx, Ly, Lz, nx, ny, nz, NA, grid, T_Bath
  use constructions, only: heatblock
  use hmatrixmod, only: hmatrixfunc
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
      real(real12) :: TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau, Lx, Ly, Lz
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
            call material(grid(i,j,k)%imaterial_type,TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau, Lx, Ly, Lz)
            grid(i,j,k)%kappa = kappa
            grid(i,j,k)%rho = rho
            grid(i,j,k)%heat_capacity = heat_capacity
            grid(i,j,k)%tau = tau
            grid(i,j,k)%Length(1) = Lx
            grid(i,j,k)%Length(2) = Ly
            grid(i,j,k)%Length(3) = Lz

            end do               
         end do
      end do


      !---------------------------------------------------
      TPPD = 0
      TPD = T_Bath

      call sparse_Hmatrix()


   end subroutine set_global_variables
!!!##########################################################################

!!!#########################################################################
!!! This sets up the H Matrix directly in sparse row storage
!!!#########################################################################
      subroutine sparse_Hmatrix()
         ! use omp_lib
        real(real12) :: H0
        integer(int12) :: i, j, len
         integer(int12), dimension(7*NA-2*(nx*ny+ny*nz+nx*nz)) :: rar, rac !7*NA is as big as it could be
         real(real12), dimension(7*NA-2*(nx*ny+ny*nz+nx*nz)) :: rav

        len = 0
        !---------------------------------------------------
        ! because len is updated in loop can not parallelize
        ! with ease. Could use atomize but maybe more ...
        ! ... computationally expensive.
        !---------------------------------------------------
      !   call omp_set_num_threads(4)
!   ! $OMP PARALLEL DO PRIVATE(i,j,H0) SHARED(len,rar,rac,rav)
         do i = 1, NA
            h0 = hmatrixfunc(i,i)
            len = len+1
            rar(len) = i
            rac(len) = i
            rav(len) = H0
            do j = i+1, NA,1
               h0 = hmatrixfunc(i,j)
               if (abs(H0) .gt. TINY) then
                  len = len+2
                  rar(len-1) = i
                  rac(len-1) = j
                  rav(len-1) = H0
                  rar(len) = j
                  rac(len) = i
                  rav(len) = H0
               end if 
            end do
         end do 
!   !$OMP END PARALLEL DO
         allocate(ra%val(len),ra%irow(len),ra%jcol(len))
         ra%n = NA
         ra%len = len
         ra%val = rav(:len)
         ra%irow = rar(:len)
         ra%jcol = rac(:len)
         
      end subroutine sparse_Hmatrix
!!!#########################################################################



  
  
end module setup

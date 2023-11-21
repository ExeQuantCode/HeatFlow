
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
      TPPD = T_Bath
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
      real(real12), dimension(na,na) :: H
      real(real12) :: H0
      integer(int12) :: i,j 
      
      ! do j=1,na
      !    do i =1,na
      !       call hmatrix(i,j,H0)
      !       h(i,j) = H0
      !    end do
      ! end do
      !write(*,'(27F12.3)') H
      call sparse_Hmatrix()
      !call SRSin(H, TINY, ra)
   end subroutine build_Hmatrix
!!!#########################################################################


!!!#########################################################################
!!! This sets up the H Matrix directly in sparse row storage
!!!#########################################################################
      subroutine sparse_Hmatrix()
         ! use omp_lib
         real(real12) :: H0
         integer(int12) :: i, j, len, count, k
         integer, dimension(3) :: addit
         len = 7*nx*ny*nz - 2*(nx*ny + ny*nz + nz*nx)
         ra%n = NA
         ra%len = len
         allocate(ra%val(len), ra%irow(len), ra%jcol(len))
         addit(1) = 1
         addit(2) = nx
         addit(3) = nx*ny


         count = 0
         parent_loop: do j = 1, NA
            i=j
            count = count + 1
            H0 = hmatrixfunc(i,j)
            ra%val(count) = H0
            ra%irow(count) = i
            ra%jcol(count) = j
            neighbour_loop: do k = 1, size(addit,1)
                i = j + addit(k)
                if ((i.gt.NA)) cycle parent_loop
                H0=hmatrixfunc(i,j)
                if (abs(H0).lt.TINY) cycle neighbour_loop
                count = count + 1
                ra%val(count) = H0
                ra%irow(count) = i
                ra%jcol(count) = j
                count = count + 1
                ra%val(count) = H0
                ra%irow(count) = j
                ra%jcol(count) = i
            end do neighbour_loop
        end do parent_loop

         
      end subroutine sparse_Hmatrix
!!!#########################################################################



  
  
end module setup

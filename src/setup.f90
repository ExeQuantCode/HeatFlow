!!!#################################################################################################
!!! Module to set up the global variables and the H matrix.
!!! This module contains the subroutines:
!!! set_global_variables, This allocates arrays and builds the apropreate H matrix
!!! sparse_Hmatrix, This sets up the H Matrix directly in sparse row storage
!!! stability, Check if the simulation will be stable. NOT FULLY IMPLEMENTED
!!! build_Hmatrix, This sets up the H Matrix and converts it into sparse row storage
!!! SparseToReal, This sets up the H Matrix and converts it into sparse row storage
!!! This module contains the variables:
!!! Temp_cur, The current temperature field
!!! Temp_p, The previous temperature field
!!! Temp_pp, The previous previous temperature field
!!! inverse_time, The inverse of the time step
!!! heat, The heat source
!!! Author: Harry Mclean, Frank Davis, Steven Hepplestone
!!!#################################################################################################
module setup
  use constants, only: real12, int12, TINY
  use inputs, only: nx, ny, nz, NA, grid, time_step, kappaBoundx, kappaBoundy, kappaBoundz & !
                     ,Check_Sparse_Full, Check_Stability, ntime,IVERB ! 
  use hmatrixmod, only: hmatrixfunc
  use globe_data, only:  ra, Temp_cur, Temp_p, Temp_pp,inverse_time, heat, lin_rhoc
  use sparse, only: SRSin
  use materials, only: material
  implicit none
  
   contains
    

!!!#################################################################################################
!!! This allocates arrays and builds the apropreate H matrix
!!!#################################################################################################
   subroutine set_global_variables()
      integer(int12) :: ix,iy,iz,index
      real(real12) :: TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau
      allocate(Temp_cur(nx, ny, nz))
      allocate(Temp_p(NA))
      allocate(Temp_pp(NA))
      allocate(lin_rhoc(NA))
      heat = 0.0_real12
      inverse_time = 1.0_real12/time_step
      !---------------------------------------------------
      ! ASign material properties to the grid construction
      ! can be expanded to include more properties at a 
      ! later date
      !---------------------------------------------------
      index = 0
      do iz = 1, nz
         do iy = 1, ny
            do ix = 1, nx
            index = index + 1
            call material(grid(ix,iy,iz)%imaterial_type,&
             TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
            grid(ix,iy,iz)%kappa = kappa
            grid(ix,iy,iz)%rho = rho
            grid(ix,iy,iz)%heat_capacity = heat_capacity
            grid(ix,iy,iz)%tau = tau*inverse_time*inverse_time
            lin_rhoc(index) = rho*heat_capacity
            if (Check_Stability) CALL stability(kappa, rho, heat_capacity, ix, iy, iz)
            end do               
         end do
      end do


      if (Check_Sparse_Full) then
         call build_Hmatrix()
      else
         call sparse_Hmatrix()
      end if



   end subroutine set_global_variables
!!!#################################################################################################

!!!#################################################################################################
!!! This sets up the H Matrix directly in sparse row storage
!!!#################################################################################################
   subroutine sparse_Hmatrix()
      ! use omp_lib
      real(real12) :: H0 ! Holds the value of the H matrix
      integer(int12) :: i, j, len, count, k ! i and j are the row and column of the H matrix
      ! Holds the values to add to the row to get the column
      integer(int12), allocatable, dimension(:) :: addit 
      ! The number of non-zero elements in the H matrix to look for
      len = 7*nx*ny*nz - 2*(nx*ny + ny*nz + nz*nx) 
      ra%n = NA ! The number of rows in the H matrix
      ra%len = len ! The number of non-zero elements in the H matrix
      ! Allocate the arrays to hold the H matrix in sparse row storage
      allocate(ra%val(len), ra%irow(len), ra%jcol(len)) 

      addit = [1] ! The values to add to the row to get the column
      if (nx .gt. 1) addit = [addit, nx] ! Add the values to add to the row to get the column
      if (ny .gt. 1) addit = [addit, nx*ny]  ! Add the values to add to the row to get the column



      
      count = 0 ! The number of non-zero elements in the H matrix
      parent_loop: do j = 1, NA ! Loop over the columns of the H matrix
         i=j ! The row of the H matrix
         count = count + 1 ! The number of non-zero elements in the H matrix
         H0 = hmatrixfunc(i,j) ! The value of the H matrix
         ra%val(count) = H0 ! The value of the H matrix
         ra%irow(count) = i ! The row of the H matrix
         ra%jcol(count) = j ! The column of the H matrix
         ! Loop over the values to add to the row to get the column
         neighbour_loop: do k = 1, size(addit,1) 
             i = j + addit(k) ! The row of the H matrix
             ! If the row is greater than the number of rows ...
             !...in the H matrix then go to the next column
             if ((i.gt.NA)) cycle parent_loop 
             H0=hmatrixfunc(i,j) ! The value of the H matrix
             ! If the value of the H matrix is less than TINY then go to the next value ...
             !...to add to the row to get the column
             if (abs(H0).lt.TINY) cycle neighbour_loop 
             count = count + 1 ! The number of non-zero elements in the H matrix
             ra%val(count) = H0 ! The value of the H matrix
             ra%irow(count) = i ! The row of the H matrix
             ra%jcol(count) = j ! The column of the H matrix
             count = count + 1 ! The number of non-zero elements in the H matrix
             ra%val(count) = H0 ! The value of the H matrix
             ra%irow(count) = j ! The row of the H matrix
             ra%jcol(count) = i ! The column of the H matrix
         end do neighbour_loop
     end do parent_loop

      
   end subroutine sparse_Hmatrix
!!!#################################################################################################

!!!#################################################################################################
!!! Check if the simulation will be stable. NOT FULLY IMPLEMENTED   
!!!#################################################################################################
   subroutine stability(kappa, rho, heat_capacity, ix, iy, iz)
      implicit none
      integer(int12) :: ix,iy,iz
      real(real12) :: kappa, rho, heat_capacity, var_stability
      real(real12) :: alpha

      !---------------------------------------------------
      ! Check stability condition
      !---------------------------------------------------
      
      alpha = kappa/(rho*heat_capacity)
      var_stability =( time_step * alpha * &
      (1 / (grid(ix,iy,iz)%length(1)**2) + 1 / ( grid(ix,iy,iz)%length(2) ** 2 ) &
           + 1 / (grid(ix,iy,iz)%length(3) ** 2 ) ) )
      if (IVERB.ge.2) write(*,*) "Stability condition = ", var_stability
      if (var_stability .gt. 1.0/12.0) then
         write(*,*) "Stability condition not met"
         write(*,*) "Stability condition = ", var_stability

         write(*,*) "time_step = ", time_step
         write(*,*) "alpha = ", alpha
         write(*,*) "dx = ", grid(ix,iy,iz)%length(1)
         write(*,*) "dy = ", grid(ix,iy,iz)%length(2)
         write(*,*) "dz = ", grid(ix,iy,iz)%length(3)
      
         stop
      end if
      if ((ix .eq. 1) .or.(iy .eq. 1) .or. (iz .eq. 1)) then
         alpha = kappaBoundx / ( rho * heat_capacity)
         var_stability =( time_step * alpha * &
              (1 / (grid(ix,iy,iz)%length(1)**2) + 1 / ( grid(ix,iy,iz)%length(2) ** 2 ) &
              + 1 / (grid(ix,iy,iz)%length(3) ** 2 ) ) )
         if (var_stability .gt. 1.0/12.0) then
            write(*,*) "Stability condition at boundary not met = ", var_stability
            write(*,*) " Boundary kappas = ", kappaBoundx, kappaBoundy, kappaBoundz
            stop
         end if
      end if
   end subroutine stability
!!!#################################################################################################

!!!#################################################################################################
!!! This sets up the H Matrix and converts it into sparse row storage
!!!#################################################################################################
   subroutine build_Hmatrix()
      integer(int12) ::  HCheck
      integer(int12) :: i,j, BCount
      real(real12) :: H(NA,NA),HT(NA,NA), H0
      H=0.0_real12
      BCount = 0
      HCheck = 0
      do j=1,na
         HCheck = 0
         do i =1,na
            H0 = hmatrixfunc(i,j)
            HCheck = HCheck + H0
            H(i,j) = H0
         end do

      end do
      ! write(*,'(3F12.3)') H
      call SRSin(H, TINY, ra)
      call SparseToReal(HT)
      if (all(abs(H-HT) < TINY)) then
         write(*,*) "H and HT are the same"
      else
         write(*,*) "H and HT are not the same"
      end if
   end subroutine build_Hmatrix
!!!#################################################################################################

!!!#################################################################################################
!!! This sets up the H Matrix and converts it into sparse row storage
!!!#################################################################################################
subroutine SparseToReal(HT)
   real(real12) :: H0
   real(real12), dimension(NA,NA) :: HT
   integer(int12) :: i, j, len, k
   integer, dimension(3) :: addit

   addit(1) = 1
   addit(2) = nx
   addit(3) = nx*ny
   
   HT = 0.0_real12
   parent_loop: do j = 1, NA
      i=j
      H0 = hmatrixfunc(i,j)
      HT(i,j) = H0
      neighbour_loop: do k = 1, size(addit,1)
          i = j + addit(k)
          if ((i.gt.NA)) cycle parent_loop
          H0=hmatrixfunc(i,j)
          if (abs(H0).lt.TINY) cycle neighbour_loop
          HT(i,j)=H0
          H0=hmatrixfunc(j,i)
          HT(j,i)=H0

      end do neighbour_loop
  end do parent_loop
   write(*, '(3F15.4)') HT
end subroutine SparseToReal
!!!#################################################################################################


end module setup

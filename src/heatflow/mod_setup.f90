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
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################
module setup
  use constants, only: real12, int12, TINY
  use inputs, only: nx, ny, nz, NA, grid, time_step, kappaBoundx1, kappaBoundy1, kappaBoundz1 
  use inputs, only: Check_Sparse_Full, Check_Stability, ntime,IVERB, Periodicx, Periodicy
  use inputs, only: Periodicz ! 
  use hmatrixmod, only: hmatrixfunc
  use globe_data, only:  ra, Temp_cur, Temp_p, Temp_pp,inverse_time, heat, lin_rhoc, Q_P
  use globe_data, only: acsr, ja, ia
  use solver, only: SRSin
  use materials, only: material
  implicit none

  public :: set_global_variables
  
   contains
    

!!!#################################################################################################
!!! This allocates arrays and builds the apropreate H matrix
!!!#################################################################################################
   subroutine set_global_variables()
      integer(int12) :: ix,iy,iz,index
      real(real12) :: kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau, em
      real(real12), dimension(3) :: vel

      allocate(Temp_cur(nx, ny, nz))
      allocate(Temp_p(NA))
      allocate(Temp_pp(NA))
      allocate(lin_rhoc(NA))
      allocate(Q_P(NA))
      Q_P(:) = 0.0_real12
      heat = 0.0_real12
      inverse_time = 1.0_real12/time_step
      !---------------------------------------------------
      ! A Sign material properties to the grid construction
      ! can be expanded to include more properties at a 
      ! later date
      !---------------------------------------------------
      write(*,*) "Setting up material properties"
      write(*,'(A,I10,A)') " Processing ", NA, " grid cells..."
      index = 0
      do iz = 1, nz
         ! Progress reporting every 10% for large grids
         if (mod(iz-1, max(1,nz/10)) == 0 .and. iz > 1) then
            write(*,'(A,I3,A)') "   Progress: ", int(100.0*real(iz)/real(nz)), "%"
         end if
         do iy = 1, ny
            do ix = 1, nx
            index = index + 1
            CALL material(grid(ix,iy,iz)%imaterial_type,&
                 kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau, em, vel)
            grid(ix,iy,iz)%kappa = kappa
            grid(ix,iy,iz)%rho = rho
            grid(ix,iy,iz)%heat_capacity = heat_capacity
            grid(ix,iy,iz)%tau = tau*inverse_time*inverse_time
            grid(ix,iy,iz)%em = em
            grid(ix,iy,iz)%vel(:) = vel(:)
            lin_rhoc(index) = rho*heat_capacity
            if (Check_Stability) CALL stability(kappa, rho, heat_capacity, ix, iy, iz)
            end do               
         end do
      end do
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      !---------------------------------------------------
      ! Check if the sparse matrix matches the full matrix
      !---------------------------------------------------
      write(*,*) "Building sparse H matrix..."
      if (Check_Sparse_Full) then
         print*, "CHECK SPARSE FULL"
         CALL build_Hmatrix()
      else
         ! Build CSR format directly (acsr, ja, ia are allocated inside sparse_Hmatrix)
         CALL sparse_Hmatrix()
         ! No need for COO->CSR conversion anymore, it's already in CSR format!
         write(*,*) "Sparse matrix setup complete."
      end if
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


   end subroutine set_global_variables
!!!#################################################################################################

!!!#################################################################################################
!!! This sets up the H Matrix directly in sparse row storage (CSR format)
!!! Modified to build CSR directly instead of COO->CSR to save memory
!!!#################################################################################################
   subroutine sparse_Hmatrix()
     implicit none
      real(real12) :: H0 ! Holds the value of the H matrix
      integer(int12) :: i, j, count, k, row ! i and j are the row and column of the H matrix
      integer(int12) :: nnz_estimate
      ! Holds the values to add to the row to get the column
      integer(int12), allocatable, dimension(:) :: addit 
      ! Temporary arrays for building each row
      real(real12), allocatable, dimension(:) :: row_vals
      integer(int12), allocatable, dimension(:) :: row_cols
      integer(int12) :: row_count, max_row_size
      
      ra%n = NA ! The number of rows in the H matrix
      
      ! Estimate nonzeros (7 per interior cell, less at boundaries)
      nnz_estimate = 7*nx*ny*nz - 2*(nx*ny + ny*nz + nz*nx)
      if (Periodicx) nnz_estimate = nnz_estimate + 2*ny*nz
      if (Periodicy) nnz_estimate = nnz_estimate + 2*nz*nx
      if (Periodicz) nnz_estimate = nnz_estimate + 2*nx*ny
      
      ! Allocate CSR arrays with initial estimate (will grow if needed)
      ! For 451^3: ~640M entries = 10GB, so allocate conservatively
      write(*,'(A,I12,A)') " Estimated nonzeros: ", nnz_estimate, ""
      allocate(acsr(nnz_estimate), ja(nnz_estimate))
      allocate(ia(NA+1))
      
      ! Setup neighbor offsets
      addit = [1] 
      if (Periodicx) addit = [addit, (nx-1)]
      if (ny .gt. 1) addit = [addit, nx]
      if ((Periodicy).and.(ny .gt. 1)) addit = [addit, (ny-1)*nx]
      if (nz .gt. 1) addit = [addit, nx*ny]
      if ((Periodicz).and.(nz .gt. 1)) addit = [addit, (nz-1)*ny*nx]
      
      ! Allocate temporary row storage (max ~13 entries per row for 3D)
      max_row_size = 2*size(addit,1) + 1
      allocate(row_vals(max_row_size), row_cols(max_row_size))
      addit = [1] ! The values to add to the row to get the column
      if (Periodicx) addit = [addit, (nx-1)]
      if (ny .gt. 1) addit = [addit, nx] ! Add the values to add to the row to get the column
      if ((Periodicy).and.(ny .gt. 1)) addit = [addit, (ny-1)*nx]
      if (nz .gt. 1) addit = [addit, nx*ny]  ! Add the values to add to the row to get the column
      if ((Periodicz).and.(nz .gt. 1)) addit = [addit, (nz-1)*ny*nx]

      
      !write(6,*) NA, nx,ny,nz
      !write(6,*) addit,size(addit,1)
      !write(6,*) NA
      !write(6,*) "========================================="

      
      count = 0 ! Total nonzeros counter
      ia(1) = 1 ! CSR row pointer (1-based for Fortran)
      
      ! Build CSR format row-by-row
      write(*,'(A)') " Building CSR matrix row-by-row..."
      parent_loop: do row = 1, NA
         ! Progress reporting every 10%
         if (mod(row-1, max(1,NA/10)) == 0 .and. row > 1) then
            write(*,'(A,I3,A,I12,A)') "   Progress: ", int(100.0*real(row)/real(NA)), &
                 "%, nnz=", count, ""
         end if
         
         row_count = 0
         
         ! Diagonal element
         j = row
         row_count = row_count + 1
         H0 = hmatrixfunc(row, j)
         row_vals(row_count) = H0
         row_cols(row_count) = j
         
         ! Off-diagonal elements (process in column-sorted order for CSR)
         ! First pass: collect all neighbors
         do k = 1, size(addit,1)
            j = row + addit(k)
            if (j > NA) cycle ! Skip if out of bounds
            
            H0 = hmatrixfunc(row, j)
            if (abs(H0) >= TINY) then
               row_count = row_count + 1
               row_vals(row_count) = H0
               row_cols(row_count) = j
            end if
         end do
         
         ! Second pass: collect reverse neighbors (j < row)
         do k = 1, size(addit,1)
            j = row - addit(k)
            if (j < 1) cycle ! Skip if out of bounds
            
            H0 = hmatrixfunc(row, j)
            if (abs(H0) >= TINY) then
               row_count = row_count + 1
               row_vals(row_count) = H0
               row_cols(row_count) = j
            end if
         end do
         
         ! Sort this row's entries by column index (required for CSR)
         call sort_row(row_vals, row_cols, row_count)
         
         ! Copy row data to CSR arrays (no bounds checking - we pre-allocated correctly)
         do i = 1, row_count
            count = count + 1
            acsr(count) = row_vals(i)
            ja(count) = row_cols(i)
         end do
         
         ! Update row pointer
         ia(row+1) = count + 1
      end do parent_loop
      
      ra%len = count
      
      ! Trim arrays to actual size if we over-estimated
      if (count < size(acsr)) then
         write(*,'(A,I12,A,I12)') " Trimming arrays from ", size(acsr), " to ", count
         call trim_csr_arrays(acsr, ja, count)
      end if
      
      deallocate(row_vals, row_cols)
      write(*,'(A,I12,A)') " CSR matrix built successfully. Actual nonzeros: ", count, ""
   end subroutine sparse_Hmatrix
!!!#################################################################################################

!!!#################################################################################################
!!! Sort a row's entries by column index (simple insertion sort, rows are small)
!!!#################################################################################################
   subroutine sort_row(vals, cols, n)
      implicit none
      integer(int12), intent(in) :: n
      real(real12), dimension(n), intent(inout) :: vals
      integer(int12), dimension(n), intent(inout) :: cols
      integer(int12) :: i, j, temp_col
      real(real12) :: temp_val
      
      do i = 2, n
         temp_val = vals(i)
         temp_col = cols(i)
         j = i - 1
         do while (j >= 1)
            if (cols(j) <= temp_col) exit
            vals(j+1) = vals(j)
            cols(j+1) = cols(j)
            j = j - 1
         end do
         vals(j+1) = temp_val
         cols(j+1) = temp_col
      end do
   end subroutine sort_row
!!!#################################################################################################

!!!#################################################################################################
!!! Trim CSR arrays to exact size
!!!#################################################################################################
   subroutine trim_csr_arrays(acsr_arr, ja_arr, final_size)
      implicit none
      integer(int12), intent(in) :: final_size
      real(real12), allocatable, dimension(:), intent(inout) :: acsr_arr
      integer(int12), allocatable, dimension(:), intent(inout) :: ja_arr
      real(real12), allocatable, dimension(:) :: temp_vals
      integer(int12), allocatable, dimension(:) :: temp_cols
      
      allocate(temp_vals(final_size), temp_cols(final_size))
      temp_vals = acsr_arr(1:final_size)
      temp_cols = ja_arr(1:final_size)
      deallocate(acsr_arr, ja_arr)
      allocate(acsr_arr(final_size), ja_arr(final_size))
      acsr_arr = temp_vals
      ja_arr = temp_cols
      deallocate(temp_vals, temp_cols)
   end subroutine trim_csr_arrays
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
      !!! This needs fixing, assuming all boundaries are the same
      if ((ix .eq. 1) .or.(iy .eq. 1) .or. (iz .eq. 1)) then
         alpha = kappaBoundx1 / ( rho * heat_capacity)
         var_stability =( time_step * alpha * &
              (1 / (grid(ix,iy,iz)%length(1)**2) + 1 / ( grid(ix,iy,iz)%length(2) ** 2 ) &
              + 1 / (grid(ix,iy,iz)%length(3) ** 2 ) ) )
         if (var_stability .gt. 1.0/12.0) then
            write(*,*) "Stability condition at boundary not met = ", var_stability
            write(*,*) " Boundary kappas = ", kappaBoundx1, kappaBoundy1, kappaBoundz1
            stop
         end if
      end if
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   end subroutine stability
!!!#################################################################################################

!!!#################################################################################################
!!! This sets up the H Matrix and converts it into sparse row storage
!!!#################################################################################################
   subroutine build_Hmatrix()
      integer(int12) :: i,j, BCount
      real(real12) :: H(NA,NA),HT(NA,NA), H0
      !---------------------------------------------------
      ! Set up the full H matrix
      !---------------------------------------------------
      H=0.0_real12
      BCount = 0
      do j=1,na
         do i =1,na
            H0 = hmatrixfunc(i,j)
            H(i,j) = H0
         end do

      end do
      write(*,'(3F12.3)') H
      CALL SRSin(H, TINY, ra)
      CALL SparseToReal(HT)
      if (all(abs(H-HT) < TINY)) then
         write(*,*) "H and HT are the same"
      else
         write(*,*) "H and HT are not the same"
      end if
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   end subroutine build_Hmatrix
!!!#################################################################################################

!!!#################################################################################################
!!! This sets up the H Matrix and converts it into sparse row storage
!!!#################################################################################################
subroutine SparseToReal(HT)
   real(real12) :: H0
   real(real12), dimension(NA,NA) :: HT
   integer(int12) :: i, j, k
   integer(int12), dimension(3) :: addit

   !---------------------------------------------------
   ! Set up the full H matrix
   !---------------------------------------------------
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
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
end subroutine SparseToReal
!!!#################################################################################################


end module setup

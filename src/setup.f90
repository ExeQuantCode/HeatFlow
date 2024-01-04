
module setup
  use constants, only: real12, int12, TINY
  use inputs, only: Lx, Ly, Lz, nx, ny, nz, NA, grid, time_step, kappaBoundx, kappaBoundy, kappaBoundz &
                     ,Check_Sparse_Full, Check_Stability, ntime
  use constructions, only: heatblock
  use hmatrixmod, only: hmatrixfunc
  use globe_data, only:  ra, TN, TPD, TPPD,inverse_time, heat
  use sparse, only: SRSin
  use materials, only: material
  implicit none
  
   contains
    

!!!##########################################################################
!!! This allocates arrays and builds the apropreate H matrix
!!!##########################################################################
   subroutine set_global_variables()
      integer(int12) :: i,j,k
      logical :: print1
      real(real12) :: TC,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau, stability, alpha, dt
      allocate(TN(nx, ny, nz))
      allocate(TPD(NA))
      allocate(TPPD(NA))
      allocate(heat(NA*ntime))
      heat = 0.0_real12
      dt = time_step
      print1 = .true.
      inverse_time = 1.0_real12/dt
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
            grid(i,j,k)%tau = tau*inverse_time*inverse_time

            !---------------------------------------------------
            ! Check stability condition
            !---------------------------------------------------
            if (Check_Stability) then
               alpha = kappa/(rho*heat_capacity)
               stability =(dt*alpha*(1/(grid(i,j,k)%length(1)**2)+1/(grid(i,j,k)%length(2)**2)+1/(grid(i,j,k)%length(3)**2)))
               if (print1) then
                  print*, "Stability condition = ", stability
                  print1 = .false.
               end if
               if (stability .gt. 1.0/12.0) then
                  print*, "Stability condition not met"
                  print*, "Stability condition = ", stability

                  print*, "dt = ", dt
                  print*, "alpha = ", alpha
                  print*, "dx = ", grid(i,j,k)%length(1)
                  print*, "dy = ", grid(i,j,k)%length(2)
                  print*, "dz = ", grid(i,j,k)%length(3)
               
                  stop
               end if
               if ((i .eq. 1) .or.(j .eq. 1) .or. (k .eq. 1)) then
                  alpha = kappaBoundx/(rho*heat_capacity)
                  stability =(dt*alpha*(1/(grid(i,j,k)%length(1)**2)+1/(grid(i,j,k)%length(2)**2)+1/(grid(i,j,k)%length(3)**2)))
                  if (stability .gt. 1.0/12.0) then
                     print*, "Stability condition at boundary not met = ", stability
                     print*, " Boundary kappas = ", kappaBoundx, kappaBoundy, kappaBoundz
                     stop
                  end if
               end if
            end if 
            end do               
         end do
      end do

      !---------------------------------------------------
      !** should impliment an if condition for sparse only
      ! if(sparse_only) then
         !** Never have to make the full H matrix, needs boundarys
         !call SPHM()
         !print*,Hsparse
      ! else
      if (Check_Sparse_Full) then
         call build_Hmatrix()
      else
         call sparse_Hmatrix()
      end if


      !call setup_grid()

   end subroutine set_global_variables
!!!##########################################################################


!!!#########################################################################
!!! This sets up the H Matrix and converts it into sparse row storage
!!!#########################################################################
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
         ! if (HCheck.ne.0) then
         !    BCount = BCount + 1
         !    print*, 'Boundary = ',BCount
         ! end if 
      end do
      ! write(*,'(3F12.3)') H
      call SRSin(H, TINY, ra)
      call SparseToReal(HT)
      if (all(abs(H-HT) < TINY)) then
         print*, "H and HT are the same"
      else
         print*, "H and HT are not the same"
      end if
   end subroutine build_Hmatrix
!!!#########################################################################
!!! This sets up the H Matrix and converts it into sparse row storage
!!!#######################################################################
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
   write(*, '(10F15.4)') HT
end subroutine SparseToReal

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

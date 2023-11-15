
module setup
  use constants, only: real12, int12, TINY
  use inputs, only: Lx, Ly, Lz, nx, ny, nz, NA, grid, T_Bath
  use constructions, only: heatblock
  use hmatrixmod, only: hmatrix
  use globe_data, only:  ra, T, TN, Told,TD,TPD, TPPD
  use sparse, only: SRSin

  implicit none
  
   contains
    

!!!##########################################################################
!!! This allocates arrays and builds the apropreate H matrix
!!!##########################################################################
   subroutine set_global_variables()

      allocate(TN(nx, ny, nz))
      allocate(T(nx, ny, nz))
      allocate(Told(nx, ny, nz))
      allocate(TD(NA))
      allocate(TPD(NA))
      allocate(TPPD(NA))           

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
      real(real12) :: H0, HB
      integer(int12) :: i, j, len
      ! real(real12), dimension(NA,NA) :: H
      !------------------------------------------------
      !Make hmatrix
      !Hmatrix consists of H_ij,0 and H_ij,B (heat flow across grid and matrix correction for boundaries
      !-----------------------------------------------
      !H_ij T_i =S_j
      ! HB = 0.0
      ! len = 0
      ! do j=1,NA 
      !    do i=1,NA
         
      !       call hmatrix(i,j,H0)
      !       H(i,j) = H0


      !     end do
      !  end do
      
      ! ! print*, len
      ! write(*,'(3F9.2)') H

      ! Convert the matrix into Sparse Row Storage.
      !call SRSin(H,TINY, ra)
      !print*, ra%len
      call sparse_Hmatrix()
      ! print*, ra%len
    end subroutine build_Hmatrix
!!!#########################################################################


!!!#########################################################################
!!! This sets up the H Matrix directly in sparse row storage
!!!#########################################################################
   subroutine sparse_Hmatrix()
     real(real12) :: H0, HB, B, val
     integer(int12) :: i, j, len
      integer(int12), allocatable :: rar(:), rac(:)
      real(real12), allocatable :: rav(:)
      len = 0 
     do j = 1, NA
        do i = 1, NA
           if (i-j .eq. 0) then
              call hmatrix(i,j, H0)
              if (abs(H0).gt. TINY) len=len+1
               ! Diagonal, major band
           else if (i-j .eq. 1) then
              call hmatrix(i,j, H0)
              if (abs(H0).gt. TINY) len=len+1
               ! X left neighbor, sub_1 band
           else if (i-j .eq. -1) then 
              call hmatrix(i,j, H0)
              if (abs(H0).gt. TINY) len=len+1
               ! X right neighbor, sup_1 band
           else if (i-j .eq. nx)  then
              call hmatrix(i,j, H0)
              if (abs(H0).gt. TINY) len=len+1
               ! Y down neighbor, sub_2 band
           else if (i-j .eq. -nx) then
              call hmatrix(i,j, H0)
              if (abs(H0).gt. TINY) len=len+1
               ! Y up neighbor, sup_2 band 
           else if (i-j .eq. nx*ny) then
              call hmatrix(i,j, H0)
              if (abs(H0).gt. TINY) len=len+1
               ! Z in neighbor, sub_3 band
           else if (i-j .eq. -nx*ny) then
              call hmatrix(i,j, H0)
              if (abs(H0).gt. TINY) len=len+1
               ! Z out neighbor, sup_3 band
           end if
        end do
     end do
   !   print*, len
   !   print*, (7*nx*ny*nz)-(2*((ny*nx)+(nx*nz)+(ny*nz)))
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

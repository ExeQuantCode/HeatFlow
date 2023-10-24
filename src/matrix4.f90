!##############################################################################################################
! This code is forms the heat equation for the system that we want to solve
!##############################################################################################################
MODULE simulator

  use constants
  use constructions
  use heating
  use materials
  use setup
  use DeA
  use readtxt
  use matrix_inversion
  use inputs

  contains  



  subroutine bmake(grid, T, TN, Told, TPD, it)
    implicit none

    ! Define dtypes for variables
    integer(int12) :: ix,iy,iz,itime,it,ii,ij,ixm1,jj,iflag, flag, imaterial, counter, a_counter, e_prime, off_prime
    !integer(int12) :: kx,ky,kz
    integer(int12) :: equ
    integer(int12) :: e
    integer(int12) :: dis
    integer(int12) :: cen
    integer(int12) :: off
    integer(int12) :: m
    integer(int12) :: endz
    integer(int12) :: i,j,k,h,kk
    integer(int12), parameter :: tolp = 8
    real(real12), parameter :: tol = 1e-8
    character:: uplo, no
    character(len=20) :: fmt_string
    real(real12) :: pa,pb,L,rho_C,cv_C, Q ,Pin,kap11,Delta_Temp,TP
    real(real12) :: h_conv, heat_capacity, HC, sound_speed, rho, volume, RC                                                     
    real(real12) :: RTerm, qdot, kappa, tau, tdx, tdy, tdz
    ! define allocatable arrays
    integer(int12), dimension(7) :: idiag 
    real(real12), allocatable :: small(:,:)
    real(real12), allocatable :: small_transpose(:, :)
    real(real12), dimension(3,2) :: l_c, h_con, kap

    real(real12), allocatable :: b_temp(:,:,:)
    real(real12), dimension(nx,ny,nz) :: QQ(nx,ny,nz)
    real(real12), allocatable :: alpha(:,:,:)
    real(real12), allocatable :: beta(:,:,:)

    real(real12), allocatable :: T_matrix(:)
    real(real12), allocatable :: TN_matrix(:)
    real(real12), allocatable :: Told_matrix(:)
    real(real12), allocatable :: heat(:)
    real(real12), allocatable :: b(:)
    real(real12), dimension(NA) :: TPD

    real(real12), allocatable ::  gammax(:)
    real(real12), allocatable :: gammay(:)
    real(real12), allocatable :: gammaz(:)
    real(real12) :: kappa3D
    real(real12), allocatable :: b_prime(:,:)

    real(real12), dimension(nx,ny,nz):: T, TN, Told
    TYPE(heatblock), dimension(nx,ny,nz) :: grid
    real(real12), allocatable:: a1(:)
    integer(int12), allocatable :: ia(:)

    ! heating bath in x,y,z variables
    real(real12) :: T_bath_1x
    real(real12) :: T_bath_nx
    real(real12) :: T_bath_1y
    real(real12) :: T_bath_ny
    real(real12) :: T_bath_1z
    real(real12) :: T_bath_nz
   
    !For congujate gradient
    integer(int12) :: iter, itmax
    !real(real12) :: tol 
    INTEGER(int12) :: n 

    ! Read parameters from txt file and use them to allocate the local variables and arrays
    real(real12), dimension(nx) :: cellengthx
    real(real12), dimension(ny) :: cellengthy
    real(real12), dimension(nz) :: cellengthz
    call readparameters(cellengthx,cellengthy,cellengthz)
    e = nx*ny*nz
    dis = nx*ny
    cen = (2*dis)+1
    off = (3*dis)+1
    m = 4*nx*ny*nz - ny*nz - nx*nz - nx*ny
    endz = (nx*ny)*(nz-1)

    ! Allocate arrays
    allocate(small(e,off))
    allocate(small_transpose(off, e))

    allocate(b_temp(nx,ny,nz))
    allocate(alpha(nx,ny,nz))
    allocate(beta(nx,ny,nz))

    allocate(T_matrix(e))
    allocate(TN_matrix(e))
    allocate(Told_matrix(e))
    allocate(heat(e))
    allocate(b(e))

    allocate(gammax(nx))
    allocate(gammay(ny))
    allocate(gammaz(nz))

    allocate(b_prime(e,1))
    allocate(a1(m))
    allocate(ia(m+1))

    !write(*,*) T_bath
    no = 'n'   
    uplo = 'u'



    e_prime = e
    off_prime = off
    Small = 0
    !    SI= 0

    if (it.eq.1) then
       T=T_bath
       Told=T_bath
       TN=T_bath
    end if
      !!! WHY INITIALISE GRID EVERY TIME STEP?
    call initiate(grid, cellengthx, cellengthy, cellengthz)


 
    T_bath_1x = T_Bath
    T_bath_nx = T_Bath
    T_bath_1y = T_Bath
    T_bath_ny = T_Bath
    T_bath_1z = T_Bath
    T_bath_nz = T_Bath
    !print*, room

    print*, nx,ny,nz
      !################################
      ! Heat equation is of form
      ! Small*TN = b_temp
      !################################
    !LOOP THROUGH THE CELLS Issues with this its not averaging cells that are different and has issue with infinity
    do ix=1,nx
       do iy=1,ny
          do iz=1,nz
             !Delta ave returns boundary averaged values and temperature gradients                 
             ! It doesn't change anything
             !print*, ix,iy,iz
             CALL DELTA_ave(T,Kap,l_c,H_con,ix,iy,iz,grid)
             call material(grid(ix,iy,iz)%imaterial_type,TP,kappa,kappa3D &
	 ,h_conv,heat_capacity,rho,sound_speed,tau)
             call heater(it, Q)
               !write(*,*) 'QQ', QQ
             !delta_ave needs to be converted from 1st order to second order
             ! make matrices for gamma !

             !gammax(ix) = (kappa*time_step*time_step)/(rho*heat_capacity*cellengthx(ix)*cellengthx(ix)*(tau+time_step))

             alpha(ix,iy,iz) = (tau+time_step)/(time_step*time_step)

             tdx = (kap(1,1)+kap(1,2)/2)/(rho*heat_capacity)
             tdy = (kap(2,1)+kap(2,2)/2)/(rho*heat_capacity)
             tdz = (kap(3,1)+kap(3,2)/2)/(rho*heat_capacity)
             
             gammax(ix) = (((kap(1,1)+kap(1,2))/2)*time_step*time_step)/(rho*heat_capacity*cellengthx(ix) &
                  *cellengthx(ix)*(tau+time_step))
             gammay(iy) = (((kap(2,1)+kap(2,2))/2)*time_step*time_step)/(rho*heat_capacity*cellengthy(iy) &
                  *cellengthy(iy)*(tau+time_step))
             gammaz(iz) = (((kap(3,1)+kap(3,2))/2)*time_step*time_step)/(rho*heat_capacity*cellengthz(iz) &
                  *cellengthz(iz)*(tau+time_step))
             !print*, rho*heat_capacity*cellengthz(iz)*cellengthz(iz)*(tau+time_step) 
             !gammaz(iz) = (tdz/(cellengthz(iz)*cellengthz(iz)))/alpha(ix,iy,iz)
             !print*, ix,iy,iz
             !print*, tau
             !print*, time_step
             
             !print*, 'ALPHA =', alpha
             beta(ix,iy,iz) = ((tau)/(time_step*time_step))!/alpha(ix,iy,iz)
             !print*, 'BETA =', beta

             !Change qq to include material properties
             !QQ(ix,iy,iz) = QQ(ix,iy,iz) *(cellengthx(ix)*cellengthy(iy)*cellengthz(iz))!/ &
             !     (sum(cellengthx)*sum(cellengthy)*sum(cellengthz))
             QQ(ix,iy,iz) = QQ(ix,iy,iz)/(rho*heat_capacity)
 
          end do
       end do
    end do



    ! MAKE THE B_TEMP MATRIX !
    ! WON'T WORK FOR 1X1X1 GRID
    !DOODOODOO
    do i = 1,nx
       do j = 1,ny
          do k = 1,nz
             !altpow = QQ(i,j,k)/alpha(i,j,k)
             !call heater(i,j,k,it,grid(i,j,k)%imaterial_type,QQ)
             !print*, i,j,k
             if (i.eq.1) then
                if (j.eq.1) then
                   if (k.eq.1) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_1x+gammay(j)*T_bath_1y &
                           +gammaz(k)*T_bath_1z
                      !print*, gammax(i)*T_bath_1x+gammay(j)*T_bath_1y+gammaz(k)*T_bath_1z
                   else if (k.eq.nz) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_1x+gammay(j)*T_bath_1y &
                           +gammaz(k)*T_bath_nz
                   else
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_1x+gammay(j)*T_bath_1y
                   end if
                else if (j.eq.ny) then
                   if (k.eq.1) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_1x+gammay(j)*T_bath_ny &
                           +gammaz(k)*T_bath_1z
                   else if (k.eq.nz) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_1x+gammay(j)*T_bath_ny &
                           +gammaz(k)*T_bath_nz
                   else
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_1x+gammay(j)*T_bath_ny
                   end if
                else
                   if (k.eq.1) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_1x+gammaz(k)*T_bath_1z
                   else if (k.eq.nz) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_1x+gammaz(k)*T_bath_nz
                   else
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_1x
                   end if
                end if
             else if (i.eq.nx) then
                if (j.eq.1) then
                   if (k.eq.1) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_nx+gammay(j)*T_bath_1y &
                           +gammaz(k)*T_bath_1z
                   else if (k.eq.nz) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_nx+gammay(j)*T_bath_1y &
                           +gammaz(k)*T_bath_nz
                   else
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_nx+gammay(j)*T_bath_1y
                   end if
                else if (j.eq.ny) then
                   if (k.eq.1) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_nx+gammay(j)*T_bath_ny &
                           +gammaz(k)*T_bath_1z
                      !print*, T_bath_1z, 'here', QQ(i,j,k), i,j,k, alpha(i,j,k)
                   else if (k.eq.nz) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_nx+gammay(j)*T_bath_ny &
                           +gammaz(k)*T_bath_nz
                      !print*, 'gammax(i) =', gammax(i)
                      !print*, T_bath_nz, 'here', QQ(i,j,k), i,j,k, alpha(i,j,k)
                   else
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_nx+gammay(j)*T_bath_ny
                   end if
                else
                   if (k.eq.1) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_nx+gammaz(k)*T_bath_1z
                   else if (k.eq.nz) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_nx+gammaz(k)*T_bath_nz
                   else
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammax(i)*T_bath_nx
                   end if
                end if
             else
                if (j.eq.1) then
                   if (k.eq.1) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammaz(k)*T_bath_1z+gammay(j)*T_bath_1y
                   else if (k.eq.nz) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammaz(k)*T_bath_nz+gammay(j)*T_bath_1y
                   else
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammay(j)*T_bath_1y
                   end if
                else if (j.eq.ny) then
                   if (k.eq.1) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammaz(k)*T_bath_1z+gammay(j)*T_bath_ny
                   else if (k.eq.nz) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammaz(k)*T_bath_nz+gammay(j)*T_bath_ny
                   else
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammay(j)*T_bath_ny
                   end if
                else
                   if (k.eq.1) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammaz(k)*T_bath_1z
                   else if (k.eq.nz) then
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)+gammaz(k)*T_bath_nz
                   else
                      !print*, i,j,k
                      b_temp(i,j,k)=QQ(i,j,k)/alpha(i,j,k)+(1+beta(i,j,k))*T(i,j,k) &
                           -beta(i,j,k)*Told(i,j,k)
                   end if
                end if
             end if
             !print*, b_temp(i,j,k)
          end do
       end do
    end do

    !print*, b_temp    


    ! MAKE THE SMALL MATRIX !

    ! DUMMY ROWS !
    ! 'Space must be allowed to store an additional kl superdiagonals,
    ! generated by fill-in as a result of row interchanges'
    ! Also need 0 band between y and z bands
    do i = 1,e
       do j = 1,off
          small(i,j) = 0.0
       end do
    end do

    i = 0
    do ix = 1,nx
       do iy = 1,ny
          do iz = 1,nz
             i = i+1
             small(i,cen) = 1+2*gammax(ix) + 2*gammay(iy) + 2*gammaz(iz)
             if ( mod(i,nx).ne.0 ) then
                small(i,cen-1)= -gammaz(iz)
                small(i,cen+1) = -gammaz(iz)
             end if
             do j = 1,e
                !print*, i,j
                if (ny.gt.1  .and.  mod((i-1)/nx,ny) .ne. 0) then
                   small(i,cen-nx) = -gammay(iy)
                else if (ny.gt.1.and.mod((j-1)/nx,ny).ne.0) then
                   small(j,cen+nx) = -gammay(iy)
                end if
             end do

             if (nz.gt.1) then
                small(i,cen-dis) = -gammax(ix)
                small(i,cen+dis) = -gammax(ix)
             end if
          end do
       end do
    end do
    
    

    

    ! SET OFFSETS !

    small(:,cen-1) = eoshift(small(:,cen-1),shift =-1,boundary = 0.e0_real12, dim=1)
    small(:,cen+nx) = eoshift(small(:,cen+nx),shift =nx,boundary = 0.e0_real12, dim=1)
    small(:,cen-dis) = eoshift(small(:,cen-dis),shift =-dis,boundary = 0.e0_real12, dim=1)


!!!################################################################################################

    !     CALCULATE INVERSE OF A     

    !    call matinv1(a,SI,e) ! TOO BIG


    !!SETS THE FORMATS                                                                                                                                                                                         
201 format (30f12.6) !Change???


    ! Change temp and b matrix to 1 columns
    flag=1
    do ix=1,nx
       do iy=1,ny
          do iz=1,nz
             Told_matrix(flag) = Told(ix,iy,iz)
             T_matrix(flag) = T(ix, iy, iz)
             TN_matrix(flag) = TN(ix,iy,iz)
             b(flag) = b_temp(ix,iy,iz)
             flag = flag+1
          end do
       end do
    end do

	TPD = b
    ! Do the math !

    small_transpose = transpose(small)
    !print*, T_matrix
    !TN_matrix = matmul(a,b)
    b_prime(:,1) = b
    !   call matinv1(dble(small_transpose),dble(b_prime),int(e))
    !print*, small_transpose
    !print*, b
    !small_transpose = dble(small_transpose)
    !b_prime = dble(b_prime)
    !print*, b_prime
    !print*, small_transpose
    n = size(b)
    !call conjugate_gradient(n, small_transpose, T_matrix, b, tol, itmax, iter)
    !call matinv1(small_transpose,b_prime,e_prime,off_prime) ! small*T = b_prime
    !print*, T_matrix
    !print*, b_prime


    ! Change back to 3d matrices
    equ = 1
    flag = 1
    do ix=1,nx
       do iy=1,ny
          do iz=1,nz
             if(abs(TN(ix,iy,iz)-b_prime(flag,1)).gt.tol) then
                TN(ix,iy,iz) = b_prime(flag,1)
             else
                equ = equ + 1
             end if
          b_temp(ix,iy,iz) = b(flag)
          flag = flag+1
         end do
      end do
   end do

 if(equ.eq.flag) then
    print*, 'EQUILIBRIUM REACHED'
 end if



 ! reset at end of timestep !
 do ix=1,nx
    do iy = 1,ny
       do iz = 1,nz
          Told(ix,iy,iz) = T(ix,iy,iz)
          T(ix,iy,iz) = TN(ix,iy,iz)
       end do
    end do
 end do
 !Print *, Told

end subroutine bmake
end MODULE simulator



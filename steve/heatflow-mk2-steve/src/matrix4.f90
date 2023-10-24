MODULE simulator

  use constants
  use parameters
  use constructions
  use heating
  use materials
  use setup
  ! use DeT
  use DeA
  use matrix_inversion

  !  use mymkl

  !  include 'mkl.fi'

  implicit none

contains  



  subroutine evolve(grid,T, TN, Told, it)
    implicit none
    TYPE(heatblock), dimension(nx,ny,nz):: grid
    integer(int12) :: ix,iy,iz,itime,it,ii,ij,ixm1,jj,iflag, flag, imaterial, counter, a_counter, e_prime, off_prime
    !integer(int12) :: kx,ky,kz
    integer(int12) :: equ
    integer(int12), parameter :: e=nx*ny*nz
    integer(int12), parameter :: dis = nx*ny
    integer(int12), parameter :: cen = (2*dis)+1
    integer(int12), parameter :: off = (3*dis)+1
    integer(int12), parameter :: m = 4*nx*ny*nz - ny*nz - nx*nz - nx*ny
    integer(int12), parameter :: endz = (nx*ny)*(nz-1)
    integer(int12) :: i,j,k,h,kk

    !    real (real12), dimension(e,e) :: a,SI           TOO BIG
    real (real12), dimension(e,off) :: small
    !real(real12), dimension(10,e) :: small
    real (real12), dimension(off,e) :: small_transpose
    real(real12), dimension(3,2) :: kap,h_con,l_c
    real(real12) :: pa,pb,L,rho_C,cv_C,Q,Pin,kap11,Delta_Temp
    real(real12), dimension(nx,ny,nz) :: T, TN, Told, b_temp, QQ
    real(real12), dimension(nx,ny,nz) :: alpha, beta
    real(real12), dimension(e) :: T_matrix, TN_matrix, Told_matrix, heat, b
    real(real12), dimension(nx) :: cellengthx, gammax
    real(real12), dimension(ny) :: cellengthy, gammay
    real(real12), dimension(nz) :: cellengthz, gammaz
    real(real12), dimension(3,3) :: kappa3D
    real(real12) :: Qp
    real(real12), dimension(e,1) :: b_prime
    integer(int12), parameter :: tolp = 8
    real(real12), parameter :: tol = 1e-8  !CHANGE AS APPROPRIATE

    real(real12), dimension(m) :: a1
    integer(int12), dimension(m+1) :: ia

    integer(int12), dimension(7) :: idiag 

    character:: uplo, no
    character(len=20) :: fmt_string



    real(real12) :: h_conv, heat_capacity, HC, sound_speed, rho, volume, RC                                                     
    real(real12) :: RTerm, qdot, kappa, tau, tdx, tdy, tdz!, altpow

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


    !    call heater(ix,iy,iz,it,grid(ix,iy,iz)%imaterial_type,QQ) !Commented out for now (wrong place?)


    !print*, room

    print*, nx,ny,nz

    !LOOP THROUGH THE CELLS
    do ix=1,nx
       do iy=1,ny
          do iz=1,nz
             !Delta ave returns boundary averaged values and temperature gradients                 
             ! It doesn't change anything
             !print*, ix,iy,iz
             CALL DELTA_ave(T,Kap,l_c,H_con,ix,iy,iz,grid)
             call material(grid(ix,iy,iz)%imaterial_type,T,kappa,kappa3D,h_conv,heat_capacity,rho,sound_speed,tau)
             call heater(ix,iy,iz,it,grid(ix,iy,iz)%imaterial_type,Qp)
             !print*, Qp
             QQ(ix,iy,iz)= Qp
             !print*, ix,iy,iz
             !delta_ave needs to be converted from 1st order to second order
             ! make matrices for gamma !

             !gammax(ix) = (kappa*time_step*time_step)/(rho*heat_capacity*cellengthx(ix)*cellengthx(ix)*(tau+time_step))

             alpha(ix,iy,iz) = (tau+time_step)/(time_step*time_step)

             tdx = (kap(1,1)+kap(1,2)/2)/(rho*heat_capacity)
             tdy = (kap(2,1)+kap(2,2)/2)/(rho*heat_capacity)
             tdz = (kap(3,1)+kap(3,2)/2)/(rho*heat_capacity)
             
             gammax(ix) = 0!(((kap(1,1)+kap(1,2))/2)*time_step*time_step)/(rho*heat_capacity*cellengthx(ix) &
                  !*cellengthx(ix)*(tau+time_step))
             gammay(iy) = 0!(((kap(2,1)+kap(2,2))/2)*time_step*time_step)/(rho*heat_capacity*cellengthy(iy) &
                  !*cellengthy(iy)*(tau+time_step))
             gammaz(iz) = (((kap(3,1)+kap(3,2))/2)*time_step*time_step)/(rho*heat_capacity*cellengthz(iz) &
                  *cellengthz(iz)*(tau+time_step))
             !print*, rho*heat_capacity*cellengthz(iz)*cellengthz(iz)*(tau+time_step) 
             !gammaz(iz) = (tdz/(cellengthz(iz)*cellengthz(iz)))/alpha(ix,iy,iz)
             !print*, ix,iy,iz
             !print*, tau
             !print*, time_step
             
             !print*, 'ALPHA =', alpha
             beta(ix,iy,iz) = ((tau)/(time_step*time_step))/alpha(ix,iy,iz)
             !print*, 'BETA =', beta

             !Change qq to include material properties
             !QQ(ix,iy,iz) = QQ(ix,iy,iz) *(cellengthx(ix)*cellengthy(iy)*cellengthz(iz))!/ &
             !     (sum(cellengthx)*sum(cellengthy)*sum(cellengthz))
             QQ(ix,iy,iz) = QQ(ix,iy,iz)/(rho*heat_capacity)
          end do
       end do
    end do
    !print*, QQ
    !print*, gammax, gammay, gammaz
    !    print*, kappa
    !    print*, ''
    !    print*, time_step
    !    print*, ''
    !    print*, tau
    !    print*, ''
    !    print*, rho
    !    print*, ''
    !    print*, heat_capacity
    !    print*, ''
    !    print*, cellengthx
    !    print*, ''
    !    print*, cellengthy
    !    print*, ''
    !    print*, cellengthz
    !    
    !    print*, 'gammax =',gammax, 'STOP'
    !    print*, 'gammay =',gammay, 'STOP'
    !    print*, 'gammaz =',gammaz, 'STOP'
    !
    !    print*, alpha
    !    print*, beta
    


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
                      print*, gammax(i)*T_bath_1x+gammay(j)*T_bath_1y+gammaz(k)*T_bath_1z
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
             print*, b_temp(i,j,k)
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
             !if (small(1,cen+nx).eq.0) then
             !   do j=1,(e-1)
             !      !print*, 'FGHGF',i,  j, small(:,cen+nx)
             !      small(j,cen+nx)=small(j+1,cen+nx)
             !      print*, 'FGHGF',i,  j, small(:,cen+nx)
             !   end do
             !   small(e,cen+nx)=0
             !   print*, 'FGHGF',i,  j, small(:,cen+nx)
             !end if
             !if (small(1,cen-nx).eq.0) then
             !   do j=1,(e-1)
             !      small(j,cen-nx)=small(j+1,cen-nx)
             !   end do
             !   small(e,cen-nx)=0
             !end if
             if (nz.gt.1) then
                small(i,cen-dis) = -gammax(ix)
                small(i,cen+dis) = -gammax(ix)
             end if
          end do
       end do
    end do
    
    

    !do i=1,e
    !   small(i,cen) = 1+2*gammax(1) + 2*gammay(1) + 2*gammaz(1)
    !end do


    ! X DIAGONALS ! 
    !do i=1,e
    !   if ( mod(i,nx).ne.0 ) then
    !      small(i,cen-1)= -gammax(1)
    !      small(i,cen+1) = -gammax(1)
    !   end if
    !end do



    ! Y DIAGONALS !
    !do i=1,e
    !   do j=1,e
    !      !kk = i+1
    !      if (ny.gt.1  .and.  mod((i-1)/nx,ny) .ne. 0) then
    !         small(i,cen-nx) = -gammay(1)
    !      else if (ny.gt.1.and.mod((j-1)/nx,ny).ne.0) then
    !         small(j,cen+nx) = -gammay(1)
    !      end if
    !   end do
    !end do


    !do j=1,e
    !   if (small(1,cen+nx).eq.0) then
    !      do i=1,(e-1)
    !         small(i,cen+nx)=small(i+1,cen+nx)
    !      end do
    !      small(e,cen+nx)=0
    !   end if
    !end do

    !do j=1,e
    !   if (small(1,cen-nx).eq.0) then
    !      do i=1,(e-1)
    !         small(i,cen-nx)=small(i+1,cen-nx)
    !      end do
    !      small(e,cen-nx)=0
    !   end if
    !end do



    ! Z DIAGONALS !

    !do i=1,endz
    !   if (nz.gt.1) then
    !      small(i,cen-dis) = -gammaz(1)
    !      small(i,cen+dis) = -gammaz(1)
    !   end if
    !end do




    ! SET OFFSETS !

    small(:,cen-1) = eoshift(small(:,cen-1),shift =-1,boundary = 0.e0_real12, dim=1)
    small(:,cen+nx) = eoshift(small(:,cen+nx),shift =nx,boundary = 0.e0_real12, dim=1)
    small(:,cen-dis) = eoshift(small(:,cen-dis),shift =-dis,boundary = 0.e0_real12, dim=1)

    !write(fmt_string,'("(",I0,"(1X,F0.9))")') e
    !write(*,trim(fmt_string)) small
    !write(*,*)
    !write(*,'(10(1X,F8.4))') (small(i,:),i=1,8)
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


    ! Do the math !

    small_transpose = transpose(small)
    !print*, T_matrix
    !TN_matrix = matmul(a,b)
    b_prime(:,1) = b
    !   call matinv1(dble(small_transpose),dble(b_prime),int(e))
    !print*, small_transpose
    !print*, b_prime
    !small_transpose = dble(small_transpose)
    !b_prime = dble(b_prime)
    !print*, b_prime
    !print*, small_transpose
    call matinv1(small_transpose,b_prime,e_prime,off_prime)
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
          !b_temp(ix,iy,iz) = b(flag)
          flag = flag+1
       end do
    end do
 end do

 if(equ.eq.flag) then
    print*, 'EQUILIBRIUM REACHED'
 end if


 !print*, TN
 write(*, '("TN(1,1,1)=",F0.7)') TN(1,1,1)
 write(*, '("TN(1,1,2)=",F0.7)') TN(1,1,2) 
 write(*, '("TN(1,2,2)=",F0.7)') TN(1,2,2)
 write(*, '("TN(1,2,3)=",F0.7)') TN(1,2,3)
 write(*, '("TN(2,2,2)=",F0.7)') TN(2,2,2)
 write(*, '(" TN(",I0,",",I0,",",I0,")= ",F0.7)') 3,3,3,TN(3,3,3)


 ! reset at end of timestep !
 do ix=1,nx
    do iy = 1,ny
       do iz = 1,nz
          Told(ix,iy,iz) = T(ix,iy,iz)
          T(ix,iy,iz) = TN(ix,iy,iz)
       end do
    end do
 end do

end subroutine evolve
end MODULE simulator



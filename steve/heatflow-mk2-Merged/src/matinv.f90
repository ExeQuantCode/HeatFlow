!##############################################################################################################
! This code solves the heat equation using sparce matrix solvers. Currently under construction
!##############################################################################################################
module MATRIX_INVERSION
  use constants
  implicit none
  
contains

!  subroutine matinv1(S,SI,dN)
  subroutine matinv1(dAB,dB,dN,dSize)
    implicit none
!    integer(int12), intent(in) :: NBB
!    real (real12), dimension (NBB,NBB) :: S, SI 
!    complex (real12), allocatable, dimension(:,:) :: a
!    complex (real12), allocatable, dimension(:)   :: work
!    integer(int12) , allocatable, dimension (:)          :: ipiv
!    
    integer i,ifail,j
    !integer(int12) i,ifail,j
!    character*1 uplo
! 
!
!!!###############################################################################

    !integer(int12) :: dN, dKL, dKU, dNRHS, dLDAB, dLDB, dINFO
    integer :: dKL, dKU, dNRHS, dLDAB, dLDB, dINFO
    integer(int12), intent(inout) :: dN,dSize
    real(real12), dimension(13,dN), intent(inout) :: dAB
    real(real12), dimension(dN,1), intent(inout) :: dB
    !double precision, dimension(13,dN), intent(inout) :: dAB
    !double precision, dimension(dN,1), intent(inout) :: dB
!    double precision, dimension(dN,1) :: dBr
    !integer(int12), allocatable, dimension(:) :: dIPIV
    integer, allocatable, dimension(:) :: dIPIV
    
    
!!!###############################################################################

    !dN = int(dN)
    !dAB = dble(dAB)
    !dB = dble(dB)
    !dSize = int(dSize)


    dKL = (dSize-1)/3
    dKU = (dSize-1)/3
    dNRHS = 1
    dLDAB = dSize
    dLDB = dN
    !print*, dB

!    dAB_transpose = transpose(dAB)
    



    
!    uplo='U'
!    allocate(A(NBB,NBB),stat=ifail)
!    if (ifail.ne.0) then
!       write(*,*) 'Error in A assignment in MATINV1'
!       write(*,*) '...stopping...'
!       stop
!    end if
!    allocate(WORK(NBB),stat=ifail)
!    if (ifail.ne.0) then
!       write(*,*) 'Error in WORK assignment in MATINV1'
!       write(*,*) '...stopping...'
!       stop
!    end if
    allocate(dIPIV(dN),stat=ifail)
    if (ifail.ne.0) then
       write(*,*) 'Error in IPIV assignment in MATINV1'
       write(*,*) '...stopping...'
       stop
    end if
!    
!    do i=1,NBB
!       do j=1,NBB
!          a(i,j)=dcmplx(s(i,j),si(i,j))
!       enddo
!    enddo
!
!
!!!###############################################################################

    
    


    
    write(6,*) 'calling dgbsv'
    call DGBSV(dN,dKL,dKU,dNRHS,dAB,dLDAB,dIPIV,dB,dLDB,dINFO)
    if(dINFO .ne. 0) then
       write(*,*) 'noooope', size(dAB), size(dB)
       !write(*,*) 'dN', shape(dN), 'dKL', shape(dKL), 'dKU', shape(dKU)
       !write(*,*) 'dNRHS', shape(dNRHS), 'dAB', shape(dAB), 'dLDAB', shape(dLDAB)
       !write(*,*) 'dIPIV', shape(dIPIV), 'dB', shape(dB), 'dLDB', shape(dLDB), 'dINFO', shape(dINFO)
       !write(*,'(10(1X,F0.4))') dAB
       !print*, dINFO
       write(66,*) ' info .ne. 0 after dgbsv: stopping'
       stop
    endif


!    print*, dB

    
!!!###############################################################################
 
!    write(6,*) 'calling zhetrf'
!    CALL ZHETRF(uplo,NBB,a,NBB,ipiv,work,NBB,info)
!    if(info .ne. 0) then
!       write(*,*) 'nope'
!       write(66,*) ' info .ne. 0 after zhetrf: stopping'
!       stop
!    endif
!    
!    if(info.eq.0) then
!!!$     compute inverse of A
!       write(6,*) 'calling zhetri'
!       CALL ZHETRI(uplo,NBB,a,NBB,ipiv,work,ifail)
!    endif
!
!
!
!    
!    if(ifail .ne. 0) then
!       write(66,*) ' ifail .ne.0 after zhetri: stopping'
!       stop
!    endif
!
    
!    if(dINFO .eq. 0) then
!!$c$$$  write(66,*)
!!$c$$$  write(66,*) ' ***** matrix inversion using Lapack ****'
!!$c$$$  write(66,*) '  matrix S'
!!$        do i=1,3*nbc
!!$c$$$  write(66,1) (s(i,j),j=1,3*nbc)
!!$        enddo
!!$c$$$  write(76,*) '  matrix SI'
!!$        do i=1,3*nbc
!!$c$$$  write(76,1) (si(i,j),j=1,3*nbc)
!!$        enddo
       
       
!       do i=1,NBB
!          do j=i,NBB
!             s(i,j)=dreal(a(i,j))
!             si(i,j)=dimag(a(i,j))
!             s(j,i)=dreal(a(i,j))
!             si(j,i)=-dimag(a(i,j))
!          enddo
!       enddo
!       
!       
       !C     !SH 19/12/03 write TOO LARGE a file for 128+
       
!!$c$$$  write(66,*) '  inverse of matrix S'
!!$c$$$  do i=1,3*nbc
!!$c$$$  write(66,1) (s(i,j),j=1,3*nbc)
!!$c$$$  enddo
!!$        
!!$c$$$  write(76,*) '  inverse of matrix SI'
!!$c$$$  do i=1,3*nbc
!!$c$$$  write(76,1) (si(i,j),j=1,3*nbc)
!!$c$$$  enddo
        
!    endif
!    deallocate(A)
    deallocate(dIPIV)
!    deallocate(work)
!   format(76f10.4)
    
  end subroutine matinv1
! This subroutine implements the Conjugate Gradient method to solve the system of linear equations Ax = b.
SUBROUTINE conjugate_gradient(n, A, x, b, tol, itmax, iter)
    IMPLICIT NONE
    INTEGER(int12) :: n        ! Size of the system
    INTEGER(int12) :: i, itmax, iter               ! Maximum number of iterations and current iteration counter
    REAL(real12), DIMENSION(n, n) :: A           ! System matrix
    REAL(real12), DIMENSION(n) :: x, b           ! Solution vector and right-hand side
    REAL(real12), DIMENSION(n) :: r, p, Ap       ! Residual, direction, and product of A and p
    REAL(real12) :: tol, rsold, rsnew, alpha, dot

    ! Calculate initial residual. This is the difference between b and Ax.
    r = b - MATMUL(A, x)
   
    ! Initially, the direction vector is set to the residual.
    p = r
   
    ! Calculate the square of the norm of the residual. This will be used to test for convergence.
    rsold = DOT_PRODUCT(r, r)

    ! Initialize the iteration counter to 0.
    iter = 0
   
    ! Main loop: iterate until the number of iterations reaches itmax or the norm of the residual falls below tol.
    DO WHILE (iter < itmax .AND. SQRT(rsold) > tol)
        iter = iter + 1
       
        ! Calculate the product of A and the direction vector.
        Ap = MATMUL(A, p)
       
        ! Calculate the dot product of p and Ap.
        dot = DOT_PRODUCT(p, Ap)
       
        ! Calculate the step size.
        alpha = rsold / dot

        ! Update the solution vector.
        x = x + alpha * p
       
        ! Update the residual.
        r = r - alpha * Ap

        ! Calculate the square of the norm of the new residual.
        rsnew = DOT_PRODUCT(r, r)
       
        ! If the norm of the new residual falls below tol, terminate the loop.
        IF (SQRT(rsnew) < tol) EXIT

        ! Update the direction vector using a combination of the new residual and the old direction.
        p = r + (rsnew/rsold) * p
       
        ! Update rsold to be the square of the norm of the new residual.
        rsold = rsnew
    ENDDO
END SUBROUTINE conjugate_gradient

end module MATRIX_INVERSION

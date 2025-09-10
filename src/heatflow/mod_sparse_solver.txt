module sparse_solver

  use mkl_pardiso
  implicit none

  private

  public :: coo2csr, &
            bicgstab, &
            solve_pardiso
  
contains

  subroutine coo2csr( nrow, &
                      nnz, &
                      a, &
                      ir, &
                      jc, &
                      acsr, &
                      ja, &
                      ia )
    
    !--------------------------------------------------------------------------!
    !! coocsr converts coo to csr.
    !
    !  discussion:
    !
    !    this routine converts a matrix that is stored in coo coordinate format
    !    a, ir, jc into a csr row general sparse acsr, ja, ia format.
    !
    !  parameters:
    !
    !    input, integer nrow, the row dimension of the matrix.
    !
    !    input, integer nnz, the number of nonzero elements in the matrix.
    !
    ! a,
    ! ir,
    ! jc    = matrix in coordinate format. a(k), ir(k), jc(k) store the nnz
    !         nonzero elements of the matrix with a(k) = actual real value of
    !         the elements, ir(k) = its row number and jc(k) = its column
    !        number. the order of the elements is arbitrary.
    !
    ! on return:
    !
    ! ir       is destroyed
    !
    !    output, real acsr(nnz), ja(nnz), ia(nrow+1), the matrix in csr
    !    compressed sparse row format.
    !--------------------------------------------------------------------------!

    ! Arguments.
    integer (kind=8), intent(in) :: nrow
    integer (kind=8), intent(in) :: nnz
    real(8), dimension(nnz), intent(in) :: a
    integer (kind=8), dimension(nnz), intent(in) :: ir
    integer (kind=8), dimension(nnz), intent(in) :: jc   
    real(8), dimension(nnz), intent(out) :: acsr
    integer, dimension(nnz), intent(out) :: ja
    integer, dimension(nrow+1), intent(out) :: ia

    ! Local variables.
    integer (kind=8) :: i, iad, j, k, k0
    real(kind=8) :: x
    
    ia(1:nrow+1) = 0
    
    ! determine the row lengths.
    
    do k = 1, nnz
       ia(ir(k)) = ia(ir(k)) + 1
    end do
    
    ! the starting position of each row.
    
    k = 1

    do j = 1, nrow + 1

       k0 = ia(j)
       ia(j) = k
       k = k + k0

    end do
    
    ! go through the structure once more. fill in output matrix.
    
    do k = 1, nnz

       i = ir(k)
       j = jc(k)
       x = a(k)
       iad = ia(i)
       acsr(iad) = x
       ja(iad) = j
       ia(i) = iad + 1

    end do
    
    ! shift back ia.
    
    do j = nrow, 1, -1
       ia(j+1) = ia(j)
    end do

    ia(1) = 1
    
    return
    
  end subroutine coo2csr
  
  !-------------------------------------------------------------------
  !	BiConjugate Gradient (Stabilised) Method
  !-------------------------------------------------------------------
  
  subroutine bicgstab( acsr, &
                       ia, &
                       ja, &
                       b, &
                       maxiter, &
                       initGuess, &
                       x, &
                       iter )

    ! Arguments
    real(8), dimension(:), intent(in) :: acsr
    integer (kind=8), dimension(:), intent(in) :: ia
    integer (kind=8), dimension(:), intent(in) :: ja
    real(8), dimension(:), intent(in) :: b
    integer, intent(in) :: maxiter
    real(8), dimension(:), intent(in) :: initGuess
    real(8), dimension(:), allocatable, intent(out) :: x
    integer, intent(out) :: iter

    ! Local variables
    integer :: i, j, k, n
    real(8), parameter :: cc = 1.0e-9
    real(8) :: alpha,beta,delta0,delta,delta_old,omega
    real(8), dimension(:), allocatable :: r, p, s, rst, temp1, temp2
    
    n = size(b,1)
    
    allocate(x(n))
    allocate(r(n))
    allocate(p(n))
    allocate(s(n))
    allocate(rst(n))
    allocate(temp1(n))
    allocate(temp2(n))
    
    call mkl_dcsrgemv("N",n,acsr,ia,ja,x,temp1)
    
    r = b - temp1

    call random_number(rst)
    
    p = r

    delta = dot_product(rst,r)

    write(*,'(a,1x,f15.3)') "Starting delta: ", delta
    
    delta0 = delta

    do i = 1, maxiter
       
       if ( norm2(r) /= norm2(r) ) then
          write(*,'(a)') "Error in solver: residual NaN"
          exit
       end if

       if(mod(i,1000).eq.0) then
          write(*,'(a,1x,i6)') 'Iteration number: ',i
          write(*,'(a,1x,f15.3)') "Residual ratio: ", norm2(r)/cc
       end if
       
       call mkl_dcsrgemv("N",n,acsr,ia,ja,p,temp1)	! temp1=A*p
       
       alpha = delta/dot_product(rst,temp1)
       s = r - alpha*temp1
       
       call mkl_dcsrgemv("N",n,acsr,ia,ja,s,temp2)	! temp2=A*s
       
       omega = dot_product(s,temp2)/dot_product(temp2,temp2)
       x = x + alpha*p + omega*s
       r = s - omega*temp2
       delta_old = delta
       delta = dot_product(rst,r)
       beta = (delta/delta_old)*(alpha/omega)
       p = r + beta*(p - omega*temp1)

       if(norm2(r) .lt. cc) then
          iter = i
          return
       end if
       
       if(i.eq.maxiter) then
          write(*,'(a)') "Maximum iterations reached."
          write(*,'(a)') "Convergence not achieved."
          write(*,'(a,1x,f15.3)') "Norm of residual: ", norm2(r)
          write(*,'(a,1x,f15.3)') "Convergence criterion: ", cc
          if((norm2(r)/cc) .lt. 2.d0) then
             write(*,'(a)') "The residual is within a small",&
                  "range of the convergence criterion."
             write(*,'(a)') "Perhaps increasing iteration ",	&
                  "count may help."
          end if
       end if
       
    end do
    
  end subroutine bicgstab
  
  !-------------------------------------------------------------------
  !	END BiConjugate Gradient (Stabilised) Method
  !-------------------------------------------------------------------
  
  !-------------------------------------------------------------------
  !	PARDISO Direct Solver
  !-------------------------------------------------------------------
  
  subroutine solve_pardiso( acsr, &
                            b, &
                            ia, &
                            ja, &
                            x )
    
    use mkl_pardiso

    ! Arguments
    real(8), dimension(:), intent(in) :: acsr
    real(8), dimension(:), intent(inout) :: b
    integer,dimension(:), intent(in) :: ia
    integer,dimension(:), intent(in) :: ja
    real(8), dimension(:), allocatable, intent(out) :: x

    ! Local variables
    type(mkl_pardiso_handle), dimension(:), allocatable  :: pt
    integer :: i,maxfct,mnum,mtype,phase,n,nrhs,error,msglvl,nnz,error1
    integer, dimension(:), allocatable :: iparm  
    integer,dimension(1) :: idum
    real(8),dimension(1) :: ddum
    
    n = size(b,1)
    nnz = size(acsr,1)
    nrhs = 1
    maxfct = 1
    mnum = 1
    
    if (.not.(allocated(x))) allocate(x(n))



    allocate(iparm(64))		!set up pardiso control parameter
    
    do i=1,64
       iparm(i) = 0
    end do
    
    iparm(1) = 1 ! no solver default
    iparm(2) = 2 ! fill-in reordering from metis
    iparm(4) = 0 ! no iterative-direct algorithm
    iparm(5) = 0 ! no user fill-in reducing permutation
    iparm(6) = 0 ! =0 solution on the first n compoments of x
    iparm(8) = 2 ! numbers of iterative refinement steps
    iparm(10) = 13 ! perturbe the pivot elements with 1e-13
    iparm(11) = 1 ! use nonsymmetric permutation and scaling mps
    iparm(13) = 0 ! maximum weighted matching algorithm is
    !switched-off (default for symmetric).
    ! try iparm(13) = 1 in case of inaccuracy
    iparm(14) = 0 ! output: number of perturbed pivots
    iparm(18) = -1 ! output: number of nonzeros in the factor lu
    iparm(19) = -1 ! output: mflops for lu factorization
    iparm(20) = 0 ! output: numbers of cg iterations
    
    error  = 0 ! initialize error flag
    msglvl = 0 ! 0=no output, 1=print statistical information
    mtype  = 11 ! real and unsymmetric matrix
    
    ! Initiliaze the internal solver memory pointer.
    ! This is only necessary for the first call of the solver.
    
    allocate (pt(64))
    do i=1,64
       pt(i)%dummy =  0
    end do
    
    phase = 11 ! Only reordering and symbolic factorization
    
    call pardiso (pt,maxfct,mnum,mtype,phase,n,acsr,ia,ja, &
         idum, nrhs, iparm, msglvl, ddum, ddum, error)
    
    if (error /= 0) then
       write(*,*) 'the following error was detected: ', error
       goto 1000
    end if
    
    phase = 22 ! only factorization
    call pardiso (pt,maxfct,mnum,mtype,phase,n,acsr,ia,ja, &
         idum, nrhs, iparm, msglvl, ddum, ddum, error)
    if (error /= 0) then
       write(*,*) 'the following error was detected: ', error
       goto 1000
    endif
    
    ! back substitution and iterative refinement
    iparm(8) = 2 ! max numbers of iterative refinement steps
    phase = 33 ! only solving
    call pardiso (pt,maxfct,mnum,mtype,phase,n,acsr,ia,ja, &
         idum, nrhs, iparm, msglvl, b, x, error)
    write(*,*) 'solve completed ... '
    if (error /= 0) then
       write(*,*) 'the following error was detected: ', error
       goto 1000
    endif
    
1000 continue
    ! termination and release of memory
    phase = -1 ! release internal memory
    call pardiso (pt,maxfct,mnum,mtype,phase,n,ddum,idum,idum, 	&
         idum, nrhs, iparm, msglvl, ddum, ddum, error1)
    
    if (error1 /= 0) then
       write(*,*) 'the following release error was detected: ',	&
            error1
       stop 1
    endif
    
    if ( error /= 0 ) stop 1
    
  end subroutine solve_pardiso
  
  !-------------------------------------------------------------------
  !	END PARDISO Direct Solver
  !-------------------------------------------------------------------
  
end module sparse_solver

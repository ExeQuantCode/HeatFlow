program solve_matrix 
  use sptype ! this is where the Sparse formats are defined
  use globe_data ! where matrix A and Sparse forms are decleared
  use sparse! this is where the linbcg is writen
  implicit none
  ! Definitions for setting up the matrix problem.
  real(DP), dimension(:), allocatable :: b, x
  real(DP), PARAMETER :: TINY=1.0e-9_dp
  ! Output variables from the subroutine.
  real(DP) :: E
  integer :: ncg

  !Define matrix
  call define_matrix()
  ! Convert the matrix into Sparse Diagonal Storage.
  call SDSin(A,TINY, da)
  ! Convert the matrix into Sparse Row Storage.
  call SRSin(A,TINY, ra)
  !write matrix
  call write_matrix()


!!!#################################################
!!! Call the CG method to solve the equation Ax=b.
!!!#################################################
  ! b:     Input - the b vector.
  ! x:     Input/Output - initial guess for x, overwritten with the final solution.
  ! itol:  Input - sets the tolerance method used to calculate error.
  ! tol:   Input - sets the convergence criteria.
  ! itmax: Input - sets the max number of iterations.
  ! iter:  Output - gives the number of the final iteration.
  ! err:   Output - records the error of the final iteration.
  ! iss:   Input - sets the Sparse Storage type (1=SRS, 2=SDS).
  call linbcg(b,x,itol=1,tol=1D-9,itmax=500,iter=ncg,err=E,iss=1)
!!!#################################################

  ! Write out the final solution, rounded to 4 decimal places.
  write(6,*) "This is the solution by the iterative CG method"
  write(6,'(12F10.4)') x 

!!!#################################################
!!!            E N D   O F   C O D E
!!!#################################################



contains
  subroutine define_matrix()
  integer :: N
!!!#################################################
!!! Define a 5 by 5 matrix, the b vector, and the initial x.
!!!#################################################
    N=5
    allocate(A(N,N),x(N),b(N))
    A = reshape([2.d0,1.d0,0.d0,0.d0,0.d0, &
         1.d0,2.d0,1.d0,0.d0,0.d0, &
         0.d0,1.d0,2.d0,1.d0,0.d0, &
         0.d0,0.d0,1.d0,2.d0,1.d0, &
         0.d0,0.d0,0.d0,1.d0,2.d0], [5,5])

    ! Define b vector.
    b = [1.d0,2.d0,3.d0,4.d0,5.d0]
    ! Set the initial guess for x.
    x = [0.d0,0.d0,0.d0,0.d0,0.d0]
!!!#################################################
  end subroutine define_matrix

  subroutine write_matrix()
  ! Variables for output formatting.
  character(12) :: form
  integer :: i,j
  integer :: N
!!!#################################################
!!! Display the A matrix (suitable for smaller matrices).
!!!#################################################
    N=5
    ! Define output string format.
    write(form, '(A,I0,A)') '(', N, 'I3)'
    write(6,*) '  ';   write(6,*) '  '

    ! Write the matrix.
    do i=1,N
       write(6,form) (int(A(i,j)), j = 1, N)
    end do
    write(6,*) '  ';   write(6,*) '  '
!!!#################################################
  end subroutine write_matrix

end program

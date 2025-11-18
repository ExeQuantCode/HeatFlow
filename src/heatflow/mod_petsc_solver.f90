module petsc_solver
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscksp.h"
  use petscksp
  implicit none
  private
  public :: petsc_init, petsc_finalize, solve_petsc_csr

contains

  subroutine petsc_init()
    integer :: ierr
    call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
  end subroutine petsc_init

  subroutine petsc_finalize()
    integer :: ierr
    call PetscFinalize(ierr)
  end subroutine petsc_finalize

  subroutine solve_petsc_csr(n, ia, ja, aval, b, x, rtol, maxit)
    integer,  intent(in) :: n
    integer,  intent(in) :: ia(:), ja(:)
    real(8),  intent(in) :: aval(:), b(:)
    real(8),  intent(inout) :: x(:)
    real(8),  intent(in) :: rtol
    integer,  intent(in) :: maxit
    Mat :: A
    Vec :: bb, xx
    KSP :: ksp
    PC  :: pc
    integer :: ierr, i, row_nz, start_k, its
    integer, allocatable :: cols0(:), idx(:)
    real(8), allocatable :: vals(:)
    real(8), pointer :: xptr(:)
    KSPConvergedReason :: reason
    real(8) :: rnorm

    if (size(ia) /= n+1) stop 'solve_petsc_csr: ia size mismatch'
    if (size(b) /= n .or. size(x) /= n) stop 'solve_petsc_csr: vector size mismatch'

    ! Create matrix with an estimated 7 nonzeros/row (adjust if needed)
    call MatCreateSeqAIJ(PETSC_COMM_SELF, n, n, 7, PETSC_NULL_INTEGER, A, ierr)

    ! Fill matrix from CSR format (ia, ja are 1-based Fortran indexing)
    do i = 1, n
       row_nz = ia(i+1) - ia(i)
       if (row_nz > 0) then
          start_k = ia(i)
          allocate(cols0(row_nz), vals(row_nz))
          ! Convert column indices from 1-based to 0-based for PETSc
          cols0 = ja(start_k:start_k+row_nz-1) - 1
          vals  = aval(start_k:start_k+row_nz-1)
          
          ! Set row i-1 (0-based) with column indices cols0 (0-based)
          call MatSetValues(A, 1, (/i-1/), row_nz, cols0, vals, INSERT_VALUES, ierr)
          deallocate(cols0, vals)
       end if
    end do
    call MatAssemblyBegin(A, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(A, MAT_FINAL_ASSEMBLY, ierr)
    
    ! Optional: Verify matrix assembly (uncomment for debugging)
    ! call MatView(A, PETSC_VIEWER_STDOUT_SELF, ierr)

    ! Create vectors
    call VecCreateSeq(PETSC_COMM_SELF, n, bb, ierr)
    call VecCreateSeq(PETSC_COMM_SELF, n, xx, ierr)

    ! Set RHS and initial guess
    allocate(idx(n))
    idx = [(i-1, i=1,n)]
    call VecSetValues(bb, n, idx, b, INSERT_VALUES, ierr)
    call VecAssemblyBegin(bb,ierr); call VecAssemblyEnd(bb,ierr)

    call VecSetValues(xx, n, idx, x, INSERT_VALUES, ierr)
    call VecAssemblyBegin(xx,ierr); call VecAssemblyEnd(xx,ierr)

    ! KSP setup
    call KSPCreate(PETSC_COMM_SELF, ksp, ierr)
    call KSPSetOperators(ksp, A, A, ierr)     ! 3-arg form (reuse automatically)
    call KSPGetPC(ksp, pc, ierr)
    call PCSetType(pc, PCJACOBI, ierr)        ! Use Jacobi (diagonal) preconditioner to match linbcg
    call KSPSetType(ksp, KSPBCGS, ierr)       ! Use BCGS to match linbcg behavior
    
    ! Set convergence tolerances
    ! rtol = relative tolerance, atol = absolute tolerance (use default), dtol = divergence tolerance, maxits = max iterations
    call KSPSetTolerances(ksp, rtol, PETSC_DEFAULT_REAL, PETSC_DEFAULT_REAL, maxit, ierr)
    
    ! Use unpreconditioned norm (matching linbcg with itol=1)
    call KSPSetNormType(ksp, KSP_NORM_UNPRECONDITIONED, ierr)
    
    ! Allow command line override of solver options
    call KSPSetFromOptions(ksp, ierr)

    call KSPSolve(ksp, bb, xx, ierr)
    
    ! Check convergence
    call KSPGetConvergedReason(ksp, reason, ierr)
    call KSPGetIterationNumber(ksp, its, ierr)
    call KSPGetResidualNorm(ksp, rnorm, ierr)
    
    ! Report convergence status (commented out by default for performance)
    ! Uncomment the next line to see convergence info every solve:
    ! write(*,'(A,I0,A,ES12.5,A,I0)') ' PETSc: iterations=', its, ', residual=', rnorm, ', reason=', reason
    
    if (reason < 0) then
       write(0,*) "WARNING: PETSc solver diverged or failed!"
       write(0,*) "  Reason code:", reason
       write(0,*) "  Iterations:", its
       write(0,*) "  Residual norm:", rnorm
       ! Don't stop - let the main code detect NaNs if needed
    end if

    ! Extract solution
    call VecGetArrayF90(xx, xptr, ierr)
    x = xptr
    call VecRestoreArrayF90(xx, xptr, ierr)

    ! Cleanup
    deallocate(idx)
    call KSPDestroy(ksp, ierr)
    call VecDestroy(bb, ierr)
    call VecDestroy(xx, ierr)
    call MatDestroy(A, ierr)
  end subroutine solve_petsc_csr

end module petsc_solver
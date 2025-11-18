module petsc_solver
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscksp.h"
  use petscksp
  implicit none
  private
  public :: petsc_init, petsc_finalize, solve_petsc_csr, petsc_cleanup

  ! Persistent PETSc objects (reused across timesteps for memory efficiency)
  Mat, save :: A_saved = PETSC_NULL_MAT
  Vec, save :: bb_saved = PETSC_NULL_VEC
  Vec, save :: xx_saved = PETSC_NULL_VEC
  KSP, save :: ksp_saved = PETSC_NULL_KSP
  logical, save :: initialized = .false.
  integer, save :: n_saved = 0

contains

  subroutine petsc_init()
    integer :: ierr
    call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
  end subroutine petsc_init

  subroutine petsc_finalize()
    integer :: ierr
    call petsc_cleanup()
    call PetscFinalize(ierr)
  end subroutine petsc_finalize

  subroutine petsc_cleanup()
    ! Clean up persistent PETSc objects
    integer :: ierr
    if (A_saved /= PETSC_NULL_MAT) call MatDestroy(A_saved, ierr)
    if (bb_saved /= PETSC_NULL_VEC) call VecDestroy(bb_saved, ierr)
    if (xx_saved /= PETSC_NULL_VEC) call VecDestroy(xx_saved, ierr)
    if (ksp_saved /= PETSC_NULL_KSP) call KSPDestroy(ksp_saved, ierr)
    A_saved = PETSC_NULL_MAT
    bb_saved = PETSC_NULL_VEC
    xx_saved = PETSC_NULL_VEC
    ksp_saved = PETSC_NULL_KSP
    initialized = .false.
    n_saved = 0
  end subroutine petsc_cleanup

  subroutine solve_petsc_csr(n, ia, ja, aval, b, x, rtol, maxit)
    integer,  intent(in) :: n
    integer,  intent(in) :: ia(:), ja(:)
    real(8),  intent(in) :: aval(:), b(:)
    real(8),  intent(inout) :: x(:)
    real(8),  intent(in) :: rtol
    integer,  intent(in) :: maxit
    
    PC  :: pc
    integer :: ierr, i, row_nz, start_k, its
    integer, allocatable :: cols0(:), idx(:), d_nnz(:)
    real(8), allocatable :: vals(:)
    real(8), pointer :: xptr(:)
    KSPConvergedReason :: reason
    real(8) :: rnorm
    logical :: rebuild_needed

    if (size(ia) /= n+1) stop 'solve_petsc_csr: ia size mismatch'
    if (size(b) /= n .or. size(x) /= n) stop 'solve_petsc_csr: vector size mismatch'

    ! Determine if we need to rebuild the matrix structure
    rebuild_needed = .false.
    if (.not. initialized) rebuild_needed = .true.
    if (n /= n_saved) rebuild_needed = .true.
    
    ! Create PETSc objects on first call or if size changed
    if (rebuild_needed) then
      ! Clean up old objects if they exist
      if (initialized) call petsc_cleanup()
      
      ! Preallocate matrix with exact nonzeros per row (saves memory)
      allocate(d_nnz(n))
      do i = 1, n
        d_nnz(i) = ia(i+1) - ia(i)
      end do
      call MatCreateSeqAIJ(PETSC_COMM_SELF, n, n, 0, d_nnz, A_saved, ierr)
      deallocate(d_nnz)
      
      ! Create persistent vectors
      call VecCreateSeq(PETSC_COMM_SELF, n, bb_saved, ierr)
      call VecCreateSeq(PETSC_COMM_SELF, n, xx_saved, ierr)
      
      ! Create and configure KSP solver (persistent across timesteps)
      call KSPCreate(PETSC_COMM_SELF, ksp_saved, ierr)
      call KSPSetOperators(ksp_saved, A_saved, A_saved, ierr)
      call KSPGetPC(ksp_saved, pc, ierr)
      call PCSetType(pc, PCJACOBI, ierr)        ! Jacobi preconditioner
      call KSPSetType(ksp_saved, KSPBCGS, ierr) ! BiCGSTAB solver
      call KSPSetTolerances(ksp_saved, rtol, PETSC_DEFAULT_REAL, &
                           PETSC_DEFAULT_REAL, maxit, ierr)
      call KSPSetNormType(ksp_saved, KSP_NORM_UNPRECONDITIONED, ierr)
      call KSPSetFromOptions(ksp_saved, ierr)
      
      initialized = .true.
      n_saved = n
    end if

    ! Update matrix values (always needed each timestep)
    call MatZeroEntries(A_saved, ierr)
    do i = 1, n
       row_nz = ia(i+1) - ia(i)
       if (row_nz > 0) then
          start_k = ia(i)
          allocate(cols0(row_nz), vals(row_nz))
          ! Convert column indices from 1-based to 0-based for PETSc
          cols0 = ja(start_k:start_k+row_nz-1) - 1
          vals  = aval(start_k:start_k+row_nz-1)
          
          ! Set row i-1 (0-based) with column indices cols0 (0-based)
          call MatSetValues(A_saved, 1, (/i-1/), row_nz, cols0, vals, INSERT_VALUES, ierr)
          deallocate(cols0, vals)
       end if
    end do
    call MatAssemblyBegin(A_saved, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(A_saved, MAT_FINAL_ASSEMBLY, ierr)
    
    ! Optional: Verify matrix assembly (uncomment for debugging)
    ! call MatView(A_saved, PETSC_VIEWER_STDOUT_SELF, ierr)

    ! Update RHS vector
    allocate(idx(n))
    idx = [(i-1, i=1,n)]
    call VecSetValues(bb_saved, n, idx, b, INSERT_VALUES, ierr)
    call VecAssemblyBegin(bb_saved,ierr); call VecAssemblyEnd(bb_saved,ierr)

    ! Update initial guess
    call VecSetValues(xx_saved, n, idx, x, INSERT_VALUES, ierr)
    call VecAssemblyBegin(xx_saved,ierr); call VecAssemblyEnd(xx_saved,ierr)
    deallocate(idx)

    ! Solve the system (reusing persistent KSP)
    call KSPSolve(ksp_saved, bb_saved, xx_saved, ierr)
    
    ! Check convergence
    call KSPGetConvergedReason(ksp_saved, reason, ierr)
    call KSPGetIterationNumber(ksp_saved, its, ierr)
    call KSPGetResidualNorm(ksp_saved, rnorm, ierr)
    
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
    call VecGetArrayF90(xx_saved, xptr, ierr)
    x = xptr
    call VecRestoreArrayF90(xx_saved, xptr, ierr)

  end subroutine solve_petsc_csr

end module petsc_solver
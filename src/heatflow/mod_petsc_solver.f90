module petsc_solver
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscksp.h"
  use petscksp
  use iso_c_binding
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
    PetscInt :: idx_array(1)
    PetscScalar :: val_array(1)
    real(8), allocatable :: vals(:)
    real(8) :: rnorm
    logical :: rebuild_needed

    ! write(*,'(A,I0)') ' [DEBUG] Entered solve_petsc_csr, n=', n
    ! call flush(6)
    
    if (size(ia) /= n+1) stop 'solve_petsc_csr: ia size mismatch'
    if (size(b) /= n .or. size(x) /= n) stop 'solve_petsc_csr: vector size mismatch'

    ! write(*,'(A)') ' [DEBUG] Size checks passed'
    ! call flush(6)

    ! Determine if we need to rebuild the matrix structure
    rebuild_needed = .false.
    if (.not. initialized) rebuild_needed = .true.
    if (n /= n_saved) rebuild_needed = .true.
    
    ! write(*,'(A,L1)') ' [DEBUG] rebuild_needed=', rebuild_needed
    ! call flush(6)
    
    ! Create PETSc objects on first call or if size changed
    if (rebuild_needed) then
      ! write(*,'(A)') ' [DEBUG] Starting PETSc object creation...'
      ! call flush(6)
      
      ! Clean up old objects if they exist
      if (initialized) call petsc_cleanup()
      
      ! write(*,'(A)') ' [DEBUG] Preallocating matrix...'
      ! call flush(6)
      
      ! Preallocate matrix with exact nonzeros per row (saves memory)
      allocate(d_nnz(n))
      do i = 1, n
        d_nnz(i) = ia(i+1) - ia(i)
      end do
      
      ! write(*,'(A,I0,A,I0)') ' [DEBUG] Creating matrix: n=', n, ', max_nnz/row=', maxval(d_nnz)
      ! call flush(6)
      
      ! Create matrix with exact preallocation (most memory-efficient)
      call MatCreateSeqAIJ(PETSC_COMM_SELF, n, n, 0, d_nnz, A_saved, ierr)
      if (ierr /= 0) then
        write(0,*) "ERROR: MatCreateSeqAIJ failed with ierr=", ierr
        write(0,*) "  Matrix size may exceed system limits"
        write(0,*) "  n=", n, ", nnz=", sum(int(d_nnz,8)), ", max_nnz/row=", maxval(d_nnz)
        stop
      end if
      
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

    ! Update RHS vector in batches to avoid memory issues with huge systems
    block
      integer, parameter :: VEC_CHUNK = 1000000
      integer :: vec_start, vec_end, vec_len, k
      integer, allocatable :: idx_vec(:)
      
      do vec_start = 1, n, VEC_CHUNK
        vec_end = min(vec_start + VEC_CHUNK - 1, n)
        vec_len = vec_end - vec_start + 1
        
        allocate(idx_vec(vec_len))
        idx_vec = [(vec_start + k - 2, k=1,vec_len)]  ! 0-based indices
        
        call VecSetValues(bb_saved, vec_len, idx_vec, b(vec_start:vec_end), INSERT_VALUES, ierr)
        deallocate(idx_vec)
      end do
    end block
    call VecAssemblyBegin(bb_saved,ierr); call VecAssemblyEnd(bb_saved,ierr)

    ! Update initial guess in batches
    block
      integer, parameter :: VEC_CHUNK = 1000000
      integer :: vec_start, vec_end, vec_len, k
      integer, allocatable :: idx_vec(:)
      
      do vec_start = 1, n, VEC_CHUNK
        vec_end = min(vec_start + VEC_CHUNK - 1, n)
        vec_len = vec_end - vec_start + 1
        
        allocate(idx_vec(vec_len))
        idx_vec = [(vec_start + k - 2, k=1,vec_len)]  ! 0-based indices
        
        call VecSetValues(xx_saved, vec_len, idx_vec, x(vec_start:vec_end), INSERT_VALUES, ierr)
        deallocate(idx_vec)
      end do
    end block
    call VecAssemblyBegin(xx_saved,ierr); call VecAssemblyEnd(xx_saved,ierr)

    ! Solve the system
    call KSPSolve(ksp_saved, bb_saved, xx_saved, ierr)
    
    if (ierr /= 0) then
       write(0,*) "ERROR: KSPSolve failed with error code:", ierr
       stop
    end if
    
    call KSPGetIterationNumber(ksp_saved, its, ierr)
    call KSPGetResidualNorm(ksp_saved, rnorm, ierr)

    ! Extract solution vector using batched VecGetValues
    block
      integer, parameter :: CHUNK_SIZE = 100000
      PetscInt, allocatable :: idx_batch(:)
      PetscScalar, allocatable :: val_batch(:)
      integer :: i_start, i_end, chunk_len, j
      
      do i_start = 1, n, CHUNK_SIZE
        i_end = min(i_start + CHUNK_SIZE - 1, n)
        chunk_len = i_end - i_start + 1
        
        allocate(idx_batch(chunk_len), val_batch(chunk_len))
        
        do j = 1, chunk_len
          idx_batch(j) = i_start + j - 2
        end do
        
        call VecGetValues(xx_saved, chunk_len, idx_batch, val_batch, ierr)
        x(i_start:i_end) = val_batch(1:chunk_len)
        
        deallocate(idx_batch, val_batch)
      end do
    end block

  end subroutine solve_petsc_csr

end module petsc_solver
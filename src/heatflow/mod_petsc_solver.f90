module petsc_solver
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscksp.h"
  use petscksp
  use iso_c_binding
  implicit none
  private
  public :: petsc_init, petsc_finalize, solve_petsc_csr, petsc_cleanup

  ! ===== PRECONDITIONER SELECTION =====
  ! Change this to switch between preconditioners:
  ! 'GAMG' = Algebraic Multigrid (best for elliptic PDEs, 10-20x faster)
  ! 'ILU'  = Incomplete LU (good general purpose, robust)
  ! 'LU'   = Direct solver (most robust, uses more memory)
  character(len=10), parameter :: PRECONDITIONER = 'LU'  ! <-- Change here!
  ! ====================================

  ! Persistent PETSc objects (reused across timesteps for memory efficiency)
  Mat, save :: A_saved
  Vec, save :: bb_saved
  Vec, save :: xx_saved
  KSP, save :: ksp_saved
  logical, save :: initialized = .false.
  integer, save :: n_saved = 0
  logical, save :: petsc_objects_nulled = .false.

contains

  subroutine petsc_init()
    integer :: ierr
    ! write(*,'(A DEBUG] Calling PetscInitialize...'
    ! call flush(6)
    call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
    ! write(*,'(A DEBUG] PetscInitialize returned ierr=', ierr
    ! call flush(6)
    ! Initialize null objects after PETSc is initialized
    if (.not. petsc_objects_nulled) then
      A_saved = PETSC_NULL_MAT
      bb_saved = PETSC_NULL_VEC
      xx_saved = PETSC_NULL_VEC
      ksp_saved = PETSC_NULL_KSP
      petsc_objects_nulled = .true.
    end if
  end subroutine petsc_init

  subroutine petsc_finalize()
    integer :: ierr
    call petsc_cleanup()
    call PetscFinalize(ierr)
  end subroutine petsc_finalize

  subroutine petsc_cleanup()
    ! Clean up persistent PETSc objects
    integer :: ierr
    if (initialized) then
      call MatDestroy(A_saved, ierr)
      call VecDestroy(bb_saved, ierr)
      call VecDestroy(xx_saved, ierr)
      call KSPDestroy(ksp_saved, ierr)
      A_saved = PETSC_NULL_MAT
      bb_saved = PETSC_NULL_VEC
      xx_saved = PETSC_NULL_VEC
      ksp_saved = PETSC_NULL_KSP
      initialized = .false.
      n_saved = 0
    end if
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
    PetscInt, allocatable :: cols0(:), d_nnz(:)
    PetscInt :: np, row_nzp, zerop
    real(8), allocatable :: vals(:)
    real(8) :: rnorm
    logical :: rebuild_needed

    ! ! write(*,'(A DEBUG] Entered solve_petsc_csr, n=', n
    ! ! call flush(6)
    
    ! write(*,'(A DEBUG] Entered solve_petsc_csr, n=', n
    ! call flush(6)
    
    ! write(*,'(A DEBUG] size(ia)=', size(ia)
    ! write(*,'(A DEBUG] size(ja)=', size(ja)
    ! write(*,'(A DEBUG] size(aval)=', size(aval)
    ! write(*,'(A DEBUG] size(b)=', size(b)
    ! write(*,'(A DEBUG] size(x)=', size(x)
    ! call flush(6)
    
    if (size(ia) /= n+1) stop 'solve_petsc_csr: ia size mismatch'
    if (size(b) /= n .or. size(x) /= n) stop 'solve_petsc_csr: vector size mismatch'

    ! write(*,'(A DEBUG] Size checks passed'
    ! call flush(6)

    ! Determine if we need to rebuild the matrix structure
    rebuild_needed = .false.
    if (.not. initialized) rebuild_needed = .true.
    if (n /= n_saved) rebuild_needed = .true.
    
    ! write(*,'(A DEBUG] rebuild_needed=', rebuild_needed
    ! call flush(6)
    
    ! Create PETSc objects on first call or if size changed
    if (rebuild_needed) then
      ! write(*,'(A DEBUG] Starting PETSc object creation...'
      ! call flush(6)
      
      ! Clean up old objects if they exist
      if (initialized) call petsc_cleanup()
      
      ! write(*,'(A DEBUG] Preallocating matrix...'
      ! call flush(6)
      
      ! Preallocate matrix with exact nonzeros per row (saves memory)
      allocate(d_nnz(n))
      
      ! write(*,'(A DEBUG] d_nnz allocated'
      ! call flush(6)
      
      np = n
      zerop = 0
      
      do i = 1, n
        d_nnz(i) = int(ia(i+1) - ia(i), kind=kind(d_nnz))
      end do
      
      ! write(*,'(A DEBUG] Creating matrix: n=', n, ', max_nnz/row=', maxval(d_nnz)
      ! write(*,'(A DEBUG] np=', np
      ! write(*,'(A DEBUG] zerop=', zerop
      ! write(*,'(A DEBUG] kind(d_nnz)=', kind(d_nnz)
      ! write(*,'(A DEBUG] size(d_nnz)=', size(d_nnz)
      ! call flush(6)
      
      ! Create matrix with exact preallocation (most memory-efficient)
      ! write(*,'(A DEBUG] Calling MatCreate...'
      ! call flush(6)
      
      ! Use a temporary local Mat object first, then assign
      block
        Mat :: A_temp
        call MatCreate(PETSC_COMM_SELF, A_temp, ierr)
        ! write(*,'(A DEBUG] MatCreate returned ierr=', ierr
        ! call flush(6)
        
        if (ierr == 0) then
          ! write(*,'(A DEBUG] Calling MatSetSizes...'
          ! call flush(6)
          call MatSetSizes(A_temp, np, np, np, np, ierr)
          ! write(*,'(A DEBUG] MatSetSizes returned ierr=', ierr
          ! call flush(6)
          
          ! write(*,'(A DEBUG] Calling MatSetType...'
          ! call flush(6)
          call MatSetType(A_temp, MATSEQAIJ, ierr)
          ! write(*,'(A DEBUG] MatSetType returned ierr=', ierr
          ! call flush(6)
          
          ! write(*,'(A DEBUG] Calling MatSeqAIJSetPreallocation...'
          ! call flush(6)
          call MatSeqAIJSetPreallocation(A_temp, zerop, d_nnz, ierr)
          ! write(*,'(A DEBUG] MatSeqAIJSetPreallocation returned ierr=', ierr
          ! call flush(6)
          
          A_saved = A_temp
        end if
      end block
      
      ! write(*,'(A DEBUG] Matrix created and assigned'
      ! call flush(6)
      
      if (ierr /= 0) then
        write(0,*) "ERROR: MatCreateSeqAIJ failed with ierr=", ierr
        write(0,*) "  Matrix size may exceed system limits"
        write(0,*) "  n=", n, ", nnz=", sum(int(d_nnz,8)), ", max_nnz/row=", maxval(d_nnz)
        stop
      end if
      
      deallocate(d_nnz)
      
      ! write(*,'(A DEBUG] Creating vectors...'
      ! call flush(6)
      
      ! Create persistent vectors - use local temps like we did for matrix
      block
        Vec :: bb_temp, xx_temp
        call VecCreateSeq(PETSC_COMM_SELF, np, bb_temp, ierr)
        ! write(*,'(A DEBUG] VecCreateSeq bb returned ierr=', ierr
        ! call flush(6)
        bb_saved = bb_temp
        
        call VecCreateSeq(PETSC_COMM_SELF, np, xx_temp, ierr)
        ! write(*,'(A DEBUG] VecCreateSeq xx returned ierr=', ierr
        ! call flush(6)
        xx_saved = xx_temp
      end block
      
      ! write(*,'(A DEBUG] Creating KSP...'
      ! call flush(6)
      
      ! Create and configure KSP solver (persistent across timesteps)
      block
        KSP :: ksp_temp
        call KSPCreate(PETSC_COMM_SELF, ksp_temp, ierr)
        ! write(*,'(A DEBUG] KSPCreate returned ierr=', ierr
        ! call flush(6)
        ksp_saved = ksp_temp
      end block
      
      call KSPSetOperators(ksp_saved, A_saved, A_saved, ierr)
      ! write(*,'(A DEBUG] KSPSetOperators returned ierr=', ierr
      ! call flush(6)
      
      call KSPGetPC(ksp_saved, pc, ierr)
      ! write(*,'(A DEBUG] KSPGetPC returned ierr=', ierr
      ! call flush(6)
      
      ! Select preconditioner based on parameter at top of module
      select case (trim(PRECONDITIONER))
        case ('GAMG')
          ! Algebraic Multigrid - Best for elliptic PDEs with varying coefficients
          ! Optimal O(1) iterations, 10-20x faster than ILU for large problems
          call PCSetType(pc, PCGAMG, ierr)
          call KSPSetType(ksp_saved, KSPGMRES, ierr)  ! GMRES works well with AMG
          write(*,'(A)') ' [Solver] Using GAMG (Algebraic Multigrid) preconditioner with GMRES'
          
        case ('ILU')
          ! Incomplete LU - Good general purpose, robust
          call PCSetType(pc, PCILU, ierr)
          call KSPSetType(ksp_saved, KSPBCGS, ierr)   ! BiCGSTAB works well with ILU
          write(*,'(A)') ' [Solver] Using ILU preconditioner with BiCGSTAB'
          
        case ('LU')
          ! Direct LU - Most robust, more memory intensive
          call PCSetType(pc, PCLU, ierr)
          call KSPSetType(ksp_saved, KSPPREONLY, ierr) ! Direct solve
          write(*,'(A)') ' [Solver] Using direct LU solver'
          
        case default
          write(*,'(A,A)') ' [Warning] Unknown preconditioner: ', trim(PRECONDITIONER)
          write(*,'(A)') '           Defaulting to ILU'
          call PCSetType(pc, PCILU, ierr)
          call KSPSetType(ksp_saved, KSPBCGS, ierr)
      end select
      
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
          cols0 = int(ja(start_k:start_k+row_nz-1) - 1, kind=kind(cols0))
          vals  = aval(start_k:start_k+row_nz-1)
          
          ! Set row i-1 (0-based) with column indices cols0 (0-based)
          row_nzp = row_nz
          call MatSetValues(A_saved, 1, [PetscInt :: i-1], row_nzp, cols0, vals, INSERT_VALUES, ierr)
          deallocate(cols0, vals)
       end if
    end do
    call MatAssemblyBegin(A_saved, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(A_saved, MAT_FINAL_ASSEMBLY, ierr)
    
    ! write(*,'(A DEBUG] Matrix assembly complete'
    ! call flush(6)
    
    ! Optional: Verify matrix assembly (uncomment for debugging)
    ! call MatView(A_saved, PETSC_VIEWER_STDOUT_SELF, ierr)

    ! Update RHS vector in batches to avoid memory issues with huge systems
    block
      integer, parameter :: VEC_CHUNK = 1000000
      integer :: vec_start, vec_end, vec_len, k
      PetscInt, allocatable :: idx_vec(:)
      PetscInt :: vec_lenp
      
      do vec_start = 1, n, VEC_CHUNK
        vec_end = min(vec_start + VEC_CHUNK - 1, n)
        vec_len = vec_end - vec_start + 1
        
        allocate(idx_vec(vec_len))
        do k = 1, vec_len
          idx_vec(k) = int(vec_start + k - 2, kind=kind(idx_vec))  ! 0-based indices
        end do
        
        vec_lenp = vec_len
        call VecSetValues(bb_saved, vec_lenp, idx_vec, b(vec_start:vec_end), INSERT_VALUES, ierr)
        deallocate(idx_vec)
      end do
    end block
    call VecAssemblyBegin(bb_saved,ierr); call VecAssemblyEnd(bb_saved,ierr)
    
    ! write(*,'(A DEBUG] RHS vector assembly complete'
    ! call flush(6)

    ! Update initial guess in batches
    block
      integer, parameter :: VEC_CHUNK = 1000000
      integer :: vec_start, vec_end, vec_len, k
      PetscInt, allocatable :: idx_vec(:)
      PetscInt :: vec_lenp
      
      do vec_start = 1, n, VEC_CHUNK
        vec_end = min(vec_start + VEC_CHUNK - 1, n)
        vec_len = vec_end - vec_start + 1
        
        allocate(idx_vec(vec_len))
        do k = 1, vec_len
          idx_vec(k) = int(vec_start + k - 2, kind=kind(idx_vec))  ! 0-based indices
        end do
        
        vec_lenp = vec_len
        call VecSetValues(xx_saved, vec_lenp, idx_vec, x(vec_start:vec_end), INSERT_VALUES, ierr)
        deallocate(idx_vec)
      end do
    end block
    call VecAssemblyBegin(xx_saved,ierr); call VecAssemblyEnd(xx_saved,ierr)
    
    ! write(*,'(A DEBUG] Initial guess vector assembly complete'
    ! call flush(6)

    ! Solve the system
    ! write(*,'(A DEBUG] Calling KSPSolve...'
    ! call flush(6)
    call KSPSolve(ksp_saved, bb_saved, xx_saved, ierr)
    
    if (ierr /= 0) then
       write(0,*) "ERROR: KSPSolve failed with error code:", ierr
       stop
    end if
    
    call KSPGetIterationNumber(ksp_saved, its, ierr)
    call KSPGetResidualNorm(ksp_saved, rnorm, ierr)
    
    ! write(*,'(A DEBUG] KSPSolve complete, iterations=', its, ', residual=', rnorm
    ! call flush(6)

    ! Extract solution vector using batched VecGetValues
    block
      integer, parameter :: CHUNK_SIZE = 100000
      PetscInt, allocatable :: idx_batch(:)
      PetscScalar, allocatable :: val_batch(:)
      PetscInt :: chunk_lenp
      integer :: i_start, i_end, chunk_len, j
      
      do i_start = 1, n, CHUNK_SIZE
        i_end = min(i_start + CHUNK_SIZE - 1, n)
        chunk_len = i_end - i_start + 1
        
        allocate(idx_batch(chunk_len), val_batch(chunk_len))
        
        do j = 1, chunk_len
          idx_batch(j) = int(i_start + j - 2, kind=kind(idx_batch))
        end do
        
        chunk_lenp = chunk_len
        call VecGetValues(xx_saved, chunk_lenp, idx_batch, val_batch, ierr)
        x(i_start:i_end) = val_batch(1:chunk_len)
        
        deallocate(idx_batch, val_batch)
      end do
    end block

  end subroutine solve_petsc_csr

end module petsc_solver
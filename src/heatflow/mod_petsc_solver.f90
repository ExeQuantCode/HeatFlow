module petsc_solver
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscksp.h"
  use petscksp
  use iso_c_binding
  implicit none
  private
  public :: petsc_init, petsc_finalize, solve_petsc_csr, petsc_cleanup
  public :: petsc_set_solver_profile, petsc_mark_matrix_changed

  integer, parameter :: SOLVER_PROFILE_LEN = 32

  ! Solver profiles, ordered roughly from fastest/safest-for-small to lowest-memory:
  !   LU      : direct solve; very robust but highest memory use.
  !   GAMG    : algebraic multigrid; good large-problem balance, usually much lower memory than LU.
  !   ILU     : iterative with incomplete LU; moderate memory, robust for smaller/medium systems.
  !   GMRES   : GMRES with Jacobi; lower memory than ILU/GAMG, may need more iterations.
  !   JACOBI  : BiCGSTAB with Jacobi; low memory, often slower.
  !   NONE    : BiCGSTAB without a preconditioner; lowest memory, slowest/least robust.
  character(len=SOLVER_PROFILE_LEN), save :: solver_profile = 'GAMG'
  integer, save :: solver_verbosity = 1

  ! Persistent PETSc objects (reused across timesteps for memory efficiency)
  Mat, save :: A_saved
  Vec, save :: bb_saved
  Vec, save :: xx_saved
  KSP, save :: ksp_saved
  logical, save :: initialized = .false.
  integer, save :: n_saved = 0
  logical, save :: petsc_objects_nulled = .false.
  logical, save :: matrix_values_loaded = .false.

contains

  subroutine petsc_set_solver_profile(profile, verbosity)
    character(len=*), intent(in) :: profile
    integer, intent(in), optional :: verbosity
    character(len=SOLVER_PROFILE_LEN) :: requested

    if (present(verbosity)) solver_verbosity = verbosity

    requested = profile
    call uppercase_inplace(requested)
    requested = adjustl(requested)

    select case (trim(requested))
      case ('', 'AUTO', 'BALANCED', 'FAST', 'GAMG')
        solver_profile = 'GAMG'
      case ('ILU')
        solver_profile = 'ILU'
      case ('LU', 'DIRECT')
        solver_profile = 'LU'
      case ('GMRES')
        solver_profile = 'GMRES'
      case ('JACOBI', 'LOWMEM', 'LOW_MEMORY', 'MEMORY')
        solver_profile = 'JACOBI'
      case ('NONE', 'NOPC', 'NO_PC')
        solver_profile = 'NONE'
      case default
        solver_profile = 'GAMG'
        write(*,'(A,A,A)') ' [Solver] Unknown _SolverMethod="', trim(profile), '"; using GAMG'
    end select

    if (initialized) call petsc_cleanup()
  end subroutine petsc_set_solver_profile

  subroutine petsc_mark_matrix_changed()
    matrix_values_loaded = .false.
  end subroutine petsc_mark_matrix_changed

  subroutine uppercase_inplace(text)
    character(len=*), intent(inout) :: text
    integer :: i, code

    do i = 1, len_trim(text)
      code = iachar(text(i:i))
      if (code >= iachar('a') .and. code <= iachar('z')) text(i:i) = achar(code - 32)
    end do
  end subroutine uppercase_inplace

  subroutine print_solver_profile()
    select case (trim(solver_profile))
      case ('LU')
        write(*,'(A)') ' [Solver] LU/direct: most robust for small runs, but highest memory. Avoid for large grids.'
      case ('GAMG')
        write(*,'(A)') ' [Solver] GAMG+GMRES: balanced large-run default; medium memory, usually much faster than low-memory methods.'
      case ('ILU')
        write(*,'(A)') ' [Solver] ILU+BiCGSTAB: moderate memory and robust, but can still grow too large on big 3D grids.'
      case ('GMRES')
        write(*,'(A)') ' [Solver] GMRES+Jacobi: lower memory than ILU/GAMG, often slower because preconditioning is weak.'
      case ('JACOBI')
        write(*,'(A)') ' [Solver] Jacobi+BiCGSTAB: low memory, usually slower; useful when LU/ILU/GAMG run out of memory.'
      case ('NONE')
        write(*,'(A)') ' [Solver] No preconditioner+BiCGSTAB: lowest memory, slowest and least robust.'
    end select
    write(*,'(A)') ' [Solver] PETSc command-line options may still override these choices.'
  end subroutine print_solver_profile

  subroutine apply_solver_profile(pc, ierr)
    PC, intent(inout) :: pc
    integer, intent(out) :: ierr

    ierr = 0
    select case (trim(solver_profile))
      case ('LU')
        call PCSetType(pc, PCLU, ierr)
        if (ierr /= 0) return
        call KSPSetType(ksp_saved, KSPPREONLY, ierr)

      case ('GAMG')
        call PCSetType(pc, PCGAMG, ierr)
        if (ierr /= 0) return
        call KSPSetType(ksp_saved, KSPGMRES, ierr)

      case ('ILU')
        call PCSetType(pc, PCILU, ierr)
        if (ierr /= 0) return
        call KSPSetType(ksp_saved, KSPBCGS, ierr)

      case ('GMRES')
        call PCSetType(pc, PCJACOBI, ierr)
        if (ierr /= 0) return
        call KSPSetType(ksp_saved, KSPGMRES, ierr)

      case ('JACOBI')
        call PCSetType(pc, PCJACOBI, ierr)
        if (ierr /= 0) return
        call KSPSetType(ksp_saved, KSPBCGS, ierr)

      case ('NONE')
        call PCSetType(pc, PCNONE, ierr)
        if (ierr /= 0) return
        call KSPSetType(ksp_saved, KSPBCGS, ierr)

      case default
        call PCSetType(pc, PCGAMG, ierr)
        if (ierr /= 0) return
        call KSPSetType(ksp_saved, KSPGMRES, ierr)
    end select

    if (solver_verbosity >= 1) call print_solver_profile()
  end subroutine apply_solver_profile

  subroutine petsc_init()
    integer :: ierr
    call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
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
      matrix_values_loaded = .false.
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

    ! ! call flush(6)
    
    
    
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
      
      
      np = n
      zerop = 0
      
      do i = 1, n
        d_nnz(i) = int(ia(i+1) - ia(i), kind=kind(d_nnz))
      end do
      
      
      ! Create matrix with exact preallocation (most memory-efficient)
      
      ! Use a temporary local Mat object first, then assign
      block
        Mat :: A_temp
        call MatCreate(PETSC_COMM_SELF, A_temp, ierr)
        
        if (ierr == 0) then
          call MatSetSizes(A_temp, np, np, np, np, ierr)
          
          call MatSetType(A_temp, MATSEQAIJ, ierr)
          
          call MatSeqAIJSetPreallocation(A_temp, zerop, d_nnz, ierr)
          
          A_saved = A_temp
        end if
      end block
      
      
      if (ierr /= 0) then
        write(0,*) "ERROR: MatCreateSeqAIJ failed with ierr=", ierr
        write(0,*) "  Matrix size may exceed system limits"
        write(0,*) "  n=", n, ", nnz=", sum(int(d_nnz,8)), ", max_nnz/row=", maxval(d_nnz)
        stop
      end if
      
      deallocate(d_nnz)
      
      
      ! Create persistent vectors - use local temps like we did for matrix
      block
        Vec :: bb_temp, xx_temp
        call VecCreateSeq(PETSC_COMM_SELF, np, bb_temp, ierr)
        bb_saved = bb_temp
        
        call VecCreateSeq(PETSC_COMM_SELF, np, xx_temp, ierr)
        xx_saved = xx_temp
      end block
      
      
      ! Create and configure KSP solver (persistent across timesteps)
      block
        KSP :: ksp_temp
        call KSPCreate(PETSC_COMM_SELF, ksp_temp, ierr)
        ksp_saved = ksp_temp
      end block
      
      call KSPSetOperators(ksp_saved, A_saved, A_saved, ierr)
      
      call KSPGetPC(ksp_saved, pc, ierr)
      
      call apply_solver_profile(pc, ierr)
      if (ierr /= 0) then
        write(0,*) "ERROR: PETSc solver profile setup failed with ierr=", ierr
        stop
      end if
      
      call KSPSetTolerances(ksp_saved, rtol, PETSC_DEFAULT_REAL, &
                           PETSC_DEFAULT_REAL, maxit, ierr)
      call KSPSetNormType(ksp_saved, KSP_NORM_UNPRECONDITIONED, ierr)
      call KSPSetFromOptions(ksp_saved, ierr)
      
      initialized = .true.
      n_saved = n
    end if

    ! The H matrix is built once during setup and normally stays fixed across
    ! timesteps. Reassembling it every solve can force PETSc to rebuild costly
    ! preconditioners/factorizations and can be a major memory/time penalty.
    if (.not. matrix_values_loaded) then
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
      matrix_values_loaded = .true.
    end if
    
    
    ! Optional: Verify matrix assembly (uncomment for debugging)

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

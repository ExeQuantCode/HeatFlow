module petsc_solver
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscksp.h"
  use petscksp
  use mpi
  implicit none
  private
  public :: petsc_init, petsc_finalize, solve_petsc_csr, petsc_cleanup, petsc_is_root, petsc_world_size

  ! ===== PRECONDITIONER SELECTION =====
  ! Change this to switch between preconditioners:
  ! 'GAMG' = Algebraic Multigrid (best for elliptic PDEs, 10-20x faster)
  ! 'ILU'  = Incomplete LU (good general purpose, robust)
  ! 'LU'   = Direct solver (most robust, uses more memory)
  character(len=10), parameter :: PRECONDITIONER = 'GAMG'
  ! ====================================

  ! Persistent PETSc objects (reused across timesteps for memory efficiency)
  Mat, save :: A_saved
  Vec, save :: bb_saved
  Vec, save :: xx_saved
  KSP, save :: ksp_saved
  logical, save :: initialized = .false.
  integer, save :: n_saved = 0
  integer, save :: comm_rank_saved = 0
  integer, save :: comm_size_saved = 1
  integer, save :: row_start_saved = 1
  integer, save :: row_end_saved = 0
  PetscInt, allocatable, save :: ia_saved(:), ja_saved(:)
  PetscInt, allocatable, save :: local_indices_saved(:)
  PetscScalar, allocatable, save :: aval_saved(:)
  PetscInt, allocatable, save :: diag_nnz_saved(:), offdiag_nnz_saved(:)
  integer, allocatable, save :: recvcounts_saved(:), displs_saved(:)
  real(8), allocatable, save :: b_local_saved(:), x_local_saved(:)

contains

  subroutine petsc_init()
    integer :: ierr
    call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
    PetscObjectNullify(A_saved)
    PetscObjectNullify(bb_saved)
    PetscObjectNullify(xx_saved)
    PetscObjectNullify(ksp_saved)
    call MPI_Comm_rank(PETSC_COMM_WORLD, comm_rank_saved, ierr)
    call MPI_Comm_size(PETSC_COMM_WORLD, comm_size_saved, ierr)
  end subroutine petsc_init

  logical function petsc_is_root()
    petsc_is_root = (comm_rank_saved == 0)
  end function petsc_is_root

  integer function petsc_world_size()
    petsc_world_size = comm_size_saved
  end function petsc_world_size

  subroutine petsc_finalize()
    integer :: ierr
    call petsc_cleanup()
    call PetscFinalize(ierr)
  end subroutine petsc_finalize

  subroutine petsc_cleanup()
    integer :: ierr

    if (.not. PetscObjectIsNull(A_saved)) then
      call MatDestroy(A_saved, ierr)
      PetscObjectNullify(A_saved)
    end if
    if (.not. PetscObjectIsNull(bb_saved)) then
      call VecDestroy(bb_saved, ierr)
      PetscObjectNullify(bb_saved)
    end if
    if (.not. PetscObjectIsNull(xx_saved)) then
      call VecDestroy(xx_saved, ierr)
      PetscObjectNullify(xx_saved)
    end if
    if (.not. PetscObjectIsNull(ksp_saved)) then
      call KSPDestroy(ksp_saved, ierr)
      PetscObjectNullify(ksp_saved)
    end if
    if (allocated(ia_saved)) deallocate(ia_saved)
    if (allocated(ja_saved)) deallocate(ja_saved)
    if (allocated(local_indices_saved)) deallocate(local_indices_saved)
    if (allocated(aval_saved)) deallocate(aval_saved)
    if (allocated(diag_nnz_saved)) deallocate(diag_nnz_saved)
    if (allocated(offdiag_nnz_saved)) deallocate(offdiag_nnz_saved)
    if (allocated(recvcounts_saved)) deallocate(recvcounts_saved)
    if (allocated(displs_saved)) deallocate(displs_saved)
    if (allocated(b_local_saved)) deallocate(b_local_saved)
    if (allocated(x_local_saved)) deallocate(x_local_saved)

    initialized = .false.
    n_saved = 0
    row_start_saved = 1
    row_end_saved = 0
  end subroutine petsc_cleanup

  subroutine compute_partition(n, rank, nproc, row_start, row_end)
    integer, intent(in) :: n, rank, nproc
    integer, intent(out) :: row_start, row_end
    integer :: base_rows, remainder_rows, local_rows

    base_rows = n / nproc
    remainder_rows = mod(n, nproc)
    local_rows = base_rows
    if (rank < remainder_rows) local_rows = local_rows + 1

    row_start = rank * base_rows + min(rank, remainder_rows) + 1
    row_end = row_start + local_rows - 1
  end subroutine compute_partition

  subroutine preallocate_local_rows(n, ia, ja)
    integer, intent(in) :: n
    integer, intent(in) :: ia(:), ja(:)
    integer :: local_row, global_row, entry_idx, nlocal
    integer :: diag_begin, diag_end

    nlocal = max(0, row_end_saved - row_start_saved + 1)
    allocate(diag_nnz_saved(nlocal), offdiag_nnz_saved(nlocal))
    diag_nnz_saved = 0
    offdiag_nnz_saved = 0

    diag_begin = row_start_saved
    diag_end = row_end_saved
    do local_row = 1, nlocal
      global_row = row_start_saved + local_row - 1
      do entry_idx = ia(global_row), ia(global_row + 1) - 1
        if (ja(entry_idx) >= diag_begin .and. ja(entry_idx) <= diag_end) then
          diag_nnz_saved(local_row) = diag_nnz_saved(local_row) + 1
        else
          offdiag_nnz_saved(local_row) = offdiag_nnz_saved(local_row) + 1
        end if
      end do
    end do
  end subroutine preallocate_local_rows

  subroutine build_distributed_matrix(n, ia, ja, aval)
    integer, intent(in) :: n
    integer, intent(in) :: ia(:), ja(:)
    real(8), intent(in) :: aval(:)

    PetscInt :: row_idx(1)
    PetscInt, allocatable :: cols0(:)
    PetscScalar, allocatable :: vals0(:)
    integer :: ierr, global_row, local_row, nlocal, row_nnz, max_row_nnz

    call preallocate_local_rows(n, ia, ja)
    nlocal = max(0, row_end_saved - row_start_saved + 1)
    max_row_nnz = max(1, maxval(ia(2:n + 1) - ia(1:n)))

    call MatCreate(PETSC_COMM_WORLD, A_saved, ierr)
    call MatSetSizes(A_saved, nlocal, nlocal, n, n, ierr)
    call MatSetType(A_saved, MATAIJ, ierr)
    call MatSeqAIJSetPreallocation(A_saved, 0, diag_nnz_saved, ierr)
    call MatMPIAIJSetPreallocation(A_saved, 0, diag_nnz_saved, 0, offdiag_nnz_saved, ierr)

    allocate(cols0(max_row_nnz), vals0(max_row_nnz))
    do local_row = 1, nlocal
      global_row = row_start_saved + local_row - 1
      row_nnz = ia(global_row + 1) - ia(global_row)
      if (row_nnz <= 0) cycle

      row_idx(1) = global_row - 1
      cols0(1:row_nnz) = ja(ia(global_row):ia(global_row + 1) - 1) - 1
      vals0(1:row_nnz) = aval(ia(global_row):ia(global_row + 1) - 1)
      call MatSetValues(A_saved, 1, row_idx, row_nnz, cols0, vals0, INSERT_VALUES, ierr)
    end do
    deallocate(cols0, vals0)

    call MatAssemblyBegin(A_saved, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(A_saved, MAT_FINAL_ASSEMBLY, ierr)
  end subroutine build_distributed_matrix

  subroutine update_distributed_matrix(n, ia, ja, aval)
    integer, intent(in) :: n
    integer, intent(in) :: ia(:), ja(:)
    real(8), intent(in) :: aval(:)

    PetscInt :: row_idx(1)
    PetscInt, allocatable :: cols0(:)
    PetscScalar, allocatable :: vals0(:)
    integer :: ierr, global_row, local_row, nlocal, row_nnz, max_row_nnz

    nlocal = max(0, row_end_saved - row_start_saved + 1)
    max_row_nnz = max(1, maxval(ia(2:n + 1) - ia(1:n)))
    allocate(cols0(max_row_nnz), vals0(max_row_nnz))

    call MatZeroEntries(A_saved, ierr)
    do local_row = 1, nlocal
      global_row = row_start_saved + local_row - 1
      row_nnz = ia(global_row + 1) - ia(global_row)
      if (row_nnz <= 0) cycle

      row_idx(1) = global_row - 1
      cols0(1:row_nnz) = ja(ia(global_row):ia(global_row + 1) - 1) - 1
      vals0(1:row_nnz) = aval(ia(global_row):ia(global_row + 1) - 1)
      call MatSetValues(A_saved, 1, row_idx, row_nnz, cols0, vals0, INSERT_VALUES, ierr)
    end do
    deallocate(cols0, vals0)

    call MatAssemblyBegin(A_saved, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(A_saved, MAT_FINAL_ASSEMBLY, ierr)
  end subroutine update_distributed_matrix

  subroutine solve_petsc_csr(n, ia, ja, aval, b, x, rtol, maxit)
    integer, intent(in) :: n
    integer, intent(in) :: ia(:), ja(:)
    real(8), intent(in) :: aval(:), b(:)
    real(8), intent(inout) :: x(:)
    real(8), intent(in) :: rtol
    integer, intent(in) :: maxit

    PC :: pc
    integer :: ierr, its
    real(8) :: rnorm
    logical :: rebuild_needed
    integer :: nlocal, rank_idx, local_row

    if (size(ia) /= n + 1) stop 'solve_petsc_csr: ia size mismatch'
    if (size(b) /= n .or. size(x) /= n) stop 'solve_petsc_csr: vector size mismatch'

    rebuild_needed = .false.
    if (.not. initialized) rebuild_needed = .true.
    if (n /= n_saved) rebuild_needed = .true.

    if (.not. rebuild_needed .and. allocated(ia_saved)) then
      if (size(ia_saved) /= size(ia) .or. size(ja_saved) /= size(ja) .or. size(aval_saved) /= size(aval)) then
        rebuild_needed = .true.
      else if (.not. all(ia_saved == ia - 1) .or. .not. all(ja_saved == ja - 1)) then
        rebuild_needed = .true.
      end if
    end if

    if (rebuild_needed) then
      if (initialized) call petsc_cleanup()

      call compute_partition(n, comm_rank_saved, comm_size_saved, row_start_saved, row_end_saved)
      nlocal = max(0, row_end_saved - row_start_saved + 1)

      allocate(ia_saved(size(ia)), ja_saved(size(ja)), aval_saved(size(aval)))
      ia_saved = ia - 1
      ja_saved = ja - 1
      aval_saved = aval

      allocate(local_indices_saved(max(1, nlocal)))
      if (nlocal > 0) then
        do local_row = 1, nlocal
          local_indices_saved(local_row) = row_start_saved + local_row - 2
        end do
      end if

      call build_distributed_matrix(n, ia, ja, aval)

      allocate(recvcounts_saved(comm_size_saved), displs_saved(comm_size_saved))
      do rank_idx = 0, comm_size_saved - 1
        call compute_partition(n, rank_idx, comm_size_saved, ierr, its)
        recvcounts_saved(rank_idx + 1) = max(0, its - ierr + 1)
        displs_saved(rank_idx + 1) = ierr - 1
      end do

      allocate(b_local_saved(max(1, nlocal)), x_local_saved(max(1, nlocal)))

      call VecCreateMPI(PETSC_COMM_WORLD, nlocal, n, bb_saved, ierr)
      call VecDuplicate(bb_saved, xx_saved, ierr)

      call KSPCreate(PETSC_COMM_WORLD, ksp_saved, ierr)
      call KSPSetOperators(ksp_saved, A_saved, A_saved, ierr)
      call KSPSetInitialGuessNonzero(ksp_saved, PETSC_TRUE, ierr)
      call KSPGetPC(ksp_saved, pc, ierr)

      select case (trim(PRECONDITIONER))
        case ('GAMG')
          call PCSetType(pc, PCGAMG, ierr)
          call KSPSetType(ksp_saved, KSPGMRES, ierr)
          if (petsc_is_root()) then
            write(*,'(A,I0,A)') ' [Solver] Using GAMG (Algebraic Multigrid) with GMRES across ', &
                 comm_size_saved, ' MPI ranks'
          end if

        case ('ILU')
          call PCSetType(pc, PCILU, ierr)
          call KSPSetType(ksp_saved, KSPBCGS, ierr)
          if (petsc_is_root()) then
            write(*,'(A,I0,A)') ' [Solver] Using ILU preconditioner with BiCGSTAB across ', &
                 comm_size_saved, ' MPI ranks'
          end if

        case ('LU')
          call PCSetType(pc, PCLU, ierr)
          call KSPSetType(ksp_saved, KSPPREONLY, ierr)
          if (petsc_is_root()) then
            write(*,'(A,I0,A)') ' [Solver] Using direct LU solver across ', comm_size_saved, ' MPI ranks'
          end if

        case default
          if (petsc_is_root()) then
            write(*,'(A,A)') ' [Warning] Unknown preconditioner: ', trim(PRECONDITIONER)
            write(*,'(A)') '           Defaulting to ILU'
          end if
          call PCSetType(pc, PCILU, ierr)
          call KSPSetType(ksp_saved, KSPBCGS, ierr)
      end select

      call KSPSetTolerances(ksp_saved, rtol, PETSC_DEFAULT_REAL, PETSC_DEFAULT_REAL, maxit, ierr)
      call KSPSetNormType(ksp_saved, KSP_NORM_UNPRECONDITIONED, ierr)
      call KSPSetFromOptions(ksp_saved, ierr)

      initialized = .true.
      n_saved = n
    else
      if (any(aval_saved /= aval)) then
        aval_saved = aval
        call update_distributed_matrix(n, ia, ja, aval)
      end if
    end if

    nlocal = max(0, row_end_saved - row_start_saved + 1)
    if (nlocal > 0) then
      b_local_saved(1:nlocal) = b(row_start_saved:row_end_saved)
      x_local_saved(1:nlocal) = x(row_start_saved:row_end_saved)
    end if

    call VecSet(bb_saved, 0.0d0, ierr)
    call VecSet(xx_saved, 0.0d0, ierr)
    if (nlocal > 0) then
      call VecSetValues(bb_saved, nlocal, local_indices_saved, b_local_saved, INSERT_VALUES, ierr)
      call VecSetValues(xx_saved, nlocal, local_indices_saved, x_local_saved, INSERT_VALUES, ierr)
    end if
    call VecAssemblyBegin(bb_saved, ierr)
    call VecAssemblyEnd(bb_saved, ierr)
    call VecAssemblyBegin(xx_saved, ierr)
    call VecAssemblyEnd(xx_saved, ierr)

    call KSPSolve(ksp_saved, bb_saved, xx_saved, ierr)
    if (ierr /= 0) then
      write(0,*) 'ERROR: KSPSolve failed with error code:', ierr
      stop
    end if

    call KSPGetIterationNumber(ksp_saved, its, ierr)
    call KSPGetResidualNorm(ksp_saved, rnorm, ierr)

    if (nlocal > 0) call VecGetValues(xx_saved, nlocal, local_indices_saved, x_local_saved, ierr)

    call MPI_Allgatherv(x_local_saved, nlocal, MPI_DOUBLE_PRECISION, x, recvcounts_saved, displs_saved, &
         MPI_DOUBLE_PRECISION, PETSC_COMM_WORLD, ierr)
  end subroutine solve_petsc_csr
end module petsc_solver
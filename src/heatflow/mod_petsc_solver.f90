module petsc_solver
#ifdef HAVE_PETSC
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscksp.h"

! PETSc 3.23 made Fortran objects automatically null-initialised and added
! PetscObjectIsNull().  Older versions still use explicit PETSC_NULL_* values.
#if (PETSC_VERSION_MAJOR > 3) || ((PETSC_VERSION_MAJOR == 3) && (PETSC_VERSION_MINOR >= 23))
#define HF_PETSC_HAS_OBJECT_IS_NULL 1
#define HF_MAT_IS_NULL(a) PetscObjectIsNull(a)
#define HF_VEC_IS_NULL(a) PetscObjectIsNull(a)
#define HF_KSP_IS_NULL(a) PetscObjectIsNull(a)
#else
#define HF_PETSC_HAS_OBJECT_IS_NULL 0
#define HF_MAT_IS_NULL(a) ((a) /= PETSC_NULL_MAT)
#define HF_VEC_IS_NULL(a) ((a) /= PETSC_NULL_VEC)
#define HF_KSP_IS_NULL(a) ((a) /= PETSC_NULL_KSP)
#endif

  use petscksp
#endif
  use mpi
  use inputs, only: solver_type
  implicit none
  private
  public :: petsc_init, petsc_finalize, solve_petsc_csr, petsc_cleanup, petsc_is_root, petsc_world_size
  public :: petsc_available, petsc_gpu_available

  ! ===== PRECONDITIONER SELECTION =====
  ! Selected at runtime via the "solver" input flag (see mod_inputs):
  ! 'gamg'      = Algebraic Multigrid (best for elliptic PDEs, CPU)
  ! 'ilu'       = Incomplete LU (good general purpose, CPU)
  ! 'lu'        = Direct solver (most robust, uses more memory, CPU)
  ! 'jacobi'    = Point Jacobi (cheap, robust on CPU and GPU)
  ! 'ilu_gpu'   = GPU ILU(0) via cuSPARSE (requires a GPU build)
  ! 'jacobi_gpu'= GPU point Jacobi (requires a GPU build)
  ! ====================================
  ! If PETSc is not available at compile time (HAVE_PETSC undefined) ...
  ! ... this module is compiled as a stub with the same public interface, ...
  ! ... so the rest of the code needs no changes. Only solver = legacy works.

#ifdef HAVE_PETSC
  ! Persistent PETSc objects (reused across timesteps for memory efficiency).
  ! PETSc >= 3.23 initialises these to its internal null sentinel when they are
  ! declared; do NOT assign PETSC_NULL_* to them (MatCreate rejects that).
  Mat, save :: A_saved
  Vec, save :: bb_saved
  Vec, save :: xx_saved
  KSP, save :: ksp_saved
  logical, save :: initialized = .false.
  logical, save :: use_gpu_backend = .false.
  logical, save :: backend_saved = .false.
  integer, save :: n_saved = 0
  integer, save :: comm_rank_saved = 0
  integer, save :: comm_size_saved = 1
  integer, save :: row_start_saved = 1
  integer, save :: row_end_saved = 0
  PetscInt, allocatable, save :: ia_saved(:), ja_saved(:)
  PetscScalar, allocatable, save :: aval_saved(:)
  PetscInt, allocatable, save :: diag_nnz_saved(:), offdiag_nnz_saved(:)
  integer, allocatable, save :: recvcounts_saved(:), displs_saved(:)
  real(8), allocatable, save :: x_local_saved(:)
#else
  integer, save :: comm_rank_saved = 0
  integer, save :: comm_size_saved = 1
#endif

contains

  !!!##################################################################
  !!! Returns whether this binary was compiled with PETSc support
  !!!##################################################################
  logical function petsc_available()
#ifdef HAVE_PETSC
    petsc_available = .true.
#else
    petsc_available = .false.
#endif
  end function petsc_available

  !!!##################################################################
  !!! Returns whether this binary was compiled with CUDA GPU support
  !!!##################################################################
  logical function petsc_gpu_available()
#ifdef HEATFLOW_GPU
    petsc_gpu_available = .true.
#else
    petsc_gpu_available = .false.
#endif
  end function petsc_gpu_available

#ifdef HAVE_PETSC
  subroutine petsc_init()
    integer :: ierr
    call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
#if !HF_PETSC_HAS_OBJECT_IS_NULL
    ! Older PETSc releases need explicit null initialisation.  Newer releases
    ! auto-initialise objects and reject a manually assigned PETSC_NULL_MAT.
    A_saved = PETSC_NULL_MAT
    bb_saved = PETSC_NULL_VEC
    xx_saved = PETSC_NULL_VEC
    ksp_saved = PETSC_NULL_KSP
#endif
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

    if (.not. HF_MAT_IS_NULL(A_saved)) call MatDestroy(A_saved, ierr)
    if (.not. HF_VEC_IS_NULL(bb_saved)) call VecDestroy(bb_saved, ierr)
    if (.not. HF_VEC_IS_NULL(xx_saved)) call VecDestroy(xx_saved, ierr)
    if (.not. HF_KSP_IS_NULL(ksp_saved)) call KSPDestroy(ksp_saved, ierr)
    if (allocated(ia_saved)) deallocate(ia_saved)
    if (allocated(ja_saved)) deallocate(ja_saved)
    if (allocated(aval_saved)) deallocate(aval_saved)
    if (allocated(diag_nnz_saved)) deallocate(diag_nnz_saved)
    if (allocated(offdiag_nnz_saved)) deallocate(offdiag_nnz_saved)
    if (allocated(recvcounts_saved)) deallocate(recvcounts_saved)
    if (allocated(displs_saved)) deallocate(displs_saved)
    if (allocated(x_local_saved)) deallocate(x_local_saved)

#if !HF_PETSC_HAS_OBJECT_IS_NULL
    A_saved = PETSC_NULL_MAT
    bb_saved = PETSC_NULL_VEC
    xx_saved = PETSC_NULL_VEC
    ksp_saved = PETSC_NULL_KSP
#endif
    initialized = .false.
    use_gpu_backend = .false.
    backend_saved = .false.
    n_saved = 0
    row_start_saved = 1
    row_end_saved = 0
  end subroutine petsc_cleanup

  !!!##################################################################
  !!! True when the selected solver has the "_gpu" suffix
  !!!##################################################################
  logical function solver_is_gpu(name)
    character(len=*), intent(in) :: name
    integer :: name_len

    name_len = len_trim(name)
    solver_is_gpu = name_len > 4
    if (solver_is_gpu) solver_is_gpu = name(name_len - 3:name_len) == '_gpu'
  end function solver_is_gpu

  !!!##################################################################
  !!! Strip the "_gpu" suffix to get the base solver name
  !!!##################################################################
  function base_solver_name(name) result(base)
    character(len=*), intent(in) :: name
    character(len=16) :: base
    integer :: name_len

    base = trim(adjustl(name))
    name_len = len_trim(base)
    if (name_len > 4) then
      if (base(name_len - 3:name_len) == '_gpu') base = base(:name_len - 4)
    end if
  end function base_solver_name

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
#ifdef HEATFLOW_GPU
    if (use_gpu_backend) then
      call MatSetType(A_saved, MATAIJCUSPARSE, ierr)
    else
      call MatSetType(A_saved, MATAIJ, ierr)
    end if
#else
    call MatSetType(A_saved, MATAIJ, ierr)
#endif
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
    PetscScalar, pointer :: barray(:), xarray(:)
    integer :: ierr, its
    real(8) :: rnorm
    logical :: rebuild_needed, want_gpu
    integer :: nlocal, rank_idx
    character(len=16) :: solver_name, base_solver

    if (size(ia) /= n + 1) stop 'solve_petsc_csr: ia size mismatch'
    if (size(b) /= n .or. size(x) /= n) stop 'solve_petsc_csr: vector size mismatch'

    solver_name = trim(adjustl(solver_type))
    want_gpu = solver_is_gpu(solver_name)
    base_solver = base_solver_name(solver_name)

    if (want_gpu .and. .not. petsc_gpu_available()) then
      if (petsc_is_root()) then
        write(0,'(A)') 'ERROR: solver = "'//trim(solver_name)//'" requires a GPU-enabled build.'
        write(0,'(A)') '       Rebuild with: make gpu   (or: make GPU=1)'
        write(0,'(A)') '       and run the resulting bin/HeatFlow-gpu binary.'
      end if
      stop 1
    end if

    rebuild_needed = .false.
    if (.not. initialized) rebuild_needed = .true.
    if (n /= n_saved) rebuild_needed = .true.
    if (want_gpu .neqv. backend_saved) rebuild_needed = .true.

    if (.not. rebuild_needed .and. allocated(ia_saved)) then
      if (size(ia_saved) /= size(ia) .or. size(ja_saved) /= size(ja) .or. size(aval_saved) /= size(aval)) then
        rebuild_needed = .true.
      else if (.not. all(ia_saved == ia - 1) .or. .not. all(ja_saved == ja - 1)) then
        rebuild_needed = .true.
      end if
    end if

    if (rebuild_needed) then
      if (initialized) call petsc_cleanup()

      use_gpu_backend = want_gpu
      backend_saved = want_gpu

      call compute_partition(n, comm_rank_saved, comm_size_saved, row_start_saved, row_end_saved)
      nlocal = max(0, row_end_saved - row_start_saved + 1)

      allocate(ia_saved(size(ia)), ja_saved(size(ja)), aval_saved(size(aval)))
      ia_saved = ia - 1
      ja_saved = ja - 1
      aval_saved = aval

      call build_distributed_matrix(n, ia, ja, aval)

      allocate(recvcounts_saved(comm_size_saved), displs_saved(comm_size_saved))
      do rank_idx = 0, comm_size_saved - 1
        call compute_partition(n, rank_idx, comm_size_saved, ierr, its)
        recvcounts_saved(rank_idx + 1) = max(0, its - ierr + 1)
        displs_saved(rank_idx + 1) = ierr - 1
      end do

      allocate(x_local_saved(max(1, nlocal)))

      call VecCreate(PETSC_COMM_WORLD, bb_saved, ierr)
      call VecSetSizes(bb_saved, nlocal, n, ierr)
#ifdef HEATFLOW_GPU
      if (use_gpu_backend) then
        call VecSetType(bb_saved, VECCUDA, ierr)
      else
        call VecSetType(bb_saved, VECMPI, ierr)
      end if
#else
      call VecSetType(bb_saved, VECMPI, ierr)
#endif
      call VecDuplicate(bb_saved, xx_saved, ierr)

      call KSPCreate(PETSC_COMM_WORLD, ksp_saved, ierr)
      call KSPSetOperators(ksp_saved, A_saved, A_saved, ierr)
      call KSPGetPC(ksp_saved, pc, ierr)

#ifdef HEATFLOW_GPU
      if (petsc_is_root()) then
        if (want_gpu) then
          write(*,'(A)') ' [Backend] GPU (CUDA): MATAIJCUSPARSE + VECCUDA'
        else
          write(*,'(A)') ' [Backend] CPU: MATAIJ + VECMPI'
        end if
      end if
#endif

      select case (trim(base_solver))
        case ('gamg')
          call PCSetType(pc, PCGAMG, ierr)
          call KSPSetType(ksp_saved, KSPGMRES, ierr)
          call KSPSetInitialGuessNonzero(ksp_saved, PETSC_TRUE, ierr)
          if (petsc_is_root()) then
            write(*,'(A,I0,A)') ' [Solver] Using GAMG (Algebraic Multigrid) with GMRES across ', &
                 comm_size_saved, ' MPI ranks'
          end if

        case ('ilu')
          call KSPSetType(ksp_saved, KSPBCGS, ierr)
          call KSPSetInitialGuessNonzero(ksp_saved, PETSC_TRUE, ierr)
#ifdef HEATFLOW_GPU
          if (want_gpu) then
            if (comm_size_saved == 1) then
              call PCSetType(pc, PCILU, ierr)
              call PCFactorSetMatSolverType(pc, MATSOLVERCUSPARSE, ierr)
              if (petsc_is_root()) then
                write(*,'(A)') ' [Solver] Using GPU ILU(0) via cuSPARSE with BiCGSTAB (1 MPI rank)'
              end if
            else
              ! PETSc has no parallel cuSPARSE ILU factorisation.
              call PCSetType(pc, PCJACOBI, ierr)
              if (petsc_is_root()) then
                write(*,'(A)') ' [Warning] ilu_gpu is only supported on 1 MPI rank;'
                write(*,'(A,I0,A)') '           falling back to GPU Jacobi across ', &
                     comm_size_saved, ' ranks'
              end if
            end if
          else
            call PCSetType(pc, PCILU, ierr)
            if (petsc_is_root()) then
              write(*,'(A,I0,A)') ' [Solver] Using ILU preconditioner with BiCGSTAB across ', &
                   comm_size_saved, ' MPI ranks'
            end if
          end if
#else
          call PCSetType(pc, PCILU, ierr)
          if (petsc_is_root()) then
            write(*,'(A,I0,A)') ' [Solver] Using ILU preconditioner with BiCGSTAB across ', &
                 comm_size_saved, ' MPI ranks'
          end if
#endif

        case ('jacobi')
          call PCSetType(pc, PCJACOBI, ierr)
          call KSPSetType(ksp_saved, KSPBCGS, ierr)
          call KSPSetInitialGuessNonzero(ksp_saved, PETSC_TRUE, ierr)
          if (petsc_is_root()) then
            write(*,'(A,I0,A)') ' [Solver] Using Jacobi preconditioner with BiCGSTAB across ', &
                 comm_size_saved, ' MPI ranks'
          end if

        case ('lu')
          call PCSetType(pc, PCLU, ierr)
          call KSPSetType(ksp_saved, KSPPREONLY, ierr)
          call KSPSetInitialGuessNonzero(ksp_saved, PETSC_FALSE, ierr)
          if (petsc_is_root()) then
            write(*,'(A,I0,A)') ' [Solver] Using direct LU solver across ', comm_size_saved, ' MPI ranks'
          end if

        case default
          if (petsc_is_root()) then
            write(*,'(A,A)') ' [Warning] Unknown preconditioner: ', trim(solver_name)
            write(*,'(A)') '           Defaulting to ILU'
          end if
          call PCSetType(pc, PCILU, ierr)
          call KSPSetType(ksp_saved, KSPBCGS, ierr)
          call KSPSetInitialGuessNonzero(ksp_saved, PETSC_TRUE, ierr)
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

    call VecGetArrayWrite(bb_saved, barray, ierr)
    call VecGetArrayWrite(xx_saved, xarray, ierr)
    if (nlocal > 0) then
      barray(1:nlocal) = b(row_start_saved:row_end_saved)
      xarray(1:nlocal) = x(row_start_saved:row_end_saved)
    end if
    call VecRestoreArrayWrite(bb_saved, barray, ierr)
    call VecRestoreArrayWrite(xx_saved, xarray, ierr)

    call KSPSolve(ksp_saved, bb_saved, xx_saved, ierr)
    if (ierr /= 0) then
      write(0,*) 'ERROR: KSPSolve failed with error code:', ierr
      stop
    end if

    call KSPGetIterationNumber(ksp_saved, its, ierr)
    call KSPGetResidualNorm(ksp_saved, rnorm, ierr)

    if (nlocal > 0) then
      call VecGetArrayRead(xx_saved, xarray, ierr)
      x_local_saved(1:nlocal) = xarray(1:nlocal)
      call VecRestoreArrayRead(xx_saved, xarray, ierr)
    end if

    call MPI_Allgatherv(x_local_saved, nlocal, MPI_DOUBLE_PRECISION, x, recvcounts_saved, displs_saved, &
         MPI_DOUBLE_PRECISION, PETSC_COMM_WORLD, ierr)
  end subroutine solve_petsc_csr

#else
  !!!##################################################################
  !!! Stub implementation for builds without PETSc (legacy solver only)
  !!!##################################################################
  subroutine petsc_init()
    integer :: ierr
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, comm_rank_saved, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, comm_size_saved, ierr)
  end subroutine petsc_init

  logical function petsc_is_root()
    petsc_is_root = (comm_rank_saved == 0)
  end function petsc_is_root

  integer function petsc_world_size()
    petsc_world_size = comm_size_saved
  end function petsc_world_size

  subroutine petsc_finalize()
    integer :: ierr
    call MPI_Finalize(ierr)
  end subroutine petsc_finalize

  subroutine petsc_cleanup()
    ! no PETSc objects in a legacy-only build
  end subroutine petsc_cleanup

  subroutine solve_petsc_csr(n, ia, ja, aval, b, x, rtol, maxit)
    integer, intent(in) :: n
    integer, intent(in) :: ia(:), ja(:)
    real(8), intent(in) :: aval(:), b(:)
    real(8), intent(inout) :: x(:)
    real(8), intent(in) :: rtol
    integer, intent(in) :: maxit
    if (petsc_is_root()) then
       write(0,*) 'ERROR: solver = "', trim(solver_type), '" requires PETSc,'
       write(0,*) '       but this binary was compiled without PETSc.'
       write(0,*) '       Set solver = legacy or rebuild with PETSc available.'
    end if
    stop 1
  end subroutine solve_petsc_csr
#endif
end module petsc_solver
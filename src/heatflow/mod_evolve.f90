!!!#################################################################################################
!!! Module to evolve the system for one time step.
!!! This module contains the subroutines:
!!!   -simulate, Evolve the system for one time step.
!!! This module contains the variables:
!!!   -S, The source vector.
!!!   -x, The temperature vector.
!!!   -Q, The heat vector.
!!!   -Qdens, The heat density vector.
!!!   -S_CAT, The Cattaneo correction vector.
!!!   -B, The boundary vector.
!!!   -ncg, The number of CG iterations.
!!!   -itol, The tolerance method used to calculate error.
!!!   -itmax, The max number of iterations.
!!!   -iter, The number of the final iteration. 
!!!   -err, The error of the final iteration.
!!!   -tol, The convergence criteria.
!!!   -iss, The Sparse Storage type (1=SRS, 2=SDS).
!!! Author: Harry Mclean, Frank Davies, Steven Hepplestone
!!!#################################################################################################

module evolution
  use constants, only: real12, int12, TINY
  use inputs, only: NA, icattaneo, isteady, nx, ny, nz, IVERB,T_System, time_step, grid, power_in
  use inputs, only: TempDepProp
  use sptype, only: I4B
  use solver, only: linbcg
  use globe_data, only: Temp_p, Temp_pp, inverse_time, heat, lin_rhoc
  use globe_data, only: acsr, ja, ia
  use globe_data, only: heated_volume
  use heating, only: heater
  use boundary_vector, only: boundary
  use cattaneo, only: S_catS
!   use tempdep, only: ChangeProp 
   use petsc_solver, only: solve_petsc_csr

  implicit none

  private
  public :: simulate
  
  ! Module-level variables for PETSc (persist across time steps)
  integer, allocatable, save :: ia32(:), ja32(:)   ! 32-bit copies for PETSc

contains

!!!#################################################################################################
!!! Subroutine to evolve the system for one time step.
!!! Inputs:
!!!   itime - Current time step (integer)
!!!#################################################################################################
  subroutine simulate(itime)
    integer(int12), intent(in) :: itime
    real(real12), dimension(NA) :: S, Q, Qdens, S_CAT, B
    real(real12), dimension(:), allocatable :: x
    integer:: ncg, itol, itmax !, iss
    integer :: iter
    real(real12) :: e, err, tol
    integer :: NA32

    !----------------------
    ! Initialize vectors
    !----------------------
    B = 0.0_real12
    Q = 0.0_real12
    Qdens = 0.0_real12
    S_CAT = 0.0_real12
    S = 0.0_real12
    !^^^^^^^^^^^^^^^^^^^^^
    
    !--------------------------------
    ! Calculate boundary Vector
    !--------------------------------
    
    CALL boundary(B)
    if (IVERB .gt. 3) write(*,*) "B average", sum(B)/size(B)
    if (IVERB .gt. 4) write(*,*) "B", B

    if (any(isnan(B(:)))) then
       write(0,*) "fatal error: NAN in B vector"
       stop 1
    end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !--------------------------------
    ! Calculate heat
    !--------------------------------
    if (any(grid%iheater .gt. 0)) then
       CALL heater(itime, Q, Qdens)

       if (any(isnan(Q(:)))) then
            write(0,*) "fatal error: NAN in Q vector"
            stop 1
         end if
       if (any(isnan(Qdens(:)))) then
            write(0,*) "fatal error: NAN in Qdens vector"
            stop 1
         end if
    end if
    
    if (IVERB .gt. 3) heat = heat + sum(Q(:))
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !------------------------------------------
    ! Calculate Cattaneo correction
    !------------------------------------------
    if ( iCAttaneo .eq. 1) then
       CALL S_catS(S_CAT)
       if (IVERB .gt. 3) write(*,*) "S_CAT average", sum(S_CAT)/size(S_CAT)
       if (IVERB .gt. 4) write(*,*) "S_CAT", S_CAT
       if (any(isnan(S_CAT))) then
            write(*,*) "fatal error: NAN in S_CAT vector"
            stop 1
         end if
    end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !---------------------------------------------
    ! Construct S vector 
    !---------------------------------------------

    ! COMPREHENSIVE DEBUG: Dump all quantities for radial cross-section at iy=16
    if (itime .le. 2) then
       block
         integer(int12) :: dbg_ix, dbg_idx, dbg_k
         real(real12) :: dbg_Ax, dbg_rowsum
         write(*,*) ''
         write(*,*) '=========================================================='
         write(*,'(A,I6)') ' DEBUG TIMESTEP ', itime
         write(*,*) '=========================================================='
         write(*,*) 'inverse_time =', inverse_time
         write(*,*) 'heated_volume =', heated_volume
         write(*,*) 'sum(Q) =', sum(Q), '  sum(Qdens) =', sum(Qdens)
         write(*,*) 'count(Qdens/=0) =', count(Qdens .ne. 0.0_real12)
         write(*,*) ''
         write(*,*) '--- Radial cross-section at iy=16, iz=1 ---'
         write(*,'(A6,A12,A12,A14,A14,A14,A14,A14)') &
              'ix', 'mat_id', 'kappa', 'rhoCp', 'Temp_p', 'B(I)', 'Qdens(I)', 'S(I)'
         do dbg_ix = 1, nx
            dbg_idx = dbg_ix + (16-1)*nx  ! 1D index for (ix, iy=16, iz=1)
            write(*,'(I6,I12,ES12.4,ES14.6,ES14.6,ES14.6,ES14.6,ES14.6)') &
                 dbg_ix, grid(dbg_ix,16,1)%imaterial_type, &
                 grid(dbg_ix,16,1)%kappa, &
                 lin_rhoc(dbg_idx), &
                 Temp_p(dbg_idx), &
                 B(dbg_idx), Qdens(dbg_idx), S(dbg_idx)
         end do
         write(*,*) ''
         write(*,*) '--- H-matrix rows for iy=16 (radial): row_sum and entries ---'
         do dbg_ix = 1, nx
            dbg_idx = dbg_ix + (16-1)*nx
            dbg_rowsum = 0.0_real12
            do dbg_k = ia(dbg_idx), ia(dbg_idx+1)-1
               dbg_rowsum = dbg_rowsum + acsr(dbg_k)
            end do
            ! Compute A*Temp_p for this row (matrix-vector product)
            dbg_Ax = 0.0_real12
            do dbg_k = ia(dbg_idx), ia(dbg_idx+1)-1
               dbg_Ax = dbg_Ax + acsr(dbg_k) * Temp_p(ja(dbg_k))
            end do
            write(*,'(A,I3,A,ES14.6,A,ES14.6,A,ES14.6)') &
                 ' ix=', dbg_ix, &
                 '  row_sum=', dbg_rowsum, &
                 '  H*Tp=', dbg_Ax, &
                 '  S=', S(dbg_idx)
         end do
         write(*,*) ''
         write(*,*) '--- Heater region at iy=32, iz=1 ---'
         write(*,'(A6,A12,A14,A14,A14,A14)') &
              'ix', 'iheater', 'Temp_p', 'B(I)', 'Qdens(I)', 'S(I)'
         do dbg_ix = 1, min(10, nx)
            dbg_idx = dbg_ix + (32-1)*nx
            write(*,'(I6,I12,ES14.6,ES14.6,ES14.6,ES14.6)') &
                 dbg_ix, grid(dbg_ix,32,1)%iheater, &
                 Temp_p(dbg_idx), B(dbg_idx), Qdens(dbg_idx), S(dbg_idx)
         end do
         write(*,*) '=========================================================='
         write(*,*) ''
       end block
    end if

    if ( iSteady .eq. 0 ) then
       S = - inverse_time * Temp_p * lin_rhoc - Qdens - B
       if (IVERB .gt. 3) then
          write(*,*) "S construction diagnostics:"
          write(*,*) "  inverse_time =", inverse_time
          write(*,*) "  Temp_p avg =", sum(Temp_p)/size(Temp_p)
          write(*,*) "  lin_rhoc avg =", sum(lin_rhoc)/size(lin_rhoc)
          write(*,*) "  Qdens avg =", sum(Qdens)/size(Qdens)
          write(*,*) "  B avg =", sum(B)/size(B)
          write(*,*) "  -inverse_time*Temp_p*lin_rhoc avg =", sum(-inverse_time*Temp_p*lin_rhoc)/size(Temp_p)
          write(*,*) "  S before S_CAT avg =", sum(S)/size(S)
       end if
       if ( iCAttaneo  .eq. 1) then
          S = S + S_CAT
       end if
    else
       S = -Qdens - B
    end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (IVERB .gt.3) write(*,*) "S average", sum(S)/size(S)
    if (IVERB .gt.4) write(*,*) "S", S
    
    if (any(isnan(S(:)))) then
       write(0,*) "fatal error: NAN in S vector"
       stop 1
    end if
    
   !----------------------------------------------------
   ! Call the CG method to solve the equation Ax=b.
   !---------------------------------------------------
   ! b/S:     Input - the b vector.
   ! x:     Input/Output - initial guess for x, overwritten with the final solution.
   ! itol:  Input - sets the tolerance method used to calculate error.
   ! tol:   Input - sets the convergence criteria.
   ! itmax: Input - sets the max number of iterations.
   ! iter:  Output - gives the number of the final iteration.
   ! err:   Output - records the error of the final iteration.
   ! iss:   Input - sets the Sparse Storage type (1=SRS, 2=SDS).
   !  x=Temp_p+(Temp_p-Temp_pp)
   !  if (any(x-Temp_p .lt. TINY)) x=x+TINY !avoid nan solver issue
    itol=1
    tol=1.e-32_real12
    itmax=500000
    ncg = 0
    iter= 0
    err=E

   !  call bicgstab(acsr, ia, ja, S, itmax, Temp_p, x, iter)
   
   ! Allocate and initialize x with a good initial guess
   allocate(x(NA))
   x = Temp_p + (Temp_p - Temp_pp)
   if (any(x - Temp_p .lt. TINY)) x = x + TINY ! avoid nan solver issue
   
   ! Debug: Print initial guess statistics
   if (IVERB .gt. 3) then
      write(*,*) "========== PETSc Solver Diagnostics =========="
      write(*,*) "Time step:", itime
      write(*,*) "Initial guess x: min=", minval(x), " max=", maxval(x), " avg=", sum(x)/size(x)
      write(*,*) "RHS S: min=", minval(S), " max=", maxval(S), " avg=", sum(S)/size(S)
      write(*,*) "Temp_p: min=", minval(Temp_p), " max=", maxval(Temp_p), " avg=", sum(Temp_p)/size(Temp_p)
      write(*,*) "Matrix acsr: min=", minval(acsr), " max=", maxval(acsr), " avg=", sum(acsr)/size(acsr)
      write(*,*) "Matrix size: n=", NA32, " nnz=", size(acsr)
   end if 
   
   ! Convert to 32-bit integers for PETSc (only on first call)
   if (.not. allocated(ia32)) then
      allocate(ia32(size(ia)), ja32(size(ja)))
      ia32 = int(ia, kind=kind(ia32))
      ja32 = int(ja, kind=kind(ja32))
   end if
   NA32 = int(NA, kind=kind(NA32))
   
   call solve_petsc_csr(NA32, ia32, ja32, acsr, S, x, tol, itmax)
   
   ! POST-SOLVE DEBUG: Show solution and residual for radial cross-section
   if (itime .le. 2) then
      block
        integer(int12) :: dbg_ix, dbg_idx, dbg_k
        real(real12) :: dbg_Ax, dbg_resid
        write(*,*) ''
        write(*,*) '--- POST-SOLVE: Solution at iy=16, iz=1 ---'
        write(*,'(A6,A14,A14,A14,A14)') &
             'ix', 'Temp_p(old)', 'x(new)', 'deltaT', 'residual'
        do dbg_ix = 1, nx
           dbg_idx = dbg_ix + (16-1)*nx
           ! Compute H*x for this row (should equal S)
           dbg_Ax = 0.0_real12
           do dbg_k = ia(dbg_idx), ia(dbg_idx+1)-1
              dbg_Ax = dbg_Ax + acsr(dbg_k) * x(ja(dbg_k))
           end do
           dbg_resid = dbg_Ax - S(dbg_idx)
           write(*,'(I6,ES14.6,ES14.6,ES14.6,ES14.6)') &
                dbg_ix, Temp_p(dbg_idx), x(dbg_idx), &
                x(dbg_idx) - Temp_p(dbg_idx), dbg_resid
        end do
        write(*,*) ''
        write(*,*) '--- POST-SOLVE: Heater region iy=32 ---'
        write(*,'(A6,A14,A14,A14)') 'ix', 'Temp_p(old)', 'x(new)', 'deltaT'
        do dbg_ix = 1, min(10, nx)
           dbg_idx = dbg_ix + (32-1)*nx
           write(*,'(I6,ES14.6,ES14.6,ES14.6)') &
                dbg_ix, Temp_p(dbg_idx), x(dbg_idx), &
                x(dbg_idx) - Temp_p(dbg_idx)
        end do
        write(*,*) '=========================================================='
      end block
   end if
   
   ! Note: Don't deallocate ia32, ja32 - keep them for next time step


   ! CALL solve_pardiso(acsr, S, ia, ja, x)
   !  CALL linbcg(S,x,itol=int(itol,I4B),tol=tol, itmax=int(itmax,I4B), iter=iter, &
         ! err=E)
         
   !
    if (any(isnan(x(:)))) then
       write(0,*) "fatal error: NAN in x tempurature vector"
       write(0,*) 'time step ', itime, "      T   ", sum(Temp_p)/size(Temp_p), E ,iter
       write(0,*) 'time step ',itime, "      x   ", sum(x)/size(x), E ,iter
       stop 1
    end if
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   !------------------------------------------------------------------------------------------------
   ! Update the temperature vector and properties if the temperature dependent properties are used.
   !------------------------------------------------------------------------------------------------
    if (IVERB .gt. 4) then !print out the average temperature and energy
      write(*,*) 
      write(*,*) 'time step ', itime, "      T   ", sum(Temp_p)/size(Temp_p), E ,iter
      write(*,*) 'time step ',itime, "      x   ", sum(x)/size(x), E ,iter
    end if
    
    Temp_pp = Temp_p
    Temp_p = x

    ! DEBUG: Verify Temp_p after assignment
    if (itime .le. 2) then
       block
         integer(int12) :: dbg_ix2, dbg_idx2
         write(*,*) ''
         write(*,'(A,I6)') ' === VERIFY Temp_p AFTER ASSIGNMENT, itime=', itime
         write(*,'(A6,A14,A14)') 'ix', 'Temp_p(1D)', 'x(1D)'
         do dbg_ix2 = 1, nx
            dbg_idx2 = dbg_ix2 + (16-1)*nx
            write(*,'(I6,ES14.6,ES14.6)') &
                 dbg_ix2, Temp_p(dbg_idx2), x(dbg_idx2)
         end do
         write(*,*) '=== END VERIFY ==='
       end block
    end if

   !  if (TempDepProp .eq. 1) then
   !    CALL ChangeProp()
   !  end if
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  end subroutine simulate



end module evolution


module evolution
  use constants, only: real12, int12, TINY
  use inputs, only: NA, icattaneo, isteady, nx, ny, nz, T_System, time_step, mixing, grid, power_in
  use sptype, only: I4B
  use sparse, only: linbcg
  use globe_data, only: TPD, TPPD, inverse_time, heat, Grid1DHR
  use heating, only: heater
  use boundary_vector, only: boundary
  use cattaneo, only: S_catS
  implicit none

  private
  public :: evolve

contains

!!!##############################################################
!!! Subroutine to evolve the system for one time step.
!!! Inputs:
!!!   it - Current time step (integer)
!!!##############################################################
  subroutine EVOLVE(it)
    integer(int12), intent(in) :: it
    real(real12), dimension(NA) :: S, x, Q, Qdens, S_CAT, B
    integer(int12) :: i, j, ncg, itol, itmax, iss, ierr, xc, yc, zc
    integer(I4B) :: iter
    real(real12) :: dt, To, Hb, e, err, tol

    !----------------------
    ! Initialize vectors
    !----------------------
    B = 0.0_real12
    Q = 0.0_real12
    Qdens = 0.0_real12
    S_CAT = 0.0_real12
    S = 0.0_real12
    !----------------------
    
    !--------------------------------
    ! Calculate boundary Vector
    !--------------------------------
    call boundary(B)
    if (any(isnan(B(:)))) call exit
    !--------------------------------

    !--------------------------------
    ! Calculate heat
    !--------------------------------
    if ( power_in .gt. TINY) then
       call heater(it, Q, Qdens)
       if (any(isnan(Q(:)))) call exit
    end if
    heat(it) = sum(Q(:))
    !--------------------------------

    !--------------------------------
    ! Calculate Cattaneo correction
    !--------------------------------
    call S_catS(S_CAT)
    if (any(isnan(S_CAT))) call exit
    !--------------------------------

    !---------------------------------------------
    ! Construct S vector 
    !---------------------------------------------
    if (iSteady .eq. 0) then
       !S = - TPPD * inverse_time * (1 - mixing) * RhoHC / 2.0_real12 &
       !     - TPD * inverse_time * mixing * RhoHC &
       !     - Qdens - B
       S = - (inverse_time * (TPPD/2.0_real12) * Grid1DHR) - Qdens - B
       if (iCAttaneo .eq. 1) then
        print *, 'S_CAT = ', S_CAT

          S = S + S_CAT
       end if
    else
       S(:) = -Qdens(:) - B(:)
    end if
    ! print *, 'B = ', B
    ! print *, 'Qdens = ', Qdens
    ! print *, 'S = ', S
    ! print *, 'inverse_time = ', inverse_time
     print *, 'TPD = ', TPD
    !---------------------------------------------

!!!#################################################
!!! Call the CG method to solve the equation Ax=b.
!!!#################################################
!!! b/S:     Input - the b vector.
!!! x:     Input/Output - initial guess for x, overwritten with the final solution.
!!! itol:  Input - sets the tolerance method used to calculate error.
!!! tol:   Input - sets the convergence criteria.
!!! itmax: Input - sets the max number of iterations.
!!! iter:  Output - gives the number of the final iteration.
!!! err:   Output - records the error of the final iteration.
!!! iss:   Input - sets the Sparse Storage type (1=SRS, 2=SDS).
    if (it .eq. 1) call INIT_EVOLVE(it,x)
    itol=1
    tol=1.e-12_real12
    itmax=50000
    ncg = 0
    iter=ncg
    err=E
    iss=1
    call linbcg(S,x,itol=int(itol,I4B),tol=tol, itmax=int(itmax,I4B), iter=iter, &
         err=E, iss=int(iss,I4B))
!!!#################################################
    ! print*, 'X= ', x
    ! Update temperature profile
    call TP_UPDATE(x)
  end subroutine EVOLVE

!!#################################################
!!! Subroutine to initialize the evolve process.
!!! Inputs:
!!!   it - Current time step (integer)
!!!   x - Initial temperature profile (real array)
!!!#################################################
  subroutine INIT_EVOLVE(it, x)
    integer(int12), intent(in) :: it
    real(real12), dimension(NA), intent(out) :: x

    ! Initialize the temperature profile to the system temperature
    if (it .eq. 1) x = T_System
  end subroutine INIT_EVOLVE
!!!#################################################

!!!#################################################
!!! Subroutine to update the temperature profile after a time step.
!!! Inputs/Outputs:
!!!   x - Updated temperature profile (real array)
!!!#################################################
  subroutine TP_UPDATE(x)
    integer(int12) :: i, j, k, index
    real(real12), dimension(NA) :: x

    ! Update the temperature profile if changes are below a threshold
    do index = 1, NA
      if (abs(x(index)-TPD(index)) .lt. 1e-12_real12) then
        x(index)=TPD(index)
      end if
   end do
   
    ! Update previous and current temperature profiles
    TPPD = TPD
    TPD = x
   
  end subroutine TP_UPDATE
!!!#################################################

end module evolution


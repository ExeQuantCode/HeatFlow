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
  use sptype, only: I4B
  use sparse, only: linbcg
  use globe_data, only: Temp_p, Temp_pp, inverse_time, heat, lin_rhoc
  use heating, only: heater
  use boundary_vector, only: boundary
  use cattaneo, only: S_catS
  implicit none

  private
  public :: simulate

contains

!!!#################################################################################################
!!! Subroutine to evolve the system for one time step.
!!! Inputs:
!!!   itime - Current time step (integer)
!!!#################################################################################################
  subroutine simulate(itime)
    integer(int12), intent(in) :: itime
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
    !^^^^^^^^^^^^^^^^^^^^^
    
    !--------------------------------
    ! Calculate boundary Vector
    !--------------------------------
    CALL boundary(B)
    if (any(isnan(B(:)))) then
      write(*,*) "fatal error: NAN in B vector"
      stop
    end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !--------------------------------
    ! Calculate heat
    !--------------------------------
    if ( power_in .gt. TINY) then
       CALL heater(itime, Q, Qdens)
       if (any(isnan(Q(:)))) then
            write(*,*) "fatal error: NAN in Q vector"
            stop
         end if
       if (any(isnan(Qdens(:)))) then
            write(*,*) "fatal error: NAN in Qdens vector"
            stop
         end if
    end if
    heat = heat + sum(Q(:))
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !------------------------------------------
    ! Calculate Cattaneo correction
    !------------------------------------------
    if (iCAttaneo .eq. 1) then
       CALL S_catS(S_CAT)
       if (IVERB .gt. 4) write(*,*) "S_CAT average", sum(S_CAT)/size(S_CAT)
       if (any(isnan(S_CAT))) then
            write(*,*) "fatal error: NAN in S_CAT vector"
            stop
         end if
    end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !---------------------------------------------
    ! Construct S vector 
    !---------------------------------------------
    if (iSteady .eq. 0) then

       S = - inverse_time * Temp_p * lin_rhoc - Qdens - B
       if (iCAttaneo .eq. 1) then
          S = S + S_CAT
       end if
    else
       S = -Qdens - B
    end if
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    if (any(isnan(S(:)))) then
       write(*,*) "fatal error: NAN in S vector"
       stop
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
    x=Temp_p+(Temp_p-Temp_pp)
    itol=1
    tol=1.e-20_real12
    itmax=50000
    ncg = 0
    iter=ncg
    err=E
    iss=1
    CALL linbcg(S,x,itol=int(itol,I4B),tol=tol, itmax=int(itmax,I4B), iter=iter, &
         err=E, iss=int(iss,I4B))
         
    if (any(isnan(x(:)))) then
       write(*,*) "fatal error: NAN in x tempurature vector"
       stop
    end if
   !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (IVERB .gt. 4) then
      write(*,*) 
      write(*,*) 'time step ', itime, "      T   ", sum(Temp_p)/size(Temp_p), E ,iter
      write(*,*) 'time step ',itime, "      x   ", sum(x)/size(x), E ,iter
    end if
    Temp_pp = Temp_p
    Temp_p = x

  end subroutine simulate



end module evolution


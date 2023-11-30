module evolution
  use constants, only: real12, int12, TINY
  use inputs, only: NA, icattaneo, isteady, nx, ny, nz, T_bath, time_step
  use sptype, only: I4B
  use sparse, only: linbcg
  use globe_data, only: TPD, TPPD, inverse_time
  use heating, only: heater
  use boundary_vector, only: boundary
  use cattaneo, only: S_catS
  implicit none

  private

  public :: evolve

  contains


  subroutine EVOLVE(it)
    real(real12), dimension(NA) :: S, x, Q, S_CAT
    real(real12), dimension(NA) :: B
    integer(int12), intent(in) :: it
    integer(int12) :: i,j, ncg, itol, itmax, iss
    integer(I4B) :: iter
    real(real12) :: dt, To, Hb, e, err, tol
    
    


    !----------------------------------------------
    !Make S-vector
    !S vector has 4 terms!
    !Tp_old, (1/delta t) T_j,old = old temps
    !Boundary B_j
    !S_j,cat = cattaneo correction
    !Q_j = heater
    !---------------------------------------------
    
    !** CALL TP_OLD(j,TO)
    B=0
    !**CALL Boundary
    call boundary(B)
    !**CALL HEATER
    call heater(it,Q)
    !**Call S_CAT
    call s_catS(s_cat)

    if (iSteady.eq.0) then
       do j=1,NA
          S(j)=(-(TPPD(j)*inverse_time/(2.0_real12)))-Q(j)-B(j)
          ! print*,B
          ! print*,S_CAT
          if (iCAttaneo.eq.1)  then
              ! print*, it
              ! print*, TPD(j)-TPPD(j)
              ! print*, 's_cat= ' ,s_cat(j)
              ! print*, 'S= ',S(j)
              ! print*, time_step
              ! print*, (time_step ** 2._real12)
               S(j)=S(j)+S_cat(j)
          end if
       end do
    else
      S=0
       do j=1,NA
       	 S(j)=S(j)-Q(j)-B(j)
       end do
    end if
    ! print*,TPD
    !!!#################################################
    !!! Call the CG method to solve the equation Ax=b.
    !!!#################################################
    ! b/S:     Input - the b vector.
    ! x:     Input/Output - initial guess for x, overwritten with the final solution.
    ! itol:  Input - sets the tolerance method used to calculate error.
    ! tol:   Input - sets the convergence criteria.
    ! itmax: Input - sets the max number of iterations.
    ! iter:  Output - gives the number of the final iteration.
    ! err:   Output - records the error of the final iteration.
    ! iss:   Input - sets the Sparse Storage type (1=SRS, 2=SDS).
    !-------------------------------
    !initialise variables
    if (it .eq. 1) call INIT_EVOLVE(it,x)
    !------------------------
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


    call TP_UPDATE(x)



  end subroutine EVOLVE


  subroutine INIT_EVOLVE(it, x)
    integer(int12), intent(in) :: it
    real(real12), dimension(NA), intent(out) :: x
    ! Ask Frank about this, why cant x be equal to T_Bath?
    if (it .eq. 1) x=T_Bath+1d-13

  end subroutine INIT_EVOLVE

  subroutine TP_UPDATE(x)
    integer(int12) :: i, j, k, index
    real(real12), dimension(NA) :: x
    !** This isnt right
    ! print*, x-TPD
    ! if (any(abs(x-TPD).lt.1e-9_real12)) x=TPD
    TPPD = TPD
    TPD = x
    x=x+1d-12
   
    

  end subroutine TP_UPDATE

end module evolution
  

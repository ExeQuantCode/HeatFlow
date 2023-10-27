MODULE EVOLUTION
  use constants, only: real12, int12
  use inputs, only: NA, icattaneo, isteady
  use sptype, only: I4B
  use sparse, only: linbcg
  use globe_data, only: TD, TPD
  implicit none

  private

  public :: evolve

contains


  subroutine EVOLVE(it,b)
    real(real12), dimension(NA) :: S, x, Q, S_CAT
    real(real12), dimension(NA), intent(in) :: B
    integer(int12), intent(in) :: it
    integer(int12) :: i,j, nT, ncg, itol, itmax, iss
    integer(I4B) :: iter
    real(real12) :: dt, To, Hb, e, err, tol
    
    
    !-------------------------------
    !initialise variables
    !**CALL INIT_EVOLVE(it,TP,TPP)
    !------------------------

    !----------------------------------------------
    !Make S-vector
    !S vector has 4 terms!
    !Tp_old, (1/delta t) T_j,old = old temps
    !Boundary B_j
    !S_j,cat = cattaneo correction
    !Q_j = heater
    !---------------------------------------------
    
    S=0
    
    !** CALL TP_OLD(j,TO)
    !**   S=TP/dt
    
    !**CALL Boundary
    
    !**CALL HEATER
    
    !**Call S_CAT
    
   
    if (iSteady.eq.0) then
       
       do j=1,nT
          S(j)=TD(j)/dt+Q(j)+B(j)
          if (iCAttaneo.eq.1)  then 
             !** S(J)=S(j)+S_cat(j)
          end if
       end do
    else
       do j=1,nT
       	 S(j)=Q(j)+B(j)
       end do
    end if
    
   
    
    !!!#################################################
    !!! Call the CG method to solve the equation Ax=b.
    !!!#################################################
    ! b:     Input - the b vector.
    ! x:     Input/Output - initial guess for x, overwritten with the final solution.
    ! itol:  Input - sets the tolerance method used to calculate error.
    ! tol:   Input - sets the convergence criteria.
    ! itmax: Input - sets the max number of iterations.
    ! iter:  Output - gives the number of the final iteration.
    ! err:   Output - records the error of the final iteration.
    ! iss:   Input - sets the Sparse Storage type (1=SRS, 2=SDS).
    itol=1
    tol=1D-9 
    itmax=500
    ncg = 0
    iter=ncg
    err=E
    iss=1
    x=100
     call linbcg(b,x,itol=int(itol,I4B),tol=1D-9, itmax=int(itmax,I4B), iter=iter, &
	err=E, iss=int(iss,I4B))
    !!!#################################################
    
    

    
    CALL TP_UPDATE()

  end subroutine EVOLVE


  
  subroutine TP_UPDATE()
    integer(int12) :: j
    DO j = 1, NA
       TD(j)=TPD(j)
       TPD(j)=TD(j)
    end DO
  end subroutine TP_UPDATE

end module
  

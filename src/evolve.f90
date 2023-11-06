module evolution
  use constants, only: real12, int12
  use inputs, only: NA, icattaneo, isteady, nx, ny, nz, T_bath, time_step
  use sptype, only: I4B
  use sparse, only: linbcg
  use globe_data, only: TD, TPD, TN, Told, T
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
        
    !** CALL TP_OLD(j,TO)
    S=0
    
    !**CALL Boundary
    call boundary(B)
    !**CALL HEATER
    call heater(it,Q)
    !**Call S_CAT
    call s_catS(s_cat)
   
    if (iSteady.eq.0) then
       do j=1,NA
          S(j)=TD(j)/(time_step)+Q(j)+B(j)
          if (iCAttaneo.eq.1)  then 
               S(j)=S(j)+S_cat(j)
          end if
       end do
    else
       do j=1,NA
       	 S(j)=S(j)+Q(j)+B(j)
       end do
    end if
    !**Unfinished implementation of b vector calculation


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
    itol=1
    tol=1D-9 
    itmax=500
    ncg = 0
    iter=ncg
    err=E
    iss=1
    x=100
    call linbcg(S,x,itol=int(itol,I4B),tol=1D-9, itmax=int(itmax,I4B), iter=iter, &
	  err=E, iss=int(iss,I4B))
    !!!#################################################
    
    

    
    call TP_UPDATE(x)



  end subroutine EVOLVE


  
  subroutine TP_UPDATE(x)
    integer(int12) :: i, j, k, index
    real(real12), dimension(NA) :: x
    !** This isnt right
    TPD = TD
    TD = x
   
    Told = T
    T = TN
    index = 1
    do k = 1, nz
      do j = 1, ny
         do i = 1, nx
            TN(i,j,k) = X(index)
            index = index+1
         end do
      end do
    end do 
  end subroutine TP_UPDATE

end module evolution
  

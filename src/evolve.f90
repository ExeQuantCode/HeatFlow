MODULE EVOLUTION
  use constants, only: real12, int12, TINY
  use inputs, only: NA, icattaneo, isteady
  use hmatrixmod, only: hmatrix
  use sptype, only: I4B
  use globe_data, only: H, ra
  use sparse, only: SRSin, linbcg

  implicit none

  private

  public :: evolve

contains

  subroutine EVOLVE(it,b)
    real(real12), dimension(NA) :: S, T, TP, TPP, x
    real(real12), dimension(NA) :: Q, B, S_CAT
    integer(int12) :: i,j, nT, it, ncg, itol, itmax, iss
    integer(I4B) :: iter
    real(real12) :: dt, H0, hboundary, To, Hb, e, err, tol
    !TP  is the previous set of temperatures
    !TPP goes back two time steps.
    if (.not. allocated(H)) allocate(H(NA,NA))
    !-------------------------------
    !initialise variables
    !CALL INIT_EVOLVE(it,TP,TPP)
    !------------------------
    
    !H_ij T_i =S_j
    
    !------------------------------------------------
    !Make hmatrix
    !Hmatrix consists of H_ij,0 and H_ij,B (heat flow across grid and matrix correction for boundaries
    !-----------------------------------------------
    if (it .eq. 1) then
	    H=0.0
	    hboundary = 0.0
	    do i=1,NA 
	       do j=1,NA
		  CALL HMATRIX(i,j,H0)
		  
		  H(i,j)=H0
		  !if (HBoundary.eq.1) then
		     !CALL HATRIX_BOUND(i,j,HB)
		  !   H(i,j)=H(i,j)+HB(i,j)
		  !end if
	       end do
	    end do
	    H(1,1) = H(2,2)
    end if 
    !print*, H
   ! Convert the matrix into Sparse Diagonal Storage.
   ! call SDSin(A,TINY, da)
   ! Convert the matrix into Sparse Row Storage.
   call SRSin(H,TINY, ra)

    !----------------------------------------------
    !Make S-vector
    !S vector has 4 terms!
    !Tp_old, (1/delta t) T_j,old = old temps
    !Boundary B_j
    !S_j,cat = cattaneo correction
    !Q_j = heater
    !---------------------------------------------
    
    S=0
    
    !CALL TP_OLD(j,TO)
    !   S=TP/dt
    
    !CALL Boundary
    
    !CALL HEATER
    
    !Call S_CAT
    
    !S=0.0
    if (iSteady.eq.0) then
       
       do j=1,nT
          S(j)=TP(j)/dt+Q(j)+B(j)
          if (iCAttaneo.eq.1)  then 
             !  S(J)=S(j)+S_cat(j)
             !NOT IMPLEMENTED
          end if
       end do
    else
       do j=1,nT
       	 S(j)=Q(j)+B(j)
       end do
    end if
    
    !----------------------------------------------------
    
    !now solve H(ij)T(i)=S(J)
    !CALL SOLVER(H,T,S)
    
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
    
    
    !-------
    
    CALL TP_UPDATE(T,TP,TPP)

  end subroutine EVOLVE


  
  subroutine TP_UPDATE(T,TP,TPP)
    integer :: j
    real(real12), dimension(NA) :: T, TP, TPP
    DO j = 1, na
       TPP(j)=TP(j)
       TP(j)=T(j)
    end DO
  end subroutine TP_UPDATE

end module
  

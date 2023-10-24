MODULE EVOLUTION
  use constants
  use parameters
  use initial
  use constructions
  implicit none

contains

  subroutine EVOLVE(iT)
    real(real12), dimension(NA,NA) :: H
    real(real12), dimension(NA) :: S, T, TP, TPP
    real(real12), dimension(NA) :: Q, B, S_CAT
    !TP  is the previous set of temperatures
    !TPP goes back two time steps.
    
    !-------------------------------
    !initialise variables
    CALL SUNROUTINE INIT_EVOLVE(it,TP,TPP)
    !------------------------
    
    !H_ij T_i =S_j
    
    !------------------------------------------------
    !Make hmatrix
    !Hmatrix consists of H_ij,0 and H_ij,B (heat flow across grid and matrix correction for boundaries
    !-----------------------------------------------
    H=0.0
    do i=1,NA 
       do j=1,NA
          CALL HMATRIX(i,j,H0)
          
          H(i,j)=H0
          if (HBoundary.eq.1) then
             CALL HMATRIX_BOUND(i,j,HB)
             H(i,j)=H(i,j)+HB(i,j)
          end if
       end do
    end do
    
    !----------------------------------------------
    !Make S-vector
    !S vector has 4 terms!
    !Tp_old, (1/delta t) T_j,old = old temps
    !Boundary B_j
    !S_j,cat = cattaneo correction
    !Q_j = heater
    !---------------------------------------------
    
    S=0
    
!    CALL TP_OLD(j,TO)
    !   S=TP/dt
    
    CALL SBoundary(B)
    
    CALL HEATER(Q)
    
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
       j=1,nT
       S(j)=Q(j)+B(j)
    end if
    
    !----------------------------------------------------
    
    !now solve H(ij)T(i)=S(J)
    !CALL SOLVER(H,T,S)
    
    
    !-------
    
    CALL TP_UPDATE(T,TP,TPP)
    
    
  end subroutine EVOLVE


  
  subroutine TP_UPDATE(T,TP,TPP)
    integer :: j
    real(rea12), dimension(NA) :: T, TP, TPP
    DO J=1,na
       TPP(J)=TP(J)
       TP(j)=T(j)
    end DO
  end subroutine TP_UPDATE
  

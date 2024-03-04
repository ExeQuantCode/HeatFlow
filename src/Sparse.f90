!!!#################################################
!!! This module is for the Conjugate Gradient solver.
!!! Author: Frank Davis
!!!#################################################
MODULE sputil
   use constants, only: real12
!Parameters for crossover from serial to parallel algorithms ...
!...(these are used only within this sputil module):
  use sptype 
  IMPLICIT NONE
  INTEGER(I4B), PARAMETER :: NPAR_ARTH=16,NPAR2_ARTH=8

  !Routines that move data:
  INTERFACE array_copy
     MODULE PROCEDURE array_copy_r, array_copy_i
  END INTERFACE array_copy

  INTERFACE assert_eq
     MODULE PROCEDURE assert_eq2,assert_eq3,assert_eq4,assert_eqn
  END INTERFACE assert_eq

  INTERFACE arth
     MODULE PROCEDURE arth_r, arth_i
  END INTERFACE arth

  INTERFACE scatter_add
     MODULE PROCEDURE scatter_add_r
  END INTERFACE scatter_add
  
CONTAINS
  !Routines that move data:
  SUBROUTINE array_copy_r(src,dest,n_copied,n_not_copied)
    !Copy array where size of source not known in advance.
    real(real12), DIMENSION(:), INTENT(IN) :: src
    real(real12), DIMENSION(:), INTENT(OUT) :: dest 
    INTEGER(I4B), INTENT(OUT) :: n_copied, n_not_copied
    n_copied=min(size(src),size(dest))
    n_not_copied=size(src)-n_copied
    dest(1:n_copied)=src(1:n_copied)
  END SUBROUTINE array_copy_r
  
  SUBROUTINE array_copy_d(src,dest,n_copied,n_not_copied)
    real(real12), DIMENSION(:), INTENT(IN) :: src
    real(real12), DIMENSION(:), INTENT(OUT) :: dest
    INTEGER(I4B), INTENT(OUT) :: n_copied, n_not_copied
    n_copied=min(size(src),size(dest))
    n_not_copied=size(src)-n_copied
    dest(1:n_copied)=src(1:n_copied)
  END SUBROUTINE array_copy_d

  SUBROUTINE array_copy_i(src,dest,n_copied,n_not_copied)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: src
    INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: dest
    INTEGER(I4B), INTENT(OUT) :: n_copied, n_not_copied
    n_copied=min(size(src),size(dest))
    n_not_copied=size(src)-n_copied
    dest(1:n_copied)=src(1:n_copied)
  END SUBROUTINE array_copy_i

  FUNCTION assert_eq2(n1,n2,string)
    !Report and die if integers not all equal (used for size checking).
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(I4B), INTENT(IN) :: n1,n2
    INTEGER(I4B) :: assert_eq2
    if (n1 == n2) then
       assert_eq2=n1 
    else
       write (*,*) 'nrerror: an assert_eq failed with this tag:', string
       STOP 'program terminated by assert_eq2'
    end if
  END FUNCTION assert_eq2

  FUNCTION assert_eq3(n1,n2,n3,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(I4B), INTENT(IN) :: n1,n2,n3
    INTEGER(I4B) :: assert_eq3

    if (n1 == n2 .and. n2 == n3) then
       assert_eq3=n1
    else
       write (*,*) 'nrerror: an assert_eq failed with this tag:', string
       STOP 'program terminated by assert_eq3'
    end if
  END FUNCTION assert_eq3

  FUNCTION assert_eq4(n1,n2,n3,n4,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(I4B), INTENT(IN) :: n1,n2,n3,n4
    INTEGER(I4B) :: assert_eq4
    if (n1 == n2 .and. n2 == n3 .and. n3 == n4) then
       assert_eq4=n1
    else
       write (*,*) 'nrerror: an assert_eq failed with this tag:', string
       STOP 'program terminated by assert_eq4'
    end if
  END FUNCTION assert_eq4

  FUNCTION assert_eqn(nn,string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: nn
    INTEGER(I4B) :: assert_eqn
    if (all(nn(2:) == nn(1))) then
       assert_eqn=nn(1)
    else
       write (*,*) 'nrerror: an assert_eq failed with this tag:', string
       STOP 'program terminated by assert_eqn'
    end if
  END FUNCTION assert_eqn

  SUBROUTINE nrerror(string)
    !Report a message, then die.
    CHARACTER(LEN=*), INTENT(IN) :: string 
    write (*,*) 'nrerror: ',string
    STOP 'program terminated by nrerror'
  END SUBROUTINE nrerror

  !Routines relating to polynomials and recurrences:

  FUNCTION arth_r(first,increment,n)
    !Array function returning an arithmetic progression.
    real(real12), INTENT(IN) :: first,increment
    INTEGER(I4B), INTENT(IN) :: n
    real(real12), DIMENSION(n) :: arth_r 
    INTEGER(I4B) :: k,k2
    real(real12) :: temp
    if (n > 0) arth_r(1)=first
    if (n <= NPAR_ARTH) then
       do k=2,n
          arth_r(k)=arth_r(k-1)+increment
       end do
    else
       do k=2,NPAR2_ARTH
          arth_r(k)=arth_r(k-1)+increment
       end do
       temp=increment*NPAR2_ARTH
       k=NPAR2_ARTH
       do
          if (k >= n) exit
          k2=k+k 
          arth_r(k+1:min(k2,n))=temp+arth_r(1:min(k,n-k))
          temp=temp+temp
          k=k2
       end do
    end if
  END FUNCTION arth_r

  
  FUNCTION arth_d(first,increment,n)
    real(real12), INTENT(IN) :: first,increment
    INTEGER(I4B), INTENT(IN) :: n
    real(real12), DIMENSION(n) :: arth_d
    INTEGER(I4B) :: k,k2
    real(real12) :: temp
    if (n > 0) arth_d(1)=first
    if (n <= NPAR_ARTH) then
       do k=2,n
          arth_d(k)=arth_d(k-1)+increment
       end do
    else
       do k=2,NPAR2_ARTH
          arth_d(k)=arth_d(k-1)+increment
       end do
       temp=increment*NPAR2_ARTH
       k=NPAR2_ARTH
       do
          if (k >= n) exit
          k2=k+k
          arth_d(k+1:min(k2,n))=temp+arth_d(1:min(k,n-k))
          temp=temp+temp
          k=k2
       end do
    end if
  END FUNCTION arth_d

  FUNCTION arth_i(first,increment,n)
    INTEGER(I4B), INTENT(IN) :: first,increment,n
    INTEGER(I4B), DIMENSION(n) :: arth_i
    INTEGER(I4B) :: k,k2,temp
    if (n > 0) arth_i(1)=first
    if (n <= NPAR_ARTH) then
       do k=2,n 
          arth_i(k)=arth_i(k-1)+increment
       end do
    else
       do k=2,NPAR2_ARTH
          arth_i(k)=arth_i(k-1)+increment
       end do
       temp=increment*NPAR2_ARTH
       k=NPAR2_ARTH
       do
          if (k >= n) exit
          k2=k+k
          arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
          temp=temp+temp
          k=k2
       end do
    end if
  END FUNCTION arth_i

  FUNCTION outerdiff_r(a,b)
    real(real12), DIMENSION(:), INTENT(IN) :: a,b
    real(real12), DIMENSION(size(a),size(b)) :: outerdiff_r
    outerdiff_r = spread(a,dim=2,ncopies=size(b)) - &
         spread(b,dim=1,ncopies=size(a))
  END FUNCTION outerdiff_r

  FUNCTION outerdiff_d(a,b)
    real(real12), DIMENSION(:), INTENT(IN) :: a,b
    real(real12), DIMENSION(size(a),size(b)) :: outerdiff_d
    outerdiff_d = spread(a,dim=2,ncopies=size(b)) - &
         spread(b,dim=1,ncopies=size(a))
  END FUNCTION outerdiff_d
  
  FUNCTION outerdiff_i(a,b)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: a,b
    INTEGER(I4B), DIMENSION(size(a),size(b)) :: outerdiff_i
    outerdiff_i = spread(a,dim=2,ncopies=size(b)) - &
         spread(b,dim=1,ncopies=size(a))
  END FUNCTION outerdiff_i
  
  !Routines for scatter-with-combine.
  SUBROUTINE scatter_add_r(dest,source,dest_index)
    real(real12), DIMENSION(:), INTENT(OUT) :: dest
    real(real12), DIMENSION(:), INTENT(IN) :: source
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: dest_index
    INTEGER(I4B) :: m,n,j,i
    n=assert_eq2(size(source),size(dest_index),'scatter_add_r')
    m=int(size(dest), int12)
    do j=1,n
       i=dest_index(j)
       if (i > 0 .and. i <= m) dest(i)=dest(i)+source(j)
    end do
  END SUBROUTINE scatter_add_r

  SUBROUTINE scatter_add_d(dest,source,dest_index)
    real(real12), DIMENSION(:), INTENT(OUT) :: dest
    real(real12), DIMENSION(:), INTENT(IN) :: source
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: dest_index
    INTEGER(I4B) :: m,n,j,i
    n=assert_eq2(size(source),size(dest_index),'scatter_add_d')
    m=size(dest)
    do j=1,n
       i=dest_index(j)
       if (i > 0 .and. i <= m) dest(i)=dest(i)+source(j)
    end do
  END SUBROUTINE scatter_add_d

  FUNCTION find_index(array, value) RESULT(idx)
    INTEGER(I4B), DIMENSION(:), INTENT(IN) :: array
    INTEGER(I4B), INTENT(IN) :: value
    INTEGER(I4B) :: idx
    INTEGER(I4B) :: i

    idx = 0 ! Default value indicating not found
    DO i = 1, SIZE(array)
       IF (array(i) == value) THEN
          idx = i
          EXIT
       END IF
    END DO
  END FUNCTION find_index

END MODULE sputil



module Sparse
use sptype
use sputil
contains


  SUBROUTINE SRSin(a,thresh,ra)
    USE sptype; USE sputil, ONLY : arth,assert_eq
    IMPLICIT NONE
    real(real12), DIMENSION(:,:), INTENT(IN) :: a
    real(real12), INTENT(IN) :: thresh
    TYPE(sprs2_dp), INTENT(OUT) :: ra
    INTEGER(I4B) :: n,len
    logical, DIMENSION(size(a,1),size(a,2)) :: mask
    n=assert_eq(size(a,1),size(a,2),'SRSin')
    mask=abs(a)>thresh
    len=count(mask)
    allocate(ra%val(len),ra%irow(len),ra%jcol(len))
    ra%n=n
    ra%len=len
    ra%val=pack(a,mask)
    ra%irow=pack(spread(arth(1,1,n),2,n),mask)
    ra%jcol=pack(spread(arth(1,1,n),1,n),mask)
  END SUBROUTINE SRSin


  
  
  SUBROUTINE SDSin(a, thresh, da)
    USE sptype; 
    USE sputil, ONLY : arth, assert_eq, find_index
    IMPLICIT NONE
    real(real12), DIMENSION(:,:), INTENT(IN) :: a
    real(real12), INTENT(IN) :: thresh
    TYPE(diag_sprs_dp), INTENT(OUT) :: da
    INTEGER(I4B) :: n, d, ndiags
    logical, DIMENSION(size(a,1),size(a,2)) :: mask
    logical, DIMENSION(2*size(a,1)-1) :: any_diags
    integer(I4B) :: i, j, o, m, c
    integer(I4B), dimension(:), allocatable :: diag_off

    n = assert_eq(size(a,1), size(a,2), 'SDSin')


    ! Identify diagonals with non-zero values
    any_diags = .false.
    ndiags = 0
    col_loop: do i = 1, n
       row_loop: do j = 1, n
          o = i - j ! offset = col-row
          d = o + n ! diag_index = offset + n (matrix side length)
          if (abs(a(i,j)).gt.thresh.and..not.any_diags(d)) then
             ndiags = ndiags + 1
             any_diags(d) = .true.
          end if
       end do row_loop
    end do col_loop

    ! Extract diagonal offsets
    allocate(diag_off(ndiags))
    m = 0
    c = 0
    diag_loop: do i = 1, size(any_diags)
       m = m + 1
       if (any_diags(i)) then
          c = c + 1
          diag_off(c) = m - n
       end if
    end do diag_loop

    da%n = n
    da%num_diags = ndiags
    allocate(da%diag_offsets(da%num_diags))
    allocate(da%vals(da%num_diags, da%n))
    da%diag_offsets = diag_off

    ! Fill da%vals with non-zero values
    col_loop2: do i = 1, n
       row_loop2: do j = 1, n
          o = i - j
          d = o + n
          if (abs(a(i,j)) > thresh) then
             c = find_index(da%diag_offsets, d - n)
             da%vals(c, j) = a(i, j)
          end if
       end do row_loop2
    end do col_loop2
  END SUBROUTINE SDSin





  SUBROUTINE SRSax(ra, x, b)
    USE sptype; USE sputil, ONLY : assert_eq
    IMPLICIT NONE
    TYPE(sprs2_dp), INTENT(IN) :: ra
    real(real12), DIMENSION (:), INTENT(IN) :: x
    real(real12), DIMENSION (:), INTENT(OUT) :: b
    INTEGER(I4B) :: ndum, j, i, m, n

    ndum = assert_eq(ra%n, size(x), size(b), 'SRSax')
    b = 0.0_dp

    n = size(ra%val)
    m = size(b)
    do j = 1, n
       i = ra%irow(j)
       if (i.gt.0.and.i.le.m) THEN
          b(i) = b(i) + ra%val(j) * x(ra%jcol(j))
       END IF
    END DO
  END SUBROUTINE SRSax

  SUBROUTINE SDSax(da, x, b)
    use sptype 
    use sputil, only : assert_eq
    implicit none
    type(diag_sprs_dp), intent(IN) :: da
    real(real12), dimension (:), intent(IN) :: x
    real(real12), dimension (:), intent(OUT) :: b
    integer(I4B) :: ndum, n, i, j, col
    ndum = assert_eq(size(x), size(b), 'SDSax')
    n=da%n
    b = 0.0_dp
    diag_loop: do j = 1, da%num_diags
       ! Loop over each element in the diagonal
       row_loop: do i = 1, n
          !row = i
          col = da%diag_offsets(j)+i
          mat_boundry: if (col.gt.0.and.col.le.da%n) then
             b(i) = b(i) + da%vals(j, i) * x(col)
          end if mat_boundry
       end do row_loop
    end do diag_loop
  END SUBROUTINE SDSax

  SUBROUTINE SRStx(ra,x,b)
    USE sptype; USE sputil, ONLY : assert_eq,scatter_add
    IMPLICIT NONE
    type(sprs2_dp), intent(IN) :: ra
    real(real12), dimension (:), intent(IN) :: x
    real(real12), dimension (:), intent(OUT) :: b
    integer(I4B) :: ndum,i,j,n,m
    ndum=assert_eq(ra%n,size(x),size(b),'SRStx')
    b=0.0_dp
    n = size(ra%val)
    m = size(b)
    do i = 1, n
       j = ra%jcol(i)
       if (j.gt.0.and.j.le.m) then
          b(j) = b(j) + ra%val(i) * x(ra%irow(i))
       end if
    end do
  END SUBROUTINE SRStx

SUBROUTINE SDStx(da, x, b)
    use sptype 
    use sputil, only : assert_eq
    implicit none
    type(diag_sprs_dp), intent(IN) :: da
    real(real12), dimension (:), intent(IN) :: x
    real(real12), dimension (:), intent(OUT) :: b
    integer(I4B) :: n, i, j, col
    n = assert_eq(da%n, size(x), size(b), 'SDSax')
    b = 0.0_dp
    diag_loop: do j = 1, da%num_diags
       ! Loop over each element in the diagonal
       row_loop: do i = 1, n
          col = -da%diag_offsets(j)+i
          mat_boundry: if (col.gt.0.and.col.le.da%n) then
             b(col) = b(col) + da%vals(j, col) * x(i)
          end if mat_boundry
       end do row_loop
    end do diag_loop
  END SUBROUTINE SDStx

  SUBROUTINE SRStp(ra)
    USE sptype
    IMPLICIT NONE
    TYPE(sprs2_dp), INTENT(INOUT) :: ra
    !Replaces ra, in sparse matrix format, by its transpose.
    INTEGER(I4B), DIMENSION(:), POINTER :: temp
    temp=>ra%irow !We need only swap the row and column pointers.
    ra%irow=>ra%jcol
    ra%jcol=>temp
  END SUBROUTINE SRStp

  SUBROUTINE SRSdiag(ra,b)
    USE sptype; USE sputil, ONLY : array_copy,assert_eq
    IMPLICIT NONE
    TYPE(sprs2_dp), INTENT(IN) :: ra
    real(real12), DIMENSION(:), INTENT(OUT) :: b
    real(real12), DIMENSION(size(b)) :: val
    INTEGER(I4B) :: k,l,ndum,nerr
    INTEGER(I4B), DIMENSION(size(b)) :: i
    LOGICAL(LGT), DIMENSION(:), ALLOCATABLE :: mask
    ndum=assert_eq(ra%n,size(b),'SRSdiag')
    l=ra%len
    allocate(mask(l))
    mask = (ra%irow(1:l) == ra%jcol(1:l))
    call array_copy(pack(ra%val(1:l),mask),val,k,nerr)
    i(1:k)=pack(ra%irow(1:l),mask)
    deallocate(mask)
    b=0.0
    b(i(1:k))=val(1:k)
  END SUBROUTINE SRSdiag

  SUBROUTINE SDSdiag(da,b)
    USE sptype; 
    USE sputil, ONLY : assert_eq
    IMPLICIT NONE
    TYPE(diag_sprs_dp), INTENT(IN) :: da
    real(real12), DIMENSION(:), INTENT(OUT) :: b
    integer(I4B) :: ndum, d
    ndum=assert_eq(da%n,size(b),'SDSdiag')
    d=find_index(da%diag_offsets, 0)
    !d=(da%n)/2
    b(:)=da%vals(d,:)
  END SUBROUTINE SDSdiag







  SUBROUTINE linbcg(b,x,itol,tol,itmax,iter,err,iss)
    USE sptype; USE sputil, ONLY : assert_eq,nrerror
    IMPLICIT NONE
    real(real12), DIMENSION(:), INTENT(IN) :: b
    real(real12), DIMENSION(:), INTENT(INOUT) :: x
    INTEGER(I4B), INTENT(IN) :: itol,itmax,iss
    real(real12), INTENT(IN) :: tol
    INTEGER(I4B), INTENT(INOUT) :: iter
    real(real12), INTENT(OUT) :: err
    real(real12), PARAMETER :: EPS=1.0e-14_dp
    INTEGER(I4B) :: n
    real(real12) :: ak,akden,bk,bkden,bknum,bnrm,dxnrm,xnrm,zm1nrm,znrm
    real(real12), DIMENSION(size(b)) :: p,pp,r,rr,z,zz
    n=assert_eq(size(b),size(x),'linbcg')
    !iter=0
    !Calculate initial residual. Input to atimes is
    !x(1:n), output is r(1:n); the final 0
    !indicates that the matrix (not its trans-
    !pose) is to be used.
    !write(6,*) 'x',x
    !write(6,*) 'r',r
    !write(6,*) 'iss',iss
    call atimes(x,r,0,iss)
    !write(6,*) 'x',x
    !write(6,*) 'r',r
    !write(6,*) 'iss',iss
    r=b-r
    rr=r
    !Uncomment this line to get the “minimum residual” variant of the algorithm.
    call atimes(r,rr,0,iss)
    !Calculate norms for use in stopping criterion, and initialize z.
    select case(itol) 
    case(1)
       bnrm=sqrt(dot_product(b,b))
       call asolve(r,z,0,iss) 
    case(2)
       call asolve(b,z,0,iss)
       bnrm=sqrt(dot_product(z,z))
       call asolve(r,z,0,iss)
    case default
       call nrerror('illegal itol in linbcg')
    end select

    solve_loop: do
       if (iter > itmax) exit
       iter=iter+1
       call asolve(rr,zz,1,iss) 
       bknum=dot_product(z,rr) 
       if (iter == 1) then
          p=z
          pp=zz
       else
          bk=bknum/bkden
          p=bk*p+z
          pp=bk*pp+zz
       end if

       call atimes(p,z,0,iss)
       bkden=bknum 
       akden=dot_product(z,pp)
       ak=bknum/akden
       call atimes(pp,zz,1,iss)
       x=x+ak*p
       r=r-ak*z
       rr=rr-ak*zz
       call asolve(r,z,0,iss) 

       select case(itol)
       case(1)
          err=sqrt(dot_product(r,r))/bnrm
       case(2)
          err=sqrt(dot_product(z,z))/bnrm
       end select

      !  write (*,*) ' iter=',iter,' err=',err, tol
       if (err.le.tol) exit
    end do solve_loop
  END SUBROUTINE linbcg

  SUBROUTINE atimes(x,r,itrnsp,iss)
    USE sptype 
    USE sputil, ONLY : assert_eq,nrerror
    USE globe_data !The matrix is accessed through this module.
    real(real12), DIMENSION(:), INTENT(IN) :: x
    real(real12), DIMENSION(:), INTENT(OUT) :: r
    INTEGER(I4B), INTENT(IN) :: itrnsp,iss
    INTEGER(I4B) :: n
    n=assert_eq(size(x),size(r),'atimes')
    select case(iss)
    case(1)
       if (itrnsp == 0) then
          call SRSax(ra,x,r)
       else
          call SRStx(ra,x,r)
       end if
    case(2)
       if (itrnsp == 0) then
          call SDSax(da,x,r)
       else
          call SDStx(da,x,r)
       end if
    case default
       call nrerror('illegal iss in atimes')  
    end select
  END SUBROUTINE atimes

  SUBROUTINE asolve(b,x,itrnsp,iss)
    USE sptype
    USE sputil, ONLY : assert_eq,nrerror
    USE globe_data !The matrix is accessed through this module.
    real(real12), DIMENSION(:), INTENT(IN) :: b
    real(real12), DIMENSION(:), INTENT(OUT) :: x
    INTEGER(I4B), INTENT(IN) :: itrnsp,iss
    INTEGER(I4B) :: ndum
    ndum=assert_eq(size(b),size(x),'asolve')
    select case(iss)
    case(1)
       call SRSdiag(ra,x)
    case(2)
       call SDSdiag(da,x)
    case default
       call nrerror('illegal iss in asolve')  
    end select
    if (any(x == 0.0)) call nrerror('asolve: singular diagonal matrix')
    x=b/x
  END SUBROUTINE asolve

end module Sparse

!###################################################################################################
! Module: TempDep
! Description: This module reads the temperature dependent properties from the Material table file ...
!        ... for each grid point and constructs the sparse matrix
! Authors: Harry Mclean
! Variable descriptions:
!  index: Keeps track of the current grid point index
!  filename: Name of the Material table file associated with the current grid point
!  ix, iy, iz: Indices for the current grid point in the x, y, and z directions, respectively
!  i, j, k: General purpose loop variables
!  num_rows, num_cols: Number of rows and columns in the temperature table
!  iostat: I/O status for file operations
!  temp_table: 2D array to hold the temperature dependent properties
!
! Subroutine descriptions:
!  ChangeProp: Main subroutine that loops over all grid points, ...
!        ... reads the temperature dependent properties, and constructs the sparse matrix
!  ReadTempDepTable: Reads the temperature dependent properties ... 
!         ... from the Material table file for a given grid point
!###################################################################################################
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!                       THIS FEATURE IS A WORK IN PROGRESS. DO NOT USE.                    !!!!!
!!!!!                                                                                          !!!!!
!!!!!                                                                                          !!!!!     
!!!!!                                                                                          !!!!!      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module TempDep
    use inputs, only: Grid, TempDepProp, Nz, Ny, Nx, NA, time_step, grid

    ! use setup, only: sparse_Hmatrix
    use sparse_solver, only: coo2csr
    use sptype, only : sprs2_dp
    use globe_data, only:  Temp_p, lin_rhoc, Temp_pp
    use constants, only: real12, int12, TINY
    use hmatrixmod, only: hmatrixfunc
    use constructions, only: GA_pair
    implicit none
    
    contains
    
    subroutine read_HC(Tfile, CVfile)
        implicit none
        ! Arguments
        integer :: n
        real(real12), allocatable, intent(out) :: Tfile(:), CVfile(:)

        ! Locals
        integer :: i, ios, unit
        character(len=256) :: line
        character(len=*), parameter :: filename = "HC_data.txt"

        ! Open the file
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "Error: could not open file ", filename
            stop
        end if

        ! First pass: count number of lines
        n = 0
        do
            read(unit,'(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) > 0) n = n + 1
        end do

        ! Allocate arrays
        allocate(Tfile(n), CVfile(n))

        ! Rewind and read data
        rewind(unit)
        do i = 1, n
            read(unit,*, iostat=ios) Tfile(i), CVfile(i)
            if (ios /= 0) then
                print *, "Error reading line ", i
                stop
            end if
        end do

        close(unit)

    end subroutine read_HC
    
    function G_A(T, rho) result(GA)
        implicit none
        integer(int12) :: index, i1, i2
        real(real12) :: G_val, A_val, T, CPm, CPp, TPm, TPp
        real(real12) :: rho
        type(GA_pair) :: GA
        !G = G*(2/dt)
        ! A = A/dt
        real(real12), allocatable :: Tfile(:), CVfile(:)

        call read_HC(Tfile, CVfile)
        
        ! Find the index in Tfile where Tfile(index) <= T < Tfile(index+1)
        index = 0
        do i1 = 1, size(Tfile)-1
            if (Tfile(i1) .ge. T .and. T .lt. Tfile(i1+1)) then
                index = i1
                exit
            end if
        end do
    
        if ((index .gt. 1) .and. (index .lt. size(CVfile))) then
            i1 = index - 1
            i2 = index+1
        end if
        if (index .eq. 0) then
            i1 = index
            i2 = index + 1
        end if
        if (index .eq. size(CVfile)) then
            i1 = index - 1
            i2 = index
        end if

        CPm = CVfile(i1)*rho
        CPp = CVfile(i2)*rho
        TPm = Tfile(i1)*rho
        TPp = Tfile(i2)*rho

        G_val = (CPp - CPm) / (TPp - TPm)

        A_val = CPp - (G_val*T)

        G_val = G_val * (2.0_real12 / time_step)
        A_val = A_val / time_step
        
        GA%G_val = G_val
        GA%A_val = A_val

    end function

    subroutine make_G_A(G, A)
    implicit none
    integer :: indx
    real(real12), dimension(NA) :: G, A
    type(GA_pair) :: GA
    
    G(:) = 0.0_real12
    A(:) = 0.0_real12


    do indx = 1, NA
        GA = G_A(Temp_p(indx), lin_rhoc(indx))
        G(indx) = GA%G_val
        A(indx) = GA%A_val
    end do

    end subroutine

    function Phi_func(G) result(Phi)
    implicit none
    integer(int12) :: i, j, k, indx
    real(real12), dimension(NA) :: G
    real(real12), dimension(NA) :: Phi
    real(real12) :: tau2
    
    Phi(:) = 0.0_real12
    indx = 1

    do k = 1, Nz
        do j = 1, Ny
            do i = 1, Nx
                tau2 = grid(i,j,k)%tau * time_step ! tau = tau/time_step**2
                Phi(indx) = G(indx) + 2.0_real12 * G(indx) * tau2
                indx = indx + 1
            end do
        end do
    end do


    end function

    function Gamma_func(G, A) result(Gamma)
    implicit none
    integer(int12) :: i, j, k, indx
    real(real12), dimension(NA) :: G,A
    real(real12), dimension(NA) :: Gamma
    real(real12) :: tau2
    indx = 1
    Gamma(:) = 0.0_real12
    do k = 1, Nz
        do j = 1, Ny
            do i = 1, Nx
                tau2 = grid(i,j,k)%tau * time_step ! tau = tau/time_step**2
                Gamma(indx) = A(indx) - (G(indx)*Temp_p(indx)) - ((4.0_real12*tau2*G(indx)*Temp_p(indx))) + &
                             ((G(indx)*tau2*Temp_pp(indx))) + A(indx)*(tau2)
                indx = indx + 1 
            end do
        end do
    end do
    end function

    function Omega_func(G,A) result(Omega)
    implicit none
    integer(int12) :: i, j, k, indx
    real(real12), dimension(NA) :: G,A, Omega
    real(real12) :: tau2

    indx = 1
    do k = 1, Nz
        do j = 1, Ny
            do i = 1, Nx
                tau2 = grid(i,j,k)%tau*time_step ! tau = tau/time_step**2
                Omega(indx) = (-1.0_real12*A(indx)*Temp_p(indx)) + (G(indx)*Temp_p(indx)*Temp_p(indx)*tau2) + &
                             ((tau2)*((-2.0_real12*A(indx)*Temp_p(indx)) + (A(indx)*Temp_pp(indx))))
                indx = indx + 1
            end do
        end do
    end do
    end function

    function gamma_M_H(gamma) result(gammaMH)
        implicit none
        real(real12), dimension(NA) :: gamma
        real(real12) :: H0 ! Holds the value of the H matrix
        integer(int12) :: i, j, len, count, k ! i and j are the row and column of the H matrix
        ! Holds the values to add to the row to get the column
        integer(int12), allocatable, dimension(:) :: addit 
        TYPE(sprs2_dp) :: gammaMH

        ! The number of non-zero elements in the H matrix to look for
        len = 7*nx*ny*nz - 2*(nx*ny + ny*nz + nz*nx)
        gammaMH%n = NA ! The number of rows in the H matrix
        gammaMH%len = len ! The number of non-zero elements in the H matrix
        ! Allocate the arrays to hold the H matrix in sparse row storage
        allocate(gammaMH%val(len), gammaMH%irow(len), gammaMH%jcol(len))
        gammaMH%val(:)=0
        gammaMH%irow(:)=-2
        gammaMH%jcol(:)=-1
        addit = [1] ! The values to add to the row to get the column
        if (ny .gt. 1) addit = [addit, nx] ! Add the values to add to the row to get the column
        if (nz .gt. 1) addit = [addit, nx*ny]  ! Add the values to add to the row to get the column

        count = 0 ! The number of non-zero elements in the H matrix
        parent_loop: do j = 1, NA ! Loop over the columns of the H matrix
            i=j ! The row of the H matrix
            count = count + 1 ! The number of non-zero elements in the H matrix
            H0 = hmatrixfunc(i,j) ! The value of the H matrix
            gammaMH%val(count) = (gamma(j)-H0) ! The value of the H matrix
            gammaMH%irow(count) = i ! The row of the H matrix
            gammaMH%jcol(count) = j ! The column of the H matrix
            ! Loop over the values to add to the row to get the column
            neighbour_loop: do k = 1, size(addit,1)
                i = j + addit(k) ! The row of the H matrix
                ! If the row is greater than the number of rows ...
                !...in the H matrix then go to the next column
                if ((i.gt.NA)) cycle parent_loop 
                    H0=hmatrixfunc(i,j) ! The value of the H matrix
                    ! If the value of the H matrix is less than TINY then go to the next value ...
                    !...to add to the row to get the column
                    if (abs(H0).lt.TINY) cycle neighbour_loop 
                        count = count + 1 ! The number of non-zero elements in the H matrix
                        gammaMH%val(count) = H0 ! The value of the H matrix
                        gammaMH%irow(count) = i ! The row of the H matrix
                        gammaMH%jcol(count) = j ! The column of the H matrix
                        count = count + 1 ! The number of non-zero elements in the H matrix
                        H0=hmatrixfunc(j,i) ! The value of the H matrix
                        gammaMH%val(count) = H0 ! The value of the H matrix
                        gammaMH%irow(count) = j ! The row of the H matrix
                        gammaMH%jcol(count) = i ! The column of the H matrix
                        !write(6,*) j,i, H0, count
            end do neighbour_loop
        end do parent_loop
    end function

    function nl_F_Cat(T) result(f_val)
    use mkl_spblas
    implicit none
    real(real12), intent(in)  :: T(:)
    real(real12), dimension(NA) :: TS, phi, omega, gamma, f_val, G, A, B
    integer(int12) :: i, j, k, indx, stat
    type(sprs2_dp) :: gammaMH
    real(real12), dimension(:), allocatable :: acsr
    integer, dimension(:), allocatable :: ja
    integer, dimension(:), allocatable :: ia

    ! MKL sparse objects
    type(sparse_matrix_t) :: A_handle
    type(matrix_descr)    :: descr
    real(real12), allocatable :: yvec(:)
    integer, allocatable :: row_start(:), row_end(:)
    integer :: nrows, ncols, nnz

    ! --- compute required quantities (you already did similar)
    G(:) = 0.0_real12
    A(:) = 0.0_real12
    CALL make_G_A(G,A)

    gamma = Gamma_func(G,A)
    
    omega = Omega_func(G,A)        ! fixed typo: was 'omage' in your file

    phi   = Phi_func(G)

    gammaMH = gamma_M_H(gamma)     ! returns COO in your sprs2_dp type

    ! Allocate arrays for CSR form
    nnz   = gammaMH%len
    nrows = gammaMH%n
    ncols = gammaMH%n
    allocate(acsr(nnz), ja(nnz), ia(nrows+1))

    ! Convert your COO -> CSR (you already do this)
    CALL coo2csr(gammaMH%n, gammaMH%len, gammaMH%val, gammaMH%irow, gammaMH%jcol, acsr, ja, ia)

    ! Convert ia (rowptr length n+1) to MKL-compatible row_start/row_end:
    allocate(row_start(nrows), row_end(nrows))
    do i = 1, nrows
        row_start(i) = ia(i)                 ! first index of row i (1-based)
        row_end(i)   = ia(i+1) - 1           ! last index of row i (1-based)
    end do

    ! Create MKL CSR handle (double precision)
    stat = mkl_sparse_d_create_csr(A_handle, SPARSE_INDEX_BASE_ONE, nrows, ncols, &
                                    row_start, row_end, ja, acsr)
    if (stat /= SPARSE_STATUS_SUCCESS) then
        write(*,*) 'mkl_sparse_d_create_csr failed, stat=', stat
        stop 1
    end if

    descr%type = SPARSE_MATRIX_TYPE_GENERAL

    ! Perform yvec = A * T  (yvec length = nrows)
    allocate(yvec(nrows))
    yvec = 0.0_real12

    stat = mkl_sparse_d_mv(SPARSE_OPERATION_NON_TRANSPOSE, 1.0_real12, A_handle, descr, T, 0.0_real12, yvec)
    if (stat /= SPARSE_STATUS_SUCCESS) then
        write(*,*) 'mkl_sparse_d_mv failed, stat=', stat
        stat = mkl_sparse_destroy(A_handle)
        stop 2
    end if

    ! Build f_val = phi * (T*T) + yvec + omega - B
    TS(:) = T(:) * T(:)
    f_val(:) = phi(:) * TS(:) + yvec(:) + omega(:)

    ! cleanup
    stat = mkl_sparse_destroy(A_handle)
    deallocate(acsr, ja, ia, row_start, row_end, yvec)

    end function nl_F_Cat


    function Jac_nl_F_Cat(T) result(jac)
    implicit none
    type(sprs2_dp) :: jac
    real(real12), dimension(NA) :: T, phi, gamma
    real(real12) :: H0 ! Holds the value of the H matrix
    integer(int12) :: i, j, len, count, k ! i and j are the row and column of the H matrix
    ! Holds the values to add to the row to get the column
    integer(int12), allocatable, dimension(:) :: addit 
    real(real12), dimension(NA) :: G, A
    ! --- compute required quantities (you already did similar)
    G(:) = 0.0_real12
    A(:) = 0.0_real12
    CALL make_G_A(G,A)              ! ensure this actually sets G and A

    phi = Phi_func(G)               ! ensure this actually sets phi
    gamma = Gamma_func(G,A)             ! ensure this actually sets gamma

    ! The number of non-zero elements in the H matrix to look for
    len = 7*nx*ny*nz - 2*(nx*ny + ny*nz + nz*nx)

    jac%n = NA ! The number of rows in the H matrix
    jac%len = len ! The number of non-zero elements in the H matrix
    ! Allocate the arjacys to hold the H matrix in sparse row stojacge
    allocate(jac%val(len), jac%irow(len), jac%jcol(len))
    jac%val(:)=0
    jac%irow(:)=-2
    jac%jcol(:)=-1
    addit = [1] ! The values to add to the row to get the column

    if (ny .gt. 1) addit = [addit, nx] ! Add the values to add to the row to get the column
    if (nz .gt. 1) addit = [addit, nx*ny]  ! Add the values to add to the row to get the column

    count = 0 ! The number of non-zero elements in the H matrix
    parent_loop: do j = 1, NA ! Loop over the columns of the H matrix
        i=j ! The row of the H matrix
        count = count + 1 ! The number of non-zero elements in the H matrix
        H0 = hmatrixfunc(i,j) ! The value of the H matrix
        jac%val(count) = 2.0_real12*phi(j)*T(j) + (gamma(j)-H0) ! The value of the H matrix
        jac%irow(count) = i ! The row of the H matrix
        jac%jcol(count) = j ! The column of the H matrix
        ! Loop over the values to add to the row to get the column
        neighbour_loop: do k = 1, size(addit,1)
            i = j + addit(k) ! The row of the H matrix
            ! If the row is greater than the number of rows ...
            !...in the H matrix then go to the next column
            if ((i.gt.NA)) cycle parent_loop 
                H0=hmatrixfunc(i,j) ! The value of the H matrix
                ! If the value of the H matrix is less than TINY then go to the next value ...
                !...to add to the row to get the column
                if (abs(H0).lt.TINY) cycle neighbour_loop 
                    count = count + 1 ! The number of non-zero elements in the H matrix
                    jac%val(count) = H0 ! The value of the H matrix
                    jac%irow(count) = i ! The row of the H matrix
                    jac%jcol(count) = j ! The column of the H matrix
                    count = count + 1 ! The number of non-zero elements in the H matrix
                    H0=hmatrixfunc(j,i) ! The value of the H matrix
                    jac%val(count) = H0 ! The value of the H matrix
                    jac%irow(count) = j ! The row of the H matrix
                    jac%jcol(count) = i ! The column of the H matrix
                    !write(6,*) j,i, H0, count
        end do neighbour_loop
     end do parent_loop
    end function
end module TempDep
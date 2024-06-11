module TempDep
    use inputs, only: Grid, TempDepProp, Nz, Ny, Nx
    use setup, only: sparse_Hmatrix
    use globe_data, only:  Temp_p, lin_rhoc
    use constants, only: real12, int12
    
    implicit none
    
    contains

    subroutine ChangeProp()
        character(len=100) :: filename
        integer(int12) :: ix,iy,iz, index
        index = 1
        !Loop over all the grid points
        do iz = 1, Nz
            do iy = 1, Ny
                do ix = 1, Nx
                    ! Construct the filename for the Material table asscoiated with the grid point
                    filename = trim('MatTable' // & 
                         trim(adjustl(char(Grid(ix, iy, iz)%imaterial_type))))
                    ! Read the temperature dependent properties from the file
                    CALL ReadTempDepTable(filename, ix, iy, iz, index)
                    index = index + 1
                end do
            end do
        end do
        ! Construct the sparse matrix
        CALL sparse_Hmatrix()
    end subroutine ChangeProp

    subroutine ReadTempDepTable(filename, ix, iy, iz, index)
        character(len=*), intent(in) :: filename
        integer(int12), intent(in) :: iz, iy, ix, index
        integer(int12) :: i, j, k, num_rows, num_cols, iostat
        real, allocatable :: temp_table(:,:)
        
        ! Open the file
        open(unit=10, file=filename, status='old', action='read', iostat=iostat)
        if (iostat .ne. 0) then
            write(*,*) 'Error opening file: ', trim(filename)
            return
        end if
        
        ! Read the number of rows and columns
        read(10, *) num_rows, num_cols
        
        ! Allocate memory for the temperature table
        allocate(temp_table(num_rows, num_cols))
        
        ! Read the temperature table
        do i = 1, num_rows
            read(10, *) (temp_table(i, j), j = 1, num_cols)
        end do
        
        ! Close the file
        close(10)
        
        ! Update the properties based on the temperature table
        ! Find the corresponding kappa value in the temperature table
        do k = 1, num_rows
            if (Temp_p(index) .le. temp_table(k, 1)) then
                exit
            end if
        end do
        if (Temp_p(index) .gt. temp_table(num_rows, 1)) then
            write(*,*) 'Temperature is out of range'
        end if
        !Apply Forward difference linear extrapolation and assing to global variables
        Grid(ix, iy, iz)%kappa =  ((temp_table(k+1,2) - temp_table(k, 2))/&
             (temp_table(k+1,1)-temp_table(k,1)))*Temp_p(index) + temp_table(k,2) 

        Grid(ix, iy, iz)%rho = ((temp_table(k+1,3) - temp_table(k, 3))/&
             (temp_table(k+1,1)-temp_table(k,1)))*Temp_p(index) + temp_table(k,3)

        Grid(ix, iy, iz)%heat_capacity = ((temp_table(k+1,4) - temp_table(k, 4))/&
             (temp_table(k+1,1)-temp_table(k,1)))*Temp_p(index) + temp_table(k,4)

        lin_rhoc(index) = Grid(ix, iy, iz)%rho * Grid(ix, iy, iz)%heat_capacity
        
        ! Deallocate the temperature table
        deallocate(temp_table)
            
    end subroutine ReadTempDepTable
end module TempDep
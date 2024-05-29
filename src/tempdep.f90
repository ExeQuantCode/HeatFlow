module TempDep
    use inputs, only: Grid, TempDepProp, Nz, Ny, Nx
    use setup, only: sparse_Hmatrix
    use globe_data, only:  Temp_p
    use constants, only: real12, int12
    
    implicit none
    
    contains

    subroutine ChangeProp()
        integer(int12) :: iz, iy, ix, index
        real(real12) :: Temp
        index = 1
        select case(TempDepProp)
        case(0)
            ! Do nothing
            continue
        case(1)
            ! Linear
            do iz = 1, Nz
                do iy = 1, Ny
                    do ix = 1, Nx
                        Grid(ix,iy,iz)%kappa = Grid(ix,iy,iz)%kappa * (1.0 + Temp_p(index))
                        index = index + 1
                    end do
                end do
            end do
            CALL sparse_Hmatrix()
        case(2)
            !Quadratic
            do iz = 1, Nz
                do iy = 1, Ny
                    do ix = 1, Nx
                        Grid(ix,iy,iz)%kappa = Grid(ix,iy,iz)%kappa * (1.0 + Temp_p(index) + Temp_p(index)**2)
                        index = index + 1
                    end do
                end do
            end do
            CALL sparse_Hmatrix()

        case(3)
            !cubic
            do iz = 1, Nz
                do iy = 1, Ny
                    do ix = 1, Nx
                        Grid(ix,iy,iz)%kappa = Grid(ix,iy,iz)%kappa * &
                              (1.0 + Temp_p(index) + Temp_p(index)**2 + Temp_p(index)**3)
                        index = index + 1
                    end do
                end do
            end do
            CALL sparse_Hmatrix()

        case(4)
            !Exponential
            do iz = 1, Nz
                do iy = 1, Ny
                    do ix = 1, Nx
                        Temp_p(index) = Temp_p(index)
                        Grid(ix,iy,iz)%kappa = Grid(ix,iy,iz)%kappa * exp(Temp_p(index))
                        index = index + 1
                    end do
                end do
            end do
            CALL sparse_Hmatrix()
        end select 
    
    end subroutine ChangeProp

    subroutine ReadTempDepTable(filename)
        character(len=*), intent(in) :: filename
        integer(int12) :: iz, iy, ix, index
        real(real12) :: Temp, kappa
        integer :: i, j, k, num_rows, num_cols
        real, allocatable :: temp_table(:,:)
        
        ! Open the file
        open(unit=10, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
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
        index = 1
        do iz = 1, Nz
            do iy = 1, Ny
                do ix = 1, Nx
                    Temp = Grid(ix, iy, iz)%Temp
                    ! Find the corresponding kappa value in the temperature table
                    do k = 1, num_rows
                        if (Temp <= temp_table(k, 1)) then
                            kappa = temp_table(k, 2)
                            exit
                        end if
                    end do
                    Grid(ix, iy, iz)%kappa = Grid(ix, iy, iz)%kappa * kappa
                    index = index + 1
                end do
            end do
        end do
        
        ! Deallocate the temperature table
        deallocate(temp_table)
        
        CALL sparse_Hmatrix()
        
    end subroutine ReadTempDepTable
end module TempDep
module TempDep
    use inputs, only: Grid, TempDepProp, Nz, Ny, Nx
    use setup, only: sparse_Hmatrix
    use globe_data, only:  Temp_p, lin_rhoc
    use constants, only: real12, int12
    
    implicit none
    
    contains

    subroutine ChangeProp()
        integer(int12) :: iz, iy, ix, index
        real(real12) :: Temp, kappa, rhoC, rho, heat_capacity
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
                        Grid(ix,iy,iz)%rho = Grid(ix,iy,iz)%rho * (1.0 + Temp_p(index))
                        Grid(ix,iy,iz)%heat_capacity = Grid(ix,iy,iz)%heat_capacity * &
                             (1.0 + Temp_p(index))
                        lin_rhoc(index) = Grid(ix,iy,iz)%rho * Grid(ix,iy,iz)%heat_capacity
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
                        Grid(ix,iy,iz)%kappa = Grid(ix,iy,iz)%kappa * &
                             (1.0 + Temp_p(index) + Temp_p(index)**2)
                        Grid(ix,iy,iz)%rho = Grid(ix,iy,iz)%rho * &
                             (1.0 + Temp_p(index) + Temp_p(index)**2)
                        Grid(ix,iy,iz)%heat_capacity = Grid(ix,iy,iz)%heat_capacity * &
                             (1.0 + Temp_p(index) + Temp_p(index)**2)
                        lin_rhoc(index) = Grid(ix,iy,iz)%rho * Grid(ix,iy,iz)%heat_capacity
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
                        Grid(ix,iy,iz)%rho = Grid(ix,iy,iz)%rho * &
                              (1.0 + Temp_p(index) + Temp_p(index)**2 + Temp_p(index)**3)
                        Grid(ix,iy,iz)%heat_capacity = Grid(ix,iy,iz)%heat_capacity * &
                              (1.0 + Temp_p(index) + Temp_p(index)**2 + Temp_p(index)**3)
                        lin_rhoc(index) = Grid(ix,iy,iz)%rho * Grid(ix,iy,iz)%heat_capacity
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
        real(real12) :: Temp, kappa, rhoC, rho, heat_capacity
        integer :: i, j, k, num_rows, num_cols, isotat
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
                    Temp = Temp_p(index)
                    ! Find the corresponding kappa value in the temperature table
                    do k = 1, num_rows
                        if (Temp <= temp_table(k, 1)) then
                            kappa = temp_table(k, 2)
                            rhoC = temp_table(k, 3)*temp_table(k, 4)
                            rho = temp_table(k, 3)
                            heat_capacity = temp_table(k, 4)
                            exit
                        end if
                    end do
                    Grid(ix, iy, iz)%kappa =  kappa
                    Grid(ix, iy, iz)%rho = rho
                    Grid(ix, iy, iz)%heat_capacity = heat_capacity
                    lin_rhoc(index) = rhoC
                    index = index + 1
                end do
            end do
        end do
        
        ! Deallocate the temperature table
        deallocate(temp_table)
        
        CALL sparse_Hmatrix()
        
    end subroutine ReadTempDepTable
end module TempDep
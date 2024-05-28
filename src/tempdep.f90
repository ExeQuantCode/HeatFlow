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

end module TempDep
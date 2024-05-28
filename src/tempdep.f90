module TempDep
    use inputs, only: Grid, TempDepProp, Nz, Ny, Nx
    use setup, only: sparse_Hmatrix
    use globe_data, only:  Temp_p
    use constants, only: real12, int12
    
    implicit none
    
    contains

    subroutine ChangeProp()
        int(int12) :: iz, iy, ix, index
        real(real12) :: Temp
        index = 1
        select(TempDepProp)
        case(0)
            ! Do nothing
            continue
        case(1)
            ! Linear
            for iz in 1: Nz
                for iy in 1: Ny
                    for ix in 1: Nx
                        Temp = Temp_p(index)
                        Grid(ix,iy,iz)%kappa = Grid(ix,iy,iz)%kappa * (1.0 + Temp)
                        index = index + 1
                    end do
                end do
            end do
            CALL sparse_Hmatrix()
        case(2)
            !Quadratic
            for iz in 1: Nz
                for iy in 1: Ny
                    for ix in 1: Nx
                        Temp = Temp_p(index)
                        Grid(ix,iy,iz)%kappa = Grid(ix,iy,iz)%kappa * (1.0 + Temp + Temp**2)
                        index = index + 1
                    end do
                end do
            end do
            CALL sparse_Hmatrix()

        case(3)
            !cubic
            for iz in 1: Nz
                for iy in 1: Ny
                    for ix in 1: Nx
                        Temp = Temp_p(index)
                        Grid(ix,iy,iz)%kappa = Grid(ix,iy,iz)%kappa * &
                              (1.0 + Temp + Temp**2 + Temp**3)
                        index = index + 1
                    end do
                end do
            end do
            CALL sparse_Hmatrix()

        case(4)
            !Exponential
            for iz in 1: Nz
                for iy in 1: Ny
                    for ix in 1: Nx
                        Temp = Temp_p(index)
                        Grid(ix,iy,iz)%kappa = Grid(ix,iy,iz)%kappa * exp(Temp)
                        index = index + 1
                    end do
                end do
            end do
            CALL sparse_Hmatrix()
        end select 
    
    end subroutine ChangeProp

end module TempDep
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
    use inputs, only: Grid, TempDepProp, Nz, Ny, Nx
    ! use setup, only: sparse_Hmatrix
    use globe_data, only:  Temp_p, lin_rhoc
    use constants, only: real12, int12
    
    implicit none
    
    contains

end module TempDep

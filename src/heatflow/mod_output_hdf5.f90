!!!#################################################################################################
!!! Module for HDF5 output with GZIP compression.
!!! Produces Python-friendly output with simple structure:
!!!   f['times']        -> (nt,) array in seconds
!!!   f['temperatures'] -> (nt, nx, ny, nz) array in Kelvin
!!! Author: HeatFlow Team
!!!#################################################################################################
module output_hdf5
  use constants, only: real12, int12
  use inputs, only: nx, ny, nz, time_step, RunName
#ifdef USE_HDF5
  use hdf5
#endif
  implicit none
  private
  public :: write_hdf5_data, init_hdf5, finalize_hdf5

#ifdef USE_HDF5
  integer(hid_t), save :: file_id
  integer(hid_t), save :: times_dset_id, temps_dset_id
  integer(hid_t), save :: times_space_id, temps_space_id
  integer(hsize_t), save :: current_step = 0
  integer(hsize_t), save :: out_nx, out_ny, out_nz
  logical, save :: is_initialized = .false.
  logical, save :: datasets_created = .false.
#endif

contains

  subroutine init_hdf5()
#ifdef USE_HDF5
    integer :: error
    character(len=1024) :: filename

    if (is_initialized) return

    ! Initialize HDF5 library
    call h5open_f(error)
    
    ! Construct filename
    write(filename, '(A,A)') "outputs/output_", trim(adjustl(RunName)) // ".h5"
    
    ! Create new file (truncating existing)
    call h5fcreate_f(trim(filename), H5F_ACC_TRUNC_F, file_id, error)
    
    current_step = 0
    datasets_created = .false.
    is_initialized = .true.
    write(*,*) " [HDF5] Initialized output file: ", trim(filename)
#else
    write(*,*) " [Warning] HDF5 output requested but not compiled with USE_HDF5=1"
#endif
  end subroutine init_hdf5

  subroutine finalize_hdf5()
#ifdef USE_HDF5
    integer :: error
    if (is_initialized) then
       if (datasets_created) then
          call h5dclose_f(times_dset_id, error)
          call h5dclose_f(temps_dset_id, error)
          call h5sclose_f(times_space_id, error)
          call h5sclose_f(temps_space_id, error)
       end if
       call h5fclose_f(file_id, error)
       call h5close_f(error)
       is_initialized = .false.
       datasets_created = .false.
       write(*,*) " [HDF5] Closed output file."
    end if
#endif
  end subroutine finalize_hdf5

#ifdef USE_HDF5
  subroutine add_string_attribute(loc_id, attr_name, attr_value)
    integer(hid_t), intent(in) :: loc_id
    character(len=*), intent(in) :: attr_name, attr_value
    integer(hid_t) :: attr_id, aspace_id, atype_id
    integer(hsize_t), dimension(1) :: adims = (/1/)
    integer :: error
    
    call h5screate_simple_f(1, adims, aspace_id, error)
    call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
    call h5tset_size_f(atype_id, int(len(attr_value), kind=8), error)
    call h5acreate_f(loc_id, attr_name, atype_id, aspace_id, attr_id, error)
    call h5awrite_f(attr_id, atype_id, attr_value, adims, error)
    call h5aclose_f(attr_id, error)
    call h5tclose_f(atype_id, error)
    call h5sclose_f(aspace_id, error)
  end subroutine add_string_attribute

  subroutine create_datasets(dnx, dny, dnz)
    integer(hsize_t), intent(in) :: dnx, dny, dnz
    integer(hid_t) :: plist_id
    integer(hsize_t), dimension(1) :: times_dims, times_maxdims, times_chunk
    integer(hsize_t), dimension(4) :: temps_dims, temps_maxdims, temps_chunk
    integer :: error
    
    out_nx = dnx
    out_ny = dny
    out_nz = dnz
    
    ! Create extendible 'times' dataset: shape (nt,)
    times_dims = (/0_8/)
    times_maxdims = (/H5S_UNLIMITED_F/)
    times_chunk = (/100_8/)  ! Chunk size for times
    
    call h5screate_simple_f(1, times_dims, times_space_id, error, times_maxdims)
    call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
    call h5pset_chunk_f(plist_id, 1, times_chunk, error)
    call h5dcreate_f(file_id, "times", H5T_NATIVE_DOUBLE, times_space_id, &
                     times_dset_id, error, dcpl_id=plist_id)
    call add_string_attribute(times_dset_id, "units", "seconds")
    call h5pclose_f(plist_id, error)
    
    ! Create extendible 'temperatures' dataset: shape (nt, nx, ny, nz) in Python
    ! Note: Fortran is column-major, so we reverse dimensions for HDF5
    ! Fortran specifies (nz, ny, nx, nt) so Python sees (nt, nx, ny, nz)
    temps_dims = (/dnz, dny, dnx, 0_8/)
    temps_maxdims = (/dnz, dny, dnx, H5S_UNLIMITED_F/)
    temps_chunk = (/min(dnz, 10_8), dny, dnx, 1_8/)  ! Chunk: 1 timestep, full XY, partial Z
    
    call h5screate_simple_f(4, temps_dims, temps_space_id, error, temps_maxdims)
    call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
    call h5pset_chunk_f(plist_id, 4, temps_chunk, error)
    call h5pset_deflate_f(plist_id, 6, error)  ! GZIP compression level 6
    call h5dcreate_f(file_id, "temperatures", H5T_NATIVE_DOUBLE, temps_space_id, &
                     temps_dset_id, error, dcpl_id=plist_id)
    call add_string_attribute(temps_dset_id, "units", "Kelvin")
    call h5pclose_f(plist_id, error)
    
    datasets_created = .true.
  end subroutine create_datasets
#endif

  subroutine write_hdf5_data(itime, Temp_cur)
    integer(int12), intent(in) :: itime
    real(real12), dimension(nx,ny,nz), intent(in) :: Temp_cur
#ifdef USE_HDF5
    integer(hid_t) :: memspace_id, filespace_id
    integer(hsize_t), dimension(1) :: times_newsize, times_offset, times_count
    integer(hsize_t), dimension(4) :: temps_newsize, temps_offset, temps_count
    integer :: error
    real(real12) :: current_time
    ! Local copies of indices with defaults if not set
    integer(int12) :: six, eix, siy, eiy, siz, eiz
    
    if (.not. is_initialized) call init_hdf5()

    six = 1;    eix = nx
    siy = 1;    eiy = ny
    siz = 1;    eiz = nz

    ! Create datasets on first write
    if (.not. datasets_created) then
       call create_datasets(eix, eiy, eiz)
    end if

    current_time = real((itime-1), real12) * time_step
    current_step = current_step + 1

    ! === Extend and write 'times' dataset ===
    times_newsize = (/current_step/)
    call h5dset_extent_f(times_dset_id, times_newsize, error)
    
    ! Get updated filespace
    call h5dget_space_f(times_dset_id, filespace_id, error)
    
    ! Select hyperslab for new data
    times_offset = (/current_step - 1/)
    times_count = (/1_8/)
    call h5sselect_hyperslab_f(filespace_id, H5S_SELECT_SET_F, times_offset, times_count, error)
    
    ! Create memory space
    call h5screate_simple_f(1, times_count, memspace_id, error)
    
    ! Write time value
    call h5dwrite_f(times_dset_id, H5T_NATIVE_DOUBLE, current_time, times_count, error, &
                    mem_space_id=memspace_id, file_space_id=filespace_id)
    
    call h5sclose_f(memspace_id, error)
    call h5sclose_f(filespace_id, error)

    ! === Extend and write 'temperatures' dataset ===
    ! Dimensions reversed: (nz, ny, nx, nt) so Python sees (nt, nx, ny, nz)
    temps_newsize = (/int(eiz, 8), int(eiy, 8), int(eix, 8), current_step/)
    call h5dset_extent_f(temps_dset_id, temps_newsize, error)
    
    ! Get updated filespace
    call h5dget_space_f(temps_dset_id, filespace_id, error)
    
    ! Select hyperslab for new data (append at end of time dimension)
    ! Dimensions reversed: (nz, ny, nx, nt) so Python sees (nt, nx, ny, nz)
    temps_offset = (/0_8, 0_8, 0_8, current_step - 1/)
    temps_count = (/int(eiz, 8), int(eiy, 8), int(eix, 8), 1_8/)
    call h5sselect_hyperslab_f(filespace_id, H5S_SELECT_SET_F, temps_offset, temps_count, error)
    
    ! Create memory space
    call h5screate_simple_f(4, temps_count, memspace_id, error)
    
    ! Write temperature data
    call h5dwrite_f(temps_dset_id, H5T_NATIVE_DOUBLE, &
                    Temp_cur(six:eix, siy:eiy, siz:eiz), temps_count, error, &
                    mem_space_id=memspace_id, file_space_id=filespace_id)
    
    call h5sclose_f(memspace_id, error)
    call h5sclose_f(filespace_id, error)

#else
    ! Dummy to avoid unused argument warnings when HDF5 is not enabled
    if (.false.) print *, itime, shape(Temp_cur)
#endif
  end subroutine write_hdf5_data

end module output_hdf5


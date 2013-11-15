module nc_read_write_interface
! Purpose:
! This module is devised for ...
!
!
! Record of revisions:
!    Date             Programmer                Description of change  
!==============    ===================   =============================
! 2011/12/22         WenYu Huang                Original Code
! ...
!
!Any comments please send to huangwenyu@mail.tsinghua.edu.cn                      
    use netcdf

    implicit none
    private
    public nc_read_write_interface_create_file
    public nc_read_write_interface_delete_file
    public nc_read_write_interface_read_var
    public nc_read_write_interface_write_var
    public nc_read_write_interface_write_dim

    !--------------------------------------------------------------------------------------------
    integer, parameter :: r8 = 8 
                                    ! the kind of the real number read/write 
                                    ! r8 = 4 for float
                                    ! r8 = 8 for double
    logical, parameter :: large_file_support = .true. 
                                    ! option for writing large file 
                                    ! large_file_support = .true. for support large netcdf file
                                    ! large_file_support = .false. for don't support large netcdf file
    logical, parameter :: debug_output = .true. 
                                    ! option for write debugs
                                    ! debug_output = .true. for write debug_output
                                    ! debug_output = .true. for don't write debug_output
    !--------------------------------------------------------------------------------------------
    interface nc_read_write_interface_create_file
        module procedure create_nc
    end interface nc_read_write_interface_create_file  

    interface nc_read_write_interface_delete_file
        module procedure file_delete 
    end interface nc_read_write_interface_delete_file  

    interface nc_read_write_interface_read_var
        module procedure read_1dvar
        module procedure read_2dvar
        module procedure read_3dvar
        module procedure read_4dvar
        module procedure read_5dvar
        module procedure read_1dvar_int
        module procedure read_2dvar_int
        module procedure read_3dvar_int
        module procedure read_4dvar_int
        module procedure read_5dvar_int
    end interface nc_read_write_interface_read_var

    interface nc_read_write_interface_write_var
        module procedure write_1dvar
        module procedure write_2dvar
        module procedure write_3dvar
        module procedure write_4dvar
        module procedure write_5dvar
        module procedure write_1dvar_int
        module procedure write_2dvar_int
        module procedure write_3dvar_int
        module procedure write_4dvar_int
        module procedure write_5dvar_int
    end interface nc_read_write_interface_write_var

    interface nc_read_write_interface_write_dim
        module procedure write_1ddim
        module procedure write_1ddim_int
    end interface nc_read_write_interface_write_dim

    contains
    !---------------------------------------------------------------------
    !                          Subroutine create_nc
    !---------------------------------------------------------------------
    subroutine create_nc (file_name)
        implicit none
        character (len = *), intent (in) :: file_name

        logical :: alive
        integer :: ncid, ierr, i, j, k

        call check_file_exist (file_name, .false.)

        if (.not. large_file_support) then
            ierr = nf90_create (file_name, nf90_noclobber, ncid)
            call handle_err (ierr)
        else if ( large_file_support) then
            ierr = nf90_create (file_name, nf90_noclobber + nf90_64bit_offset, ncid)
            call handle_err (ierr)
        end if 

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine create_nc
    !---------------------------------------------------------------------
    !                      End of Subroutine create_nc
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                       Subroutine file_delete
    !---------------------------------------------------------------------
    subroutine file_delete (file_name)
        implicit none
        character (len = *), intent (in) :: file_name

        logical alive

        inquire (file = file_name, exist = alive)
        if (alive) then
            open (11, file = file_name)
            close (11, status = 'delete')
        end if

        call check_file_exist (file_name, .false.)
    end subroutine file_delete
    !---------------------------------------------------------------------
    !                   End of Subroutine file_delete
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_1dvar
    !---------------------------------------------------------------------
    subroutine read_1dvar (var, file_name, var_name, start_count, volume_count, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: volume_count (n_d)
        integer,                intent (in)         :: start_count  (n_d)
        character (len = *),    intent (in)         :: var_name
        character (len = *),    intent (in)         :: file_name
        real (r8),              intent (out)        :: var (dim_count (1))

        integer :: ierr, ncid, var_id, i, j, k
        
        call check_file_exist (file_name, .true.)
        
        ierr = nf90_open (file_name, nf90_nowrite, ncid)
        call handle_err (ierr)

        ierr = nf90_inq_varid (ncid, var_name, var_id) 
        call handle_err (ierr)

        ierr = nf90_get_var (ncid, var_id, var, start_count, volume_count)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine read_1dvar
    !---------------------------------------------------------------------
    !                      End of Subroutine read_1dvar
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_1dvar_int
    !---------------------------------------------------------------------
    subroutine read_1dvar_int (var, file_name, var_name, start_count, volume_count, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: volume_count (n_d)
        integer,                intent (in)         :: start_count  (n_d)
        character (len = *),    intent (in)         :: var_name
        character (len = *),    intent (in)         :: file_name
        integer,                intent (out)        :: var (dim_count (1))

        integer :: ierr, ncid, var_id, i, j, k
        
        call check_file_exist (file_name, .true.)
        
        ierr = nf90_open (file_name, nf90_nowrite, ncid)
        call handle_err (ierr)

        ierr = nf90_inq_varid (ncid, var_name, var_id) 
        call handle_err (ierr)

        ierr = nf90_get_var (ncid, var_id, var, start_count, volume_count)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine read_1dvar_int
    !---------------------------------------------------------------------
    !                      End of Subroutine read_1dvar_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_1dvar 
    !---------------------------------------------------------------------
    subroutine write_1dvar (dvar, file_name, dim_name, dvar_name, long_name, units, Fill_Value, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        real (r8),              intent (in)         :: Fill_Value
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dvar_name 
        character (len = *),    intent (in)         :: dim_name     (n_d) 
        character (len = *),    intent (in)         :: file_name 
        real (r8),              intent (in)         :: dvar (dim_count (1))

        integer :: ierr, ncid, dim_dimid (n_d), dvar_id
        integer :: i, j, k

        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        do k = 1, n_d
            ierr = nf90_inq_dimid (ncid, dim_name (k), dim_dimid (k))
            call handle_err (ierr)
        end do
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_float, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_double, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        end if

        ierr = nf90_put_att (ncid, dvar_id, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "units", units)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "_FillValue", Fill_Value)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dvar_id, dvar)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_1dvar 
    !---------------------------------------------------------------------
    !                      End of Subroutine write_1dvar 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_1dvar_int 
    !---------------------------------------------------------------------
    subroutine write_1dvar_int (dvar, file_name, dim_name, dvar_name, long_name, units, Fill_Value,  dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: Fill_Value
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dvar_name 
        character (len = *),    intent (in)         :: dim_name     (n_d) 
        character (len = *),    intent (in)         :: file_name 
        integer,                intent (in)         :: dvar (dim_count (1))

        integer :: ierr, ncid, dim_dimid (n_d), dvar_id
        integer :: i, j, k

        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        do k = 1, n_d
            ierr = nf90_inq_dimid (ncid, dim_name (k), dim_dimid (k))
            call handle_err (ierr)
        end do
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_float, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_double, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        end if

        ierr = nf90_put_att (ncid, dvar_id, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "units", units)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "_FillValue", Fill_Value)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dvar_id, dvar)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_1dvar_int 
    !---------------------------------------------------------------------
    !                    End of Subroutine write_1dvar_int 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_1ddim 
    !---------------------------------------------------------------------
    subroutine write_1ddim (dim_var, file_name, dim_name, long_name, units, num_dim)
        implicit none
        integer,                intent (in)         :: num_dim
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dim_name 
        character (len = *),    intent (in)         :: file_name 
        real (r8),              intent(in)          :: dim_var (num_dim)

        integer :: ierr, ncid, dim_dimid, dim_varid
        
        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        ierr = nf90_def_dim (ncid, dim_name, num_dim, dim_dimid)
        call handle_err (ierr)
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dim_name, nf90_float, dim_dimid, dim_varid)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dim_name, nf90_double, dim_dimid, dim_varid)
            call handle_err (ierr)
        end if
        
        ierr = nf90_put_att (ncid, dim_varid, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dim_varid, "units", units)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dim_varid, dim_var)
        call handle_err (ierr)
        
        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_1ddim 
    !---------------------------------------------------------------------
    !                      End of Subroutine write_1ddim 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_1ddim_int 
    !---------------------------------------------------------------------
    subroutine write_1ddim_int (dim_var, file_name, dim_name, long_name, units, num_dim)
        implicit none
        integer,                intent (in)         :: num_dim
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dim_name 
        character (len = *),    intent (in)         :: file_name 
        integer,                intent (in)         :: dim_var (num_dim)

        integer :: ierr, ncid, dim_dimid, dim_varid
        
        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        ierr = nf90_def_dim (ncid, dim_name, num_dim, dim_dimid)
        call handle_err (ierr)
        
        ierr = nf90_def_var (ncid, dim_name, nf90_int, dim_dimid, dim_varid)
        call handle_err (ierr)
        
        ierr = nf90_put_att (ncid, dim_varid, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dim_varid, "units", units)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dim_varid, dim_var)
        call handle_err (ierr)
        
        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_1ddim_int 
    !---------------------------------------------------------------------
    !                      End of Subroutine write_1ddim_int 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_2dvar
    !---------------------------------------------------------------------
    subroutine read_2dvar (var, file_name, var_name, start_count, volume_count, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: volume_count (n_d)
        integer,                intent (in)         :: start_count  (n_d)
        character (len = *),    intent (in)         :: var_name
        character (len = *),    intent (in)         :: file_name
        real (r8),              intent (out)        :: var (dim_count (1), dim_count (2))

        integer :: ierr, ncid, var_id, i, j, k
        
        call check_file_exist (file_name, .true.)
        
        ierr = nf90_open (file_name, nf90_nowrite, ncid)
        call handle_err (ierr)

        ierr = nf90_inq_varid (ncid, var_name, var_id) 
        call handle_err (ierr)

        ierr = nf90_get_var (ncid, var_id, var, start_count, volume_count)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine read_2dvar
    !---------------------------------------------------------------------
    !                      End of Subroutine read_2dvar
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_2dvar_int
    !---------------------------------------------------------------------
    subroutine read_2dvar_int (var, file_name, var_name, start_count, volume_count, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: volume_count (n_d)
        integer,                intent (in)         :: start_count  (n_d)
        character (len = *),    intent (in)         :: var_name
        character (len = *),    intent (in)         :: file_name
        integer,                intent (out)        :: var (dim_count (1), dim_count (2))

        integer :: ierr, ncid, var_id, i, j, k
        
        call check_file_exist (file_name, .true.)
        
        ierr = nf90_open (file_name, nf90_nowrite, ncid)
        call handle_err (ierr)

        ierr = nf90_inq_varid (ncid, var_name, var_id) 
        call handle_err (ierr)

        ierr = nf90_get_var (ncid, var_id, var, start_count, volume_count)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine read_2dvar_int
    !---------------------------------------------------------------------
    !                      End of Subroutine read_2dvar_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_2dvar 
    !---------------------------------------------------------------------
    subroutine write_2dvar (dvar, file_name, dim_name, dvar_name, long_name, units, Fill_Value, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        real (r8),              intent (in)         :: Fill_Value
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dvar_name 
        character (len = *),    intent (in)         :: dim_name     (n_d) 
        character (len = *),    intent (in)         :: file_name 
        real (r8),              intent (in)         :: dvar (dim_count (1), dim_count (2))

        integer :: ierr, ncid, dim_dimid (n_d), dvar_id
        integer :: i, j, k

        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        do k = 1, n_d
            ierr = nf90_inq_dimid (ncid, dim_name (k), dim_dimid (k))
            call handle_err (ierr)
        end do
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_float, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_double, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        end if

        ierr = nf90_put_att (ncid, dvar_id, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "units", units)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "_FillValue", Fill_Value)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dvar_id, dvar)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_2dvar 
    !---------------------------------------------------------------------
    !                      End of Subroutine write_2dvar 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_2dvar_int 
    !---------------------------------------------------------------------
    subroutine write_2dvar_int (dvar, file_name, dim_name, dvar_name, long_name, units, Fill_Value, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: Fill_Value
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dvar_name 
        character (len = *),    intent (in)         :: dim_name     (n_d) 
        character (len = *),    intent (in)         :: file_name 
        integer,                intent (in)         :: dvar (dim_count (1), dim_count (2))

        integer :: ierr, ncid, dim_dimid (n_d), dvar_id
        integer :: i, j, k

        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        do k = 1, n_d
            ierr = nf90_inq_dimid (ncid, dim_name (k), dim_dimid (k))
            call handle_err (ierr)
        end do
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_float, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_double, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        end if

        ierr = nf90_put_att (ncid, dvar_id, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "units", units)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "_FillValue", Fill_Value)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dvar_id, dvar)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_2dvar_int 
    !---------------------------------------------------------------------
    !                    End of Subroutine write_2dvar_int 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_3dvar
    !---------------------------------------------------------------------
    subroutine read_3dvar (var, file_name, var_name, start_count, volume_count, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: volume_count (n_d)
        integer,                intent (in)         :: start_count  (n_d)
        character (len = *),    intent (in)         :: var_name
        character (len = *),    intent (in)         :: file_name
        real (r8),              intent (out)        :: var (dim_count (1), dim_count (2), dim_count (3))

        integer :: ierr, ncid, var_id, i, j, k
        
        call check_file_exist (file_name, .true.)
        
        ierr = nf90_open (file_name, nf90_nowrite, ncid)
        call handle_err (ierr)

        ierr = nf90_inq_varid (ncid, var_name, var_id) 
        call handle_err (ierr)

        ierr = nf90_get_var (ncid, var_id, var, start_count, volume_count)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine read_3dvar
    !---------------------------------------------------------------------
    !                      End of Subroutine read_3dvar
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_3dvar_int
    !---------------------------------------------------------------------
    subroutine read_3dvar_int (var, file_name, var_name, start_count, volume_count, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: volume_count (n_d)
        integer,                intent (in)         :: start_count  (n_d)
        character (len = *),    intent (in)         :: var_name
        character (len = *),    intent (in)         :: file_name
        integer,                intent (out)        :: var (dim_count (1), dim_count (2), dim_count (3))

        integer :: ierr, ncid, var_id, i, j, k
        
        call check_file_exist (file_name, .true.)
        
        ierr = nf90_open (file_name, nf90_nowrite, ncid)
        call handle_err (ierr)

        ierr = nf90_inq_varid (ncid, var_name, var_id) 
        call handle_err (ierr)

        ierr = nf90_get_var (ncid, var_id, var, start_count, volume_count)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine read_3dvar_int
    !---------------------------------------------------------------------
    !                      End of Subroutine read_3dvar_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_3dvar 
    !---------------------------------------------------------------------
    subroutine write_3dvar (dvar, file_name, dim_name, dvar_name, long_name, units, Fill_Value, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        real (r8),              intent (in)         :: Fill_Value
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dvar_name 
        character (len = *),    intent (in)         :: dim_name     (n_d) 
        character (len = *),    intent (in)         :: file_name 
        real (r8),              intent (in)         :: dvar (dim_count (1), dim_count (2), dim_count (3))

        integer :: ierr, ncid, dim_dimid (n_d), dvar_id
        integer :: i, j, k

        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        do k = 1, n_d
            ierr = nf90_inq_dimid (ncid, dim_name (k), dim_dimid (k))
            call handle_err (ierr)
        end do
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_float, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_double, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        end if

        ierr = nf90_put_att (ncid, dvar_id, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "units", units)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "_FillValue", Fill_Value)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dvar_id, dvar)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_3dvar 
    !---------------------------------------------------------------------
    !                      End of Subroutine write_3dvar 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_3dvar_int 
    !---------------------------------------------------------------------
    subroutine write_3dvar_int (dvar, file_name, dim_name, dvar_name, long_name, units, Fill_Value, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: Fill_Value
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dvar_name 
        character (len = *),    intent (in)         :: dim_name     (n_d) 
        character (len = *),    intent (in)         :: file_name 
        integer,                intent (in)         :: dvar (dim_count (1), dim_count (2), dim_count (3))

        integer :: ierr, ncid, dim_dimid (n_d), dvar_id
        integer :: i, j, k

        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        do k = 1, n_d
            ierr = nf90_inq_dimid (ncid, dim_name (k), dim_dimid (k))
            call handle_err (ierr)
        end do
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_float, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_double, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        end if

        ierr = nf90_put_att (ncid, dvar_id, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "units", units)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "_FillValue", Fill_Value)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dvar_id, dvar)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_3dvar_int 
    !---------------------------------------------------------------------
    !                    End of Subroutine write_3dvar_int 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_4dvar
    !---------------------------------------------------------------------
    subroutine read_4dvar (var, file_name, var_name, start_count, volume_count, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: volume_count (n_d)
        integer,                intent (in)         :: start_count  (n_d)
        character (len = *),    intent (in)         :: var_name
        character (len = *),    intent (in)         :: file_name
        real (r8),              intent (out)        :: var (dim_count (1), dim_count (2), dim_count (3), dim_count (4))

        integer :: ierr, ncid, var_id, i, j, k
        
        call check_file_exist (file_name, .true.)
        
        ierr = nf90_open (file_name, nf90_nowrite, ncid)
        call handle_err (ierr)

        ierr = nf90_inq_varid (ncid, var_name, var_id) 
        call handle_err (ierr)

        ierr = nf90_get_var (ncid, var_id, var, start_count, volume_count)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine read_4dvar
    !---------------------------------------------------------------------
    !                      End of Subroutine read_4dvar
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_4dvar_int
    !---------------------------------------------------------------------
    subroutine read_4dvar_int (var, file_name, var_name, start_count, volume_count, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: volume_count (n_d)
        integer,                intent (in)         :: start_count  (n_d)
        character (len = *),    intent (in)         :: var_name
        character (len = *),    intent (in)         :: file_name
        integer,                intent (out)        :: var (dim_count (1), dim_count (2), dim_count (3), dim_count (4))

        integer :: ierr, ncid, var_id, i, j, k
        
        call check_file_exist (file_name, .true.)
        
        ierr = nf90_open (file_name, nf90_nowrite, ncid)
        call handle_err (ierr)

        ierr = nf90_inq_varid (ncid, var_name, var_id) 
        call handle_err (ierr)

        ierr = nf90_get_var (ncid, var_id, var, start_count, volume_count)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine read_4dvar_int
    !---------------------------------------------------------------------
    !                      End of Subroutine read_4dvar_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_3dvar 
    !---------------------------------------------------------------------
    subroutine write_4dvar (dvar, file_name, dim_name, dvar_name, long_name, units, Fill_Value, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        real (r8),              intent (in)         :: Fill_Value
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dvar_name 
        character (len = *),    intent (in)         :: dim_name     (n_d) 
        character (len = *),    intent (in)         :: file_name 
        real (r8),              intent (in)         :: dvar (dim_count (1), dim_count (2), dim_count (3), dim_count (4))

        integer :: ierr, ncid, dim_dimid (n_d), dvar_id
        integer :: i, j, k

        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        do k = 1, n_d
            ierr = nf90_inq_dimid (ncid, dim_name (k), dim_dimid (k))
            call handle_err (ierr)
        end do
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_float, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_double, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        end if

        ierr = nf90_put_att (ncid, dvar_id, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "units", units)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "_FillValue", Fill_Value)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dvar_id, dvar)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_4dvar 
    !---------------------------------------------------------------------
    !                      End of Subroutine write_4dvar 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_4dvar_int 
    !---------------------------------------------------------------------
    subroutine write_4dvar_int (dvar, file_name, dim_name, dvar_name, long_name, units, Fill_Value, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: Fill_Value
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dvar_name 
        character (len = *),    intent (in)         :: dim_name     (n_d) 
        character (len = *),    intent (in)         :: file_name 
        integer,                intent (in)         :: dvar (dim_count (1), dim_count (2), dim_count (3), dim_count (4))

        integer :: ierr, ncid, dim_dimid (n_d), dvar_id
        integer :: i, j, k

        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        do k = 1, n_d
            ierr = nf90_inq_dimid (ncid, dim_name (k), dim_dimid (k))
            call handle_err (ierr)
        end do
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_float, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_double, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        end if

        ierr = nf90_put_att (ncid, dvar_id, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "units", units)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "_FillValue", Fill_Value)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dvar_id, dvar)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_4dvar_int 
    !---------------------------------------------------------------------
    !                    End of Subroutine write_4dvar_int 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_5dvar
    !---------------------------------------------------------------------
    subroutine read_5dvar (var, file_name, var_name, start_count, volume_count, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: volume_count (n_d)
        integer,                intent (in)         :: start_count  (n_d)
        character (len = *),    intent (in)         :: var_name
        character (len = *),    intent (in)         :: file_name
        real (r8),              intent (out)        :: var (dim_count (1), dim_count (2), dim_count (3), dim_count (4), dim_count (5))

        integer :: ierr, ncid, var_id, i, j, k
        
        call check_file_exist (file_name, .true.)
        
        ierr = nf90_open (file_name, nf90_nowrite, ncid)
        call handle_err (ierr)

        ierr = nf90_inq_varid (ncid, var_name, var_id) 
        call handle_err (ierr)

        ierr = nf90_get_var (ncid, var_id, var, start_count, volume_count)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine read_5dvar
    !---------------------------------------------------------------------
    !                      End of Subroutine read_5dvar
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine read_5dvar_int
    !---------------------------------------------------------------------
    subroutine read_5dvar_int (var, file_name, var_name, start_count, volume_count, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: volume_count (n_d)
        integer,                intent (in)         :: start_count  (n_d)
        character (len = *),    intent (in)         :: var_name
        character (len = *),    intent (in)         :: file_name
        integer,                intent (out)        :: var (dim_count (1), dim_count (2), dim_count (3), dim_count (4), dim_count (5))

        integer :: ierr, ncid, var_id, i, j, k
        
        call check_file_exist (file_name, .true.)
        
        ierr = nf90_open (file_name, nf90_nowrite, ncid)
        call handle_err (ierr)

        ierr = nf90_inq_varid (ncid, var_name, var_id) 
        call handle_err (ierr)

        ierr = nf90_get_var (ncid, var_id, var, start_count, volume_count)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)
    end subroutine read_5dvar_int
    !---------------------------------------------------------------------
    !                      End of Subroutine read_5dvar_int
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_5dvar 
    !---------------------------------------------------------------------
    subroutine write_5dvar (dvar, file_name, dim_name, dvar_name, long_name, units, Fill_Value, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        real (r8),              intent (in)         :: Fill_Value
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dvar_name 
        character (len = *),    intent (in)         :: dim_name     (n_d) 
        character (len = *),    intent (in)         :: file_name 
        real (r8),              intent (in)         :: dvar (dim_count (1), dim_count (2), dim_count (3), dim_count (4), dim_count (5))

        integer :: ierr, ncid, dim_dimid (n_d), dvar_id
        integer :: i, j, k

        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        do k = 1, n_d
            ierr = nf90_inq_dimid (ncid, dim_name (k), dim_dimid (k))
            call handle_err (ierr)
        end do
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_float, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_double, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        end if

        ierr = nf90_put_att (ncid, dvar_id, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "units", units)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "_FillValue", Fill_Value)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dvar_id, dvar)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_5dvar 
    !---------------------------------------------------------------------
    !                      End of Subroutine write_5dvar 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine write_5dvar_int 
    !---------------------------------------------------------------------
    subroutine write_5dvar_int (dvar, file_name, dim_name, dvar_name, long_name, units, Fill_Value, dim_count, n_d)
        implicit none
        integer,                intent (in)         :: n_d
        integer,                intent (in)         :: dim_count    (n_d)
        integer,                intent (in)         :: Fill_Value
        character (len = *),    intent (in)         :: units 
        character (len = *),    intent (in)         :: long_name 
        character (len = *),    intent (in)         :: dvar_name 
        character (len = *),    intent (in)         :: dim_name     (n_d) 
        character (len = *),    intent (in)         :: file_name 
        integer,                intent (in)         :: dvar (dim_count (1), dim_count (2), dim_count (3), dim_count (4), dim_count (5))

        integer :: ierr, ncid, dim_dimid (n_d), dvar_id
        integer :: i, j, k

        ierr = nf90_open (file_name, nf90_write, ncid)
        call handle_err (ierr)

        ierr = nf90_redef (ncid)
        call handle_err (ierr)

        do k = 1, n_d
            ierr = nf90_inq_dimid (ncid, dim_name (k), dim_dimid (k))
            call handle_err (ierr)
        end do
        
        if (r8 == 4) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_float, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        else if (r8 == 8) then
            ierr = nf90_def_var (ncid, dvar_name, nf90_double, (/dim_dimid/), dvar_id)
            call handle_err (ierr)
        end if

        ierr = nf90_put_att (ncid, dvar_id, "long_name", long_name)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "units", units)
        call handle_err (ierr)

        ierr = nf90_put_att (ncid, dvar_id, "_FillValue", Fill_Value)
        call handle_err (ierr)

        ierr = nf90_enddef (ncid)
        call handle_err (ierr)

        ierr = nf90_put_var (ncid, dvar_id, dvar)
        call handle_err (ierr)

        ierr = nf90_close (ncid)
        call handle_err (ierr)

    end subroutine write_5dvar_int 
    !---------------------------------------------------------------------
    !                    End of Subroutine write_5dvar_int 
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine check_file_exist
    !---------------------------------------------------------------------
    subroutine check_file_exist (file_name, t_or_f)
        implicit none
        logical, optional,      intent (in)         ::  t_or_f
        character (len = *),    intent (in)         ::  file_name

        logical alive

        inquire (file = file_name, exist = alive)

        if (present (t_or_f)) then
            if (alive .and. (.not. t_or_f)) then
                write (*, *), "File "//trim(file_name)//" existed!"
                stop; return
            else if ((.not. alive) .and. t_or_f) then
                write (*, *), "File "//trim(file_name)//" not existed!"
                stop; return
            end if
        end if
    end subroutine check_file_exist
    !---------------------------------------------------------------------
    !                      End of Subroutine check_file_exist
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                       Subroutine handle_err
    !---------------------------------------------------------------------
    subroutine handle_err (ierr)
        integer, intent(in) :: ierr

        if (ierr /= nf90_noerr) then
            write (6,*) trim (nf90_strerror (ierr))
            stop "Stopped"
        end if
    end subroutine handle_err
    !---------------------------------------------------------------------
    !                   End of Subroutine handle_err
    !---------------------------------------------------------------------
end module nc_read_write_interface

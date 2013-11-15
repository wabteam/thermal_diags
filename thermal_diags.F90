program thermal_diags
! Purpose:
! This code is devised for ...
!
!
! Record of revisions:
!    Date             Programmer                Description of change  
!==============    ===================   =============================
! 2013/11/13         WenYu Huang                Original Code
! 2013/11/14         WenYu Huang                V2 
! ...
!
! hwy (2013/11/14) has successfully tested the WOA2009 monthly data with
!    integer,    parameter   ::  num_lon     =   360
!    integer,    parameter   ::  num_lat     =   180
!    integer,    parameter   ::  num_lev     =   24 
!    integer,    parameter   ::  num_time    =   12
! and the namelist is as follows:
!&thermal_diags_nml
!    thetao_fi   =   "/public/nfs/9/observation/WOA2009/temperature_monthly_1deg.nc" 
!    so_fi       =   "/public/nfs/9/observation/WOA2009/salinity_monthly_1deg.nc" 
!    thetao_name =   "t_an"
!    so_name     =   "s_an"
!    lon_name    =   "lon"
!    lat_name    =   "lat"
!    lev_name    =   "depth"
!    time_name   =   "time"
!    if_lev_full_need    =  .false.
!    if_273	=   .false.
!    output_title=   "WOA2009_"
!/
! hwy (2013/11/14) has successfully tested the FGOALS-g2 monthly data with
!    integer,    parameter   ::  num_lon     =   360
!    integer,    parameter   ::  num_lat     =   196
!    integer,    parameter   ::  num_lev     =   30 
!    integer,    parameter   ::  num_time    =   1872
! and the namelist is as follows:
!&thermal_diags_nml
!    thetao_fi   =   "/public/nfs/9/hwy/indian_ocean_dipole_analysis/data/mod/thetao/thetao_Omon_FGOALS-g2_historical_r1i1p1_185001-200512.nc"
!    so_fi       =   "/public/nfs/9/hwy/indian_ocean_dipole_analysis/data/mod/so/so_Omon_FGOALS-g2_historical_r1i1p1_185001-200512.nc"
!    thetao_name =   "thetao"
!    so_name     =   "so"
!    lon_name    =   "lon"
!    lat_name    =   "lat"
!    lev_name    =   "lev"
!    time_name   =   "time"
!    if_lev_full_need    =  .true.
!    if_273	=   .true.
!    output_title=   "FGOALS-g2_1850-2005_"
!/




!Any comments please send to huangwenyu@mail.tsinghua.edu.cn                      
    use nc_read_write_interface

    implicit none
    
    integer,    parameter   ::  r8          =   8 
    integer,    parameter   ::  num_lon     =   360
    integer,    parameter   ::  num_lat     =   196
    integer,    parameter   ::  num_lev     =   30 
    integer,    parameter   ::  num_time    =   12
    integer,    parameter   ::  imt         =   num_lon
    integer,    parameter   ::  jmt         =   num_lat
    integer,    parameter   ::  km          =   num_lev

    integer :: i, j, k, t
    integer :: lev_250, lev_500, lev_700, lev_1000 
    real (r8) :: remain_250, remain_500, remain_700, remain_1000 
    real (r8) :: HC_depth

    real (r8), allocatable, dimension(:)        ::  lon
    real (r8), allocatable, dimension(:)        ::  lat
    real (r8), allocatable, dimension(:)        ::  lev
    real (r8), allocatable, dimension(:)        ::  lev_full
    real (r8), allocatable, dimension(:)        ::  zkt 
    real (r8), allocatable, dimension(:)        ::  dz 
    real (r8), allocatable, dimension(:)        ::  time 
    real (r8), allocatable, dimension(:,:,:)    ::  so
    real (r8), allocatable, dimension(:,:,:)    ::  thetao
    real (r8), allocatable, dimension(:,:,:)    ::  density 
    real (r8), allocatable, dimension(:,:,:)    ::  pdensity 
    integer,   allocatable, dimension(:,:,:)    ::  vit 
    integer,   allocatable, dimension(:,:)      ::  nit 
    integer,   allocatable, dimension(:,:)      ::  itnu 
    real (r8), allocatable, dimension(:,:,:)    ::  Z20 
    real (r8), allocatable, dimension(:,:,:)    ::  mld_t01
    real (r8), allocatable, dimension(:,:,:)    ::  mld_t02
    real (r8), allocatable, dimension(:,:,:)    ::  mld_r001
    real (r8), allocatable, dimension(:,:,:)    ::  mld_r002
    real (r8), allocatable, dimension(:,:,:)    ::  mld_r003
    real (r8), allocatable, dimension(:,:,:)    ::  HC100 
    real (r8), allocatable, dimension(:,:,:)    ::  HC150 
    real (r8), allocatable, dimension(:,:,:)    ::  HC250 
    real (r8), allocatable, dimension(:,:,:)    ::  HC500 
    real (r8), allocatable, dimension(:,:,:)    ::  HC700 
    real (r8), allocatable, dimension(:,:,:)    ::  HC1000 
    real (r8) :: miss
    real (r8) :: ref_273

    character (len =    500)    ::  namelist_file_name
    
    character (len  =   500)    ::  thetao_fi   =   "/public/nfs/9/hwy/indian_ocean_dipole_analysis/data/mod/thetao/"//&
                                                    "thetao_Omon_FGOALS-g2_historical_r1i1p1_185001-200512.nc"
    character (len  =   500)    ::  so_fi       =   "/public/nfs/9/hwy/indian_ocean_dipole_analysis/data/mod/so/"//&
                                                    "so_Omon_FGOALS-g2_historical_r1i1p1_185001-200512.nc"
    character (len  =   500)    ::  thetao_name =   "thetao"
    character (len  =   500)    ::  so_name     =   "so"
    character (len  =   500)    ::  lon_name    =   "lon"
    character (len  =   500)    ::  lat_name    =   "lat"
    character (len  =   500)    ::  lev_name    =   "lev"
    character (len  =   500)    ::  time_name   =   "time"
    logical                     ::  if_lev_full_need    =  .false.
    logical                     ::  if_273              =  .false.
    
    character (len  =   500)    ::  Z20_fi      =   "Z20.nc"
    character (len  =   500)    ::  mld_t01_fi  =   "mld_t01.nc"
    character (len  =   500)    ::  mld_t02_fi  =   "mld_t02.nc"
    character (len  =   500)    ::  mld_r001_fi =   "mld_r001.nc"
    character (len  =   500)    ::  mld_r002_fi =   "mld_r002.nc"
    character (len  =   500)    ::  mld_r003_fi =   "mld_r003.nc"
    character (len  =   500)    ::  HC100_fi    =   "HC100.nc"
    character (len  =   500)    ::  HC150_fi    =   "HC150.nc"
    character (len  =   500)    ::  HC250_fi    =   "HC250.nc"
    character (len  =   500)    ::  HC500_fi    =   "HC500.nc"
    character (len  =   500)    ::  HC700_fi    =   "HC700.nc"
    character (len  =   500)    ::  HC1000_fi   =   "HC1000.nc"
    character (len  =   500)    ::  output_title 
    character (len  =   500)    ::  temp_title 

    namelist /thermal_diags_nml/  thetao_fi,    &
                                  so_fi,        &
                                  thetao_name,  &
                                  so_name,      &
                                  lon_name,     &
                                  lat_name,     &
                                  lev_name,     &
                                  time_name,    &
                                  if_lev_full_need,&
                                  if_273,       &
                                  output_title


    allocate(lon(num_lon))
    allocate(lat(num_lat))
    allocate(lev(num_lev))
    allocate(lev_full(num_lev+1))
    allocate(zkt(num_lev))
    allocate(dz(num_lev))
    allocate(time(num_time))
    allocate(so(num_lon,num_lat,num_lev))
    allocate(thetao(num_lon,num_lat,num_lev))
    allocate(density(num_lon,num_lat,num_lev))
    allocate(pdensity(num_lon,num_lat,num_lev))
    allocate(vit(num_lon,num_lat,num_lev))
    allocate(nit(num_lon,num_lat))
    allocate(itnu(num_lon,num_lat))
    allocate(Z20(num_lon,num_lat,num_time))
    allocate(mld_t01(num_lon,num_lat,num_time))
    allocate(mld_t02(num_lon,num_lat,num_time))
    allocate(mld_r001(num_lon,num_lat,num_time))
    allocate(mld_r002(num_lon,num_lat,num_time))
    allocate(mld_r003(num_lon,num_lat,num_time))
    allocate(HC100(num_lon,num_lat,num_time))
    allocate(HC150(num_lon,num_lat,num_time))
    allocate(HC250(num_lon,num_lat,num_time))
    allocate(HC500(num_lon,num_lat,num_time))
    allocate(HC700(num_lon,num_lat,num_time))
    allocate(HC1000(num_lon,num_lat,num_time))


    call get_command_argument (1, namelist_file_name)
    open (11, file = namelist_file_name)
        read (11, nml = thermal_diags_nml)
    close (11)

    if (if_273) then
        ref_273 = 273.15d0
    else
        ref_273 = 0.0d0
    end if


    temp_title  =   Z20_fi
    Z20_fi      =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   mld_t01_fi
    mld_t01_fi  =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   mld_t02_fi
    mld_t02_fi  =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   mld_r001_fi
    mld_r001_fi =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   mld_r002_fi
    mld_r002_fi =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   mld_r003_fi
    mld_r003_fi =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   HC100_fi
    HC100_fi    =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   HC150_fi
    HC150_fi    =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   HC250_fi
    HC250_fi    =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   HC500_fi
    HC500_fi    =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   HC700_fi
    HC700_fi    =   trim(output_title)//temp_title
    temp_title  =   ""

    temp_title  =   HC1000_fi
    HC1000_fi   =   trim(output_title)//temp_title
    temp_title  =   ""


    do t = 1, num_time
        print *, t
        call nc_read_write_interface_read_var (lon,    thetao_fi, lon_name,    [1],      [num_lon],                  [num_lon],                  1)  
        call nc_read_write_interface_read_var (lat,    thetao_fi, lat_name,    [1],      [num_lat],                  [num_lat],                  1)  
        call nc_read_write_interface_read_var (lev,    thetao_fi, lev_name,    [1],      [num_lev],                  [num_lev],                  1)  
        zkt = lev
        if (minval(lev) < 0.0d0) then
            print *,"check place 1 in thermal_diags.F90"
            stop
        end if 
        call nc_read_write_interface_read_var (time,   thetao_fi, time_name,   [1],      [num_time],                 [num_time],                 1)  
        call nc_read_write_interface_read_var (so,     so_fi,     so_name,     [1,1,1,t],[num_lon,num_lat,num_lev,1],[num_lon,num_lat,num_lev,1],4)  
        call nc_read_write_interface_read_var (thetao, thetao_fi, thetao_name, [1,1,1,t],[num_lon,num_lat,num_lev,1],[num_lon,num_lat,num_lev,1],4)  
        
        if (if_lev_full_need) then
            lev_full(1) = 0.0d0
            do k = 2, num_lev+1
                lev_full(k) = 2*lev(k-1)-lev_full(k-1)
            end do
            do k = 1, num_lev
                dz(k) = lev_full(k+1)-lev_full(k)
            end do
            print *, lev_full
        end if

        miss    =   maxval(thetao)
        where (thetao /= miss) vit = 1
        where (thetao == miss) vit = 0
        do i = 1, num_lon
        do j = 1, num_lat
            nit(i,j) = sum(vit(i,j,:))
        end do
        end do
        itnu = nit
        do i = 1, num_lon
        do j = 1, num_lat
        do k = 1, num_lev
            if (vit(i,j,k) == 0) then
                pdensity(i,j,k) = miss
                density(i,j,k)  = miss
            else
                call density_mcdougall2003(pdensity(i,j,k),thetao(i,j,k)-ref_273,so(i,j,k),lev(1))                
                call density_mcdougall2003(density(i,j,k),thetao(i,j,k)-ref_273,so(i,j,k),lev(k))                
            end if
        end do
        end do
        end do

        if (if_lev_full_need) then
            call get_right_depth_for_HC(250.0d0, lev_250, remain_250)
            print *, "250m: ", lev_full(lev_250+1)-remain_250
            call get_right_depth_for_HC(500.0d0, lev_500, remain_500)
            print *, "500m: ", lev_full(lev_500+1)-remain_500
            call get_right_depth_for_HC(700.0d0, lev_700, remain_700)
            print *, "700m: ", lev_full(lev_700+1)-remain_700
            call get_right_depth_for_HC(1000.0d0, lev_1000, remain_1000)
            print *, "1000m: ", lev_full(lev_1000+1)-remain_1000
        end if

    do i = 1, num_lon
        if (.not.if_lev_full_need) then
            exit
        end if
        do j = 1, num_lat
            HC100(i,j,t) = 0.0d0
            HC150(i,j,t) = 0.0d0
            HC250(i,j,t) = 0.0d0
            HC500(i,j,t) = 0.0d0
            HC700(i,j,t) = 0.0d0
            HC1000(i,j,t) = 0.0d0
            if (nit(i,j) == 0) then
                HC100(i,j,t) = miss
                HC150(i,j,t) = miss
                HC250(i,j,t) = miss 
                HC500(i,j,t) = miss
                HC700(i,j,t) = miss
                HC1000(i,j,t) = miss
            else
                do k = 1, min(10,nit(i,j))
                    HC100(i,j,t) = HC100(i,j,t)+pdensity(i,j,k)*(thetao(i,j,k)-ref_273)*dz(k)*3850.0d0
                end do
                do k = 1, min(15,nit(i,j))
                    HC150(i,j,t) = HC150(i,j,t)+pdensity(i,j,k)*(thetao(i,j,k)-ref_273)*dz(k)*3850.0d0
                end do
                do k = 1, min(lev_250,nit(i,j))
                    if (k == lev_250) then
                        HC_depth = dz(k) - remain_250
                    else
                        HC_depth = dz(k)
                    end if
                    HC250(i,j,t) = HC250(i,j,t)+pdensity(i,j,k)*(thetao(i,j,k)-ref_273)*HC_depth*3850.0d0
                end do
                do k = 1, min(lev_500,nit(i,j))
                    if (k == lev_500) then
                        HC_depth = dz(k) - remain_500
                    else
                        HC_depth = dz(k)
                    end if
                    HC500(i,j,t) = HC500(i,j,t)+pdensity(i,j,k)*(thetao(i,j,k)-ref_273)*HC_depth*3850.0d0
                end do
                do k = 1, min(lev_700,nit(i,j))
                    if (k == lev_700) then
                        HC_depth = dz(k) - remain_700
                    else
                        HC_depth = dz(k)
                    end if
                    HC700(i,j,t) = HC700(i,j,t)+pdensity(i,j,k)*(thetao(i,j,k)-ref_273)*HC_depth*3850.0d0
                end do
                do k = 1, min(lev_1000,nit(i,j))
                    if (k == lev_1000) then
                        HC_depth = dz(k) - remain_1000
                    else
                        HC_depth = dz(k)
                    end if
                    HC1000(i,j,t) = HC1000(i,j,t)+pdensity(i,j,k)*(thetao(i,j,k)-ref_273)*HC_depth*3850.0d0
                end do
            end if
        end do
        end do




        call get_mld_by_dt_omega(mld_t01(:,:,t),thetao, 0.1d0)
        call get_mld_by_dt_omega(mld_t02(:,:,t),thetao, 0.2d0)
        call get_mld_by_dt_omega(mld_r001(:,:,t),pdensity, 0.01d0)
        call get_mld_by_dt_omega(mld_r002(:,:,t),pdensity, 0.02d0)
        call get_mld_by_dt_omega(mld_r003(:,:,t),pdensity, 0.03d0)
        call get_thermocline_depth_omega(Z20(:,:,t), thetao)
        where (nit == 0) mld_t01(:,:,t) = miss 
        where (nit == 0) mld_t02(:,:,t) = miss 
        where (nit == 0) mld_r001(:,:,t) = miss 
        where (nit == 0) mld_r002(:,:,t) = miss 
        where (nit == 0) mld_r003(:,:,t) = miss 
        where (nit == 0) Z20(:,:,t) = miss 

    end do

    call nc_read_write_interface_delete_file (Z20_fi)
    call nc_read_write_interface_create_file (Z20_fi)
    call nc_read_write_interface_write_dim (time, Z20_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  Z20_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  Z20_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (Z20,  Z20_fi, ["lon ", "lat ", "time"],  "Z20", "thermalhaline depth", "m", miss, [num_lon,num_lat,num_time],3)

    call nc_read_write_interface_delete_file (mld_t01_fi)
    call nc_read_write_interface_create_file (mld_t01_fi)
    call nc_read_write_interface_write_dim (time, mld_t01_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  mld_t01_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  mld_t01_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (mld_t01,  mld_t01_fi, ["lon ", "lat ", "time"],  "mld_t01", "mixed layer depth", "m", miss, [num_lon,num_lat,num_time],3)

    call nc_read_write_interface_delete_file (mld_t02_fi)
    call nc_read_write_interface_create_file (mld_t02_fi)
    call nc_read_write_interface_write_dim (time, mld_t02_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  mld_t02_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  mld_t02_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (mld_t02,  mld_t02_fi, ["lon ", "lat ", "time"],  "mld_t02", "mixed layer depth", "m", miss, [num_lon,num_lat,num_time],3)

    call nc_read_write_interface_delete_file (mld_r001_fi)
    call nc_read_write_interface_create_file (mld_r001_fi)
    call nc_read_write_interface_write_dim (time, mld_r001_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  mld_r001_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  mld_r001_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (mld_r001,  mld_r001_fi, ["lon ", "lat ", "time"],  "mld_r001", "mixed layer depth", "m", miss, [num_lon,num_lat,num_time],3)

    call nc_read_write_interface_delete_file (mld_r002_fi)
    call nc_read_write_interface_create_file (mld_r002_fi)
    call nc_read_write_interface_write_dim (time, mld_r002_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  mld_r002_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  mld_r002_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (mld_r002,  mld_r002_fi, ["lon ", "lat ", "time"],  "mld_r002", "mixed layer depth", "m", miss, [num_lon,num_lat,num_time],3)
    

    call nc_read_write_interface_delete_file (mld_r003_fi)
    call nc_read_write_interface_create_file (mld_r003_fi)
    call nc_read_write_interface_write_dim (time, mld_r003_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  mld_r003_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  mld_r003_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (mld_r003,  mld_r003_fi, ["lon ", "lat ", "time"],  "mld_r003", "mixed layer depth", "m", miss, [num_lon,num_lat,num_time],3)

    if (.not.if_lev_full_need) then
        stop 
    end if
    

    call nc_read_write_interface_delete_file (HC100_fi)
    call nc_read_write_interface_create_file (HC100_fi)
    call nc_read_write_interface_write_dim (time, HC100_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  HC100_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  HC100_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (HC100,  HC100_fi, ["lon ", "lat ", "time"],  "HC100", "Heat Content in upper 100 m", "Jm-2", miss, [num_lon,num_lat,num_time],3)

    call nc_read_write_interface_delete_file (HC150_fi)
    call nc_read_write_interface_create_file (HC150_fi)
    call nc_read_write_interface_write_dim (time, HC150_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  HC150_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  HC150_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (HC150,  HC150_fi, ["lon ", "lat ", "time"],  "HC150", "Heat Content in upper 150 m", "Jm-2", miss, [num_lon,num_lat,num_time],3)

    call nc_read_write_interface_delete_file (HC250_fi)
    call nc_read_write_interface_create_file (HC250_fi)
    call nc_read_write_interface_write_dim (time, HC250_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  HC250_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  HC250_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (HC250,  HC250_fi, ["lon ", "lat ", "time"],  "HC250", "Heat Content in upper 250 m", "Jm-2", miss, [num_lon,num_lat,num_time],3)

    call nc_read_write_interface_delete_file (HC500_fi)
    call nc_read_write_interface_create_file (HC500_fi)
    call nc_read_write_interface_write_dim (time, HC500_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  HC500_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  HC500_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (HC500,  HC500_fi, ["lon ", "lat ", "time"],  "HC500", "Heat Content in upper 500 m", "Jm-2", miss, [num_lon,num_lat,num_time],3)

    call nc_read_write_interface_delete_file (HC700_fi)
    call nc_read_write_interface_create_file (HC700_fi)
    call nc_read_write_interface_write_dim (time, HC700_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  HC700_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  HC700_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (HC700,  HC700_fi, ["lon ", "lat ", "time"],  "HC700", "Heat Content in upper 700 m", "Jm-2", miss, [num_lon,num_lat,num_time],3)

    call nc_read_write_interface_delete_file (HC1000_fi)
    call nc_read_write_interface_create_file (HC1000_fi)
    call nc_read_write_interface_write_dim (time, HC1000_fi, "time", "time", "days since 0001-01",num_time)
    call nc_read_write_interface_write_dim (lon,  HC1000_fi, "lon",  "lon",  "degrees_east",      num_lon)
    call nc_read_write_interface_write_dim (lat,  HC1000_fi, "lat",  "lat",  "degrees_north",     num_lat)
    call nc_read_write_interface_write_var (HC1000,  HC1000_fi, ["lon ", "lat ", "time"],  "HC1000", "Heat Content in upper 1000 m", "Jm-2", miss, [num_lon,num_lat,num_time],3)
contains
    !---------------------------------------------------------------------
    !                    subroutine get_mld_by_dt_omega
    !---------------------------------------------------------------------
    subroutine get_mld_by_dt_omega(mld_out, ts_in, delta_t)
        real (r8), intent (in)    :: ts_in   (imt, jmt, km)
        real (r8), intent (out)   :: mld_out (imt, jmt)
        real (r8), intent (in)    :: delta_t
                                  !the difference of potential temperature and the mixed layer depth in Celsius degree
        integer :: i, j, k

        do j = 1, jmt
        do i = 1, imt
            if (vit (i, j, 1) < 0.5d0) then
                mld_out (i, j) = 0.0d0 
                cycle 
            end if
            call get_mld_by_dt_scm_omega (mld_out(i, j), ts_in(i, j, 1:km), delta_t, zkt(1:km), km, itnu(i,j))
        end do
        end do
        return
    end subroutine get_mld_by_dt_omega
    !---------------------------------------------------------------------
    !                  end of subroutine get_mld_by_dt_omega
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                subroutine get_mld_by_dt_scm_omega
    !---------------------------------------------------------------------
    subroutine get_mld_by_dt_scm_omega(mld_out, ts_in, delta_t, zkt_in, km_in, itnu_in)
        integer,    intent (in) ::  itnu_in     
                                    !the number of vertical levels for ocean
        integer,    intent (in) ::  km_in
                                    !the number of vertical levels
        real (r8),  intent (in),    dimension (1:km_in) ::  zkt_in   
                                    !the depth of the center position of each levels (in metres) (can be positive/negative)
        real (r8),  intent (in) ::  delta_t
                                    !the difference of potential temperature and the mixed layer depth in Celsius degree
        real (r8),  intent (in),    dimension (1:km_in) ::  ts_in
                                    !potential temperature in Celsius degree
        real (r8),  intent (out) ::  mld_out
                                    !mixed layer depth in m
        real (r8) :: tm 
        integer :: k

        do k = 1, itnu_in
            if (abs(ts_in(1)-ts_in(k)) > delta_t) then
                tm      = ts_in(1) - sign(delta_t,ts_in(1)-ts_in(k))
                mld_out = zkt_in(k) + (zkt_in(k-1)-zkt_in(k))*(tm-ts_in(k))/(ts_in(k-1)-ts_in(k)+1.0d-20)
                mld_out = abs(mld_out)
                return 
            end if
        end do
        mld_out = zkt_in(itnu_in)
        mld_out = abs(mld_out)
        return
    end subroutine get_mld_by_dt_scm_omega
    !---------------------------------------------------------------------
    !             end of subroutine get_mld_by_dt_scm_omega
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                 subroutine get_thermocline_depth_omega
    !---------------------------------------------------------------------
    subroutine get_thermocline_depth_omega(thc_depth_out, ts_in)
        real (r8), intent (in)    :: ts_in   (imt, jmt, km)
        real (r8), intent (out)   :: thc_depth_out (imt, jmt)
        integer :: i, j, k
    
        do j = 1, jmt
        do i = 1, imt
            if (vit (i, j, 1) < 0.5d0) then
                thc_depth_out (i, j) = 0.0d0 
                cycle 
            end if
            call get_thermocline_depth_scm_omega (thc_depth_out(i, j), ts_in(i, j, 1:km), 20.0d0+ref_273, zkt(1:km), km, itnu(i,j))
            if (thc_depth_out(i,j) .gt. 500.0d0) then
                print *, ts_in(i,j,1:itnu(i,j))
                print *, "------------------------------" 
                print *,  "max: ",thc_depth_out(i,j)
                print *, lev(20:21)
                print *, "------------------------------" 

            end if
        end do
        end do
        return
        return
    end subroutine get_thermocline_depth_omega
    !---------------------------------------------------------------------
    !             end of subroutine get_thermocline_depth_omega
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !              subroutine get_thermocline_depth_scm_omega
    !---------------------------------------------------------------------
    subroutine get_thermocline_depth_scm_omega(thc_depth_out, ts_in, fixt, zkt_in, km_in, itnu_in)
        integer,    intent (in) ::  itnu_in     
                                    !the number of vertical levels for ocean
        integer,    intent (in) ::  km_in
                                    !the number of vertical levels
        real (r8),  intent (in),    dimension (1:km_in) ::  zkt_in   
                                    !the depth of the center position of each levels (in metres) (can be positive/negative)
        real (r8),  intent (in) ::  fixt
                                    !the potential temperature at the thermocline depth in Celsius degree
        real (r8),  intent (in),    dimension (1:km_in) ::  ts_in
                                    !potential temperature in Celsius degree
        real (r8),  intent (out) :: thc_depth_out 
                                    !mixed layer depth in m
        integer :: k                            
        real (r8)  :: ts_max, ts_min

        ts_max = maxval (ts_in(1:itnu_in))
        ts_min = minval (ts_in(1:itnu_in))

        if (fixt < ts_min) then
            thc_depth_out = zkt_in(itnu_in)
            thc_depth_out = abs (thc_depth_out)
            return
        else if (fixt > ts_max .or. fixt > ts_in(1)) then
            thc_depth_out = 0.0d0
            return
        end if

        do k = 1, itnu_in - 1
            if ((fixt >= ts_in(k+1) .and. fixt <= ts_in(k)) .or. &
                (fixt <= ts_in(k+1) .and. fixt >= ts_in(k))) then
                thc_depth_out = zkt_in(k) + (zkt_in(k+1)-zkt_in(k))*(fixt-ts_in(k))/(ts_in(k+1)-ts_in(k)+1.0d-20)        
                thc_depth_out = abs (thc_depth_out)
                return
            end if
        end do
    
    end subroutine get_thermocline_depth_scm_omega
    !---------------------------------------------------------------------
    !          end of subroutine get_thermocline_depth_scm_omega
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                   Subroutine density_mcdougall2003
    !---------------------------------------------------------------------
    subroutine density_mcdougall2003(rho_out,theta_in,s_in,p_in)
        real(r8), intent(in)  :: theta_in, s_in, p_in
        real(r8), intent(out) :: rho_out
        real(r8) :: p_dbar, a(0:11), b(0:12), P1, P2
        ! the p_in input must be in m or dbar
        p_dbar = abs(p_in)

        a = (/ +9.99843699d+2, +7.35212840d+0, -5.45928211d-2, &
               +3.98476704d-4, +2.96928239d+0, -7.23268813d-3, &
               +2.12382341d-3, +1.04004591d-2, +1.03970529d-7, &
               +5.18761880d-6, -3.24041825d-8, -1.23869360d-11  /)

        b = (/ +1.0d0,          +7.28606739d-3,  -4.60835542d-5,  &
               +3.68390573d-7,  +1.80809186d-10, +2.14691708d-3,  &
               -9.27062484d-6,  -1.78343643d-10, +4.76534122d-6,  &
               +1.63410736d-9,  +5.30848875d-6,  -3.03175128d-16, &
               -1.27934137d-17  /)
        
        P1 = a(0) + a(1)*theta_in**1 & 
                  + a(2)*theta_in**2 &
                  + a(3)*theta_in**3 &
                  + a(4)*s_in &
                  + a(5)*s_in*theta_in &
                  + a(6)*s_in**2 &
                  + a(7)*p_dbar &
                  + a(8)*p_dbar*theta_in**2 &
                  + a(9)*p_dbar*s_in &
                  + a(10)*p_dbar**2 &
                  + a(11)*p_dbar**2*theta_in**2
        

        P2 = b(0) + b(1)*theta_in**1 &
                  + b(2)*theta_in**2 &
                  + b(3)*theta_in**3 &
                  + b(4)*theta_in**4 &
                  + b(5)*s_in        &
                  + b(6)*s_in*theta_in**1 &
                  + b(7)*s_in*theta_in**3 &
                  + b(8)*s_in**(3.0d0/2.0d0) &
                  + b(9)*s_in**(3.0d0/2.0d0)*theta_in**2 &
                  + b(10)*p_dbar &
                  + b(11)*p_dbar**2*theta_in**3 &
                  + b(12)*p_dbar**3*theta_in

        rho_out = P1/P2
    end subroutine density_mcdougall2003
    !---------------------------------------------------------------------
    !               End of Subroutine density_mcdougall2003
    !---------------------------------------------------------------------
    !---------------------------------------------------------------------
    !                          Subroutine get_right_depth_for_HC
    !---------------------------------------------------------------------
    subroutine get_right_depth_for_HC(depth_in, lev_out, remain_out)
        real (r8), intent (in) :: depth_in
        real (r8), intent (out):: remain_out 
        integer,   intent (out):: lev_out
        integer :: i, j, k

        if (depth_in <= 0.0d0 .or. minval(lev_full) < 0.0d0) then
            print *, "check place 1 in subroutine:¡¡get_right_depth_for_HC"
            stop
        end if
        
        if (depth_in > maxval(lev_full)) then
            print *, "check place 2 in subroutine:¡¡get_right_depth_for_HC"
            stop
        end if

        do k = 1, num_lev
            if (lev_full(k) < depth_in .and. lev_full(k+1) >= depth_in) then
                lev_out = k
                remain_out =  lev_full(k+1)-depth_in
                return
            end if
        end do

        print *, "check place 3 in subroutine:¡¡get_right_depth_for_HC"
        stop
    
    end subroutine get_right_depth_for_HC
    !---------------------------------------------------------------------
    !                      End of Subroutine get_right_depth_for_HC
    !---------------------------------------------------------------------
    

end program thermal_diags

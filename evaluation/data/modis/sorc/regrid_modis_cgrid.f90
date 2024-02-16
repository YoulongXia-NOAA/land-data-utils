program regrid_modis_cgrid

use netcdf

implicit none

character*256 :: modis_regrid_filename = '/scratch2/NCEPDEV/land/data/evaluation/MODIS/albedo/land_mapping_files/MODIS0.05_to_land_mapping.C96.nc'
character*256 :: land_static_filename = '/scratch2/NCEPDEV/land/data/forcing/era5/static/ufs-land_C96_static_fields.nc'
character*256 :: modis_land_filename = '/scratch2/NCEPDEV/land/data/evaluation/MODIS/landcover/MCD12C1.A2021001.061.nc'
character*256 :: modis_filename 
character*256 :: output_filename 

character*7   :: datestring
double precision  :: sec_since
integer           :: offset_ss
integer           :: dim_time, id_time, id_lat, id_lon
character*19      :: current_date  ! current date
character*19      :: since_date = "1970-01-01 00:00:00"
character*8       :: daystring
character*4       :: yyyy
character*2       :: mm, dd

integer*2, allocatable :: majority_land_cover(:,:)
integer*2, allocatable :: barren_fraction    (:,:)
integer*2, allocatable :: snow_fraction      (:,:)
integer*2, allocatable :: urban_fraction     (:,:)
integer*2, allocatable :: water_fraction     (:,:)

integer*2, allocatable :: albedo_quality  (:,:)
integer*2, allocatable :: percent_snow    (:,:)
integer*2, allocatable :: modis_bsa_nir   (:,:)
integer*2, allocatable :: modis_bsa_vis   (:,:)
integer*2, allocatable :: modis_bsa_sw    (:,:)
integer*2, allocatable :: modis_wsa_nir   (:,:)
integer*2, allocatable :: modis_wsa_vis   (:,:)
integer*2, allocatable :: modis_wsa_sw    (:,:)

integer  , allocatable :: mapping_location(:,:)

integer  , allocatable :: vegetation_category(:)
real     , allocatable :: latitude(:)
real     , allocatable :: longitude(:)

integer*2, allocatable :: percent_good_data  (:)
integer*2, allocatable :: data_regrid        (:)

integer :: error, ncid, dimid, varid(8)
integer :: nlocations, idim_modis_length, jdim_modis_length
integer*2, parameter ::  nodata_int = -32767

!call getarg(1,datestring)

open(18, file='date_input.txt', status='old')
read(18,'(A7)') datestring
read(18,'(A8)') daystring
read(18,'(A4)') yyyy
read(18,'(A2)') mm
read(18,'(A2)') dd
close(18)

offset_ss=0
current_date=yyyy//'-'//mm//'-'//dd//' 12:00:00'
call calc_sec_since(since_date, current_date, offset_ss, sec_since)

!====================================
! read land static file

error = nf90_open(trim(land_static_filename), NF90_NOWRITE, ncid)
  call netcdf_err(error, 'opening file: '//trim(land_static_filename) )
  
error = nf90_inq_dimid(ncid, "location", dimid)
  call netcdf_err(error, 'inquire location dimension' )

error = nf90_inquire_dimension(ncid, dimid, len = nlocations)
  call netcdf_err(error, 'reading location dimension' )

allocate(latitude(nlocations))
allocate(longitude(nlocations))   
allocate(vegetation_category(nlocations))

error = nf90_inq_varid(ncid, 'latitude', varid(1))
 call netcdf_err(error, 'inquire latitude variable' )
error = nf90_get_var(ncid, varid(1), latitude)
 call netcdf_err(error, 'reading latitude variable' )

error = nf90_inq_varid(ncid, 'longitude', varid(1))
 call netcdf_err(error, 'inquire longitude variable' )
error = nf90_get_var(ncid, varid(1), longitude)
 call netcdf_err(error, 'reading longitude variable' )

error = nf90_inq_varid(ncid, 'vegetation_category', varid(1))
 call netcdf_err(error, 'inquire vegetation_category variable' )
error = nf90_get_var(ncid, varid(1), vegetation_category)
 call netcdf_err(error, 'reading vegetation_category variable' )

error = nf90_close(ncid)
 call netcdf_err(error, 'closing file: '//trim(land_static_filename) )

!====================================
! read mapping file

error = nf90_open(trim(modis_regrid_filename),nf90_nowrite, ncid)
  call netcdf_err(error, 'opening file: '//trim(modis_regrid_filename) )
    
error = nf90_inq_dimid(ncid, "idim", dimid)
  call netcdf_err(error, 'inquire idim dimension' )

error = nf90_inquire_dimension(ncid, dimid, len = idim_modis_length)
  call netcdf_err(error, 'reading idim dimension' )
   
error = nf90_inq_dimid(ncid, "jdim", dimid)
  call netcdf_err(error, 'inquire jdim dimension' )

error = nf90_inquire_dimension(ncid, dimid, len = jdim_modis_length)
  call netcdf_err(error, 'reading jdim dimension' )
   
allocate(mapping_location(idim_modis_length,jdim_modis_length))

error = nf90_inq_varid(ncid, 'mapping_location', varid(1))
 call netcdf_err(error, 'inquire mapping_location variable' )

error = nf90_get_var(ncid, varid(1), mapping_location)
 call netcdf_err(error, 'reading mapping_location variable' )
    
error = nf90_close(ncid)
 call netcdf_err(error, 'closing file: '//trim(modis_regrid_filename) )

!====================================
! read land cover file

error = nf90_open(trim(modis_land_filename),nf90_nowrite, ncid)
  call netcdf_err(error, 'opening file: '//trim(modis_land_filename) )
   
allocate(majority_land_cover(idim_modis_length,jdim_modis_length))
allocate(      snow_fraction(idim_modis_length,jdim_modis_length))
allocate(     water_fraction(idim_modis_length,jdim_modis_length))

error = nf90_inq_varid(ncid, 'majority_land_cover_igbp', varid(1))
 call netcdf_err(error, 'inquire majority_land_cover_igbp variable' )

error = nf90_get_var(ncid, varid(1), majority_land_cover)
 call netcdf_err(error, 'reading majority_land_cover variable' )

error = nf90_inq_varid(ncid, 'snow_fraction', varid(1))
 call netcdf_err(error, 'inquire snow_fraction variable' )

error = nf90_get_var(ncid, varid(1), snow_fraction)
 call netcdf_err(error, 'reading snow_fraction variable' )
    
error = nf90_inq_varid(ncid, 'water_fraction', varid(1))
 call netcdf_err(error, 'inquire water_fraction variable' )

error = nf90_get_var(ncid, varid(1), water_fraction)
 call netcdf_err(error, 'reading water_fraction variable' )
    
error = nf90_close(ncid)
 call netcdf_err(error, 'closing file: '//trim(modis_land_filename) )

!====================================
! read data file

allocate(percent_snow(idim_modis_length,jdim_modis_length))
allocate(albedo_quality(idim_modis_length,jdim_modis_length))

allocate(modis_bsa_sw(idim_modis_length,jdim_modis_length))
allocate(modis_bsa_nir(idim_modis_length,jdim_modis_length))
allocate(modis_bsa_vis(idim_modis_length,jdim_modis_length))

allocate(modis_wsa_sw(idim_modis_length,jdim_modis_length))
allocate(modis_wsa_nir(idim_modis_length,jdim_modis_length))
allocate(modis_wsa_vis(idim_modis_length,jdim_modis_length))

modis_filename = '/scratch2/NCEPDEV/land/data/evaluation/MODIS/albedo/orig/MCD43C3.A'//trim(datestring)//'.061.nc'

error = nf90_open(trim(modis_filename),nf90_nowrite, ncid)
  call netcdf_err(error, 'opening file: '//trim(modis_filename) )
    
error = nf90_inq_varid(ncid, 'Percent_Snow', varid(1))
 call netcdf_err(error, 'inquire percent_snow variable' )

error = nf90_get_var(ncid, varid(1), percent_snow)
 call netcdf_err(error, 'reading percent_snow variable' )
    
error = nf90_inq_varid(ncid, 'Albedo_Quality', varid(1))
 call netcdf_err(error, 'inquire albedo_quality variable' )

error = nf90_get_var(ncid, varid(1), albedo_quality)
 call netcdf_err(error, 'reading albedo_quality variable' )
    
! read black sky modis albedos

error = nf90_inq_varid(ncid, 'Albedo_BSA_shortwave', varid(1))
 call netcdf_err(error, 'inquire modis_bsa_sw variable' )

error = nf90_get_var(ncid, varid(1), modis_bsa_sw)
 call netcdf_err(error, 'reading modis_bsa_sw variable') 

error = nf90_inq_varid(ncid, 'Albedo_BSA_NIR', varid(1))
 call netcdf_err(error, 'inquire modis_bsa_nir variable' )

error = nf90_get_var(ncid, varid(1), modis_bsa_nir)
 call netcdf_err(error, 'reading modis_bsa_nir variable')

error = nf90_inq_varid(ncid, 'Albedo_BSA_VIS', varid(1))
 call netcdf_err(error, 'inquire modis_bsa_vis variable' )

error = nf90_get_var(ncid, varid(1), modis_bsa_vis)
 call netcdf_err(error, 'reading modis_bsa_vis variable')

!  read white sky modis alebdos

error = nf90_inq_varid(ncid, 'Albedo_WSA_shortwave', varid(1))
 call netcdf_err(error, 'inquire modis_wsa_sw variable' )

error = nf90_get_var(ncid, varid(1), modis_wsa_sw)
 call netcdf_err(error, 'reading modis_wsa_sw variable' )

error = nf90_inq_varid(ncid, 'Albedo_WSA_NIR', varid(1))
 call netcdf_err(error, 'inquire modis_wsa_nir variable' )

error = nf90_get_var(ncid, varid(1), modis_wsa_nir)
 call netcdf_err(error, 'reading modis_wsa_nir variable')

error = nf90_inq_varid(ncid, 'Albedo_WSA_VIS', varid(1))
 call netcdf_err(error, 'inquire modis_wsa_vis variable' )

error = nf90_get_var(ncid, varid(1), modis_wsa_vis)
 call netcdf_err(error, 'reading modis_wsa_vis variable')
    
error = nf90_close(ncid)
 call netcdf_err(error, 'closing file: '//trim(modis_filename) )

allocate(data_regrid(nlocations))
allocate(percent_good_data(nlocations))

!====================================
! write remapped albedo

output_filename = '/scratch2/NCEPDEV/land/data/evaluation/MODIS/albedo/C96/MCD43C3.A'//trim(daystring)//'.061.C96.nc'

error = nf90_create(output_filename, ior(nf90_netcdf4,nf90_classic_model), ncid)
 call netcdf_err(error, 'creating file='//trim(output_filename) )

! --- define location and time

error = nf90_def_dim(ncid, 'location', nlocations, dimid)
 call netcdf_err(error, 'defining location dimension' )
error = nf90_def_dim(ncid, 'time', NF90_UNLIMITED, dim_time)
  call netcdf_err(error, 'defining time dimension' )

error = nf90_def_var(ncid, 'time', nf90_double, dim_time, id_time)
  call netcdf_err(error, 'defining time' )
error = nf90_put_att(ncid, id_time, "long_name", "time")
  call netcdf_err(error, 'defining time long name' )
error = nf90_put_att(ncid, id_time, "units", "seconds since "//since_date)
  call netcdf_err(error, 'defining time units' )

!--- define longitude
error = nf90_def_var(ncid, 'lon', nf90_double, dimid, id_lon)
  call netcdf_err(error, 'defining lon' )
error = nf90_put_att(ncid, id_lon, "long_name", "longitude")
  call netcdf_err(error, 'defining lon long name' )

!--- define latitude
error = nf90_def_var(ncid, 'lat', nf90_double, dimid, id_lat)
  call netcdf_err(error, 'defining lat' )
error = nf90_put_att(ncid, id_lat, "long_name", "latitude")
  call netcdf_err(error, 'defining lat long name' )

!======================================
! white-sky albedo
! shortwave

error = nf90_def_var(ncid, 'albedo_wsa_shortwave', nf90_short, (/dimid, dim_time/), varid(1))
 call netcdf_err(error, 'defining albedo_wsa_shortwave' )

error = nf90_put_att(ncid, varid(1), "_FillValue", nodata_int)
 call netcdf_err(error, 'adding albedo_wsa_shortwave _FillValue' )

error = nf90_put_att(ncid, varid(1), "scale_factor", 0.001)
 call netcdf_err(error, 'adding albedo_wsa_shortwave _FillValue' )

error = nf90_def_var(ncid, 'percent_good_data', nf90_short, (/dimid, dim_time/), varid(2))
 call netcdf_err(error, 'defining percent_good_data_wsa_sw' )

error = nf90_put_att(ncid, varid(2), "_FillValue", nodata_int)
 call netcdf_err(error, 'adding percent_good_data_wsa_sw _FillValue' )

! percent of snow

error = nf90_def_var(ncid, 'percent_snow', nf90_short, (/dimid, dim_time/), varid(3))
 call netcdf_err(error, 'defining percent_snow' )

error = nf90_put_att(ncid, varid(3), "_FillValue", nodata_int)
 call netcdf_err(error, 'adding percent_snow _FillValue' )

! near infared

error = nf90_def_var(ncid, 'albedo_wsa_nir', nf90_short, (/dimid, dim_time/), varid(4))
 call netcdf_err(error, 'defining albedo_wsa_bir' )

error = nf90_put_att(ncid, varid(4), "_FillValue", nodata_int)
 call netcdf_err(error, 'adding albedo_wsa_nir _FillValue' )

error = nf90_put_att(ncid, varid(4), "scale_factor", 0.001)
 call netcdf_err(error, 'adding albedo_wsa_nir _FillValue' )

! visiable

error = nf90_def_var(ncid, 'albedo_wsa_vis', nf90_short, (/dimid, dim_time/), varid(5))
 call netcdf_err(error, 'defining albedo_wsa_vis' )

error = nf90_put_att(ncid, varid(5), "_FillValue", nodata_int)
 call netcdf_err(error, 'adding albedo_wsa_vis _FillValue' )

error = nf90_put_att(ncid, varid(5), "scale_factor", 0.001)
 call netcdf_err(error, 'adding albedo_wsa_vis _FillValue' )

!===============================================================
! black sky albedo
! short wave

error = nf90_def_var(ncid, 'albedo_bsa_shortwave', nf90_short, (/dimid, dim_time/), varid(6))
 call netcdf_err(error, 'defining albedo_bsa_shortwave' )

error = nf90_put_att(ncid, varid(6), "_FillValue", nodata_int)
 call netcdf_err(error, 'adding albedo_bsa_shortwave _FillValue' )

error = nf90_put_att(ncid, varid(6), "scale_factor", 0.001)
 call netcdf_err(error, 'adding albedo_bsa_shortwave _FillValue' )

! near infared

error = nf90_def_var(ncid, 'albedo_bsa_nir', nf90_short, (/dimid, dim_time/), varid(7))
 call netcdf_err(error, 'defining albedo_bsa_bir' )

error = nf90_put_att(ncid, varid(7), "_FillValue", nodata_int)
 call netcdf_err(error, 'adding albedo_bsa_nir _FillValue' )

error = nf90_put_att(ncid, varid(7), "scale_factor", 0.001)
 call netcdf_err(error, 'adding albedo_bsa_nir _FillValue' )

! visiable

error = nf90_def_var(ncid, 'albedo_bsa_vis', nf90_short, (/dimid, dim_time/), varid(8))
 call netcdf_err(error, 'defining albedo_bsa_vis' )

error = nf90_put_att(ncid, varid(8), "_FillValue", nodata_int)
 call netcdf_err(error, 'adding albedo_bsa_vis _FillValue' )

error = nf90_put_att(ncid, varid(8), "scale_factor", 0.001)
 call netcdf_err(error, 'adding albedo_bsa_vis _FillValue' )

error = nf90_enddef(ncid)
 call netcdf_err(error, 'defining output_filename' )

! --- put time, lat, lon data
error = nf90_put_var(ncid, id_time, sec_since)
    call netcdf_err(error, 'writing time record')

error = nf90_put_var(ncid, id_lon, longitude)
    call netcdf_err(error, 'writing lon record')

error = nf90_put_var(ncid, id_lat, latitude)
  call netcdf_err(error, 'writing lat record')

call average_calAlbedo(idim_modis_length, jdim_modis_length, nlocations, mapping_location, modis_wsa_sw, majority_land_cover, vegetation_category, water_fraction, albedo_quality, data_regrid)

error = nf90_put_var(ncid, varid(1), data_regrid, start = (/1,1/), count = (/nlocations,1/))
 call netcdf_err(error, 'writing wsa_sw_regrid variable')

call average_calGoodpoint(idim_modis_length, jdim_modis_length, nlocations, mapping_location, modis_wsa_sw, majority_land_cover, vegetation_category, water_fraction, albedo_quality, percent_good_data)

error = nf90_put_var(ncid, varid(2), percent_good_data, start = (/1,1/), count = (/nlocations,1/))
 call netcdf_err(error, 'writing percent_good_data_wsa_sw variable')

call average_calAlbedo(idim_modis_length, jdim_modis_length, nlocations, mapping_location, percent_snow, majority_land_cover, vegetation_category, water_fraction, albedo_quality, data_regrid)

error = nf90_put_var(ncid, varid(3), data_regrid, start = (/1,1/), count = (/nlocations,1/))
 call netcdf_err(error, 'writing percent_snow_regrid variable')

call average_calAlbedo(idim_modis_length, jdim_modis_length, nlocations, mapping_location, modis_wsa_nir, majority_land_cover, vegetation_category, water_fraction, albedo_quality, data_regrid)

error = nf90_put_var(ncid, varid(4), data_regrid, start = (/1,1/), count = (/nlocations,1/))
 call netcdf_err(error, 'writing wsa_nir_regrid variable')

call average_calAlbedo(idim_modis_length, jdim_modis_length, nlocations, mapping_location, modis_wsa_vis, majority_land_cover, vegetation_category, water_fraction, albedo_quality, data_regrid)

error = nf90_put_var(ncid, varid(5), data_regrid, start = (/1,1/), count = (/nlocations,1/))
 call netcdf_err(error, 'writing wsa_vis_regrid variable')

call average_calAlbedo(idim_modis_length, jdim_modis_length, nlocations, mapping_location, modis_bsa_sw, majority_land_cover, vegetation_category, water_fraction, albedo_quality, data_regrid)

error = nf90_put_var(ncid, varid(6), data_regrid, start = (/1,1/), count = (/nlocations,1/))
 call netcdf_err(error, 'writing bsa_sw_regrid variable')

call average_calAlbedo(idim_modis_length, jdim_modis_length, nlocations, mapping_location, modis_bsa_nir, majority_land_cover, vegetation_category, water_fraction, albedo_quality, data_regrid)

error = nf90_put_var(ncid, varid(7), data_regrid, start = (/1,1/), count = (/nlocations,1/))
 call netcdf_err(error, 'writing bsa_nir_regrid variable')

call average_calAlbedo(idim_modis_length, jdim_modis_length, nlocations, mapping_location, modis_bsa_vis, majority_land_cover, vegetation_category, water_fraction, albedo_quality, data_regrid)

error = nf90_put_var(ncid, varid(8), data_regrid, start = (/1,1/), count = (/nlocations,1/))
 call netcdf_err(error, 'writing bsa_vis_regrid variable')

error = nf90_close(ncid)
 call netcdf_err(error, 'closing output_filename')

end program


subroutine average_calAlbedo(idim_modis_length, jdim_modis_length, nlocations, mapping_location, modis_wsa_sw, majority_land_cover, vegetation_category, water_fraction, albedo_quality, data_regrid)

!--------------------------------------------------------------
! computing averaged albedo values  for a given FV3 Cxx grid
!-------------------------------------------------------------

implicit none

integer :: nlocations, idim_modis_length, jdim_modis_length
integer  , parameter ::  quality_limit = 2  ! will use data > this value
integer*2, parameter ::  water_threshold = 0 ! remove water grids > this value

integer*2, dimension(idim_modis_length,jdim_modis_length) :: modis_wsa_sw
integer*2, dimension(idim_modis_length,jdim_modis_length) :: albedo_quality
integer*2, dimension(idim_modis_length,jdim_modis_length) :: water_fraction
integer*2, dimension(idim_modis_length,jdim_modis_length) :: majority_land_cover
integer*2, dimension(nlocations)                          :: data_regrid

integer  , dimension(idim_modis_length,jdim_modis_length) :: mapping_location
integer  , dimension(nlocations)                          :: vegetation_category
integer  , dimension(nlocations)                          :: land_grids
real     , dimension(nlocations)                          :: wsa_sw_regrid  

integer ::  modis_i, modis_j, location
integer :: land_check
integer*2, parameter ::  nodata_int = -32767

wsa_sw_regrid = 0.0
land_grids = 0

do modis_i = 1, idim_modis_length
do modis_j = 1, jdim_modis_length

  location = mapping_location(modis_i,modis_j)

  if(location > 0) then

    land_check = 0
 
! only use vegetation types that agree
    if(majority_land_cover(modis_i,modis_j) == vegetation_category(location)) &
      land_check = 1

! remove water grids above a threshold
    if(water_fraction(modis_i,modis_j) > water_threshold) land_check = 0

! remove permanent snow grids 
    if(vegetation_category(location) == 15) land_check = 0

! remove lower quality observations below (keep data with quality flag 0,1,2) 
    if(albedo_quality(modis_i,modis_j) > quality_limit) land_check = 0

! confirm removal of missing data 
    if(modis_wsa_sw(modis_i,modis_j) < 0) land_check = 0

    if(land_check == 1) then

      land_grids(location) = land_grids(location) + 1
      wsa_sw_regrid(location)  =  wsa_sw_regrid(location) +  modis_wsa_sw(modis_i,modis_j)

    end if ! land_check == 1

  end if ! location > 0

end do
end do

where(land_grids >  0) wsa_sw_regrid = wsa_sw_regrid/land_grids

data_regrid = nint(wsa_sw_regrid)
where(land_grids == 0) data_regrid = nodata_int

return

end subroutine average_calAlbedo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculate percent of good data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine average_calGoodpoint(idim_modis_length, jdim_modis_length, nlocations, mapping_location, modis_wsa_sw, majority_land_cover, vegetation_category, water_fraction, albedo_quality, percent_good_data_regrid)

!--------------------------------------------------------------
! computing  percentage of good data points in total modis 
! points for a given FV3 Cxx grid
!-------------------------------------------------------------

implicit none

integer :: nlocations, idim_modis_length, jdim_modis_length
integer  , parameter ::   quality_limit = 2  ! will use data > this value
integer*2, parameter :: water_threshold = 0 ! remove water grids > this value

integer*2, dimension(idim_modis_length,jdim_modis_length) :: modis_wsa_sw
integer*2, dimension(idim_modis_length,jdim_modis_length) :: albedo_quality
integer*2, dimension(idim_modis_length,jdim_modis_length) :: water_fraction
integer*2, dimension(idim_modis_length,jdim_modis_length) :: majority_land_cover
integer*2, dimension(nlocations)                          :: percent_good_data_regrid

integer  , dimension(idim_modis_length,jdim_modis_length) :: mapping_location
integer  , dimension(nlocations)                          :: vegetation_category
integer  , dimension(nlocations)                          :: land_grids
integer  , dimension(nlocations)                          :: land_grids_total
real     , dimension(nlocations)                          :: percent_good_data

integer ::  modis_i, modis_j, location
integer :: land_check
integer*2, parameter ::  nodata_int = -32767

land_grids = 0
land_grids_total = 0
percent_good_data = 0.0

do modis_i = 1, idim_modis_length
do modis_j = 1, jdim_modis_length

  location = mapping_location(modis_i,modis_j)

  if(location > 0) then

    land_check = 0
! only use vegetation types that agree
    if(majority_land_cover(modis_i,modis_j) == vegetation_category(location)) &
      land_check = 1

! remove water grids above a threshold
    if(water_fraction(modis_i,modis_j) > water_threshold) land_check = 0

! remove permanent snow grids
    if(vegetation_category(location) == 15) land_check = 0

! remove lower quality observations below (keep data with quality flag 0,1,2)
    if(albedo_quality(modis_i,modis_j) > quality_limit) land_check = 0

! confirm removal of missing data
    if(modis_wsa_sw(modis_i,modis_j) < 0) land_check = 0

    if(land_check == 1) then

      land_grids(location) = land_grids(location) + 1

    end if ! land_check == 1

! ccount all modis points in a given FV3 grid

    if(land_check == 1 .or. land_check == 0) then

      land_grids_total(location) = land_grids_total(location) + 1

    end if ! land_check =1 or 0


  end if ! location > 0

end do
end do

where(land_grids >  0) percent_good_data = 100.0*real(land_grids)/real(land_grids_total)

percent_good_data_regrid = nint(percent_good_data)
where(land_grids == 0) percent_good_data_regrid = nodata_int

return

end subroutine average_calGoodpoint


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculate time in seconds
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine calc_sec_since(since_date, current_date, offset_ss, sec_since)

! calculate number of seconds between since_date and current_date

double precision      :: sec_since
character*19 :: since_date, current_date  ! format: yyyy-mm-dd hh:nn:ss
integer      :: offset_ss
integer      :: since_yyyy, since_mm, since_dd, since_hh, since_nn, since_ss
integer      :: current_yyyy, current_mm, current_dd, current_hh, current_nn, current_ss
logical      :: leap_year = .false.
integer      :: iyyyy, imm
integer, dimension(12), parameter :: days_in_mm = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

  sec_since = 0

  read(since_date( 1: 4),  '(i4)') since_yyyy
  read(since_date( 6: 7),  '(i2)') since_mm
  read(since_date( 9:10),  '(i2)') since_dd
  read(since_date(12:13),  '(i2)') since_hh
  read(since_date(15:16),  '(i2)') since_nn
  read(since_date(18:19),  '(i2)') since_ss

  read(current_date( 1: 4),  '(i4)') current_yyyy
  read(current_date( 6: 7),  '(i2)') current_mm
  read(current_date( 9:10),  '(i2)') current_dd
  read(current_date(12:13),  '(i2)') current_hh
  read(current_date(15:16),  '(i2)') current_nn
  read(current_date(18:19),  '(i2)') current_ss

! not worrying about the complexity of non-recent leap years
! calculate number of seconds in all years
  do iyyyy = since_yyyy, current_yyyy
    if(mod(iyyyy,4) == 0) then
      sec_since = sec_since + 366*86400
    else
      sec_since = sec_since + 365*86400
    end if
  end do

! remove seconds from since_year
  if(mod(since_yyyy,4) == 0) leap_year = .true.

  do imm = 1,since_mm-1
    sec_since = sec_since - days_in_mm(imm)*86400
  end do

  if(leap_year .and. since_mm > 2) sec_since = sec_since - 86400

  sec_since = sec_since - (since_dd - 1) * 86400

  sec_since = sec_since - (since_hh) * 3600

  sec_since = sec_since - (since_nn) * 60

  sec_since = sec_since - (since_ss)

! remove seconds in current_year

  leap_year = .false.
  if(mod(current_yyyy,4) == 0) leap_year = .true.

  do imm = current_mm+1, 12
    sec_since = sec_since - days_in_mm(imm)*86400
  end do
  if(leap_year .and. current_mm < 3) sec_since = sec_since - 86400

  sec_since = sec_since - (days_in_mm(current_mm) - current_dd) * 86400

  sec_since = sec_since - (23 - current_hh) * 3600

  sec_since = sec_since - (59 - current_nn) * 60

  sec_since = sec_since - (60 - current_ss)

  sec_since = sec_since + offset_ss

end subroutine calc_sec_since

subroutine netcdf_err( err, string )
    
!--------------------------------------------------------------
! if a netcdf call returns an error, print out a message
! and stop processing.
!--------------------------------------------------------------
    
use netcdf

implicit none
    
integer, intent(in) :: err
character(len=*), intent(in) :: string
character(len=80) :: errmsg
    
if( err == nf90_noerr )return
errmsg = nf90_strerror(err)
print*,''
print*,'fatal error: ', trim(string), ': ', trim(errmsg)
print*,'stop.' 
stop 10

return

end subroutine netcdf_err
 
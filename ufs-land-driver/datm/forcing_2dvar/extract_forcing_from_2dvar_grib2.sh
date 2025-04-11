#!/bin/sh
# on hera:

export inputdir=/scratch2/NCEPDEV/stmp3/Youlong.Xia/fromHPSS/snow2dvar

# first step - use previous 18z 6-hour fcst to create current day 00z data
analdate1=2021093018
first_date=2021100100

YYYY=`expr $analdate1 | cut -c1-4`
MM=`expr $analdate1 | cut -c5-6`
DD=`expr $analdate1 | cut -c7-8`
HH=`expr $analdate1 | cut -c9-10`
cp $inputdir/gdas.${YYYY}${MM}${DD}/${HH}/model/atmos/master/gdas.t${HH}z.sfluxgrbf006.grib2 gdas.${analdate1}.t${HH}z.sfluxgrbf006.grib2
ifile=gdas.${analdate1}.t${HH}z.sfluxgrbf006.grib2
ofile_tmp=snow2dvar_forcing_${first_date}.grib2

wgrib2 -s ${ifile} | grep ":TMP:2 m" | wgrib2 -i ${ifile} -append -grib  ${ofile_tmp}
wgrib2 -s ${ifile} | grep ":SPFH:2 m" | wgrib2 -i ${ifile} -append -grib  ${ofile_tmp}
wgrib2 -s ${ifile} | grep ":DSWRF:surface:6" | wgrib2 -i ${ifile} -append  -grib ${ofile_tmp}
wgrib2 -s ${ifile} | grep ":DLWRF:surface:6" | wgrib2 -i ${ifile} -append  -grib ${ofile_tmp}
wgrib2 -s ${ifile} | grep ":UGRD:10 m" | wgrib2 -i ${ifile} -append  -grib  ${ofile_tmp}
wgrib2 -s ${ifile} | grep ":VGRD:10 m" | wgrib2 -i ${ifile} -append  -grib  ${ofile_tmp}
wgrib2 -s ${ifile} | grep ":PRES:sur" | wgrib2 -i ${ifile}  -append -grib  ${ofile_tmp}
wgrib2 -s ${ifile} | grep ":PRATE:sur" | wgrib2 -i ${ifile} -append -grib ${ofile_tmp}

wgrib2 ${ofile_tmp} -netcdf snow2dvar_forcing_${first_date}.nc
rm -rf ${ifile}
rm -rf ${ofile_tmp}

analdate=2021100100

while [ $analdate -le 2021103118 ];do
   YYYY=`expr $analdate | cut -c1-4`
   MM=`expr $analdate | cut -c5-6`
   DD=`expr $analdate | cut -c7-8`
   HH=`expr $analdate | cut -c9-10`
   # get valid date for fhr06 files
   date1=`./incdate.sh $analdate 6`
   # get valid date for fhr03 files
   date2=`./incdate.sh $analdate 3`
   cp $inputdir/gdas.${YYYY}${MM}${DD}/${HH}/model/atmos/master/gdas.t${HH}z.sfluxgrbf006.grib2 gdas.${analdate}.t${HH}z.sfluxgrbf006.grib2
   ifile=gdas.${analdate}.t${HH}z.sfluxgrbf006.grib2
   ofile_tmp=snow2dvar_forcing_${date1}.grib2 

   wgrib2 -s ${ifile} | grep ":TMP:2 m" | wgrib2 -i ${ifile} -append -grib  ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":SPFH:2 m" | wgrib2 -i ${ifile} -append -grib  ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":DSWRF:surface:6" | wgrib2 -i ${ifile} -append  -grib ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":DLWRF:surface:6" | wgrib2 -i ${ifile} -append  -grib ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":UGRD:10 m" | wgrib2 -i ${ifile} -append  -grib  ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":VGRD:10 m" | wgrib2 -i ${ifile} -append  -grib  ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":PRES:sur" | wgrib2 -i ${ifile}  -append -grib  ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":PRATE:sur" | wgrib2 -i ${ifile} -append -grib ${ofile_tmp}
   
   wgrib2 ${ofile_tmp} -netcdf snow2dvar_forcing_${date1}.nc

   ncks -v PRATE_surface snow2dvar_forcing_${date1}.nc PRATE_${date1}.nc   

   if [ $? -eq 0 ]; then
      rm -rf ${ifile}
      rm -rf ${ofile_tmp}
   fi

   cp $inputdir/gdas.${YYYY}${MM}${DD}/${HH}/model/atmos/master/gdas.t${HH}z.sfluxgrbf003.grib2 gdas.${analdate}.t${HH}z.sfluxgrbf003.grib2
   ifile=gdas.${analdate}.t${HH}z.sfluxgrbf003.grib2   
   ofile_tmp=snow2dvar_forcing_${date2}.grib2

   wgrib2 -s ${ifile} | grep ":TMP:2 m" | wgrib2 -i ${ifile} -append -grib  ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":SPFH:2 m" | wgrib2 -i ${ifile} -append -grib  ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":DSWRF:surface:3" | wgrib2 -i ${ifile} -append  -grib ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":DLWRF:surface:3" | wgrib2 -i ${ifile} -append  -grib ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":UGRD:10 m" | wgrib2 -i ${ifile} -append  -grib  ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":VGRD:10 m" | wgrib2 -i ${ifile} -append  -grib  ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":PRES:sur" | wgrib2 -i ${ifile}  -append -grib  ${ofile_tmp}
   wgrib2 -s ${ifile} | grep ":PRATE:sur" | wgrib2 -i ${ifile} -append -grib ${ofile_tmp}

   wgrib2 ${ofile_tmp} -netcdf snow2dvar_forcing_${date2}.nc
 
   ncks -v time snow2dvar_forcing_${date2}.nc time_${date1}.nc
   ncks -A -v PRATE_surface PRATE_${date1}.nc snow2dvar_forcing_${date2}.nc
   ncks -A -v time time_${date1}.nc snow2dvar_forcing_${date2}.nc

   if [ $? -eq 0 ]; then
      rm -rf ${ifile}
      rm -rf ${ofile_tmp}
      rm -rf PRATE_${date1}.nc
      rm -rf time_${date1}.nc
   fi
   
   analdate=`./incdate.sh $analdate 6`
done

cdo mergetime snow2dvar_forcing_${YYYY}${MM}*.nc combine_forcing_${YYYY}${MM}.nc
rm -rf  snow2dvar_forcing_${YYYY}${MM}*.nc

exit

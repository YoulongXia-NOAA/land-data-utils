#!/bin/sh -l
#
# -- Specify queue
##SBATCH -q debug
#SBATCH -q batch
#
# -- Specify under which account a job should run
#SBATCH --account=fv3-cpu
#
# -- Set the name of the job, or Slurm will default to the name of the script
#SBATCH --job-name=create_datm_files
#
# -- Tell the batch system to set the working directory to the current working directory
#SBATCH --chdir=.
#
# -- Request tasks, this should correspond to the number of lines in your regrid-tasks file
##SBATCH --ntasks=40
#SBATCH --ntasks=32
#
# -- Specify a maximum wallclock
# -- C96  : ~45 minutes
# -- C192 : ~45 minutes
# -- C384 : ~2 hours
# -- C768 : ~2 hours
# -- C1152: ~3 hours
#
#SBATCH --time=1:30:00

module purge
module load ncl/6.6.2

atm_res="C96"
ocn_res="mx100"
grid_version="hr3"
grid_extent="global"
datm_source="GDAS"
datm_source_path="/scratch1/NCEPDEV/stmp2/Michael.Barlage/forcing/gdas/orig/"
elevation_source_filename="/scratch2/NCEPDEV/land/data/ufs-land-driver/datm/GDAS/elevation/gfs.orography.t1534.nc"
static_file_path="/scratch2/NCEPDEV/land/data/ufs-land-driver/vector_inputs/"
weights_path="/scratch2/NCEPDEV/land/data/ufs-land-driver/weights/"
interpolation_method="bilinear"
regrid_tasks_file="regrid-tasks.2015-2022.GDAS.modified"

#################################################################################
#  shouldn't need to modify anything below
#################################################################################

# create weights filename and check if it exists

if [ -e $elevation_source_filename ]; then 
  echo "using elevation_source_filename:"$elevation_source_filename
else
  echo "ERROR: elevation_source_filename does not exist"
  exit 1
fi

# the default location for output files is $atm_res.$ocn_res

if [ $grid_extent = "global" ]; then 
  res=$atm_res.$ocn_res
else
  res=$atm_res.$ocn_res.$grid_extent
fi

grid=$res"_hr3"
output_path=$res"/"

if [ -d $output_path ]; then 
  echo "BEWARE: output_path directory exists and overwriting is allowed"
else
  echo "creating directory: "$output_path
  mkdir -p $output_path
fi

# create weights filename and check if it exists

weights_filename=$weights_path$output_path$datm_source"-"$grid"_"$interpolation_method"_wts.nc"

if [ -e $weights_filename ]; then 
  echo "using weights_filename:"$weights_filename
else
  echo "ERROR: weights_filename does not exist: "$weights_filename
  exit 2
fi

# create static filename and check if it exists

static_filename=$static_file_path$output_path"ufs-land_"$grid"_static_fields.nc"

if [ -e $static_filename ]; then 
  echo "using static_filename:"$static_filename
else
  echo "ERROR: static_filename does not exist: "$static_filename
  exit 3
fi

# create elevation filename

elevation_filename="elevation_"$datm_source"_"$grid".nc"

echo "creating elevation_filename:"$elevation_filename

# create elevation difference file

cmdparm="'static_filename="\"$static_filename"\"' "
cmdparm=$cmdparm"'elevation_source_filename="\"$elevation_source_filename"\"' "
cmdparm=$cmdparm"'datm_source="\"$datm_source"\"' "
cmdparm=$cmdparm"'weights_filename="\"$weights_filename"\"' "
cmdparm=$cmdparm"'elevation_filename="\"$elevation_filename"\"' "

echo "variable list sent to elevation creating NCL script"
echo $cmdparm

eval "time ncl create_vector_elevation.ncl $cmdparm"

# regrid the source data atmosphere

echo "Creating datm files"

echo "elevation_filename = $elevation_filename" > regrid_parameter_assignment
echo "weights_filename = $weights_filename" >> regrid_parameter_assignment
echo "datm_source_path = $datm_source_path" >> regrid_parameter_assignment
echo "output_preamble = "$output_path$datm_source"-"$grid >> regrid_parameter_assignment

srun -l --multi-prog $regrid_tasks_file

rm -f $elevation_filename
rm -f regrid_parameter_assignment


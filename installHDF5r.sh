#!/bin/bash
wget https://cran.r-project.org/src/contrib/hdf5r_1.0.0.tar.gz

module purge
module load gcc/6.1 openmpi/1.10.2-gcc6.1.0 hdf5-parallel/1.8.18-gcc6.1.0 zlib/1.2.11-gcc

export HDF5_ROOT="/cxfs/projects/spack/opt/spack/linux-scientific6-x86_64/gcc-6.1.0/hdf5-1.8.18-5d7vrctlkalwf2bga4xrltdokbxvntb4"

R CMD INSTALL hdf5r_1.0.0.tar.gz --configure-args="--with-hdf5=$HDF5_ROOT/bin/h5pcc"

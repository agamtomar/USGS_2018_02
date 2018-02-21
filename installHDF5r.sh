#!/bin/bash
wget https://cran.r-project.org/src/contrib/hdf5r_1.0.0.tar.gz

module purge
module load gcc/6.1  hdf5-serial/1.8.18-gcc6.1.0 zlib/1.2.11-gcc

R CMD INSTALL hdf5r_1.0.0.tar.gz

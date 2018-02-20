#!/bin/bash

wget https://cran.r-project.org/src/contrib/pbdMPI_0.3-4.tar.gz
wget https://cran.r-project.org/src/contrib/rlecuyer_0.3-4.tar.gz

module purge
module load openmpi/1.10.2-gcc6.1.0 gcc/6.1

export OPENMPI_ROOT=/cxfs/projects/spack/opt/spack/linux-scientific6-x86_64/gcc-6.1.0/openmpi-1.10.2-l7qolpj3spxhxcrpotarz5dncdjt3bip

R CMD INSTALL rlecuyer_0.3-4.tar.gz
R CMD INSTALL pbdMPI_0.3-4.tar.gz --configure-args="--with-mpi-type=OPENMPI \
--with-mpi-include=$OPENMPI_ROOT/include --with-mpi-libpath=$OPENMPI_ROOT/lib" --no-test-load

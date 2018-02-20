#!/bin/bash
#SBATCH --nodes=1                    		# Number of requested nodes
#SBATCH --time=0:10:00               		# Max wall time
#SBATCH --partition=normal             		# Specify Summit haswell nodes
#SBATCH --ntasks-per-node=20     	        # Number of tasks per job
#SBATCH --job-name=scaling_tutorial                    # Job submission name
#SBATCH --output=scaling.%j.out               # Output file name with Job ID
#SBATCH -A training

# purge all existing modules
module purge

# load the compiler and mpi
module load intel/psxe-2018u1

# run the code (C/Fortran)
mpiexec -np 2 ./mpi_messages.out

# run the code (Python)
#mpiexec -np 2 python ./mpi_messages.out

# run the code (R)
#mpiexec -np 2 R ./mpi_messages.out

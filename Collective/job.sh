#!/bin/bash
#SBATCH --nodes=1                    		# Number of requested nodes
#SBATCH --time=0:10:00               		# Max wall time
#SBATCH --partition=normal             		# Specify Summit haswell nodes
#SBATCH --ntasks-per-node=10     	        # Number of tasks per job
#SBATCH --job-name=scaling_tutorial                    # Job submission name
#SBATCH --output=scaling.%j.out               # Output file name with Job ID
#SBATCH -A training
#SBATCH --reservation=training

# purge all existing modules
module purge

# load the compiler and mpi
module load intel/psxe-2018u1

# run the code (C/Fortran)
srun --mpi=pmi2 -n 10 Examples/mpi_barrier.fexe

# or load python
module purge
module load python/pPython3
# run the code (Python)
echo "Python"
srun --mpi=pmi2 -n 10 python Examples/mpi_barrier.py

# or load R
module purge
module load openmpi/1.10.2-gcc6.1.0 gcc/6.1 zlib/1.2.11-gcc
# run the code (R)
echo "R"
srun --mpi=pmi2 -n 10 Rscript Examples/mpi_barrier.R


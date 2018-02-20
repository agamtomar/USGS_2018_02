#!/bin/bash
#SBATCH --nodes=4                    		# Number of requested nodes
#SBATCH --time=0:10:00               		# Max wall time
#SBATCH --partition=normal             		# Specify Summit haswell nodes
#SBATCH --ntasks-per-node=20     	        # Number of tasks per job
#SBATCH --job-name=scaling_tutorial                    # Job submission name
#SBATCH --output=scaling.%j.out               # Output file name with Job ID
#SBATCH -A training
#SBATCH --reservation=training

# purge all existing modules
module purge

# load the compiler and mpi
module load intel/psxe-2018u1


#
bash scaling_script.sh

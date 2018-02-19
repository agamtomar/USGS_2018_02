#!/bin/bash
#SBATCH --nodes=2                    		# Number of requested nodes
#SBATCH --time=0:05:00               		# Max wall time
#SBATCH --partition=normal             		# Specify Summit haswell nodes
#SBATCH --ntasks-per-node=24     	        # Number of tasks per job
#SBATCH --job-name=scaling_tutorial                    # Job submission name
#SBATCH --output=scaling.%j.out               # Output file name with Job ID

# purge all existing modules
module purge

# load the compiler and mpi
module load intel/16.0.3

cd

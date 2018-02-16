#!/bin/bash
#SBATCH --nodes=2                    		# Number of requested nodes
#SBATCH --time=0:05:00               		# Max wall time
#SBATCH --qos=debug                  		# Specify debug QOS
#SBATCH --partition=shas             		# Specify Summit haswell nodes
#SBATCH --ntasks=48          	 	        # Number of tasks per job
#SBATCH --job-name=mpitest                  # Job submission name
#SBATCH --output=%j.out            # Output file name with Job ID


# Written by:	Nick Featherstone
# Date:		17 May 2017

# purge all existing modules
module purge

# load the relevant compiler & mpi modules
module load intel
module load impi


mpirun -np 48 ./mpi_hello.out


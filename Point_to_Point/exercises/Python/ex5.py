from mpi4py import MPI
import sys
import numpy
import random
import os

###################################################
# Fill in the body of this custom allreduce function
# (use a series of calls to Send and Recv)
def my_allreduce(valin, rop):
    """ Performs a reduction on valin across all processes in MPI_COMM_WORLD.
         Reduced value is stored in valout
         Reduction operation is controlled by rop:
         rop = MPI_SUM => Summation
         rop = MPI_MIN => Min
         rop = MPI_MAX => Max """
    valout = 0
    return valout


###########################
# Initialize Communication
cw = MPI.COMM_WORLD
rank = cw.rank
size = cw.size
proc = MPI.Get_processor_name()

#############################################
# Read problem parameters from command line

num = numpy.zeros(1,dtype='i')
if (rank == 0):
    try:
        num[0] = int(sys.argv[2])
    except:
        num[0] = 1


cw.Bcast(num,root=0)
op = num[0]

if (op > 3):
    op = 1
if (op < 1):
    op = 1

num[0] = rank

ans = my_allreduce(num,op)

print('my_rank = '+str(rank)+' ; reduced value = ',ans)


MPI.Finalize()

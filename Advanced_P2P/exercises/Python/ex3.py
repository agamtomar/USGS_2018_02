from mpi4py import MPI
import sys
import numpy
import random
import os

###################################################
# Fill in the body of this custom allreduce function
# (use a series of calls to Send and Recv)

# There are many more clever ways to do this.  Here, we take a
# straightforward approach and use logic similar to our earlier
# deadlock exercise (exercise 3)

def my_allreduce(valin, rop):
    """ Performs a reduction on valin across all processes in MPI_COMM_WORLD.
         Reduced value is stored in valout
         Reduction operation is controlled by rop:
         rop = MPI_SUM => Summation
         rop = MPI_MIN => Min
         rop = MPI_MAX => Max """


    sval = numpy.zeros(1,dtype='d')
    rval = numpy.zeros(1,dtype='d')

    left = rank -1
    right = rank +1
    if ( right > size-1):
        right  = 0
    if ( left < 0 ):
         left = size-1    

    parity = rank%2

    valout = valin
    sval[0] = valin
    for i in range(1,size):
        mtag = i
        if (parity == 0):

            cw.Send(sval,dest=right,tag=mtag)

            cw.Recv(rval, source=left,tag=mtag)
        else:

            cw.Recv(rval, source=left,tag=mtag)

            cw.Send(sval,dest=right,tag=mtag)

        if (rop == 1):
            valout = valout+rval[0]
            sval[0] = rval[0]
        if (rop == 2):
            if (rval[0] < valout):
                valout = rval[0]
            sval[0] = valout
        if (rop == 3):
            if (rval[0] > valout):
                valout = rval[0]
            sval[0] = valout

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


ans = my_allreduce(rank,op)

print('my_rank = '+str(rank)+' ; reduced value = ',ans)


MPI.Finalize()

from mpi4py import MPI
import sys
import numpy
import random
import os


# When this program works properly, processes will print
# my_rank = 0; buffer = 0 0 0 2 2 2
# my_rank = 1; buffer = 0 1 1 1 1 1
# my_rank = 2; buffer = 2 1 1 2 2 2
# Depending on your python flush settings, the ordering of the three
# lines above might change, but buffer values and corresponding rank
# should remain unchanged.


def report_buffer():
    for i in range(size):
        if (i == rank):
            print('my_rank = ', rank, ' my_buffer = ', mybuffer)
        cw.Barrier()




###########################
# Initialize Communication
cw = MPI.COMM_WORLD
rank = cw.rank
size = cw.size
proc = MPI.Get_processor_name()


#######################
# Initialize data 
mybuffer=numpy.zeros(6,dtype='i')
mybuffer[:] = rank

if (rank == 0):
    rnums = mybuffer[3:6]  # rnums is a view into mybuffer (not a copy)
    cw.Recv(rnums, source=2,tag=2)

    snums = mybuffer[0:1]
    cw.Send(snums,dest=1, tag=1)


if (rank == 1):
    rnums = mybuffer[0:1]
    cw.Recv(rnums,source=0, tag=1)

    snums = mybuffer[1:3]
    cw.Send(snums,dest=2,tag=1)

if (rank == 2):
    snums = mybuffer[3:6]
    cw.Send(snums,dest=0,tag=2)

    rnums = mybuffer[1:3]
    cw.Recv(rnums,source=1,tag=2)

report_buffer()




MPI.Finalize()

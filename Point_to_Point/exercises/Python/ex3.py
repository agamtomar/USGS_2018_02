#/////////////////////////////////////////
#   Token-Passing Deadlock Exercise
#  
#   Calling sequence:
#       mpiexec -np X python ex3.py -N Y
#       where Y indicates the number of integers sent during each pass
#       
#   Results:
#       When the program completes successfully, each process will possess a buffer containing
#       num_proc*Y values, with the value of each mpi rank occurring Y times.'
#
#   Example 1:
#           mpiexec -np 3 python ex3.py -N 1 
#               gives
#           my_rank = 0; buffer =  0 2 1
#           my_rank = 1; buffer =  1 0 2
#           my_rank = 2; buffer =  2 1 0
#       
#   Example 2:
#           mpiexec -np 4 python ex3.py -N 2
#               gives
#           my_rank = 0; buffer =  0 0 3 3 2 2 1 1
#           my_rank = 1; buffer =  1 1 0 0 3 3 2 2
#           my_rank = 2; buffer =  2 2 1 1 0 0 3 3
#           my_rank = 3; buffer =  3 3 2 2 1 1 0 0
#
#   NOTE:  depending on your bufferflush settings, the lines above may appear out of order.

from mpi4py import MPI
import sys
import numpy
import random
import os


###########################
# Initialize Communication
cw = MPI.COMM_WORLD
rank = cw.rank
size = cw.size
proc = MPI.Get_processor_name()

#############################################
# Read problem parameters from command line
ofile='tmp.dat'
n = numpy.zeros(1,dtype='i')
if (rank == 0):
    try:
        n[0] = int(sys.argv[2])
    except:
        n[0] = 1


cw.Bcast(n,root=0)
ofile = 'Numbers_N'+str(n[0])
num = n[0]

#######################
# Initialize data 
mybuffer=numpy.zeros(num*size,dtype='i')
mybuffer[:] = rank

left = rank -1
right = rank +1
if ( right > size-1):
    right  = 0
if ( left < 0 ):
     left = size-1





############################################################
# Communicate, perform calculation on rank 0, and record results


offset = 0
for i in range(1,size):
    mtag = i
    svals = mybuffer[offset:offset+num]
    cw.Send(svals,dest=right,tag=mtag)
    offset = offset+num
    rvals = mybuffer[offset:offset+num]
    cw.Recv(rvals, source=left,tag=mtag)

print('Buffer sum is: ', numpy.sum(mybuffer))

MPI.Finalize()

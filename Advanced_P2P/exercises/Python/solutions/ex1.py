#/////////////////////////////////////////
#   Collatz Sequence Computation
#
#   Records the Collatz Sequence lengths for all numbers in the range [1,N] 
#   with sequence lengths greater than the threshold T.  
#
#   Calling example:
#       mpiexec -np 10 python ex3.py -N 100 -T 15   (records sequence lengths greater than 15 in range [1,100])
#
#   The calculation of sequence lengths is carried out in parallel, and 
#   results are communicated to process 0.

from mpi4py import MPI
import sys
import numpy
import random
import os

def collatz_length(n):
    length = 1
    i = n
    while (i > 1):
        length += 1
        imod = i % 2
        if (imod == 0):
            i = i//2
        else:
            i = 3*i + 1
    return length



###########################
# Initialize Communication
cw = MPI.COMM_WORLD
rank = cw.rank
size = cw.size
proc = MPI.Get_processor_name()

#############################################
# Read problem parameters from command line
ofile='tmp.dat'
pars = numpy.zeros(2,dtype='i')
if (rank == 0):
    try:
        pars[0] = int(sys.argv[2])
    except:
        pars[0] = 1
    try:
        pars[1] = int(sys.argv[4])
    except:
        pars[1] = 0

cw.Bcast(pars,root=0)

num = pars[0]
threshold = pars[1]



##############################################################
# Split up the workload
# Each process examines numbers in the range [my_min, my_max]

dnum = num//size
modcheck = num % size
my_min = rank*dnum+1
if (rank < modcheck):
    my_min = my_min+rank
    my_max = my_min+dnum
else:
    my_min = my_min+modcheck
    my_max = my_min+dnum-1

###################################################
# Calculate the sequence lengths, recording those numbers
# with a sequence length greater than threshold
lengths = []
icount=0
for i in range(my_min,my_max+1):
    length = collatz_length(i)
    if (length > threshold):
        icount+=1
        lengths.append([i,length])

#create numpy array to hold our values
if (icount > 0):
    seq_info = numpy.zeros((2,icount),dtype='i',order='F')

    for i in range(icount):
        seq_info[0,i] = lengths[i][0]
        seq_info[1,i] = lengths[i][1]


#############################################################
# Each process tells rank 0 how many numbers it has recorded

if (rank == 0):
    counts = numpy.zeros(size,dtype='i')
    counts[0] = icount
    reqs = []
    # Post the sends and keep a list of our requests
    for i in range(1,size):
        thiscount=counts[i:i+1]
        rreq = cw.Irecv(thiscount, source=i, tag=i)
        reqs.append(rreq)

    # Call the wait method of each request
    for i in range(size-1):
        reqs[i].Wait()
else:
    mycount = numpy.zeros(1,dtype='i')
    mycount[0] = icount
    sreq = cw.Isend(mycount, dest=0, tag = rank)
    sreq.Wait()

#############################################################
#  Next, rank 0 collects the numbers and sequence lengths from 
#  each of the other processes
if (rank == 0):
    ns = numpy.sum(counts)
    all_lengths = numpy.zeros((2,ns),dtype='i',order='F')
    offset = 0
    if (icount > 0):
        all_lengths[0:2,0:icount] = seq_info[0:2,0:icount]
        offset = offset+icount

    reqs = []
    for i in range(1,size):
        if (counts[i] > 0):
            rvals=all_lengths[0:2,offset:offset+counts[i]]
            rreq = cw.Irecv(rvals, source=i, tag=i)
            reqs.append(rreq)
            offset = offset+counts[i]

    # Call the wait method of each request
    for i in range(len(reqs)):
        reqs[i].Wait()

    # Finally, have rank 0 print out the results
    for i in range(ns):
        nstr = str(all_lengths[0,i])
        lstr = str(all_lengths[1,i])
        print(nstr+' has a sequence length of '+lstr+'.') 

else:
    sreq = cw.Isend(seq_info, dest=0, tag = rank)
    sreq.Wait()

MPI.Finalize()

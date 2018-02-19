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



###########################
# Initialize Communication
cw = MPI.COMM_WORLD
rank = cw.rank
size = cw.size
proc = MPI.Get_processor_name()


if (rank == 0):
    print(str(size)+' MPI processes are now active')


# In this example, ranks 0 and num_proc-1 send each other a single integer
# We use blocking Sends and Recvs.  To avoid deadlock, rank size-1 sends first, 
# then receives.  Rank 0, on the other hand, receives first, and then sends

stoken = 3*rank+1  # The number ranks 0 and size-1 send



if (rank == (size-1)):
    mydest = 0     # The destination rank
    mytag = rank   # A unique tag for this message( dest should expect the same tag)
    nvals = 1    # The number of values we are sending

    # We create a single-element numpy array to hold our value
    svals = numpy.zeros(nvals,dtype='i')
    svals[0] = stoken

    # Send and receive are METHODS of the MPI Communicator (which we call cw)
    cw.Send(svals,dest=mydest,tag=mytag )

    # Next, num_proc -1 receives a message from rank 0
    mysource = 0
    yourtag = 0
    nvals =1

    rvals = numpy.zeros(nvals,dtype='i')
    cw.Recv(rvals,source=mysource,tag=yourtag)


if (rank == 0):
    mysource = size-1
    yourtag = size-1
    nvals = 1
    svals = numpy.zeros(nvals,dtype='i')
    rvals = numpy.zeros(nvals,dtype='i')

    cw.Recv(rvals,source=mysource,tag=yourtag) # Receive from rank size-1

    mydest = size-1
    mytag = rank
    cw.Send(svals,dest=mydest,tag=mytag) # Send to rank size-1

if ((rank == 0) or  (rank == size-1)):
    print('Rank '+str(rank)+' has received the value '+str(rvals[0]) +' from rank '+str(mysource)+'.')



MPI.Finalize()

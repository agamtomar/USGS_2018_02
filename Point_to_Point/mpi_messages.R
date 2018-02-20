suppressMessages(library(pbdMPI, quietly = TRUE))

###################################################
# Fill in the body of this custom allreduce function
# (use a series of calls to Send and Recv)

# There are many more clever ways to do this.  Here, we take a
# straightforward approach and use logic similar to our earlier
# deadlock exercise (exercise 3)



###########################
## Initialize Communication

init()
size = comm.size()
rank = comm.rank()

if (rank == 0) {
    print(sprintf('%d processor are now active', size))
}

# In this example, ranks 0 and num_proc-1 send each other a single integer
# We use blocking Sends and Recvs.  To avoid deadlock, rank size-1 sends first, 
# then receives.  Rank 0, on the other hand, receives first, and then sends

stoken = 3*rank+1  # The number ranks 0 and size-1 send

if (rank == (size-1)) {
    mydest = 0     # The destination rank
    mytag = rank   # A unique tag for this message( dest should expect the same tag)
    nvals = 1    # The number of values we are sending

 
    # Send and receive are METHODS of the MPI Communicator (which we call cw)
    send(stoken, rank.dest=mydest, tag=mytag )

    # Next, size-1 receives a message from rank 0
    mysource = 0
    yourtag = 0

    rvals = 0
    rvals = recv(rvals, rank.source=mysource, tag=yourtag)
}

if (rank == 0) {
    mysource = size-1
    yourtag = size-1
    nvals = 1
    rvals = 0
    rvals = recv(rvals, rank.source=mysource, tag=yourtag) # Receive from rank size-1

    mydest = size-1
    mytag = rank
    send(stoken, rank.dest=mydest, tag=mytag) # Send to rank size-1
}

if ((rank == 0) | (rank == size-1)) {
    print(sprintf('Rank %d has received the value %d from rank %d.', rank, rvals, mysource))
}

finalize()

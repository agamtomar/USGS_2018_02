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
#           mpiexec -np 3 Rscript ex3.R -N 1 
#               gives
#           my_rank = 0; buffer =  0 2 1
#           my_rank = 1; buffer =  1 0 2
#           my_rank = 2; buffer =  2 1 0
#       
#   Example 2:
#           mpiexec -np 4 Rscript ex3.R -N 2
#               gives
#           my_rank = 0; buffer =  0 0 3 3 2 2 1 1
#           my_rank = 1; buffer =  1 1 0 0 3 3 2 2
#           my_rank = 2; buffer =  2 2 1 1 0 0 3 3
#           my_rank = 3; buffer =  3 3 2 2 1 1 0 0
#
#   NOTE:  depending on your bufferflush settings, the lines above may appear out of order.
suppressMessages(library(pbdMPI, quietly = TRUE))

# initialize MPI
init()
size = comm.size()
rank = comm.rank()

if (rank == 0) {
    print(sprintf("%d MPI processes are active", size))
    args = commandArgs(TRUE)
    n = as.integer(args[1])
    print(sprintf("n is: %d", n))
    nb = bcast(n)
} else {
    n = 0
    nb = bcast(n)
}

## initialize the data
mybuffer = integer(nb*size)
mybuffer[] = rank
rvals = integer(nb)

left = rank - 1
right = rank + 1
if ( right > size-1) {
    right  = 0
}
if ( left < 0 ) {
    left = size-1
}

print(sprintf("My rank: %d, left: %d, right %d;", rank, left, right))

## Communication
parity = rank %% 2

offset = 1
for (i in 1:(size-1)) {
    mtag = 1
    if (parity == 0) {
        svals = mybuffer[offset:(offset+nb-1)]
        send(svals, rank.dest=right, tag=mtag)

        offset = offset+nb
        rvals <- recv(rvals, rank.source=left, tag=mtag)
        mybuffer[offset:(offset+nb-1)] = rvals
    }
    else {
        offset = offset+nb
        rvals <- recv(rvals, rank.source=left, tag=mtag)
        mybuffer[offset:(offset+nb-1)] = rvals

        svals = mybuffer[(offset-nb):(offset-1)]
        send(svals, rank.dest=right, tag=mtag)
    }
}

cat('Buffer sum is: ')
print(mybuffer)

finalize()


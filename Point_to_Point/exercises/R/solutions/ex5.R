#/////////////////////////////////////////
# 
suppressMessages(library(pbdMPI, quietly = TRUE))

myAllReduce <- function(myrank, size, valin, rop)
{

    left = rank - 1
    right = rank + 1
    if ( right > (size-1)) {
        right  = 0
    }
    if ( left < 0 ) {
        left = size-1
    }
    parity = rank %% 2

    valout <- valin
    sval <- valout
    rval <- 0
    for (i in 1:(size-1)) {
        mtag = 1
        if (parity == 0) {
            send(sval, rank.dest=right, tag=mtag)
            recv(rval, rank.source=left, tag=mtag)
        }
        else {
            recv(rval, rank.source=left, tag=mtag)
            send(sval, rank.dest=right, tag=mtag)
        }
        if (rop == 1) {
            valout <- valout+rval
            sval <- rval
        }
        if (rop == 2) {
            valout <- valout*rval
            sval <- rval
        }
    }
    return(valout)
}


# initialize MPI
init()
size = comm.size()
rank = comm.rank()

if (rank == 0) {
    print(sprintf("%d MPI processes are active", size))
    args = commandArgs(TRUE)
    n = as.integer(args[1])
    print(sprintf("Operation is: %d", n))
    nb = bcast(n)
} else {
    n = 0
    nb = bcast(n)
}

ans = myAllReduce(rank, size, rank, nb)

print(ans)
    
finalize()


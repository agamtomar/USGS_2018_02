#/////////////////////////////////////////
# 
suppressMessages(library(pbdMPI, quietly = TRUE))

myAllReduce <- function(myrank, size, valin, rop)
{
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


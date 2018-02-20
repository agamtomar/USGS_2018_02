suppressMessages(library(pbdMPI, quietly = TRUE))

# When this program works properly, processes will print
# my_rank = 0; buffer = 0 0 0 2 2 2
# my_rank = 1; buffer = 0 1 1 1 1 1
# my_rank = 2; buffer = 2 1 1 2 2 2
# Depending on your R flush settings, the ordering of the three
# lines above might change, but buffer values and corresponding rank
# should remain unchanged.

reportBuffer <- function(rank, size, mybuffer) {
    for (i in 0:size-1) {
        if (i == rank) {
            cat(sprintf('my_rank = %d my buffer = ', rank))
            cat(mybuffer, fill=TRUE)
        }
        barrier()
    }
}

# initialize MPI
init()
size = comm.size()
rank = comm.rank()


## initialize the data
mybuffer = integer(6)
mybuffer[] = rank

if (rank == 0) {
    rnums = integer(3)
    recv(rnums, rank.source=2, tag=2)
    mybuffer[4:6] = rnums
    snums = mybuffer[1]
    send(snums, rank.dest=1, tag=1)
}
if (rank == 1) {
    rnums = integer(1)
    recv(rnums, rank.source=0, tag=1)
    mybuffer[1] = rnums
    snums = mybuffer[2:3]
    send(snums, rank.dest=2, tag=1)
}
if (rank == 2) {
    snums = mybuffer[4:6]
    send(snums, rank.dest=0, tag=2)
    rnums = integer(2) 
    recv(rnums, rank.source=1, tag=2)
    mybuffer[2:3] = rnums
}

reportBuffer(rank, size, mybuffer)

finalize()


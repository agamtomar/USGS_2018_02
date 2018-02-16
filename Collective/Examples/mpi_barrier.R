suppressMessages(library(pbdMPI, quietly = TRUE))

init()
.comm.size <- comm.size()
.comm.rank <- comm.rank()

# comm.print is collective in pbdR
print(.comm.rank)
print(.comm.size)

if (.comm.rank == 0) {
    print(sprintf("%d MPI processes are active", .comm.size))
}

barrier()

for (i in 0:(.comm.size-1)) {
    if (.comm.rank == i) {
        print(sprintf("Hello from rank %d out of %d mpi process", .comm.rank, .comm.size))
    }
}

finalize()

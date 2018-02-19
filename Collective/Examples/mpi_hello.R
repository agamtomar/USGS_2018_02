suppressMessages(library(pbdMPI, quietly = TRUE))

init()
.comm.size <- comm.size()
.comm.rank <- comm.rank()

# MPI_Get_processor_name doesn't exisit in pbdMPI

if (.comm.rank == 0) {
    print(sprintf("%d MPI processes are active", .comm.size))
}

comm.print(sprintf("Hello from rank %d out of %d mpi process", .comm.rank, .comm.size), all.rank = TRUE)

finalize()

suppressMessages(library(pbdMPI, quietly = TRUE))

init()
.comm.size <- comm.size()
.comm.rank <- comm.rank()

ofile =  'tmp.dat'

if (.comm.rank == 0) {
        print(sprintf("%d MPI processes are active", .comm.size))
        args = commandArgs(TRUE)
        n = as.integer(args[1])
        print(sprintf("n is: %d", n))
        nb = bcast(n)
} else {
    n = 0
    nb = bcast(n)
}

myNumbers = runif(nb)

if (.comm.rank == 0) {
    barrier()
    yourNumbers = numeric(nb)
    recv(yourNumbers, rank.source = 1, tag = 1)
    dodProduct = myNumbers %*% yourNumbers
       
} else if (.comm.rank == 1) {
    barrier()
    send(myNumbers, rank.dest = 0, tag =1)
}

finalize()


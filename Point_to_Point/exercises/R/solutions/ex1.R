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

ofile = paste("Numbers_N",nb, sep="")
myNumbers = runif(nb)

if (.comm.rank == 0) {
    # write the data on rank 0
    f = file(ofile, "w+")
    for (i in 1:nb) {
        writeLines(toString(myNumbers[i]), f)
    }
    close(f)
    barrier()
    # receive the data and create dot product
    yourNumbers = numeric(nb)
    recv(yourNumbers, rank.source = 1, tag = 1)
    dodProduct = myNumbers %*% yourNumbers
    print(sprintf("The dot product is: %f", dodProduct))
       
} else if (.comm.rank == 1) {
    barrier()
    send(myNumbers, rank.dest = 0, tag =1)
    if (file.exists(ofile)) {
        f = file(ofile, "a+")
        for (i in 1:nb) {
            writeLines(toString(myNumbers[i]), f)
        }
        close(f)
    } else {
        print("Error: Rank 1 unable to find", ofile)
    }
}

finalize()


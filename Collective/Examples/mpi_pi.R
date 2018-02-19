suppressMessages(library(pbdMPI, quietly = TRUE))

f <- function(a) {
    val <- 4.0 / (1.0+a*a)
    return(val)
}


init()
.comm.size <- comm.size()
.comm.rank <- comm.rank()


if (.comm.rank == 0) {
    print(sprintf("%d MPI processes are active", .comm.size))
    n <- 10000
    print(sprintf("n is: %d", n))
    nb <- bcast(n)
} else {
    n <- 0
    nb <- bcast(n)
}


h <- 1./nb

mysum <- 0
for (i in seq(.comm.rank+1, nb, .comm.size)) {
    x <- h*(i-0.5)
    mysum <- mysum + f(x)
}
mypi = h*mysum


#Next, each process collectively communicates all values of mypi,
#adds them up, and stores the value in pi.

pi <- reduce(mypi, op = "sum")

comm.print(sprintf("Pi is approximatel %f", pi))

finalize()

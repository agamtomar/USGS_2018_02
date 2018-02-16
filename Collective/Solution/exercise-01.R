suppressMessages(library(pbdMPI, quietly = TRUE))

init()
.comm.size <- comm.size()
.comm.rank <- comm.rank()

f <- as.double(0)
if (.comm.rank == 0) {
  args = commandArgs()
  print(args)
  f <- as.numeric(tail(args,1))
}
comm.print(f, all.rank = TRUE)

f <- bcast(f)
comm.print(f, all.rank = TRUE)

r = as.double(.comm.rank + 1) * sin(f)
# note that pbdMPI doesn't have a Get_processor_name function
comm.print(sprintf("%d*sin(%f) = %f", .comm.rank+1, f, r), all.rank = TRUE)

results <- gather(r)
comm.print(results)

finalize()

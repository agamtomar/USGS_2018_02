# Solution to exercise 1
from mpi4py import MPI
import numpy as np
import sys

cw = MPI.COMM_WORLD
rank = cw.rank
size = cw.size

f = np.array([0.0]) # needs to be an array or list
# only processor 0 reads input
if rank == 0:
    f[0] = float(sys.argv[1])
    print(f)

# All processes particiate in broadcast
cw.Bcast(f, 0)

# Computation and I/O on all processes
r = np.array([0.0]) # since we want to gather the results needs to be an array or other dtype
r[0] = float(rank+1)*np.sin(f[0])
proc = MPI.Get_processor_name()
print("Hello from process %d on node %s" % (rank, proc))
print("%d*sin(%f) = %f" % (rank, f[0], r[0]))

results = np.zeros(1) #  define results array on all processes
if rank == 0:
    results = np.zeros(size)

cw.Gather(r, results, 0)
if rank == 0:
    print(results)

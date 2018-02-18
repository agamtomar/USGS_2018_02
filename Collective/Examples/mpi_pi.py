#MPI Hello World
from mpi4py import MPI

cw = MPI.COMM_WORLD
rank = cw.rank
size = cw.size
proc = MPI.Get_processor_name()



if rank == 0:
    print("{} MPI processes are now active".format(size))

print('Hello from node {} rank {} out of {} mpi processes'.format(proc, rank, size))

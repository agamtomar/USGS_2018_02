from mpi4py import MPI

cw = MPI.COMM_WORLD
rank = cw.rank
size = cw.size
proc = MPI.Get_processor_name()

print(rank)
print(size)

if rank == 0:
    print("{} MPI processes are now active".format(size))

cw.Barrier()

# Consider the loop below.  Where can we place another call to mpi_barrier to ensure
# that the MPI tasks print their 'hello' in ascending order based on rank?

for i in range(size):
    if (rank == i):
        print('Hello from node {} rank {} out of {} mpi processes'.format(proc, rank, size))

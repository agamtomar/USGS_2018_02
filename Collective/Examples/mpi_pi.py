#/////////////////////////////////////////////////////////////////////////
#   Parallel computation of Pi
#
#   Each node: 
#    1) receives the number of rectangles used in the approximation.
#    2) calculates the areas of it's rectangles.
#    3) Synchronizes for a global summation.
#   Node 0 prints the result.
#
#***************************************************************************
from mpi4py import MPI
import numpy
def f(a):
    val = 4.0 / (1.0+a*a)
    return val

cw = MPI.COMM_WORLD
rank = cw.rank
size = cw.size
proc = MPI.Get_processor_name()


# When working with MPI and Python, it's best to work with Numpy Arrays.
# When working with scalars that are communicated, we create 1-element Numpy Arrays.
n=numpy.zeros(1,dtype='i')

#Rank 0 reads the user input
if rank == 0:
    print("{} MPI processes are now active".format(size))
    nstr = input('Enter the number of intervals to use in the integration: \n')
    n[0] = str(nstr)
    print('n is: ', n[0])


#Rank 0 broadcasts its value to all other ranks
cw.Bcast(n, root = 0)

h = numpy.zeros(1,dtype='d')
pi = numpy.zeros(1,dtype='d')
mysum = numpy.zeros(1,dtype='d')

h[0] = 1.0/n[0]

for i in range(rank+1,n[0],size):
    x = h*(i-0.5)
    mysum[0] += f(x)
mypi = h*mysum

#Next, each process collectively communicates all values of mypi,
#adds them up, and stores the value in pi.
cw.Reduce(mypi,pi,op=MPI.SUM)

if (rank == 0):
    print("Pi is approximately {:0,.8f}".format(pi[0]))


MPI.Finalize()

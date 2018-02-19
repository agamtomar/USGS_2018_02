from mpi4py import MPI
import sys
import numpy
import random
import os

###################################################
# Define a few functions used in the calculation
def gen_data(f,pars):
    pi = numpy.pi
    dx = 2*pi/num
    x = 0.0
    for i in range(num):
        f[i] = pars[0] * x**( pars[1] / (1+rank) )
        x = x+dx

def get_new_params(ind,amp,val):
    amp[0] = 1.0/val
    amp[1] = (ind+1)*2

###########################
# Initialize Communication
cw = MPI.COMM_WORLD
rank = cw.rank
size = cw.size
proc = MPI.Get_processor_name()

#############################################
# Read problem parameters from command line
ofile='tmp.dat'
n = numpy.zeros(1,dtype='i')
if (rank == 0):
    try:
        n[0] = int(sys.argv[2])
    except:
        n[0] = 128


cw.Bcast(n,root=0)
ofile = 'Numbers_N'+str(n[0])
num = n[0]

#######################
# Initialize data 
my_func=numpy.zeros(num*size,dtype='d')
fpars = numpy.zeros(2,dtype='d')
mysum = numpy.zeros(1,dtype='d')
fullsum = numpy.zeros(1,dtype='d')
fullsum[0] = 1.0
num_iter = 10


######################
# Iterate

for i in range(num_iter):
    if (rank == 0):
        get_new_params(i,fpars,fullsum)
        cw.Bcast(fpars,root=0)

    gen_data(my_func,fpars)
    mysum[0] = numpy.sum(my_func)
    cw.Reduce(mysum,fullsum,op=MPI.SUM)

if (rank == 0):
    print('Complete!  Sum is: ', fullsum[0])




MPI.Finalize()

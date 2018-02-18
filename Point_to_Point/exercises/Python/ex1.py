from mpi4py import MPI
import sys
import numpy
import random
import os


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
    n[0] = int(sys.argv[2])
    #ofile = 'Numbers_N'+str(n[0])
    #try:
    #    os.remove(ofile)
    #except:
    #    pass


cw.Bcast(n,root=0)
ofile = 'Numbers_N'+str(n[0])


#######################
# Initialize data (random number sequence)
my_numbers=numpy.zeros(n[0],dtype='d')
for i in range(n[0]):
    my_numbers[i] = random.random()



############################################################
# Communicate, perform calculation on rank 0, and record results

if (rank == 0):
    # Open the file
    #'The value is {:0,.2f}'.format(x)

    myfile = open(ofile,"w")
    for i in range(n[0]):
        istr = "{:8d}".format(i)
        fstr = "{:.5e}".format(my_numbers[i])
        myfile.write(istr+' '+fstr+"\n")
    myfile.close()

    # Receive data
    your_numbers = numpy.zeros(n[0],dtype='d')
    cw.Recv(your_numbers, source=1,tag=1)

    # Peform calculation
    dprod = 0.0
    for i in range(n[0]):
        dprod += your_numbers[i]*my_numbers[i]

    print('The dot product is: ', dprod)

elif (rank == 1):
    cw.Send(my_numbers,dest=0,tag=1)
    fcheck = os.path.exists(ofile)
    if (fcheck):
        myfile = open(ofile,"a")
        for i in range(n[0]):
            istr = "{:8d}".format(i+n[0])
            fstr = "{:.5e}".format(my_numbers[i])
            myfile.write(istr+' '+fstr+"\n")
        myfile.close()
    else:
        print("ERROR:   Rank 1 unable to find "+ofile)

MPI.Finalize()

################################################################
#
#   Exercise:  Modify this code so that the calculation of 
#              Collatz-sequence lengths is distributed across
#              multiple processors. 
#              Each process is given a unique range of numbers for
#              which to compute the sequence length.
#           
#              Modify this code so that the local maximum is reduced
#              across all processes (see reduction.py).
#              Note that since we are working with integers, you should
#              use the Numpy 'int32' datatype and the MPI4PY MPI.INTEGER
#              datatype for communication.
#

def collatz_length(n):
    val = n
    length = 1
    while (val != 1):
        length += 1
        if ((val % 2) == 0):
            val = val//2
        else:
            val =3*val+1
    return length

def main():
    from mpi4py import MPI
    import numpy as np

    num_proc  = MPI.COMM_WORLD.Get_size()
    my_rank   = MPI.COMM_WORLD.Get_rank()
    node_name = MPI.Get_processor_name()
    comm = MPI.COMM_WORLD



    n_local = 1000
    n_global = n_local*num_proc

    # Each process examines numbers in the range [istart,iend)
    istart = n_local*my_rank+1
    iend   = istart+n_local

    max_len = 1
    max_i = 1
    for i in range(istart,iend):
        length = collatz_length(i)
        if (length > max_len):
            max_i = i
            max_len = length


    msg = 'The longest Collatz sequence in the interval [1,'+str(n_global)+'] has length '+str(max_len)+'.'
    print(msg)

main()

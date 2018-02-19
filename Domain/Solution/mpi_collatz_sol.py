################################################################
#
#   Solution:  Collatz-sequence length
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

    # max_len now contains the local maximum
    #
    # To find the global maximum, we define a few
    # single-element numpy arrays and use them in tandem with Allreduce
    # Since we are using integer values, we use int32 and MPI.INTEGER datatypes
    local_max = np.ndarray(1,dtype='i')
    local_max[0] = max_len
    global_max = np.ndarray(1,dtype='i')
    comm.Allreduce(local_max, global_max, op=MPI.MAX)
    msg = 'The longest Collatz sequence in the interval [1,'+str(n_global)+'] has length '+str(global_max[0])+'.'
    print(msg)

    MPI.Finalize()

main()

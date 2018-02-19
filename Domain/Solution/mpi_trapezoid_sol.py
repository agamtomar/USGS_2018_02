#######################################################3
#
#       Solution:  Trapezoidal Integration
#
#
def myfunc(x):
    val = x*x*x
    return val
def trapezoid_int(a, b, ntrap):
    ## Integrates myfunc(x) from a to b
    h = (b-a)/(ntrap-1)

    val = myfunc(a)
    integral = 0.5*val
    val = myfunc(b)
    integral = integral+0.5*val

    for i in range(ntrap):
        x = a+i*h
        integral = integral+myfunc(x)

    integral = integral*h
    return integral

def main():
    """
    Python, mpi4py parallel hello world.
    """

    from mpi4py import MPI
    import sys
    import numpy
    import time

    num_proc  = MPI.COMM_WORLD.Get_size()
    my_rank   = MPI.COMM_WORLD.Get_rank()
    node_name = MPI.Get_processor_name()
    comm = MPI.COMM_WORLD

    if (my_rank == 0):
        sys.stdout.write("  %d MPI Processes are now active.\n" %(num_proc))
    comm.Barrier()


    ntests = 100
    ntrap = 1000000//num_proc  # Each rank gets 1000,000/num_proc trapezoids




    xone = 1.0  # The global limits of integration
    xtwo = 2.0

    # Each rank should integrate between a unique pair of values myxone and myxtwo
    deltax = (xtwo-xone)/num_proc
    myxone = xone+deltax*my_rank
    myxtwo = myxone+deltax

    local_integral  = numpy.ndarray(1, dtype='float64') 
    global_integral = numpy.ndarray(1, dtype='float64')
    t0 = time.time()
    for i in range(ntests):

        local_integral[0] = trapezoid_int(myxone,myxtwo,ntrap)
        # The call to Allreduce will sum the value of local_integral across
        # all processes, and store it in global_integral        	
        comm.Allreduce(local_integral, global_integral, op=MPI.SUM)

    t1 = time.time()
    sys.stdout.write("  Rank %d contributes %f to the global integral value of %f.\n" 
                     % (my_rank, local_integral, global_integral))
    sys.stdout.flush()

    dt = t1-t0
    dt_local =  numpy.ndarray(1,dtype='float64')
    dt_local[0] = dt
    dt_global = numpy.ndarray(1,dtype='float64')

    comm.Allreduce(dt_local, dt_global, op=MPI.SUM)
    dt_avg = (dt_global/num_proc)/ntests
    if (my_rank == 0):
        sys.stdout.write('  Average integration time for %d MPI ranks is %f seconds.\n' % (num_proc, dt_avg[0]))
    MPI.Finalize()
main()


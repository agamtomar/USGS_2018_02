# Scaling Exercise

In this hands on session, you will peform a series of scaling studies using an existing program.  If you wish, you may view the Fortran source code, but for this exercise, you only need to compile and run the program.

To compile the program, simply type "make" .    This will generate an executable named scale.out.

Scale.out peforms a smoothing operation on a 2-D grid, multiple times and in parallel.   The calling syntax is:  

           mpiexec -np N ./program -nx X -ny Y -nt T 
           
 where the following command-line parameters must be specified
           
           N:   number of MPI ranks  
           X:   number of gridpoints in x-direction (default = 1024)  
           Y:   number of gridpoints in y-direction (default = 4096)  
           T:   number of iterations to run for (default = 100)  
           

Your job is to carry out a strong- and weak-scaling study using scale.



# Scaling Exercise

In this hands on session, you will peform a series of scaling studies using an existing program.  If you wish, you may view the Fortran source code, but for this exercise, you only need to compile and run the program.

To compile the program, simply type "make" .    This will generate an executable named scale.out.

Scale.out peforms a smoothing operation on a 2-D grid, multiple times and in parallel.   The calling syntax is:  

           mpiexec -np N ./scale.out -nx X -ny Y -nt T 
           
 where the following command-line parameters must be specified
           
           N:   number of MPI ranks  
           X:   number of gridpoints in x-direction (default = 1024)  
           Y:   number of gridpoints in y-direction (default = 4096)  
           T:   number of iterations to run for (default = 100)  
           

Note that the Y gridpoints are distributed across the N processors.  This means that Y should be at least N and that (ideally) N should divide evenly into Y.  Each process contains all X gridpoints in the X-direction; those are not distributed.

When the program completes, it will pring a single line of text, indicating the problem parameters and time, separated by whitespaces, formatted as:

           N time X Y T

Alternatively, you can run the program with "time" in front of it to have the system measure the time for you:

           time mpiexec -np N ./scale.out -nx X -ny Y -nt T

Your job is to carry out a strong- and weak-scaling study using scale.out.   

1.  For the strong scaling study, pick at least three combinations of (X,Y) including one where X=1 and time the performance of this code on up to 48 cores.

2.  For the weak scaling study, pick at least three combinations of (X, Y/N), where Y/N is held constant, and time the performance of your code on up to 48 cores.

For each study and problem size, identify the largest number of cores you might use for that computation (keep in mind the 80% rule).

Note that while you may do this "manually", looping within a bash or c-shell script called from within your job script can make this process much more efficient.   Solutions created using bash loops and Python plotting are provided in the solutions directory.

 



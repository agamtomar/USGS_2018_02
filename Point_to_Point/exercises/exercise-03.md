# Point-to-Point Communication: Exercise 3

Consider the program ex3.{f90,cpp, py, R}.  This program is designed as follows:

- Each process allocates a buffer of size num_proc*N.
- Each process communicates with the rank below and above it num_proc-1 times, sending messages of size N.
- When the program completes, the buffer contains N values set equal to each MPI_RANK.

For example, running the program as mpiexec -np 4 ./ex3.out -N 2 will yield the result:  
my_rank = 0; buffer =  0 0 3 3 2 2 1 1  
my_rank = 1; buffer =  1 1 0 0 3 3 2 2  
my_rank = 2; buffer =  2 2 1 1 0 0 3 3  
my_rank = 3; buffer =  3 3 2 2 1 1 0 0  


There is a bug in the current implementation of the program.  For small N, it seems to always work.  For sufficiently large N, however, the program always hangs.

Identify and correct the issue producing the deadlock.





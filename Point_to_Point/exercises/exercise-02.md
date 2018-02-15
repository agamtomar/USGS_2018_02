# Point-to-Point Communication: Exercise 2

Consider the program ex2.{f90,cpp, py, R}.  This program is designed such that when it completes, the following message will be printed:

my_rank = 0; buffer = 000222  
my_rank = 1; buffer = 011111  
my_rank = 2; buffer = 211222  

As it is written, however, there are **two** bugs preventing this program from executing properly.  Carefully examine the calls to MPI_Send and MPI_Recv and clear the deadlock.





# Advanced Point-to-Point Communication: Exercise 1

Consider the program ex1.{f90,cpp, py, R}.  

This program is designed to compute, in parallel, the Collatz sequence lengths for all numbers in the range [1,N], recording those lengths greater than a threshold T.  A calling example is  
mpiexec -np 16 ./ex1.out -N 100 -T 3 ,  
which will record all sequence lengths greater than 3 for numbers in the range [1,100].

The program is written using non-blocking communication and has a few crucial pieces:
- The work is split up across all processors such that everyone computes sequence lengths for subranges in the global range [1,N]
- Each process sends an initial message to Rank 0 indicating how many sequence lengths greater than T were found associated with numbers in [1,N].
- Using that information, Rank 0 allocates space to receive each other Rank's numbers and associated sequence lengths for those lengths passing the threshold value T.
- Each Rank then sends its numbers and sequence lengths to Rank 0.

This program is moderately complex, but is almost working.  As it is written, however, it crashes due to a small bug somewhere in the Isend/Ireceive logic.  Can you identify and clear this bug?  

**Note:** There are no bugs in either the problem distribution (i.e., computing my_min and my_max).  Nor are there issues in the sequence computation.  You may assume that those portions of the program work correctly and that the bug lies with the use of ISends and IRecvs.




# Point-to-Point Communication: Exercise 1

Consider the program ex1.{f90,cpp, py, R}.  This program is designed to do the following:

- Each process generates a sequence of N random numbers.
- Rank 0 writes its N numbers to a file.
- Rank 1 sends its numbers to Rank 0, and Rank 0 computes the dot product of the two vectors.
- While Rank 0 computes the dot product, Rank 1 appends its random numbers to the file created by Rank 0.

The result should be a file containing a list of 2N random numbers, but there is a problem.  
Unless N is very large, the program seems to crash randomly, working somtimes, but not others.

Can you identify and correct the issue?

The program is called with the syntax:
mpiexec -np 2 ./ex1.out -N 10    

In this example, the output would be stored in a file named Numbers_N10.

**Note:** When testing your program, be sure to delete the output file (if it exists) before each execution.  You may find the script **loopit** to be useful for this purpose.  This script will run multiple tests for any value of N given to it.  The calling sequence (for 10 random numbers) is:  
./loopit 10



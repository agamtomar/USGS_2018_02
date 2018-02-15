# Point-to-Point Communication: Exercise 4 Solution

Consider the program ex4.{f90,cpp, py, R}.  

In this example, we commited a common error.  Namely, rank 0 alone computes some special results that it must broadcast to all other ranks.  Our error was to place the broadcast statement within the IF (my_rank == 0) construct.   Pulling the broadcast out of the IF construct clears the deadlock.





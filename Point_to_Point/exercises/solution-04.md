# Point-to-Point Communication: Exercise 4 Solution

In this example, we commited a common error.  Namely, rank 0 alone computes some special results that it must broadcast to all other ranks.  Our error was to place the broadcast statement within the IF (my_rank == 0) construct.   Pulling the broadcast out of the IF construct clears the deadlock.





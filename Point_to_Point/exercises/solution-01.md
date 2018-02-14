# Point-to-Point Communication: Exercise 1  Solution

Source-code solutions for this exericse may be found in the solutions folder associated with each programming language.

In this example, the bug stems from the fact that MPI_Send is **nonblocking** for small message sizes.  For sufficiently large messages, our code works fine.  For small messages, however, Rank 1 may race ahead of Rank 0 attempt to access the output file before it has been created.

One possible solution to this problem is a to add a call to MPI_Barrier following our send/receive pairs.  This forces Rank 1 to wait until Rank 0 has received before proceeding.  As Rank 0 creates the file before posting the receive, we can be assured that the file will exist when Rank 1 attempts access.




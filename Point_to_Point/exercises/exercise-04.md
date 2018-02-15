# Point-to-Point Communication: Exercise 4

Consider the program ex4.{f90,cpp, py, R}.  

So far, we have examined deadlocks in the context of point-to-point communication, but collective communication can be susceptible to deadlock behavior in certain circumstances.

Consider the example program in this example.  It illustrates a common sequence of collective operations, namely:
- Rank 0 computes some parameters that are broadcast to all other ranks
- A reduction on some number(s) is performed across all ranks
- The process repeated, with information from step 1 feeding into step 2, and so on

This program almost works, but will crash or hang depending on your MPI implementation.   Can you clear the deadlock?






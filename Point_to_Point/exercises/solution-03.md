# Point-to-Point Communication: Exercise 3 Solutions

Source-code solutions for this exericse may be found in the solutions folder associated with each programming language.

This exercise illustrates another common cause of deadlock, **conflicting send/receive posts**.

In the original implementation of the code, we see that each time through the loop, all processes post a send and then a receive:  
Call MPI_Send  
Call MPI_Recv  

For small messages (small N), the Send behaves in a nonblocking fashion, and there are no problems.   For large N, however, the Send is blocking.  Since all ranks are waiting on the send to complete, no rank is free to post a receive, and we reach a deadlock.

The solution is for some subset of ranks to post a Recv **first**.  In our implementation of the solution, we have even-numbered ranks post a send, followed by a receive.   Odd-numbered ranks post a receive, and then a send, breaking the deadlock.





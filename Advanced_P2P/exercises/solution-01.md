# Advanced Point-to-Point Communication: Exercise 1

Source-code solutions may be found within their respective language directories.

This program crashes due to a commonly occuring bug in non-blocking algorithms:  incorrect MPI_Wait and MPI_WaitAll logic.  Note that in the initial round of sends, all Ranks execute a WaitAll.   This is only appropriate, however, for Rank 0, which is receiving multiple messages.   Nonzero ranks instead send only a single message to Rank 0 and should call MPI_Wait instead.  The solution is to mimic the logic used in the second round of messages, where only Rank 0 calls MPI_WaitAll.






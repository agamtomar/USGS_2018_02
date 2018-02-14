# Point-to-Point Communication: Exercise 2 Solution

Source-code solutions for this exericse may be found in the solutions folder associated with each programming language.

This exercise illustrates two common causes of deadlock:
- **Mismatched send/receive buffer sizes:**  In the original version of this program, Rank 0 expects to receive 2 integers from Rank 2.  Rank 2, however, is attempting to send 3 integers to Rank 0.  The buffer sizes must be consistent for the send/receive pair to complete.  In order to generate the expected output, Rank 0's receive should be modified to accept 3 integers.    
- **Mismatched send/receive tags:** The call to MPI_Send executed by Rank 1 uses a message tag of 1.  The corresponding receive from Rank 2 uses a message tag of 2.   This bug can be fixed by making these two tags match (either value is fine).


# Point-to-Point Communication: Exercise 5 Solution

A sample solution may be found in the solutions directory for your chosen programming language.

Our approach to the solution involves sending data in a ring, as in exercise 3.  On the first pass through the loop, a process communicates its number to the right.   The second time through the loop, the process is repeated, but now the number passed is different (based on the specified reduction operation).   By executing this loop num_proc-1 times, each process gets a chance to examine (whether directly or indirectly) each other process's value.   







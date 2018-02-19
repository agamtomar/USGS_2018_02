# Collective Communication: Exercise 1

- Write a MPI program that does the following
  - Process 0 reads in a floating point number
  - Use MPI_Bcast to send the number to each Process
  - Each process computes `(process_id+1)*sin(number)`
  - Each process prints:
    - `Hello from process %d on node %s`
    - `%d*sin(%f) = %f`
  - Process 0 collects all results
  - Process 0 prints: `Number of MPI processes = %d`
  - Process 0 prints all results

Use the following MPI functions:
```
MPI_Init
MPI_Finalize
MPI_Commm_size
MPI_Comm_rank
MPI_Bcast
MPI_Gather
MPI_Processor_name
```

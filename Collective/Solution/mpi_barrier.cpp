#include <stdio.h>
#include <mpi.h>

int main(int argc, char** argv) {
    int num_proc;  // The number of processes
    int my_rank; // the rank of this process (ranges from 0-nproc-1) 
    // The next string will hold the node's name.
    // Note that the value of MPI_MAX_PROCESSOR_NAME is defined by the MPI distribution
    char node_name[MPI_MAX_PROCESSOR_NAME]; // string to hold the node's name
    int name_len; // the number of characters in node_name
    int i;

    MPI_Init(NULL, NULL);
    MPI_Comm_size(MPI_COMM_WORLD, &num_proc);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Get_processor_name(node_name, &name_len);

    if (my_rank == 0) {
        printf("  %d MPI Processes are now active.\n", num_proc);
    }
    // Execution of the parallel region pauses at the barrier and resumes once all threads have
    // reached the barrier.

    MPI_Barrier(MPI_COMM_WORLD);

    // Consider the loop below.  Where can we place another call to mpi_barrier to ensure
    // that the MPI tasks print their 'hello' in ascending order based on rank? 
    for (int i=0; i<num_proc; ++i) {
       MPI_Barrier(MPI_COMM_WORLD);
       if (my_rank == i) {
	 printf("  Hello world from processor %s, rank %d out of %d processors\n",
		node_name, my_rank, num_proc);
       }
    }
    
    MPI_Finalize();
  
}


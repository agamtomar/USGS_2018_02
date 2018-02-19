#include <stdio.h>
#include <mpi.h>
int main(int argc, char** argv) {
    int num_proc;  // The number of processes
    int my_rank; // the rank of this process (ranges from 0-nproc-1) 
    // The next string will hold the node's name.
    // Note that the value of MPI_MAX_PROCESSOR_NAME is defined by the MPI distribution
    char node_name[MPI_MAX_PROCESSOR_NAME]; // string to hold the node's name (e.g., shas0118)
    int name_len; // the number of characters in node_name
    int i;
    int stoken;
    int rtoken;
    int source, dest, tag, nvals;
    int ierr;
    

    ierr = MPI_Init(NULL, NULL);
    ierr = MPI_Comm_size(MPI_COMM_WORLD, &num_proc);
    ierr = MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    ierr = MPI_Get_processor_name(node_name, &name_len);

    stoken = 3*my_rank+1;
    rtoken = -1;

    if (my_rank == 0)
    {
        printf("  %d MPI Processes are now active.\n", num_proc);
    }
    ierr = MPI_Barrier(MPI_COMM_WORLD);



    // In this example, ranks 0 and num_proc-1 each send each other a single integer
    // We use blocking Sends and Recvs.  To avoid deadlock, rank num_proc-1 sends first, 
    // then receives.  Rank 0, on the other hand, receives first, and then sends

    if (my_rank == (num_proc-1))
    {
        //The highest rank sends a message to rank 0

        dest = 0;      // The destination rank
        tag = my_rank; // A unique tag for this message (destination should expect same tag)
        nvals = 1;     // The number of values we are transmitting

        //We send the value of the stoken variable
        ierr = MPI_Send(&stoken, nvals, MPI_INT, dest, tag, MPI_COMM_WORLD);

        //The highest rank now receives a message from rank num_proc-1
        source = 0;  // The source of the message
        tag = 0;     // The message tag (must match the tag  set by sender)
        nvals = 1;            // The number of values we will receive

        //The value of token will be overwritten by whatever num_proc-1 sends.
        ierr = MPI_Recv(&rtoken, nvals, MPI_INT, source, tag, MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);


        printf("  Rank %d has received the token %d from rank %d.\n", my_rank, rtoken, source);
    }

    if (my_rank == 0)
    {
        //Rank 0 receives a message from rank num_proc-1
        source = num_proc-1;  // The source of the message
        tag = num_proc-1;     // The message tag (must match the tag  set by sender)
        nvals = 1;            // The number of values we will receive

        //The value of rtoken will be overwritten by whatever num_proc-1 sends.
        ierr = MPI_Recv(&rtoken, nvals, MPI_INT, source, tag, MPI_COMM_WORLD,
                 MPI_STATUS_IGNORE);

        //Rank 0 now sends to rank num_proc -1

        dest = num_proc-1;      // The destination rank
        tag = my_rank; // A unique tag for this message (destination should expect same tag)
        nvals = 1;     // The number of values we are transmitting

        //We send the value of the stoken variable

        ierr = MPI_Send(&stoken, nvals, MPI_INT, dest, tag, MPI_COMM_WORLD);

        printf("  Rank %d has received the token %d from rank %d.\n", my_rank, rtoken, source);


    }

    
    MPI_Finalize();
  
}


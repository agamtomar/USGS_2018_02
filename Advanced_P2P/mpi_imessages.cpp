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
    MPI_Request reqs[2];    // Array of interrupt requests used for Isend and IRecv
    MPI_Status mstat[2];   // Status array used for MPI Wait-All


    ierr = MPI_Init(NULL, NULL);
    ierr = MPI_Comm_size(MPI_COMM_WORLD, &num_proc);
    ierr = MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    ierr = MPI_Get_processor_name(node_name, &name_len);

    stoken = my_rank*3+1;
    rtoken = -1;

    if (my_rank == 0)
    {
        printf("  %d MPI Processes are now active.\n", num_proc);
    }
    ierr = MPI_Barrier(MPI_COMM_WORLD);


    // In this example, ranks 0 and num_proc-1 each send each other a single integer
    // We use ISends and IRecvs, allowing both sends and receives to be posted
    // simultaneously.

    if (my_rank == (num_proc-1))
    {
        //The highest rank sends a message to rank 0

        dest = 0;      // The destination rank
        tag = my_rank; // A unique tag for this message (destination should expect same tag)
        nvals = 1;     // The number of values we are transmitting

        //We send the value of the stoken variable
        ierr = MPI_Isend(&stoken, nvals, MPI_INT, dest, tag, MPI_COMM_WORLD, &reqs[0]);


        //Rank 0 receives a message from rank num_proc-1
        source = 0;  // The source of the message
        tag = 0;     // The message tag (must match the tag  set by sender)
        nvals = 1;            // The number of values we will receive



        //The value of rtoken will be overwritten by whatever num_proc-1 sends.
        ierr = MPI_Irecv(&rtoken, nvals, MPI_INT, source, tag, MPI_COMM_WORLD,
             &reqs[1]);

        // Now, we wait on both the send (reqs[0]) and the receive (reqs[1]) to complete
        ierr = MPI_Waitall(2, reqs, mstat);
        printf("  Rank %d has received the token %d from rank %d.\n", my_rank, rtoken, source);

    }

    if (my_rank == 0)
    {
        //Post an Isend to Rank num_proc-1

        dest = num_proc-1;      // The destination rank
        tag = my_rank; // A unique tag for this message (destination should expect same tag)
        nvals = 1;     // The number of values we are transmitting

        //We send the value of the stoken variable
        ierr = MPI_Isend(&stoken, nvals, MPI_INT, dest, tag, MPI_COMM_WORLD, &reqs[0]);

        //Rank 0 receives a message from rank num_proc-1
        source = num_proc-1;  // The source of the message
        tag = num_proc-1;     // The message tag (must match the tag  set by sender)
        nvals = 1;            // The number of values we will receive

        //The value of rtoken will be overwritten by whatever num_proc-1 sends.
        ierr = MPI_Irecv(&rtoken, nvals, MPI_INT, source, tag, MPI_COMM_WORLD,
             &reqs[1]);

        // Now, we wait on both the send (reqs[0]) and the receive (reqs[1]) to complete
        ierr = MPI_Waitall(2, reqs, mstat);
        printf("  Rank %d has received the token %d from rank %d.\n", my_rank, rtoken, source);
    }



    ierr = MPI_Finalize();
  
}


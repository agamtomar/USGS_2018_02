///////////////////////////////////////////////////////////////////
//
//          MPI Solution:  Collatz Sequence
//

#include <stdio.h>
#include <mpi.h>
int collatz_length(int n) {
    int i;
    int length;
    length=1;
    i = n;
    while (i > 1) {
        length +=1;
        if ( ( i%2 ) ==0 ){
            i = i/2;
        } else {
            i = 3*i+1;
        }
    }
    return length;
}

int main(int argc, char** argv) {
    int ntests;
    int jstart;
    int jend;
    int jlength;
    int jmax;
    int njlocal; 
    int max_num_local;
    int max_num_global;
    int max_length_local;
    int max_length_global;
    int jsave;
    int my_rank;
    int num_proc;
    int ierr;

    MPI_Init(NULL, NULL);
    MPI_Comm_size(MPI_COMM_WORLD, &num_proc);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    njlocal = 1000000;
    jmax = njlocal*num_proc;

    if (my_rank == 0)
    {
        printf("  Calculating Collatz-sequence lengths over the interval [1,%d] using %d processes.\n",
                jmax, num_proc);
    }

    // (1)   
    jstart = 1+my_rank*njlocal;
    jend = jstart+njlocal-1;

    max_length_local=1;
    max_num_local=1;
    for (int j=jstart; j<jend; ++j) {
        jlength = collatz_length(j);
        if (jlength > max_length_local){
            max_num_local=j;
            max_length_local=jlength;
        }
    }
    max_length_global=0;
    max_num_global=0;

    // (2) 
    MPI_Allreduce(&max_length_local, &max_length_global, 1, MPI_INTEGER, MPI_MAX,
                 MPI_COMM_WORLD);

    // (3;  here's one way to handle the tricky part)
    if (max_length_global != max_length_local){
        max_num_local=-1;
    }
    MPI_Allreduce(&max_num_local, &max_num_global, 1, MPI_INTEGER, MPI_MAX,
                 MPI_COMM_WORLD);

    if (my_rank == 0){
        printf("  The longest Collatz-sequence in the interval [ 1, %d ] has length %d.\n", jmax,max_length_global);
        printf("  This sequence-length occurs for the number %d.\n", max_num_global);

    }
    ierr = MPI_Finalize();
}


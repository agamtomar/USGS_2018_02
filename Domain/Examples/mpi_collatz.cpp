///////////////////////////////////////////////////////////////////
//
//          MPI Exercise:  Collatz Sequence
//
//          Modify the program below so that it reports the number
//          with the largest collatz sequence length, and its length,
//          occuring in the interval [1, N Million] where N is the 
//          number of MPI ranks.
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
  int ierr;
  int ntests;
  int jstart;
  int jend;
  int jlength;
  int jmax;              // The maximum number whose sequence-length is calculated by any processor
  int njlocal;           // How many numbers each process checks the sequence length for.
  int max_length_local;  // The maximum sequence length calculated locally
  int max_length_global; // The maximum sequence length calculated globally
  int max_num_local;     // The number with the longest sequence-length found locally
  int max_num_global;    // The number possessing the global max sequence length
  int my_rank;
  int num_proc;

  ierr = MPI_Init(NULL, NULL);
  ierr = MPI_Comm_size(MPI_COMM_WORLD, &num_proc);
  ierr = MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  njlocal = 1000000;
  jmax = njlocal*num_proc;

  if (my_rank == 0)
    {
      printf("  Calculating Collatz-sequence lengths over the interval [1,%d] using %d processes.\n",jmax, num_proc);
    }

  // (1) Change jstart and jend so that all rank compute a sequence lengths for numbers
  //     in a unique portion of the interval [1,jmax]
  jstart = 1;
  jend = jmax;

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
  // (2) Use an Allreduce to get the global maximum of the sequence lengths
  printf(" The longest Collatz-sequence in the interval [ 1, %d ] has length %d.\n", jmax,max_length_global);

  // (3;  A little bit trickier...) Use an Allreduce to assign the number associated with that sequence length to max_num_global
  printf(" This sequence-length occurs for the number %d.\n", max_num_global);
  ierr = MPI_Finalize();
}


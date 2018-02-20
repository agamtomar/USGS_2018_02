///////////////////////////////////////////
//   Token-Passing Deadlock Exercise
//  
//   Calling sequence:
//       mpiexec -np X python ex3.py -N Y
//       where Y indicates the number of integers sent during each pass
//       
//   Results:
//       When the program completes successfully, each process will possess a buffer containing
//       num_proc*Y values, with the value of each mpi rank occurring Y times.'
//
//   Example 1:
//           mpiexec -np 3 python ex3.py -N 1 
//               gives
//           my_rank = 0; buffer =  0 2 1
//           my_rank = 1; buffer =  1 0 2
//           my_rank = 2; buffer =  2 1 0
//       
//   Example 2:
//           mpiexec -np 4 python ex3.py -N 2
//               gives
//           my_rank = 0; buffer =  0 0 3 3 2 2 1 1
//           my_rank = 1; buffer =  1 1 0 0 3 3 2 2
//           my_rank = 2; buffer =  2 2 1 1 0 0 3 3
//           my_rank = 3; buffer =  3 3 2 2 1 1 0 0
//
//   NOTE:  depending on your bufferflush settings, the lines above may appear out of order.

#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdlib>
#include <cmath>
#include <vector>

#include "mpi.h"

using namespace std;

int main ( int argc, char *argv[] );
void report_buffer(int& mr, int& np, int b[6]);
		      
int main(int argc, char *argv[])
{
  int my_rank, num_proc, ierr;
  int n;

  ierr = MPI_Init(&argc, &argv);
  ierr = MPI_Comm_size( MPI_COMM_WORLD, &num_proc);
  ierr = MPI_Comm_rank( MPI_COMM_WORLD, &my_rank);

  cout << "Process " << my_rank << " of " << num_proc << "\n";

  // read command line arguments
  if ( my_rank == 0 ) {
    if ( 1 < argc ) {
      n = atoi ( argv[1] );
    } else {
      n = 1;
    }
  }
  ierr = MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);

  // initialize data
  int  *mybuffer;
  mybuffer = new int[n*num_proc];
  for (int i=0; i<n*num_proc; i++) mybuffer[i] = my_rank;

  int left = my_rank - 1;
  int right = my_rank + 1;
  if (right > num_proc-1) right = 0;
  if (left < 0) left = num_proc-1;

  cout << "My rank: " << my_rank << " left " << left << " right " << right << "\n";

  // communicate, perform calculation on rank 0 and record results
  int parity = my_rank % 2;
  int offset = 0;

  for (int i = 0; i<num_proc; i++) {
    int mtag = 1;
    if (parity == 0) {
      ierr = MPI_Send((void *) &mybuffer[offset], n, MPI_INT, right, mtag, MPI_COMM_WORLD);
      offset = offset + n;
      ierr = MPI_Recv((void *) &mybuffer[offset], n, MPI_INT, left, mtag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    } else {
      offset = offset + n;
      ierr = MPI_Recv((void *) &mybuffer[offset], n, MPI_INT, left, mtag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

      ierr = MPI_Send((void *) &mybuffer[offset-n], n, MPI_INT, right, mtag, MPI_COMM_WORLD);
    }
  }

  int sum = 0;
  for (int i = 0; i < n*num_proc; i++) {
    sum += mybuffer[i];
  }

  cout << "Buffer sum is: " << sum << "\n";
  free(mybuffer);
  ierr = MPI_Finalize();
}


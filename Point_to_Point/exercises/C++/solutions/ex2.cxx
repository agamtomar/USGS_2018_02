// Program only runs with 3 processors
// When this program works properly, processes will print
// my_rank = 0; buffer = 0 0 0 2 2 2
// my_rank = 1; buffer = 0 1 1 1 1 1
// my_rank = 2; buffer = 2 1 1 2 2 2
// Depending on your python flush settings, the ordering of the three
// lines above might change, but buffer values and corresponding rank
// should remain unchanged.

#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdlib>
#include <cmath>
#include <random>

#include "mpi.h"

using namespace std;

int main ( int argc, char *argv[] );
void report_buffer(int& mr, int& np, int b[6]);
		      
int main(int argc, char *argv[])
{
  int my_rank, num_proc, ierr;

  ierr = MPI_Init(&argc, &argv);
  ierr = MPI_Comm_size( MPI_COMM_WORLD, &num_proc);
  ierr = MPI_Comm_rank( MPI_COMM_WORLD, &my_rank);

  cout << "Process " << my_rank << " of " << num_proc << "\n";

  int mybuffer[6] = {my_rank, my_rank, my_rank, my_rank, my_rank, my_rank};

  // Processor zero does the following
  if (my_rank == 0) {
    // Receive data from processor 2
    ierr = MPI_Recv((void *) &mybuffer[3], 3, MPI_INT, 2, 2, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    // Send data to processor 1
    ierr = MPI_Send((void *) &mybuffer[0], 1, MPI_INT, 1, 1, MPI_COMM_WORLD);
  }

  // Processor 1 does the following
  if (my_rank == 1) {
    // Receive data from processor 0
    ierr = MPI_Recv((void *) &mybuffer[0], 1, MPI_INT, 0, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    // Send data to processor 2
    ierr = MPI_Send((void *) &mybuffer[1], 2, MPI_INT, 2, 2, MPI_COMM_WORLD);
  }
  
  // Processor 2 does the following
  if (my_rank == 2) {
    // Send data to processor 0
    ierr = MPI_Send((void *) &mybuffer[3], 3, MPI_INT, 0, 2, MPI_COMM_WORLD);

    // Receive data from processor 0
    ierr = MPI_Recv((void *) &mybuffer[1], 2, MPI_INT, 1, 2, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
  }
  report_buffer(my_rank, num_proc, mybuffer);
  
  ierr = MPI_Finalize();
}

void report_buffer(int& mr, int& np, int b[6]) {
  for (int i = 0; i<np; i++) {
    if (i == mr) {
      cout << "My rank = " << mr << " my_buffer = " << b[0] << b[1] << b[2] << b[3] << b[4] << b[5] << "\n";
    }
  }
}


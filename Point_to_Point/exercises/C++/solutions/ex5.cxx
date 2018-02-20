///////////////////////////////////////////
// Write your own Allreduce
// 

#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdlib>
#include <cmath>

#include "mpi.h"

using namespace std;

int main ( int argc, char *argv[] );
double my_all_reduce(int my_rank, int num_proc, double valin);


int main(int argc, char *argv[])
{
  int my_rank, num_proc, ierr;
  int op;

  ierr = MPI_Init(&argc, &argv);
  ierr = MPI_Comm_size( MPI_COMM_WORLD, &num_proc);
  ierr = MPI_Comm_rank( MPI_COMM_WORLD, &my_rank);

  // read command line arguments
  if ( my_rank == 0 ) {
    if ( 1 < argc ) {
      op = atoi ( argv[1] );
    } else {
      op = 1;
    }
  }
  ierr = MPI_Bcast(&op, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Op myop;
  myop = MPI_PROD;
  if (op > 2) myop = MPI_SUM;
  if (op < 1) myop = MPI_SUM;

  double valin = (double) my_rank+1;
  double ans = my_all_reduce(my_rank, num_proc, valin);

  cout << "My rank =" << my_rank << "; reduced value = " << ans << "\n";
		  
  ierr = MPI_Finalize();
}

double my_all_reduce(int my_rank, int num_proc, double valin) {
  // Performs a reduction on valin across all processes in MPI_COMM_WORLD.
  // Reduced value is stored in valout we just use a sum

  int left = my_rank - 1;
  int right = my_rank + 1;
  if (right > num_proc-1) right = 0;
  if (left < 0) left = num_proc-1;


  // communicate, perform calculation on rank 0 and record results
  int parity = my_rank % 2;

  double valout = valin;
  double rval = 0;
  double sval = valin;
  int ierr;
  for (int i = 1; i<num_proc; i++) {
    int mtag = i;
    if (parity == 0) {
      ierr = MPI_Send((void *) &sval, 1, MPI_DOUBLE, right, mtag, MPI_COMM_WORLD);
      ierr = MPI_Recv((void *) &rval, 1, MPI_DOUBLE, left, mtag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    } else {
      ierr = MPI_Recv((void *) &rval, 1, MPI_DOUBLE, left, mtag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
      ierr = MPI_Send((void *) &sval, 1, MPI_DOUBLE, right, mtag, MPI_COMM_WORLD);
    }
      valout = valout+rval;
      sval = rval;
  }
  return valout;
}


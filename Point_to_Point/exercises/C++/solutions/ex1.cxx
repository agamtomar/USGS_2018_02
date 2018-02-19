#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdlib>
#include <cmath>
#include <random>

#include "mpi.h"

using namespace std;

int main ( int argc, char *argv[] );
int initialization(int& mr, int& np, int argc, char *argv[], double * &my_numbers);
		      
int main(int argc, char *argv[])
{
  double *your_numbers, *my_numbers;
  int my_rank, num_proc, ierr;
  
  int n = initialization(my_rank, num_proc, argc, argv, my_numbers);

  cout << "Process " << my_rank << " of " << num_proc << " Problem size: " << n << "\n";

  const string filename = string("Numbers_N")+string(argv[1]);

  // Processor zero does the following
  if (my_rank == 0) {
    ofstream myfile;
    myfile.open(filename, ios::out);
    for (int i = 0; i < n; i++) {
      myfile << my_numbers[i] << "\n";
    }
    myfile.close();
    // barrier here to make sure the file is created before the message exchange
    ierr = MPI_Barrier(MPI_COMM_WORLD);

    // Receive data from processor 1
    your_numbers = new double[n];
    ierr = MPI_Recv((void *) your_numbers, n, MPI_DOUBLE, 1, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

    // perform calculation
    double dprod = 0.0;
    for (int i=0; i<n; i++){
      dprod += my_numbers[i]*your_numbers[i];
    }
    cout << "The dot product is " << dprod << "\n";
  }

  // Processor 1 does the following
  if (my_rank == 1) {
    ierr = MPI_Barrier(MPI_COMM_WORLD);
    ierr = MPI_Send((void *) my_numbers, n, MPI_DOUBLE, 0, 1, MPI_COMM_WORLD);
    
    ofstream myfile;
    myfile.exceptions ( ofstream::failbit | ofstream::badbit );
    try {
      myfile.open(filename, ofstream::out | ofstream::app);
      for (int i = 0; i < n; i++) {
	myfile << my_numbers[i] << "\n";
      }
      myfile.close();
    }
    catch (const ofstream::failure& e) {
      cout << "Error appending to file\n";
    }
  }

  ierr = MPI_Finalize();						     
}

int initialization(int& my_rank, int& num_proc, int argc, char *argv[], double * &a)
{
  int ierr;
  int n;

  ierr = MPI_Init(&argc, &argv);
  ierr = MPI_Comm_size( MPI_COMM_WORLD, &num_proc);
  ierr = MPI_Comm_rank( MPI_COMM_WORLD, &my_rank);

  if ( my_rank == 0 ) {
      if ( 1 < argc ) {
	n = atoi ( argv[1] );
      }
  }

  ierr = MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);
  a = new double[n];
  for (int i = 0; i < n; i++) {
    a[i] = (double) rand() / double (RAND_MAX);
  }
  cout << a[0] << n << "\n";
  return n;
}

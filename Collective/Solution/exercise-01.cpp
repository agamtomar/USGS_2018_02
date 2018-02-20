#include <cstdlib>
#include <cmath>
#include <mpi.h>

using namespace std;

int main(int argc, char** argv) {
    int num_proc;  // The number of processes
    int my_rank; // the rank of this process (ranges from 0-nproc-1) 
    // The next string will hold the node's name.
    // Note that the value of MPI_MAX_PROCESSOR_NAME is defined by the MPI distribution
    char node_name[MPI_MAX_PROCESSOR_NAME]; // string to hold the node's name
    int name_len; // the number of characters in node_name
    int ierr;
    double f, r;
    double *results;

    MPI_Init(NULL, NULL);
    MPI_Comm_size(MPI_COMM_WORLD, &num_proc);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Get_processor_name(node_name, &name_len);

    if (my_rank == 0) {
        printf("  %d MPI Processes are now active.\n", num_proc);
	if ( 1 < argc ) {
	  f = atof ( argv[1] );
	}
    }

    MPI_Bcast(&f, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    r = sin(f)*(double) num_proc+1;
    cout << "Hello from process " << my_rank << " on node " << node_name <<"\n";
    cout << my_rank+1 << "*sin(" << f << ") = " << r << "\n";
    if (my_rank == 0) {
      results = new double[num_proc];
    }
    MPI_Gather(&r, 1, MPI_DOUBLE, results, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);

    if (my_rank == 0) {
      for (int i=0; i<num_proc; ++i) {
	cout << results[i] << "\n";
      }
    }
    
    MPI_Finalize();
  
}


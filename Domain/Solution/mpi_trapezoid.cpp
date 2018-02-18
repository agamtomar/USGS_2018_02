#include <stdio.h>
#include <mpi.h>
double myfunc(double x) {
    return x*x*x;
}

double trapezoid_int(double a, double b, int ntrap) {
    // Integrates f(x) from a to b
    double integral;
    double h;
    double val;
    double x;
    int i;
    h = (b-a)/(ntrap-1); // step size

    val = myfunc(a);
    integral = 0.5*val;
    val = myfunc(b);
    integral = integral+0.5*val;  // add the endpoints

    {
        for (i=1; i<ntrap-1; ++i)
        {
            x = a+i*h;
            integral = integral+myfunc(x);
        }
    }
    integral = integral*h;
    return integral;
}



int main(int argc, char** argv) {
    // Print a hello world message from the main thread.
    double myval;
    double xone, xtwo, myxone, myxtwo;
    double local_integral;
    double global_integral;
    double deltax;
    int ntrap;
    int ntests;
    int my_rank;
    int num_proc;

    MPI_Init(NULL, NULL);
    MPI_Comm_size(MPI_COMM_WORLD, &num_proc);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);


    ntests = 2;
    //ntests = 1000; //uncomment once you are sure your code is working
    ntrap = 1000000/num_proc;

    xone = 1.0;
    xtwo = 2.0;
    // Each rank should integrate between a unique pair of values myxone and myxtwo
    // What should deltax and myxone be to make this work?

    deltax = (xtwo-xone);
    myxone = xone;
    myxtwo = myxone+deltax;

    if (my_rank == 0)
    {
        printf("  Calculating the integral of f(x) = x^3 from %f to %f\n",xone,xtwo);
        printf("  %d times, using %d trapezoids and %d MPI ranks.\n", ntests, ntrap, num_proc);
    }
    for (int i=0; i<ntests; ++i)
    {
        global_integral = 0.0;
        local_integral = trapezoid_int(myxone,myxtwo,ntrap);

        // The call to MPI_Allreduce will sum the value of local_integral across
        // all processes, and store it in global_integral
        MPI_Allreduce(&local_integral, &global_integral, 1, MPI_DOUBLE, MPI_SUM,
                      MPI_COMM_WORLD);
    }


    printf("  Rank %d contributes %f to the global integral value of %f.\n", my_rank, local_integral, global_integral);

}


# Point-to-Point Communication: Exercise 5

Consider the program ex5.{f90,cpp, py, R}.  

Now that you are experienced with handling deadlocks, let's construct a simple parallel algorithm using point-to-point sends and receives.

Ultimately, collective operations are built on a series of point-to-point messages.  Consider our example code, and fill in the body of the function MyAllReduceD.  This function should return the global sum, the global minimum, or global maximum of the input value (my_number) based on the value passed to the op argument when calling the program as:
mpiexec -np N ./ex5.out -op X

X=1 => Summation  
X=2 => Minimum  
X=3 => Maximum  







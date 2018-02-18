PROGRAM MAIN 
    IMPLICIT NONE
    include "mpif.h"
    REAL*8  :: xone,xtwo, myxone,myxtwo, deltax
    REAL*8  :: local_integral, global_integral
    INTEGER :: i, ntrap, ntests, num_proc,my_rank,ierr
    CHARACTER*8 :: proc_string, rank_str
    CHARACTER*9 :: loc_str, glob_str

    Call MPI_INIT( ierr )
    Call MPI_Comm_size(MPI_COMM_WORLD, num_proc,ierr)
    Call MPI_Comm_rank(MPI_COMM_WORLD, my_rank,ierr)
    
    ntests = 2
    ntrap = 1000000/num_proc  !Each rank gets 1000,000/num_proc trapezoids
    !ntests = 1000  !Uncomment this line once  you are sure your code is working


    xone = 1.0
    xtwo = 2.0

    ! Each rank should integrate between a unique pair of values myxone and myxtwo
    ! What should deltax and myxone be to make this work?
    deltax = xtwo-xone
    myxone = xone
    myxtwo = myxone+deltax

    Write(proc_string,'(i8)')num_proc
    Write(rank_str,'(i8)')my_rank
    
    If (my_rank .eq. 0) Then
        Write(6,*)"  Calculating the integral of f(x) = x^3 from 1.0 to 2.0."
        Write(6,*)"  10,000 times, using 1,000,000 trapezoids and " &
            & //trim(adjustl(proc_string))//" MPI ranks."
    Endif

    Do i = 1, ntests
        local_integral = trapezoid_int(myxone,myxtwo,ntrap)
        ! The call to MPI_Allreduce will sum the value of local_integral across
        ! all processes, and store it in global_integral
        Call MPI_Allreduce(local_integral, global_integral, 1, &
            & MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD,ierr)
    Enddo

    Write(loc_str,'(F9.6)')local_integral
    Write(glob_str,'(F9.6)')global_integral

    Write(6,*)"  Rank "//trim(adjustl(rank_str))//" contributes "&
        &//loc_str//" to the global integral value of "//glob_str//"."


Contains

Function myfunc(x) result(xcubed)
    Real*8, Intent(In) :: x
    Real*8 :: xcubed
    xcubed = x*x*x
End Function myfunc

Function trapezoid_int(a,b,ntrap) result(integral)
    !Integrates f(x) from a to b
    Real*8, Intent(In) :: a, b
    Integer, Intent(In) :: ntrap
    Real*8 :: integral, h, val, x
    Integer :: i

    h = (b-a)/(ntrap-1) ! step size

    val = myfunc(a)
    integral = 0.5*val
    val = myfunc(b)
    integral = integral+0.5*val  ! add the endpoints

    DO i = 1, ntrap-2
        x = a+i*h
        integral = integral+myfunc(x)
    Enddo

    integral = integral*h
End Function trapezoid_int
END PROGRAM MAIN


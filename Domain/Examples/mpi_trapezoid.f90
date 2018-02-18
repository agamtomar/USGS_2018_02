PROGRAM MAIN 
    USE mpi
    IMPLICIT NONE

    INTEGER, PARAMETER :: wp = KIND(1.d0)

    REAL(wp)  :: xone,xtwo, myxone,myxtwo, deltax
    REAL(wp)  :: local_integral, global_integral
    INTEGER :: i, ntrap, ntests, num_proc,my_rank,ierr
    CHARACTER(wp) :: proc_string, rank_str
    CHARACTER(len=9) :: loc_str, glob_str

    CALL MPI_INIT( ierr )
    CALL MPI_Comm_size(MPI_COMM_WORLD, num_proc,ierr)
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank,ierr)

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

    WRITE(proc_string,'(i8)')num_proc
    WRITE(rank_str,'(i8)')my_rank

    IF (my_rank .EQ. 0) THEN
        WRITE(6,*)"  Calculating the integral of f(x) = x^3 from 1.0 to 2.0."
        WRITE(6,*)"  10,000 times, using 1,000,000 trapezoids and " &
             & //TRIM(ADJUSTL(proc_string))//" MPI ranks."
    ENDIF

    DO i = 1, ntests
        local_integral = trapezoid_int(myxone,myxtwo,ntrap)
        ! The call to MPI_Allreduce will sum the value of local_integral across
        ! all processes, and store it in global_integral
        CALL MPI_Allreduce(local_integral, global_integral, 1, &
             & MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD,ierr)
    ENDDO

    WRITE(loc_str,'(F9.6)')local_integral
    WRITE(glob_str,'(F9.6)')global_integral

    WRITE(6,*)"  Rank "//TRIM(ADJUSTL(rank_str))//" contributes "&
         &//loc_str//" to the global integral value of "//glob_str//"."


CONTAINS

    FUNCTION myfunc(x) RESULT(xcubed)
        REAL(wp), INTENT(In) :: x
        REAL(wp) :: xcubed
        xcubed = x*x*x
    END FUNCTION myfunc

    FUNCTION trapezoid_int(a,b,ntrap) RESULT(integral)
        !Integrates f(x) from a to b
        REAL(wp), INTENT(In) :: a, b
        INTEGER, INTENT(In) :: ntrap
        REAL(wp) :: integral, h, val, x
        INTEGER :: i

        h = (b-a)/(ntrap-1) ! step size

        val = myfunc(a)
        integral = 0.5*val
        val = myfunc(b)
        integral = integral+0.5*val  ! add the endpoints

        DO i = 1, ntrap-2
            x = a+i*h
            integral = integral+myfunc(x)
        ENDDO

        integral = integral*h
    END FUNCTION trapezoid_int
END PROGRAM MAIN


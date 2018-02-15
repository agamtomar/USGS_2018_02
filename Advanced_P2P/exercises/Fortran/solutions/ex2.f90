!/////////////////////////////////////////
!   Token-Passing Deadlock Exercise
!  
!   Calling sequence:
!       mpiexec -np X ./ex3.out -N Y
!       where Y indicates the number of integers sent during each pass
!       
!   Results:
!       When the program completes successfully, each process will possess a buffer containing
!       num_proc*Y values, with the value of each mpi rank occurring Y times.'
!
!   Example 1:
!           mpiexec -np 3 ./ex3.out -N 1 
!               gives
!           my_rank = 0; buffer =  0 2 1
!           my_rank = 1; buffer =  1 0 2
!           my_rank = 2; buffer =  2 1 0
!       
!   Example 2:
!           mpiexec -np 4 ./ex3.out -N 2
!               gives
!           my_rank = 0; buffer =  0 0 3 3 2 2 1 1
!           my_rank = 1; buffer =  1 1 0 0 3 3 2 2
!           my_rank = 2; buffer =  2 2 1 1 0 0 3 3
!           my_rank = 3; buffer =  3 3 2 2 1 1 0 0
PROGRAM MAIN
    USE ISO_FORTRAN_ENV, ONLY : output_unit
    USE MPI
    IMPLICIT NONE
    CHARACTER*8 :: rank_string, nproc_string, nstring
    INTEGER :: i, ierr, my_rank, num_proc
    INTEGER :: nnum=1, left, right, nbuffer, mtag, offset
    INTEGER, ALLOCATABLE :: buffer(:)
    INTEGER :: irqs(2)
    INTEGER :: mstat(MPI_STATUS_SIZE, 2)
    CALL INITIALIZATION()


    left = my_rank -1
    right = my_rank +1
    IF ( right   .gt. num_proc-1) right  = 0
    IF ( left   .lt.     0     ) left = num_proc-1

    offset = 1


    DO i = 1, num_proc -1
        mtag =i

        CALL MPI_ISend(buffer(offset), nnum, MPI_INTEGER, right, mtag, MPI_COMM_WORLD , irqs(1), ierr)
        offset = offset + nnum
        CALL MPI_IRecv(buffer(offset), nnum, MPI_INTEGER,  left, mtag, MPI_COMM_WORLD, irqs(2), ierr)

        CALL MPI_WAITALL(2,irqs,mstat,ierr)

    ENDDO


    CALL CHECK_SUM()

    IF (num_proc*nnum .le. 20) THEN
        CALL REPORT_BUFFER()
    ENDIF

    CALL MPI_Finalize(ierr) 

    

CONTAINS

    SUBROUTINE CHECK_SUM()
        IMPLICIT NONE
        INTEGER :: j
        ! If you ever need processes to print in order, a barrier can be helpful (but costly)
        DO j = 0, num_proc-1
            IF (j .eq. my_rank) THEN
                Write(output_unit,'(a,i1,a,i)')'my_rank = ', my_rank, '; sum = ', sum(buffer)
            ENDIF
            CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
        ENDDO

    END SUBROUTINE CHECK_SUM

    SUBROUTINE REPORT_BUFFER()
        IMPLICIT NONE
        INTEGER :: j
        ! If you ever need processes to print in order, a barrier can be helpful (but costly)
        DO j = 0, num_proc-1
            IF (j .eq. my_rank) THEN
                Write(output_unit,'(a,i1,a,20i2)')'my_rank = ', my_rank, '; buffer = ', buffer
            ENDIF
            CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
        ENDDO

    END SUBROUTINE REPORT_BUFFER


    SUBROUTINE Parse_Command_line(n)
            IMPLICIT NONE
            INTEGER, INTENT(INOUT)   :: n
            INTEGER :: nc                    ! Number of command-line arguments
            INTEGER :: i                    
            CHARACTER(len=1024) :: argname  ! Argument key
            CHARACTER(len=1024) :: val      ! Argument value

            nc = command_argument_count()
            DO i=1,nc,2
                    CALL get_command_argument(i, argname)
                    CALL get_command_argument(i+1, val)
                    SELECT CASE(argname)
                            CASE('-N')
                                    READ(val, '(I8)') n
                            CASE DEFAULT
                                    WRITE(output_unit,'(a)') ' '
                                    WRITE(output_unit,'(a)') &
                                    ' Unrecognized option: '// trim(argname)
                    END SELECT
            ENDDO

    END SUBROUTINE Parse_Command_Line


    SUBROUTINE Initialization()
        IMPLICIT NONE
        !//////////////////////////////
        ! Initialize MPI
        CALL MPI_INIT( ierr )
        CALL MPI_Comm_size(MPI_COMM_WORLD, num_proc,ierr)
        CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank,ierr)

        !/////////////////////////////////////////
        ! Rank 0 reads command-line arguments and 
        ! broadcasts to other ranks.
        IF (my_rank .eq. 0) CALL Parse_Command_Line(nnum)
        CALL MPI_Bcast(nnum,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)


        !/////////////////////////////////////////////
        ! Create an array of length N*num_proc on each process
        ALLOCATE(buffer(1:nnum*num_proc))
        buffer(:) = my_rank


    END SUBROUTINE Initialization

END PROGRAM MAIN

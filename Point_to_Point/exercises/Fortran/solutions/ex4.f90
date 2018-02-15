
PROGRAM MAIN
    USE ISO_FORTRAN_ENV, ONLY : output_unit
    USE MPI
    IMPLICIT NONE
    CHARACTER*8 :: rank_string, nproc_string, nstring
    INTEGER :: i, ierr, my_rank, num_proc, num_iter
    INTEGER :: mstatus(MPI_STATUS_SIZE)
    INTEGER :: nnum=128, left, right, nbuffer, mtag, offset, parity
    REAL*8, Allocatable :: f(:)
    REAL*8 :: fpars(2), fullsum, mysum

    CALL INITIALIZATION()

    fullsum=1.0d0
    num_iter = 10
    DO i = 1, num_iter
        IF (my_rank .eq. 0) THEN
            CALL GET_NEW_PARAMS(i,fpars,fullsum)  ! Compute function parameters for the  next iteration
        ENDIF
        CALL MPI_Bcast(fpars,2,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)

        CALL GEN_DATA(f,fpars)  ! Each process generates its new function
        mysum = sum(f)          ! And computes its integral

        ! Broadcast the integral, and then rank 0 uses this to compute new function parameters
        CALL MPI_AllReduce(mysum,fullsum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr) 
    ENDDO

    IF (my_rank .eq. 0) Write(output_unit,*)'Complete!'

    CALL MPI_Finalize(ierr) 


CONTAINS

    SUBROUTINE GEN_DATA(func,pars)
        IMPLICIT NONE
        REAL*8, INTENT(INOUT) :: func(:)
        REAL*8, INTENT(IN) :: pars(2)
        REAL*8 :: dx, x
        REAL*8 :: pi = 3.1415926535897932384626433832795028841972d0
        INTEGER :: i
        ! Each process executes this subroutine
        ! and generates a unique f(x)
        dx = 2*pi/nnum
        x = 0.0d0
        DO i = 1, nnum
            f(i) = pars(1)*x**(pars(2)/(1+my_rank))
            x = x+dx
        ENDDO

    END SUBROUTINE GEN_DATA

    SUBROUTINE GET_NEW_PARAMS(ind, amp, val)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: ind
        REAL*8, INTENT(INOUT) :: amp(2)
        REAL*8, INTENT(IN) :: val
        ! Only rank 0 sets the function parameters
        amp(1)=1.0/val
        amp(2)=ind*2
        
    END SUBROUTINE GET_NEW_PARAMS



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
        ALLOCATE(f(1:nnum))
        f(:) = 0


    END SUBROUTINE Initialization

END PROGRAM MAIN


PROGRAM MAIN
    USE ISO_FORTRAN_ENV, ONLY : output_unit
    USE MPI
    IMPLICIT NONE
    CHARACTER*8 :: rank_string, nproc_string, nstring
    INTEGER :: i, ierr, my_rank, num_proc, num_iter

    INTEGER :: op=1,  redux_op
    REAL*8 :: my_number, reduced
    CALL INITIALIZATION()



    my_number = my_rank
    reduced = 0.0d0

    IF (op .gt. 3) op = 1
    IF (op .lt. 0) op = 1

    IF (op .eq. 1) redux_op = MPI_SUM
    IF (op .eq. 2) redux_op = MPI_MIN
    IF (op .eq. 3) redux_op = MPI_MAX

    Call MyAllReduceD(my_number,reduced,redux_op)

    CALL Print_Vals()

    CALL MPI_Finalize(ierr) 


CONTAINS

    SUBROUTINE Print_Vals()
        IMPLICIT NONE
        INTEGER :: j
        DO j = 0, num_proc-1
            IF (j .eq. my_rank) THEN
                Write(output_unit,'(a,i2,a,F12.4)')'my_rank = ', my_rank, '; reduced value = ', reduced
            ENDIF
            CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
        ENDDO

    END SUBROUTINE PRINT_VALS

    SUBROUTINE MyAllReduceD(valin, valout, rop)
        ! Performs a reduction on valin across all processes in MPI_COMM_WORLD.
        ! Reduced value is stored in valout
        ! Reduction operation is controlled by rop:
        ! rop = MPI_SUM => Summation
        ! rop = MPI_MIN => Min
        ! rop = MPI_MAX => Max
        IMPLICIT NONE
        REAL*8 , Intent(In)  :: valin
        REAL*8 , Intent(Out) :: valout
        INTEGER, Intent(In)  :: rop
        REAL*8 ::  rval, sval
        INTEGER :: parity, mtag, left, right, ierr
        INTEGER :: mstatus(MPI_STATUS_SIZE)



    END SUBROUTINE MyAllReduceD

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
                            CASE('-op')
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
        IF (my_rank .eq. 0) CALL Parse_Command_Line(op)
        CALL MPI_Bcast(op,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)



    END SUBROUTINE Initialization

END PROGRAM MAIN

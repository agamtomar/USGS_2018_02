PROGRAM MAIN
    USE ISO_FORTRAN_ENV, ONLY : output_unit
    USE MPI
    IMPLICIT NONE
    CHARACTER*8 :: rank_string, nproc_string, nstring
    INTEGER :: i, ierr, mstatus, my_rank, num_proc
    INTEGER :: nnum=1
    REAL*8  :: the_dot_product = 0.0D0
    REAL*8, ALLOCATABLE :: your_numbers(:), my_numbers(:)
    CHARACTER*120 :: fprefix = 'Numbers_N', fname


    CALL INITIALIZATION()


    IF (my_rank .eq. 0) THEN

        OPEN(unit=15,file=fname,form='formatted', status='replace',access='stream')
        DO i = 1, nnum
            WRITE(15,'(i8,e12.3)')i, my_numbers(i)
        ENDDO
        CLOSE(15)

        ALLOCATE(your_numbers(1:nnum))
        your_numbers = 0
        CALL MPI_Recv(your_numbers, nnum, MPI_DOUBLE_PRECISION, 1, 1, MPI_COMM_WORLD, mstatus, ierr)
        
        CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
        DO i = 1, nnum
            the_dot_product = the_dot_product+your_numbers(i)*my_numbers(i)
        ENDDO

        WRITE(output_unit,*)'The product is: ', the_dot_product
    ELSE

        CALL MPI_Send(my_numbers,nnum,MPI_DOUBLE_PRECISION,0,my_rank,MPI_COMM_WORLD,ierr)
        CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

        OPEN(unit=15,file=fname,form='formatted', status='old',access='stream',position='append')
        DO i = 1, nnum
            WRITE(15,'(i,e12.3)')my_rank*nnum+i, my_numbers(i)
        ENDDO
        CLOSE(15)

    ENDIF


    CALL MPI_Finalize(ierr) 

    

CONTAINS


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

        ! ~~~ Set the output filename ~~~
        WRITE(nstring,'(i8)')nnum
        fname = TRIM(fprefix)//ADJUSTL(TRIM(nstring))


        !/////////////////////////////////////////////
        ! Generate a sequence of N random numbers on 
        ! each process.
        ALLOCATE(my_numbers(1:nnum))
        CALL RANDOM_NUMBER(my_numbers)


    END SUBROUTINE Initialization

END PROGRAM MAIN

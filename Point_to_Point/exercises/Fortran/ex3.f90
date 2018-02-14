PROGRAM MAIN
    USE ISO_FORTRAN_ENV, ONLY : output_unit
    USE MPI
    IMPLICIT NONE
    CHARACTER*8 :: rank_string, nproc_string, nstring
    INTEGER :: i, ierr, mstatus, my_rank, num_proc
    INTEGER :: nnum=1, left, right, nbuffer, mtag, offset, parity
    INTEGER, ALLOCATABLE :: buffer(:)


    CALL INITIALIZATION()


    left = my_rank -1
    right = my_rank +1
    IF ( right   .gt. num_proc-1) right  = 0
    IF ( left   .lt.     0     ) left = num_proc-1

    offset = 1
    mtag = 1

    parity = MOD(my_rank,2)

    Do i = 0, num_proc-1
        IF (my_rank .eq. i) THEN
            write(6,*)'info: ', my_rank, left, right
        ENDIF
        Call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    ENDDO


    DO i = 0, num_proc -1
        IF (parity .eq. MOD(i,2)) THEN
            Write(6,*)'send: ', my_rank, right, i
            CALL MPI_Send(buffer(offset), nnum, MPI_INTEGER, right, mtag, MPI_COMM_WORLD ,ierr)
            offset = offset + nnum
            Write(6,*)'rec2: ', my_rank, left, i
            CALL MPI_Recv(buffer(offset), nnum, MPI_INTEGER,  left, mtag, MPI_COMM_WORLD, mstatus, ierr)
        ELSE
            Write(6,*)'recv: ', my_rank, left, i
            offset = offset + nnum
            CALL MPI_Recv(buffer(offset)     , nnum, MPI_INTEGER,  left, mtag, MPI_COMM_WORLD, mstatus, ierr)
            Write(6,*)'snd2: ', my_rank, right, i
            CALL MPI_Send(buffer(offset-nnum), nnum, MPI_INTEGER, right, mtag, MPI_COMM_WORLD ,ierr)
        ENDIF
    ENDDO

    Write(6,*)'The sum: ', sum(buffer)

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


        !/////////////////////////////////////////////
        ! Create an array of length N*num_proc on each process
        ALLOCATE(buffer(1:nnum*num_proc))
        buffer(:) = my_rank


    END SUBROUTINE Initialization

END PROGRAM MAIN

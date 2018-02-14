PROGRAM MAIN
    USE ISO_FORTRAN_ENV, ONLY : output_unit
    USE MPI
    IMPLICIT NONE
    CHARACTER*8 :: rank_string, nproc_string, nstring
    INTEGER :: i, ierr, mstatus, my_rank, num_proc
    INTEGER  :: buffer(1:6)

    ! When this program works properly, processes will print
    ! my_rank = 0; buffer = 0 0 0 2 2 2
    ! my_rank = 1; buffer = 0 1 1 1 1 1
    ! my_rank = 2; buffer = 2 1 1 2 2 2

    CALL INITIALIZATION()

    ! Buffer space for sending and receiving data
    buffer(:) = my_rank

    IF (my_rank .eq. 0) THEN
        ! Receive from rank 2, send to rank 1
        CALL MPI_RECV( buffer(4), 2, MPI_INTEGER, 2,2, MPI_COMM_WORLD, mstatus, ierr)
        CALL MPI_SEND( buffer(1), 1, MPI_INTEGER, 1,1, MPI_COMM_WORLD,ierr)
    ENDIF

    IF (my_rank .eq. 1) THEN
        ! Receive from rank 1, send to rank 2
        CALL MPI_RECV( buffer(1), 1, MPI_INTEGER, 0,1, MPI_COMM_WORLD, mstatus, ierr)
        CALL MPI_SEND( buffer(2), 2, MPI_INTEGER, 2,1, MPI_COMM_WORLD,ierr)        
    ENDIF

    IF (my_rank .eq. 2) THEN
        ! Receive from rank 1, send to rank 0
        CALL MPI_SEND( buffer(4), 3, MPI_INTEGER, 0 ,2, MPI_COMM_WORLD,ierr)
        CALL MPI_RECV( buffer(2), 2, MPI_INTEGER, 1,2, MPI_COMM_WORLD, mstatus, ierr)
    ENDIF

    CALL REPORT_BUFFER()


    CALL MPI_Finalize(ierr) 
  

CONTAINS

    SUBROUTINE REPORT_BUFFER()
        IMPLICIT NONE
        INTEGER :: i
        ! If you ever need processes to print in order, a barrier can be helpful (but costly)
        DO i = 0, num_proc-1
            IF (i .eq. my_rank) THEN
                Write(output_unit,'(a,i1,a,6i1)')'my_rank = ', my_rank, '; buffer = ', buffer
            ENDIF
            CALL MPI_Barrier(MPI_COMM_WORLD,ierr)
        ENDDO
    END SUBROUTINE REPORT_BUFFER


    SUBROUTINE Initialization()
        IMPLICIT NONE
        INTEGER :: ierr
        !//////////////////////////////
        ! Initialize MPI
        CALL MPI_INIT( ierr )
        CALL MPI_Comm_size(MPI_COMM_WORLD, num_proc,ierr)
        CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank,ierr)
        IF (num_proc .lt. 3) THEN
            IF (my_rank .eq. 0) THEN
                WRITE(output_unit,*)'Error:  number of MPI ranks must be at least 3'
                WRITE(output_unit,*)'        number specified: ', num_proc
            ENDIF
            CALL MPI_FINALIZE(ierr)
            STOP
        ENDIF
    END SUBROUTINE Initialization

END PROGRAM MAIN

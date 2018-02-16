PROGRAM MAIN

    USE mpi                     ! Using the module is better then the include file

    IMPLICIT NONE
    
    CHARACTER*8 :: rank_string, nproc_string
    CHARACTER(MPI_MAX_PROCESSOR_NAME) :: node_name
    INTEGER :: i, ierr, num_proc, my_rank, name_len

    CALL MPI_INIT(ierr)
    CALL MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierr)
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    CALL MPI_Get_processor_name(node_name, name_len, ierr)


    WRITE(rank_string,'(i8)') my_rank
    WRITE(nproc_string,'(i8)') num_proc

    IF (my_rank == 0) THEN
        WRITE(6,*)"  "//TRIM(ADJUSTL(nproc_string))//" MPI Processes are now active."
    ENDIF
    ! Execution of the parallel region pauses at the barrier and resumes once all MPI processes have
    ! reached the barrier.
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

    ! Consider the loop below.  Where can we place another call to mpi_barrier to ensure
    ! that the MPI tasks print their 'hello' in ascending order based on rank? 
    DO i = 0, num_proc
        CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
        IF (my_rank .EQ. i) THEN
            WRITE(6,*)"  Hello from node "//TRIM(node_name)//" rank "// &
                 & TRIM(ADJUSTL(rank_string))//" out of "//TRIM(ADJUSTL(nproc_string))//" processors." 
        ENDIF
    ENDDO
    CALL MPI_Finalize(ierr)
END PROGRAM MAIN

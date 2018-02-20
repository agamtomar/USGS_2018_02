PROGRAM MAIN

    USE mpi                     ! Using the module is better then the include file

    IMPLICIT NONE
    
    CHARACTER(len=8) :: rank_string, nproc_string
    CHARACTER(MPI_MAX_PROCESSOR_NAME) :: node_name
    INTEGER :: i, ierr, num_proc, my_rank, name_len, nc
    REAL*8 :: f, r
    REAL*8, allocatable :: results(:)
    CHARACTER(len=1024) :: val

    CALL MPI_INIT(ierr)
    CALL MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierr)
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    CALL MPI_Get_processor_name(node_name, name_len, ierr)


    WRITE(rank_string,'(i8)') my_rank
    WRITE(nproc_string,'(i8)') num_proc

    IF (my_rank == 0) THEN
        nc = command_argument_count()
        call get_command_argument(1, val)
        read(val, '(F8.5)') f
        write(*,*) "N = ", f
    ENDIF

    call MPI_Bcast(f, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)
    ! Computation and IO for all
    r = dble(my_rank+1)*sin(f)
    write(*,*) "Hello from process ", my_rank, " on node ", node_name
    write(*,*) my_rank+1,"*sin(", f, ") = ", r

    if (my_rank == 0) then
        allocate(results(num_proc))
    end if
    
    call MPI_Gather(r, 1, MPI_DOUBLE_PRECISION, results, 1, &
         MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

    if (my_rank == 0) then
        Do i=1,num_proc
            write(*,*) results(i)
        end Do
    end if
    
    CALL MPI_Finalize(ierr)
END PROGRAM MAIN

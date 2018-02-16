PROGRAM MAIN
	Implicit None
    include "mpif.h"
	Character*8 :: rank_string, nproc_string
    Character(MPI_MAX_PROCESSOR_NAME) :: node_name
    Integer :: ierr, num_proc, my_rank, name_len

    Call MPI_INIT( ierr )
    Call MPI_Comm_size(MPI_COMM_WORLD, num_proc,ierr)
    Call MPI_Comm_rank(MPI_COMM_WORLD, my_rank,ierr)
    Call MPI_Get_processor_name(node_name, name_len,ierr)


    Write(rank_string,'(i8)')my_rank
    Write(nproc_string,'(i8)')num_proc

    If (my_rank .eq. 0) Then
        !Sometimes we might like for only a single thread to report certain information.
        !This avoids redundant output.
        Write(6,*)"  "//trim(adjustl(nproc_string))//" MPI processes are now active."
    Endif
    Write(6,*)"  Hello from node "//trim(node_name)//" rank "// &
        & trim(adjustl(rank_string))//" out of "//trim(adjustl(nproc_string))//" processors." 

    Call MPI_Finalize(ierr)
END PROGRAM MAIN

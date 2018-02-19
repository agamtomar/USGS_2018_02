PROGRAM MAIN
	Implicit None
    include "mpif.h"
	Character*8 :: rank_string, nproc_string, rank_string2
    Integer :: i, ierr, num_proc, dest, source ,tag,nvals
    Integer :: stoken, rtoken, my_rank

    Call MPI_INIT( ierr )
    Call MPI_Comm_size(MPI_COMM_WORLD, num_proc,ierr)
    Call MPI_Comm_rank(MPI_COMM_WORLD, my_rank,ierr)


    Write(rank_string,'(i8)')my_rank
    Write(nproc_string,'(i8)')num_proc

    stoken = 3*my_rank+1
    rtoken = -1

    If (my_rank .eq. 0) Then
        Write(6,*)nproc_string//" MPI Processes are now active."
    Endif
    Call MPI_BARRIER(MPI_COMM_WORLD,ierr)


    ! In this example, ranks 0 and num_proc-1 each send each other a single integer
    ! We use blocking Sends and Recvs.  To avoid deadlock, rank num_proc-1 sends first, 
    ! then receives.  Rank 0, on the other hand, receives first, and then sends

    If (my_rank .eq. (num_proc-1)) Then
        ! The highest rank sends a message to rank 0

        dest = 0  ! The destination rank
        tag = my_rank ! A unique tag for this message (destination should expect same tag)
        nvals = 1 ! The number of values we are transmitting

        !We send the value of the token variable
        Call MPI_Send(stoken, nvals, MPI_INTEGER, dest,tag, &
            & MPI_COMM_WORLD,ierr)

        ! Next, num_proc-1 receives a message from rank 0
        source = 0  ! The source of the message
        tag = 0     ! The message tag (must match the tag  set by sender)
        nvals = 1            ! The number of values we will receive

        !The value of token will be overwritten by whatever num_proc-1 sends.
        Call MPI_Recv(rtoken, nvals, MPI_INTEGER, source, tag, &
            & MPI_COMM_WORLD, MPI_STATUS_IGNORE,ierr)
        Write(6,'(a,i0,a,i0,a,i0,a)')"  Rank ", my_rank, " has received the token ",rtoken, &
                   " from rank ", source, "."
    Endif

    If (my_rank .eq. 0) Then
        !Rank 0 receives a message from rank num_proc-1

        source = num_proc-1  ! The source of the message
        tag = num_proc-1     ! The message tag (must match the tag  set by sender)
        nvals = 1            ! The number of values we will receive

        !The value of token will be overwritten by whatever num_proc-1 sends.
        Call MPI_Recv(rtoken, nvals, MPI_INTEGER, source, tag, &
            & MPI_COMM_WORLD, MPI_STATUS_IGNORE,ierr)

        !Rank 0 now sends a message to process num_proc -1

        dest = num_proc-1  ! The destination rank
        tag = my_rank ! A unique tag for this message (destination should expect same tag)
        nvals = 1 ! The number of values we are transmitting

        !We send the value of the token variable
        Call MPI_Send(stoken, nvals, MPI_INTEGER, dest,tag, &
            & MPI_COMM_WORLD,ierr)

        Write(6,'(a,i0,a,i0,a,i0,a)')"  Rank ", my_rank, " has received the token ",rtoken, &
                   " from rank ", source, "."
    Endif

    Call MPI_Finalize(ierr)
END PROGRAM MAIN

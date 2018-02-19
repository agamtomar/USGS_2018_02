PROGRAM MAIN
	Implicit None
    include "mpif.h"
	Character*8 :: rank_string, nproc_string, rank_string2
    Integer :: i, ierr, num_proc, dest, source ,tag,nvals
    Integer :: stoken, rtoken, my_rank
    INTEGER :: reqs(2) !Array of interrupt requests used for Isend and IRecv
    INTEGER :: mstat(MPI_STATUS_SIZE, 2) !Status array used for MPI Wait-All
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
    ! We use ISends and IRecvs, allowing both sends and receives to be posted
    ! simultaneously.


    If (my_rank .eq. (num_proc-1)) Then
        ! num_proc-1 posts a non-blocking send to rank 0

        dest = 0  ! The destination rank
        tag = my_rank ! A unique tag for this message (destination should expect same tag)
        nvals = 1 ! The number of values we are transmitting

        !We send the value of the token variable
        Call MPI_ISend(stoken, nvals, MPI_INTEGER, dest,tag, &
            & MPI_COMM_WORLD,reqs(1),ierr)

        ! Next, num_proc-1 posts a non-blocking receive from rank 0
        source = 0  ! The source of the message
        tag = 0     ! The message tag (must match the tag  set by sender)
        nvals = 1            ! The number of values we will receive

        !The value of token will be overwritten by whatever num_proc-1 sends.
        Call MPI_IRecv(rtoken, nvals, MPI_INTEGER, source, tag, &
            & MPI_COMM_WORLD, reqs(2),ierr)

        CALL MPI_Waitall(2, reqs, mstat, ierr)
        Write(6,'(a,i0,a,i0,a,i0,a)')"  Rank ", my_rank, " has received the token ",rtoken, &
                   " from rank ", source, "."
    Endif

    If (my_rank .eq. 0) Then
        !Rank 0 posts a non-blocking send to process num_proc -1

        dest = num_proc-1  ! The destination rank
        tag = my_rank ! A unique tag for this message (destination should expect same tag)
        nvals = 1 ! The number of values we are transmitting

        !We send the value of the token variable
        Call MPI_ISend(stoken, nvals, MPI_INTEGER, dest,tag, &
            & MPI_COMM_WORLD,reqs(1), ierr)

        !Rank 0 posts a non-blocking receive from rank num_proc-1

        source = num_proc-1  ! The source of the message
        tag = num_proc-1     ! The message tag (must match the tag  set by sender)
        nvals = 1            ! The number of values we will receive

        !The value of token will be overwritten by whatever num_proc-1 sends.
        Call MPI_IRecv(rtoken, nvals, MPI_INTEGER, source, tag, &
            & MPI_COMM_WORLD, reqs(2),ierr)



        CALL MPI_Waitall(2, reqs, mstat, ierr)
        Write(6,'(a,i0,a,i0,a,i0,a)')"  Rank ", my_rank, " has received the token ",rtoken, &
                   " from rank ", source, "."
    Endif

    Call MPI_Finalize(ierr)
END PROGRAM MAIN

!/////////////////////////////////////////////////////////////////
!
!          MPI Solution:  Collatz Sequence
!
PROGRAM MAIN 
    IMPLICIT NONE
    include "mpif.h"

    Integer :: j
    Integer :: jstart
    Integer :: jend
    Integer :: jlength
    Integer :: jmax              ! The maximum number whose sequence-length is calculated by any processor
    Integer :: njlocal           ! How many numbers each process checks the sequence length for.
    Integer :: max_length_local  ! The maximum sequence length calculated locally
    Integer :: max_length_global ! The maximum sequence length calculated globally
    Integer :: max_num_local     ! The number with the longest sequence-length found locally
    Integer :: max_num_global    ! The number possessing the global max sequence length
    Integer :: my_rank
    Integer :: num_proc
    Integer :: ierr



    Call MPI_INIT( ierr )
    Call MPI_Comm_size(MPI_COMM_WORLD, num_proc,ierr)
    Call MPI_Comm_rank(MPI_COMM_WORLD, my_rank,ierr)
    

    njlocal = 1000000
    jmax = njlocal*num_proc

    
    If (my_rank .eq. 0) Then
       Write(6,'(a,i0,a,i0,a)')"  Calculating Collatz-sequence lengths over the interval [1,", jmax, &
                "] using ", num_proc, " processes."
    Endif

    jstart = 1+my_rank*njlocal
    jend = jstart+njlocal-1

    max_length_local=1
    max_num_local=1
    Do j = jstart, jend
        jlength = collatz_length(j)
        If (jlength .gt. max_length_local) Then 
            max_num_local=j
            max_length_local=jlength
        Endif

    Enddo
    max_length_global=0
    max_num_global=0

    Call MPI_Allreduce(max_length_local, max_length_global, 1, &
                       MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD,ierr)

    If (max_length_global .ne. max_length_local) Then
        max_num_local = -1
    Endif

    Call MPI_Allreduce(max_num_local, max_num_global, 1, &
                       MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD,ierr)


    If (my_rank .eq. 0) Then
        Write(6,'(a,i0,a,i0,a)') "  The longest Collatz-sequence in the interval [ 1,", jmax, &
                "] has length ",max_length_global, "."
        Write(6,'(a,i0,a)')"  This sequence-length occurs for the number ",max_num_global, "."

    Endif
    Call MPI_Finalize(ierr)
Contains

    Function collatz_length(n) result(length)
        Integer, Intent(In) :: n
        Integer :: i, length
        length = 1
        i = n
        Do While (i .gt. 1)
            length = length+1
            If ( MOD(i,2) .eq. 0) THEN
                i = i/2
            Else
                i = 3*i+1
            Endif
        Enddo
        
    End Function collatz_length



END PROGRAM MAIN


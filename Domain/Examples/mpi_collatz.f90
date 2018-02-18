!/////////////////////////////////////////////////////////////////
!
!          MPI Exercise:  Collatz Sequence
!
!          Modify the program below so that it reports the number
!          with the largest collatz sequence length, and its length,
!          occuring in the interval [1, N Million] where N is the 
!          number of MPI ranks.
PROGRAM MAIN 
    use mpi

    IMPLICIT NONE

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
    ! (1) Change jstart and jend so that all rank compute a sequence lengths for numbers
    !     in a unique portion of the interval [1,jmax]
    jstart = 1
    jend = jmax

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





    ! (2) Use an Allreduce to get the global maximum of the sequence lengths
    Write(6,'(a,i0,a,i0,a)') "  The longest Collatz-sequence in the interval [ 1,", jmax, &
            "] has length ",max_length_global, "."
    ! (3;  A little bit trickier...) Use an Allreduce to assign the number associated with that sequence length to max_num_global
    Write(6,'(a,i0,a)')"  This sequence-length occurs for the number ",max_num_global, "."


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


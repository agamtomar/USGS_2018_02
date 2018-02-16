!/////////////////////////////////////////
!   Collatz Sequence Computation
!
!   Records the Collatz Sequence lengths for all numbers in the range [1,N] 
!   with sequence lengths greater than the threshold T.  
!   Calling example:
!       mpiexec -np 10 ./ex3.out -N 100 -T 15   (records sequence lengths greater than 15 in range [1,100])
!
!   The calculation of sequence lengths is carried out in parallel, and 
!   results are communicated to process 0.
PROGRAM MAIN
    USE ISO_FORTRAN_ENV, ONLY : output_unit
    USE MPI
    IMPLICIT NONE
    CHARACTER*8 :: rank_string, nproc_string, nstring
    INTEGER :: i, j, ierr, my_rank, num_proc
    INTEGER :: nnum=1, threshold=0, left, right, nbuffer, mtag, offset
    INTEGER, ALLOCATABLE :: lengths(:,:), all_lengths(:,:), counts(:)
    INTEGER :: irqs(2), length, icount
    INTEGER, ALLOCATABLE :: mstat(:,:), rirqs(:)
    INTEGER :: my_min, my_max, dnum, ns
    INTEGER :: modcheck, sirq
    LOGICAL :: report =.true.
    CALL INITIALIZATION()

    IF (nnum .lt. num_proc) nnum = num_proc

    IF (my_rank .eq. 0) THEN
        Write(output_unit,*)''
        Write(output_unit,'(a,i8,a,i8,a)')' Calculating Collatz Sequence lengths greater than ', &
                                            threshold, ' for all numbers in the range [1,',nnum,']'
    ENDIF



    ! This program prints a list of all numbers in the range [1,N] whose Collatz-sequence
    ! length is greater than the threshold value (defaults to 10)

    ! Split up the work among each process
    
    dnum = nnum/num_proc
    modcheck = MOD(nnum,num_proc)
    my_min = my_rank*dnum+1
    IF (my_rank .lt. modcheck) THEN
        my_min = my_min+my_rank
        my_max = my_min+ dnum
    ELSE
        my_min = my_min+modcheck
        my_max = my_min+dnum-1
    ENDIF

    ! Calculate the sequence lengths, recording those greater than threshold.
    Allocate(lengths(2,my_max-my_min+1))
    lengths = -1
    icount = 0
    Do i = my_min, my_max
        length = Collatz_Length(i)
        IF (length .gt. threshold) THEN
            icount=icount+1
            lengths(1,icount) = i
            lengths(2,icount) = length
        ENDIF
    Enddo

    !////////////////////////////////////////////////
    ! Set report = True to compare results from each proces
    ! to Rank 0's collated results at the end.
    IF (report) THEN
        IF (my_rank .eq. 0) THEN
            WRITE(output_unit,*)' '
            WRITE(output_unit,*)'Displaying results local to each process: '
            WRITE(output_unit,*)' '
        ENDIF
        DO j = 0, num_proc-1
            IF (j .eq. my_rank) THEN
                Do i = 1, icount
                    IF (lengths(2,i) .gt. -1) THEN
                        Write(output_unit,*)'i, sequence length: ', lengths(1,i), lengths(2,i)
                    ENDIF
                Enddo
            ENDIF

            CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
        ENDDO
    ENDIF

    !///////////////////////////////////////////////////////////////////////////////
    !
    ! Every process sends icount (number of sequences with lengths > threshold) to rank 0
    !
    IF (my_rank .eq. 0) THEN
        ALLOCATE(counts(0:num_proc-1))
        ALLOCATE(rirqs(1:num_proc-1))
        counts(0) = icount
        DO i = 1, num_proc-1
            CALL MPI_IRecv(counts(i),1,MPI_INTEGER,i,i,MPI_COMM_WORLD , rirqs(i), ierr)
        ENDDO
        CALL MPI_WAITALL(num_proc-1,rirqs,mstat,ierr)
    ELSE
        CALL MPI_ISEND(icount,1, MPI_INTEGER,0,my_rank,MPI_COMM_WORLD,sirq,ierr)
        CALL MPI_WAITALL(num_proc-1,rirqs,mstat,ierr)
    ENDIF


    !////////////////////////////////////////////////////////////////////
    !
    ! Next, everyone with icount >0 sends their sequence info to rank 0
    !
    IF (my_rank .eq. 0) THEN
        ns = sum(counts)
        ALLOCATE(all_lengths(2,ns))
        offset=1
        IF (icount .gt. 0) THEN
            all_lengths(1:2,offset:offset+icount-1) = lengths(1:2, 1:icount)
            offset = offset+icount
        ENDIF

        DO i = 1, num_proc-1
            IF (counts(i) .gt. 0) THEN
                CALL MPI_IRecv(all_lengths(1,offset),2*counts(i),MPI_INTEGER,i,i,MPI_COMM_WORLD , rirqs(i), ierr)
                offset = offset+counts(i)
            ENDIF
        ENDDO

        CALL MPI_WAITALL(num_proc-1,rirqs,mstat,ierr)
    ELSE
        IF (icount .gt. 0) THEN
            CALL MPI_ISEND(lengths(1,1),2*icount, MPI_INTEGER,0,my_rank,MPI_COMM_WORLD,sirq,ierr)
            CALL MPI_WAIT(sirq,mstat,ierr)
        ENDIF
    ENDIF


    !//////////////////////////////////////////////////////////////////////
    !
    !  Rank 0 displays the results
    !
    CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
    IF (my_rank .eq. 0) THEN
        CALL sleep(1)  ! Give stdout a second to catch up
        WRITE(output_unit,*)' '
        WRITE(output_unit,*)'Displaying collated results from process 0: '
        WRITE(output_unit,*)' '

        DO i = 1, ns
            WRITE(output_unit,*)'i, sequence length: ', all_lengths(1,i), all_lengths(2,i)
        ENDDO
    ENDIF

    CALL MPI_Finalize(ierr) 

    

CONTAINS



    FUNCTION collatz_length(n) result(length)
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
        
    END FUNCTION collatz_length



    SUBROUTINE Parse_Command_line(n,thresh)
            IMPLICIT NONE
            INTEGER, INTENT(INOUT)   :: n, thresh
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
                            CASE('-T')
                                    READ(val, '(I20)') thresh
                            CASE DEFAULT
                                    WRITE(output_unit,'(a)') ' '
                                    WRITE(output_unit,'(a)') &
                                    ' Unrecognized option: '// trim(argname)
                    END SELECT
            ENDDO

    END SUBROUTINE Parse_Command_Line


    SUBROUTINE Initialization()
        IMPLICIT NONE
        INTEGER :: pars(2)
        !//////////////////////////////
        ! Initialize MPI
        CALL MPI_INIT( ierr )
        CALL MPI_Comm_size(MPI_COMM_WORLD, num_proc,ierr)
        CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank,ierr)

        !/////////////////////////////////////////
        ! Rank 0 reads command-line arguments and 
        ! broadcasts to other ranks.
        IF (my_rank .eq. 0) CALL Parse_Command_Line(nnum,threshold)
        pars(1) = nnum
        pars(2) = threshold
        CALL MPI_Bcast(pars,2,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        nnum = pars(1)
        threshold =pars(2)
        IF (my_rank .eq. 0)  THEN
            ALLOCATE(mstat(MPI_STATUS_SIZE, num_proc-1))
        ELSE
            ALLOCATE(mstat(MPI_STATUS_SIZE,1))
        ENDIF
    END SUBROUTINE Initialization

END PROGRAM MAIN

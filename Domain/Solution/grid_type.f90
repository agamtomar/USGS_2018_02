MODULE my_mpi
    ! Description: 
    !   Module for mpi to help associating data to multiple processors
    IMPLICIT NONE
    
CONTAINS 
    ! p  : total number of processors
    ! n  : total number of points
    ! id : rank
    ! j  : global index
    
    INTEGER FUNCTION block_owner(j,p,n)
        INTEGER, INTENT(in) :: j,p,n
        block_owner = ((p*(j+1)-1)/n)
        RETURN
    END FUNCTION block_owner

    INTEGER FUNCTION block_low(id,p,n)
        INTEGER, INTENT(in) :: id,p,n
        block_low =  (id*n)/p       
        RETURN
    END FUNCTION block_low
    
    INTEGER FUNCTION block_high(id,p,n)
        INTEGER, INTENT(in) :: id,p,n
        block_high = ((id+1)*n)/p-1
        RETURN
    END FUNCTION block_high
    
    INTEGER FUNCTION block_size(id,p,n)
        INTEGER, INTENT(in) :: id,p,n
        block_size = (block_low(id+1,p,n)-block_low(id,p,n))
        RETURN
    END FUNCTION block_size

END MODULE my_mpi
    
!///////////////////////////////////////////////////////////////////////////////////////////
!   This program is intended for use illustrating the concepts of a 1d and 2d partitioning 
!   of an arrary.  There is no useful work done by the program at the moment
!   
!   The user can switch between 1d and 2d partitioning
!   Usage:
!           mpiexec -np N ./program -nx X -ny Y -nt T -partition P
!           N:   number of MPI ranks
!           X:   number of gridpoints in x-direction (default = 4000)
!           Y:   number of gridpoints in y-direction (default = 4000)
!           T:   number of iterations to run for (default = 100)
!           P:   Integer value 1 or 2; 1 => 1D partition, 2 => 2d partition
!
!   Description:
!           The program will run for T timesteps and execute a smoothing operation on an array of size
!           X x Y at each timestep.
PROGRAM grid

    USE ISO_FORTRAN_ENV, ONLY : output_unit, compiler_version, compiler_options
    USE mpi

    IMPLICIT NONE

    INTEGER, PARAMETER :: wp = KIND(1.d0)
    INTEGER, PARAMETER :: ndummy = 1

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !   Problem Control Parameters
    INTEGER :: niter = 100
    INTEGER :: part_type = 1   ! 1 for 1d partition; 2 for 2d partion
    INTEGER :: nyglobal = 4000
    INTEGER :: nxglobal = 4000

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    TYPE grid_exchange_type
        INTEGER :: north           ! north neighbor MPI rank
        INTEGER :: south           ! south neighbor MPI rank
        INTEGER :: east            ! east neighbor MPI rank
        INTEGER :: west            ! west neighbor MPI rank
        INTEGER :: nx              ! number of local points in x direction
        INTEGER :: ny              ! number of local points in y direction
        INTEGER :: comm            ! cartesian communicator - derived from MPI_COMM_WORLD
        INTEGER :: rank            ! rank in the cartesian communicator
        INTEGER :: MPI_row             ! MPI row data type
        integer :: MPI_col             ! MPI col data type
    END TYPE grid_exchange_type

    INTEGER :: num_proc        ! total number of processors
    INTEGER :: my_rank         ! my rank in the MPI_COMM_WORLD communicator
    INTEGER :: ierr

    TYPE(grid_exchange_type) :: gi
    REAL(wp) :: ev = 0._wp, wv = 0._wp, sv = 0._wp, nv = 1._wp
    REAL(wp), ALLOCATABLE :: f(:,:), df(:,:)
    
    ! Initialize MPI
    CALL MPI_Init(ierr)
    CALL MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierr)
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

    CALL grab_args(nxglobal, nyglobal, niter, part_type)

    CALL setup_partition(nxglobal, nyglobal, part_type, num_proc, gi)

    CALL initialize(ndummy, gi, ev, wv, nv, sv, f, df)
    CALL main_loop(ndummy, niter, gi, f, df)

    call MPI_Type_free(gi%MPI_col, ierr)
    call MPI_Type_free(gi%MPI_row, ierr)

    CALL MPI_Finalize(ierr)

CONTAINS

    SUBROUTINE main_loop(nd, ni, gi, f, df)
        INTEGER, INTENT(in) :: nd, ni
        TYPE(grid_exchange_type) :: gi
        REAL(wp) :: f(1-nd:, 1-nd:), df(1-nd:, 1-nd:)
        INTEGER :: n, i, j
        DO n = 1, ni
            ! Simple Smoothing operation
            DO j = 1, gi%ny
                DO i = 1, gi%nx
                    df(i,j) = 0.25 * ( f(i+1,j) + f(i-1,j) + f(i,j+1) + f(i,j-1) )
                ENDDO
            ENDDO

            DO j = 1, gi%ny
                DO i = 1, gi%nx
                    f(i,j) = df(i,j)
                ENDDO
            ENDDO
            CALL exchange_boundary(nd, gi, f)
        ENDDO
    END SUBROUTINE Main_Loop


    SUBROUTINE exchange_boundary(nd, gi, t)
        INTEGER, INTENT(in) :: nd
        TYPE(grid_exchange_type), INTENT(inout) :: gi
        REAL(wp), INTENT(inout) :: t(1-nd:, 1-nd:)

        INTEGER, PARAMETER :: tag = 10
        INTEGER :: rrequest(4), srequest(4)
        INTEGER :: ierr
        INTEGER :: nx, ny

        nx = gi%nx
        ny = gi%ny
        !starting the non-blocking receive first
        CALL MPI_Irecv(t(1, ny+1), 1, gi%MPI_col, gi%east,  tag, gi%comm, rrequest(1), ierr)
        CALL MPI_Irecv(t(1,    0), 1, gi%MPI_col, gi%west,  tag, gi%comm, rrequest(2), ierr)
        CALL MPI_Irecv(t(nx+1, 1), 1, gi%MPI_row, gi%north, tag, gi%comm, rrequest(3), ierr)
        CALL MPI_Irecv(t(0,    1), 1, gi%MPI_row, gi%south, tag, gi%comm, rrequest(4), ierr)

        CALL MPI_Isend(t(1, ny), 1, gi%MPI_col, gi%east,  tag, gi%comm, srequest(1), ierr)
        CALL MPI_Isend(t(1, 1) , 1, gi%MPI_col, gi%west,  tag, gi%comm, srequest(2), ierr)
        CALL MPI_Isend(t(nx, 1), 1, gi%MPI_row, gi%north, tag, gi%comm, srequest(3), ierr)
        CALL MPI_Isend(t(1,  1), 1, gi%MPI_row, gi%south, tag, gi%comm, srequest(4), ierr)

        CALL MPI_Waitall(4, rrequest, MPI_STATUSES_IGNORE, ierr)
        CALL MPI_Waitall(4, srequest, MPI_STATUSES_IGNORE, ierr)
    END SUBROUTINE exchange_boundary

    SUBROUTINE initialize(nd, gi, ev, wv, nv, sv, f, df)
        INTEGER, INTENT(in) :: nd
        TYPE(grid_exchange_type), INTENT(in) :: gi
        REAL(wp), INTENT(in) :: ev, wv, nv, sv
        REAL(wp), INTENT(inout), ALLOCATABLE :: f(:,:), df(:,:)
        
        ALLOCATE(    f(1-nd:gi%nx+nd, 1-nd:gi%ny+nd))
        ALLOCATE(   df(1-nd:gi%nx+nd, 1-nd:gi%ny+nd))

        f( :,:) = float(gi%rank)
        df(:,:) = float(gi%rank)
        
        f( :, gi%ny+1:gi%ny+ndummy) = ev
        df(:, gi%ny+1:gi%ny+ndummy) = ev

        f( :, 1-nd:1) = wv
        df(:, 1-nd:1) = wv

        f( gi%nx+1:gi%nx+ndummy, :) = sv
        df(gi%nx+1:gi%nx+ndummy, :) = sv

        f( 1-nd:1, :) = nv
        df(1-nd:1, :) = nv        
    END SUBROUTINE initialize


    SUBROUTINE setup_partition(gnx, gny, npt, np, gi)
        USE my_mpi, ONLY: block_size

        IMPLICIT NONE
        
        INTEGER, INTENT(in) :: gnx, gny ! global problem size in x and y direction, dummy points
        INTEGER, INTENT(in) :: npt, np      ! partition in 1 or 2 dimension, number of processors
        TYPE(grid_exchange_type), INTENT(inout) :: gi

        INTEGER, PARAMETER :: ndims = 2
        
        INTEGER :: psize(ndims) = 0        ! process grid dimensions will be determinted by MPI_DIMS_CREATE
        INTEGER :: coords(ndims) = 0
        LOGICAL :: periodic(ndims)
        INTEGER :: ierr

        psize(:) = 0
        CALL MPI_Dims_create(np, npt, psize, ierr)
        ! now create a cartesian communicator
        periodic = .FALSE.            ! we don't have periodic boundary conditions
        CALL MPI_Cart_create(MPI_COMM_WORLD, npt, psize, periodic, .TRUE., gi%comm, ierr)
        ! get the new rank since we allowed reordering in MPI_Cart_Create
        CALL MPI_Comm_rank(gi%comm, gi%rank, ierr)
        ! now create the coordinates in the processor grid 
        CALL MPI_Cart_coords(gi%comm, gi%rank, npt, coords, ierr)
        IF (npt == 1) THEN
            WRITE(*,*) 'rank: ', gi%rank, ' cord(1):', coords(1)
            psize(2) = 1
        ELSE
            WRITE(*,*) 'rank: ', gi%rank, ' cord(1):', coords(1), ' cord(2):', coords(2)
        END IF
        ! distribute grid points
        gi%nx = block_size(gi%rank, psize(1), gnx)
        gi%ny = block_size(gi%rank, psize(2), gny)
        ! Create a data type for a colum and row for the data exchange
        ! the col type is not really necessary but makes it more convenient
        call MPI_Type_vector(gi%nx, 1, 1, MPI_DOUBLE_PRECISION, gi%MPI_col, ierr)
        call MPI_Type_commit(gi%MPI_col, ierr)
        call MPI_Type_vector(gi%ny, 1, gi%nx+2, MPI_DOUBLE_PRECISION, gi%MPI_row, ierr)
        call MPI_Type_commit(gi%MPI_row, ierr)
        WRITE(*,*) 'rank:', gi%rank, gi%nx, gi%ny
        ! now setup the neighbor ranks for communication
        ! north/east
        CALL MPI_Cart_shift(gi%comm, 1, 1, gi%west, gi%east, ierr)
        gi%north = MPI_PROC_NULL
        gi%south = MPI_PROC_NULL
        IF (npt == 2) THEN
            CALL MPI_Cart_shift(gi%comm, 0, 1, gi%north, gi%south, ierr)
        ENDIF
        ! now print the coordinates of the sending and receiving sides
        ! check for MPI_PROC_NULL
        ! allocate east buffer
        IF (gi%east /= MPI_PROC_NULL) THEN
            CALL MPI_Cart_coords(gi%comm, gi%east, ndims, coords, ierr)
            WRITE(*,*) 'rank: ', gi%rank, ' east neighbor: ', gi%east, coords
        END IF
        ! allocate west buffer
        IF (gi%west /= MPI_PROC_NULL) THEN
            CALL MPI_Cart_coords(gi%comm, gi%west, ndims, coords, ierr)
            WRITE(*,*) 'rank: ', gi%rank, ' west neighbor: ', gi%west, coords
        END IF
        ! allocate north buffer
        IF (gi%north /= MPI_PROC_NULL) THEN
            CALL MPI_Cart_coords(gi%comm, gi%north, ndims, coords, ierr)
            WRITE(*,*) 'rank: ', gi%rank, ' north neighbor: ', gi%north, coords
        END IF
        ! allocate so_uth buffer
        IF (gi%south /= MPI_PROC_NULL) THEN
            CALL MPI_Cart_coords(gi%comm, gi%south, ndims, coords, ierr)
            WRITE(*,*) 'r_ank: ', gi%rank, ' south neighbor: ', gi%south, coords
        END IF
    END SUBROUTINE setup_partition


    SUBROUTINE grab_args(numx, numy, numiter, dtype)
        IMPLICIT NONE

        INTEGER, INTENT(INOUT)   :: numx
        INTEGER, INTENT(INOUT)   :: numy
        INTEGER, INTENT(INOUT)   :: dtype
        INTEGER, INTENT(INOUT)   :: numiter

        INTEGER :: n                    ! Number of command-line arguments
        INTEGER :: i                    
        CHARACTER(len=1024) :: argname  ! Argument key
        CHARACTER(len=1024) :: val      ! Argument value

        n = command_argument_COUNT()
        DO i=1,n,2
            CALL get_command_ARGUMENT(i, argname)
            CALL get_command_ARGUMENT(i+1, val)
            SELECT CASE(argname)
            CASE('-nx')
                READ(val, '(I8)') numx
            CASE('-ny')
                READ(val, '(I8)') numy
            CASE('-nt')
                READ(val, '(I8)') numiter
            CASE('-partition')
                READ(val, '(I8)') dtype
            CASE DEFAULT
                WRITE(output_unit,'(a)') ' '
                WRITE(output_unit,'(a)') &
                     ' Unrecognized option: '// TRIM(argname)
            END SELECT
        ENDDO
    END SUBROUTINE grab_args


END PROGRAM

program mpi_gather_ex
  !----------------------------------------------------------------------------
  ! Test for mpi_gather function.
  !
  !
  !
  ! Hongyang Zhou, hyzhou@umich.edu  09/11/2017
  !----------------------------------------------------------------------------
  use mpi

  implicit none

  integer :: error
  integer :: id,tag,p
  integer :: x
  integer :: status(MPI_STATUS_SIZE)
  real    :: wtime0
  integer, allocatable :: xglobal(:)
  !----------------------------------------------------------------------------

  ! Initialize MPI
  call MPI_Init(error)

  ! Get the number of processes
  call MPI_Comm_size(MPI_COMM_WORLD, p, error)

  ! Get the individual process ID
  call MPI_Comm_rank(MPI_COMM_WORLD, id, error)

  ! Print message from master worker
  if (id==0) then
     write(*,*) 'MPI test program: scan summation'
     write(*,*) '# of processes  = ',p
  end if

  x = id

  ! Print output summation from master worker
  if (id==0) then
     allocate(xglobal(0:p-1))
     write(*,*) '-----------------------------------'
     write(*,*) 'scan array = ',xglobal
  end if

  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error)

  call MPI_Gather(x,1,MPI_INTEGER,xglobal,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)
  
  if (id==0) then
     write(*,*) 'xglobal=',xglobal
     deallocate(xglobal)
  end if

  ! Shut down MPI
  call MPI_Finalize(error)

end program mpi_gather_ex

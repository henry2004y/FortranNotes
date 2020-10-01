program probe_it
  ! Example showing how to use probe and get_count
  ! to find the size of an incoming message
  use mpi
  integer :: myid,numprocs
  integer :: status(MPI_STATUS_SIZE)
  integer :: mytag,icount,ierr,iray(10)
  !----------------------------------------------------------------------------
  
  call MPI_INIT( ierr )
  call MPI_COMM_RANK( MPI_COMM_WORLD, myid, ierr )
  call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )

  mytag=123; iray=0; icount=0

  if(myid == 0)then
     ! Process 0 sends a message of size 5
     icount=5
     iray(1:icount)=1
     call MPI_SEND(iray,icount,MPI_INTEGER, &
          1,mytag,MPI_COMM_WORLD,ierr)
  endif
  if(myid == 1)then
     ! process 1 uses probe and get_count to find the size
     call mpi_probe(0,mytag,MPI_COMM_WORLD,status,ierr)
     call mpi_get_count(status,MPI_INTEGER,icount,ierr)
     write(*,*)"getting ", icount," values"
     call mpi_recv(iray,icount,MPI_INTEGER,0, &
          mytag,MPI_COMM_WORLD,status,ierr)
  endif
  
  !write(*,*) "Processor id=",myid,"iray=",iray
  write(*,*) 'iray=',iray
  
  call mpi_finalize(ierr)

End program probe_it

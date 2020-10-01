program pingpong
  !----------------------------------------------------------------------------
  ! Two processes repeatedly pass a message back and forth.
  ! Note: this program only works for two processors!
  !
  ! Hongyang Zhou, hyzhou@umich.edu  09/24/2017
  !----------------------------------------------------------------------------
  use mpi
  
  implicit none

  integer, parameter :: PING_PONG_LIMIT = 10
  integer :: ping_pong_count = 0
  integer :: world_rank, partner_rank, total_rank
  integer :: status(MPI_STATUS_SIZE), error
  real    :: wtime0
  !----------------------------------------------------------------------------

  ! Initialize MPI
  call MPI_Init(error)

  ! Get the number of processes
  call MPI_Comm_size(MPI_COMM_WORLD, total_rank, error)

  ! Get the individual process ID
  call MPI_Comm_rank(MPI_COMM_WORLD, world_rank, error)

  ! Print message from master worker
  if (world_rank==0) then
     wtime0 = MPI_Wtime()
     write(*,*) 'MPI test program: pingpong'
     write(*,*) '# of processes  = ',total_rank
  end if

  partner_rank = mod(world_rank+1,2)
  
  do while (ping_pong_count < PING_PONG_LIMIT)
     if ( world_rank == mod(ping_pong_count,2) ) then
        ! Increment the ping pong count before you send it
        ping_pong_count = ping_pong_count + 1
        call MPI_Send(ping_pong_count, 1, MPI_INTEGER, partner_rank, 0, &
             MPI_COMM_WORLD, error)
        write(*,*) world_rank," sent and incremented ping_pong_count ", &
             ping_pong_count," to ", partner_rank
     else
        call MPI_Recv(ping_pong_count, 1, MPI_INTEGER, partner_rank, 0, &
             MPI_COMM_WORLD, MPI_STATUS_IGNORE,error)
        write(*,*) world_rank," received ping_pong_count ",ping_pong_count, &
             " from ",partner_rank
     end if
  end do


  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error)

  ! Print output summation from master worker
  if (world_rank==0) then
     write(*,*) '-------------------'
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime0
  end if
  
  ! Shut down MPI
  call MPI_Finalize(error)

end program pingpong

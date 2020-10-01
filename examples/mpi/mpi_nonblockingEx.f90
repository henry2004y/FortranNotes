program main
  !----------------------------------------------------------------------------
  ! Illustrates the usage of nonblocking communication.
  ! Note: this program only works for two processors!
  !
  ! The output is a mess...
  !
  ! Hongyang Zhou, hyzhou@umich.edu  09/24/2017
  !----------------------------------------------------------------------------
  use mpi
  
  implicit none

  integer :: pool_size, my_rank, error
  real    :: wtime0
  character(len=30) :: send_buffer, my_cpu_name, recv_buffer
  integer :: my_name_length, count
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  !----------------------------------------------------------------------------

  ! Initialize MPI
  call MPI_Init(error)

  ! Get the number of processes
  call MPI_Comm_size(MPI_COMM_WORLD, pool_size, error)

  ! Get the individual process ID
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, error)

  ! Print message from master worker
  if (my_rank==0) then
     wtime0 = MPI_Wtime()
     write(*,*) 'MPI test program: nonblocking communication'
     write(*,*) '# of processes  = ',pool_size
  end if

  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error)
  
  if (my_rank == 0) then
     
     call MPI_Get_processor_name(my_cpu_name, my_name_length, error)
     
     write(*,*) send_buffer, "Dear Task 1"
     write(*,*) "Please do not send any more messages."
     write(*,*) "Please send money instead."
     write(*,*) "Yours faithfully,"
     write(*,*) "Task 0"
     write(*,*) "Running on ", my_cpu_name
     
     call MPI_Isend (send_buffer, len(send_buffer) + 1, MPI_CHAR, 1 , 77, &
          MPI_COMM_WORLD, request, error)
     
     write(*,*) "hello there user, I've just started this send and &
          &I'm having a good time relaxing."
     
     call MPI_Wait (request, status, error)
     write(*,*) "hello there user, it looks like the message has been sent."
     
     if (request == MPI_REQUEST_NULL) then
        write(*,*) "    the send request is MPI_REQUEST_NULL now"
     else 
        write(*,*) "    the send request still lingers"
     end if
     
  elseif (my_rank == 1) then
     
     call MPI_Get_processor_name(my_cpu_name, my_name_length, error)
     call MPI_Irecv (recv_buffer, 31, MPI_CHAR, 0, 77, MPI_COMM_WORLD, &
    request,error)
     write(*,*) "hello there user, I've just started this receive on ", &
          my_cpu_name," and I'm having a good time relaxing."
     call MPI_Wait (request, status, error)
     call MPI_Get_count (status, MPI_CHAR, count, error)
     write(*,*) "hello there user, it looks like ",count,&
          "characters have just arrived;"
     !write(*,*) recv_buffer

     if (request == MPI_REQUEST_NULL) then
        write(*,*) "    the receive request is MPI_REQUEST_NULL now"
     else
        write(*,*) "    the receive request still lingers"
     end if
  end if
            
  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error)

  ! Print output summation from master worker
  if (my_rank==0) then
     write(*,*) '-------------------'
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime0
  end if
  
  ! Shut down MPI
  call MPI_Finalize(error)

end program main

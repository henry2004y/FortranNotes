program sum_mpi
  !----------------------------------------------------------------------------
  ! Test for summing array of #N using MPI.
  !
  ! 1. send to proc=0, then sum it there
  ! 2. send to previous worker, sum sequentially
  ! 3. binary-tree like summation
  ! 4. mpi_reduce
  ! 5. isend,irecv
  !
  !
  ! Hongyang Zhou, hyzhou@umich.edu  09/08/2017
  !----------------------------------------------------------------------------
  use mpi
  
  implicit none

  integer :: error
  integer :: id,tag
  integer :: i,j,p
  integer :: x,xsum,xin,partialsum
  integer :: status(MPI_STATUS_SIZE)
  real    :: wtime(0:4)
  !----------------------------------------------------------------------------

  ! Initialize MPI
  call MPI_Init(error)

  ! Get the number of processes
  call MPI_Comm_size(MPI_COMM_WORLD, p, error)

  ! Get the individual process ID
  call MPI_Comm_rank(MPI_COMM_WORLD, id, error)

  ! Print message from master worker
  if (id==0) then
     wtime(0) = MPI_Wtime()
     write(*,*) 'MPI test program: summation'
     write(*,*) '# of processes  = ',p
  end if

  ! Assign x value
  x = id

  ! Checkpoint
  !call MPI_Barrier(MPI_COMM_WORLD, error)

  ! version I: send to proc=0, then sum it there
  if (id/=0) then
     call MPI_Send(x, 1, MPI_INTEGER, 0, tag, MPI_COMM_WORLD, error )
  else ! id==0
     xsum = x
     do i=1,p-1
        call MPI_Recv(xin, 1, MPI_INTEGER, i, tag, MPI_COMM_WORLD, &
             status, error )
        xsum = xsum + xin
     end do
  end if

  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-----------------------------------'
     write(*,*) 'Version One: pass to and sum in one'
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime(0)
     write(*,*) 'Total summation = ',xsum
  end if

  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error)

  if (id==0) then
     wtime(1) = MPI_Wtime()
  end if

  !--------------------------------------------------------------
  ! Version II
  xsum = x
  if (id<p-1) then
     call MPI_Recv(partialsum, 1, MPI_INTEGER, id+1, tag, MPI_COMM_WORLD, &
          status, error )
     xsum = xsum + partialsum
  end if
  if (id>0) then
      call MPI_Send(xsum, 1, MPI_INTEGER, id-1, tag, MPI_COMM_WORLD, error )
  end if

  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-----------------------------------'
     write(*,*) 'Version Two: sequential sum'
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime(1)
     write(*,*) 'Total summation = ',xsum
  end if

  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error)
  
  if (id==0) then
     wtime(2) = MPI_Wtime()
  end if
  
  !--------------------------------------------------
  !version III: binary-tree summation, expecting O(log2(n)) time
  xsum = x
  do i=1,ceiling(log(real(p))/log(2.))
     ! This may not be ideal, since it only effects the first round!
     ! The following if works for double only
     !if (mod(id,2**i)==0) then
     if (mod(id,2**i)==0 .and. id+2**(i-1)<=p-1) then
    
        call MPI_Recv(xin, 1, MPI_INTEGER, id+2**(i-1), tag, MPI_COMM_WORLD, &
             status, error)
        xsum = xsum + xin
     elseif (mod(id,2**(i-1))==0) then
        call MPI_Send(xsum, 1, MPI_INTEGER, id-2**(i-1), tag, MPI_COMM_WORLD, &
             error )
     end if
  end do

  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-----------------------------------'
     write(*,*) 'Version Three: binary tree'
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime(2)
     write(*,*) 'Total summation = ',xsum
  end if

  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error)

  if (id==0) then
     wtime(3) = MPI_Wtime()
  end if
  
  !---------------------------------------------------
  !version IV: mpi_reduce
  
  call MPI_Reduce(x, xsum,1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, error)

  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-----------------------------------'
     write(*,*) 'Version Four: mpi_reduce'
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime(3)
     write(*,*) 'Total summation = ',xsum
  end if  

  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error)

  if (id==0) then
     wtime(4) = MPI_Wtime()
  end if
  
  !---------------------------------------------------
  !version V: non-blocking send/recv
  ! On my Macbook pro, this is slower than blocking version, and it is
  !theoretically wrong if the partialsum is added before each processor gets
  !the data.
  xsum = x
  if (id<p-1) then
     call MPI_Irecv(partialsum, 1, MPI_INTEGER, id+1, tag, MPI_COMM_WORLD, &
          status, error )
     xsum = xsum + partialsum
  end if
  if (id>0) then
      call MPI_Isend(xsum, 1, MPI_INTEGER, id-1, tag, MPI_COMM_WORLD, error )
  end if

  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-----------------------------------'
     write(*,*) 'Version Five: isend/irecv'
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime(1)
     write(*,*) 'Total summation = ',xsum
  end if


  ! Shut down MPI
  call MPI_Finalize(error)
  
end program sum_mpi

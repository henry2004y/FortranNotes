program scan_mpi
  !----------------------------------------------------------------------------
  ! Test for scanning array of #N using MPI for a fully connected cores.
  ! 
  ! See the requirement in EECS587, Assign1.
  !
  ! Hongyang Zhou, hyzhou@umich.edu  09/11/2017
  !----------------------------------------------------------------------------
  use mpi
  
  implicit none

  integer :: error
  integer :: id,tag
  integer :: i,p,gap
  integer :: x,xin
  integer :: status(MPI_STATUS_SIZE)
  real    :: wtime0
  integer, allocatable :: xscan(:)
  !----------------------------------------------------------------------------

  ! Initialize MPI
  call MPI_Init(error)

  ! Get the number of processes
  call MPI_Comm_size(MPI_COMM_WORLD, p, error)

  ! Get the individual process ID
  call MPI_Comm_rank(MPI_COMM_WORLD, id, error)

  ! Print message from master worker
  if (id==0) then
     allocate(xscan(0:p-1))
     wtime0 = MPI_Wtime()
     write(*,*) 'MPI test program: scan summation'
     write(*,*) '# of processes  = ',p
  end if

  x = id
  
  ! binary-tree like scanning
  do i=1,ceiling(log(real(p))/log(2.))
     gap = 2**(i-1)
     if (id+gap<p) then
        call MPI_Send(x, 1, MPI_INTEGER, id+gap, tag, MPI_COMM_WORLD, &
              error)
     end if    
     if (id-gap>=0) then
        call MPI_Recv(xin, 1, MPI_INTEGER, id-gap, tag, MPI_COMM_WORLD, &
             status, error )
        x = x + xin
     end if
  end do

  ! Pass all values to master process
  call MPI_Gather(x,1,MPI_INTEGER,xscan,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)

  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-----------------------------------'
     write(*,*) 'scan array = ',xscan
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime0
     deallocate(xscan)
  end if

  
  ! Shut down MPI
  call MPI_Finalize(error)
  
end program scan_mpi

! Get rid of multiply or exponential, better than I did
j = 1
do while(j<p)
   if (id+j<p) send(id+j,a)
   if (id-j>=0) recv(id-j,a)
   a = a+v
   j = j+j
end do

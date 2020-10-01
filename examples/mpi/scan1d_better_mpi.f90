program scan1d_better_mpi
  !----------------------------------------------------------------------------
  ! Test for scanning array of #N using MPI for a 1D mesh cores.
  ! Each core is connected only to its adjacent neighbors.
  ! See the details in EECS587, Assign1.
  !
  ! Hongyang Zhou, hyzhou@umich.edu  09/12/2017
  !----------------------------------------------------------------------------
  use mpi
  
  implicit none

  integer :: error
  integer :: id,tag
  integer :: i,p
  integer :: v,vin
  integer :: status(MPI_STATUS_SIZE)
  real    :: wtime0
  integer, allocatable :: vscan(:)
  !----------------------------------------------------------------------------

  ! Initialize MPI
  call MPI_Init(error)

  ! Get the number of processes
  call MPI_Comm_size(MPI_COMM_WORLD, p, error)
  
  ! Get the individual process ID
  call MPI_Comm_rank(MPI_COMM_WORLD, id, error)


  ! Print message from master worker
  if (id==0) then
     allocate(vscan(0:p-1))
     wtime0 = MPI_Wtime()
     write(*,*) 'MPI test program: scan summation'
     write(*,*) '# of processes  = ',p
  end if

  v = id+1

  ! This is the KEY PART!
  ! Since the recv is blocked, we don`t need for loops in this case!
  ! This is a valuable point!
  if (id/=0) then 
     call MPI_Recv(vin,1,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD, &
          status,error)
     v = v + vin
  end if

  if (id/=p-1) then
     call MPI_Send(v,1,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD,error)
  end if
  

  ! Pass all values to master process
  call MPI_Gather(v,1,MPI_INTEGER,vscan,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)
  
  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-------------------'
     write(*,*) 'scan array in 1D = ',vscan
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime0
     deallocate(vscan)
  end if
  
  ! Shut down MPI
  call MPI_Finalize(error)  

end program scan1d_better_mpi

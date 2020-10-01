program smoothing1d
  !----------------------------------------------------------------------------
  ! Test for 1d smoothing.
  ! Each neighboring processor exchange info of ghost cell values and boundary
  ! cell values.
  ! Assume 1 level of ghost cells, and each 'block' has nI true cells.
  ! Periodic boundary conditions
  !
  ! This is the first step in block-wise communication. After success, you can
  ! extend to 2d and 3d meshes.
  !
  ! Hongyang Zhou, hyzhou@umich.edu  09/24/2017
  !----------------------------------------------------------------------------
  use mpi_f08
  
  implicit none
  
  ! The overall state variable
  real, allocatable :: State_GB(:,:)

  ! The local state pieces on each proc
  real, allocatable :: State_G(:)
  
  ! Number of physical cells
  integer, parameter :: nI = 3

  ! Number of ghost cells
  integer, parameter :: nG = 1

  ! Number of smoothing iterations
  integer, parameter :: nIter = 1
  
  integer :: iBlock,nBlock,RBlock,LBlock
  integer :: i,j,tag=1
  TYPE(MPI_Request) :: RSrequest, RRrequest, LSrequest, LRrequest
  TYPE(MPI_Status) :: status
  integer, parameter :: dp = kind(1.0d0)
  real(kind = dp)    :: wtime0
  !----------------------------------------------------------------------------

  ! Initialize MPI
  call MPI_Init()

  ! Get the number of processes
  call MPI_Comm_size(MPI_COMM_WORLD, nBlock)
  
  ! Get the individual proc ID
  call MPI_Comm_rank(MPI_COMM_WORLD, iBlock)


  ! Print message from master proc
  if (iBlock==0) then
     wtime0 = MPI_Wtime()
     write(*,*) 'MPI test program: 1d grid'
     write(*,*) '# of processes  = ',nBlock

     ! Allocate state var
     allocate(State_GB(1-nG:nI+nG,nBlock))
     ! Initialize state
     forall(i=1:nBlock)
        State_GB(:,i) = i
     end forall
  end if

  ! Allocate local state variable on each proc
  allocate(State_G(1-nG:nI+nG))

  ! Separate state to all blocks
  call MPI_Scatter(State_GB,nI+2*nG,MPI_REAL,State_G,nI+2*nG,MPI_REAL,0, &
       MPI_COMM_WORLD)

  ! Calculate left/right neighbor indexes
  RBlock = iBlock+1
  if(RBlock==nBlock) RBlock = 0

  LBlock = iBlock-1
  if(LBlock<0) LBlock = nBlock-1

  ! nIter times of smoothing with non-blocking send & recv
  !hyzhou: I think this part can be simplified!
  do i=1,nIter
     call MPI_Isend(State_G(nI-nG+1:nI),nG,MPI_REAL,RBlock,tag, &
          MPI_COMM_WORLD, RSrequest)
     call MPI_Irecv(State_G(0-nG+1:0),nG,MPI_REAL,LBlock,MPI_ANY_TAG, &
          MPI_COMM_WORLD, RRrequest)
     call MPI_Isend(State_G(1:2-nG),nG,MPI_REAL,LBlock,tag, &
          MPI_COMM_WORLD, LSrequest)
     call MPI_Irecv(State_G(nI+1:nI+nG),nG,MPI_REAL,RBlock,MPI_ANY_TAG, &
          MPI_COMM_WORLD, LRrequest)
     ! update non-boundary cells
     do j=2,nI-1
        State_G(j) = 0.5*( State_G(j-1) + State_G(j+1) )
     end do
     
     ! wait for completion of sending of boundary values
     call MPI_Wait(RSrequest, status)
     ! wait for completion of receipt in ghost values
     call MPI_Wait(RRrequest, status)
     ! wait for completion of sending of boundary values
     call MPI_Wait(LSrequest, status)
     ! wait for completion of receipt in ghost values
     call MPI_Wait(LRrequest, status)

     ! update boundary cells
     State_G(1)  = 0.5*( State_G(0)    + State_G(2)    )
     State_G(nI) = 0.5*( State_G(nI-1) + State_G(nI+1) )
  end do
  
  call MPI_Gather(State_G,nI+2*nG,MPI_REAL,State_GB,nI+2*nG,MPI_REAL, &
       0,MPI_COMM_WORLD)
  
  if(iBlock==0) then
     write(*,*) '-----------------------'
     write(*,*) State_GB(1:nI,:)
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime0
     deallocate(State_GB)
  end if
  
  deallocate(State_G)
  
  ! Shut down MPI
  call MPI_Finalize()  

end program smoothing1d

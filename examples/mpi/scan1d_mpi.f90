program scan1d_mpi
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
  integer :: v(2),vin(2)
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

  v(:) = id+1
  
  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error)
  
  
  ! Exchange info one by one
  do i=1,p-1
     if (i<p/2.) then
        ! Pass info from left to right
        if (id==i-1) then
           call MPI_Send(v,2,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD,error)
        end if
        if (id==i) then
           call MPI_Recv(vin,2,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD, &
                status,error)
           v = v + vin
        end if

        ! Pass info from right to left
        if (id==p-i) then
           call MPI_Send(v(2),1,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD,error)
        end if
        if (id==p-i-1) then
           call MPI_Recv(vin(2),1,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD, &
                status,error)
           v(2) = v(2) + vin(2)
        end if
        
     elseif (i>p/2.) then
        ! Pass info from left to right
        if (id==i-1) then
           call MPI_Send(v,2,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD,error)
        end if
        if (id==i) then
           call MPI_Recv(vin,2,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD,status,error)
           v(1) = v(1) + vin(1)
           v(2) = vin(2)
        end if

        ! Pass info from right to left
        if (id==p-i) then
           !write(*,*) 'vsum=',v(2)
           call MPI_Send(v(2),1,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD,error)
        end if
        if (id==p-i-1) then
           call MPI_Recv(v(2),1,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD, &
                status,error)
        end if
        
     else ! Exchange info between middle two
        !if (id==i-1) then
        !   call MPI_Send(v,2,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD,error)
        !   call MPI_Recv(v(2),1,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD, &
        !        status,error)
        !end if
        !if (id==i) then
        !   call MPI_Recv(vin,2,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD, &
        !        status,error)
        !   v = v + vin
        !   call MPI_Send(v(2),1,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD,error)
        !end if

        ! This is equivalent to the above for handling exchange between
        ! middle two workers, and probably a better way for coding. 
        if (id==i-1) then
           call MPI_Sendrecv(v, 2, MPI_INTEGER, id+1,tag, &
                vin, 2, MPI_INTEGER, id+1, tag, &
                MPI_COMM_WORLD, status, error)
           v(2) = v(1) + vin(2)
        end if
        if (id==i) then
           call MPI_Sendrecv(v, 2, MPI_INTEGER, id-1,tag, &
                vin, 2, MPI_INTEGER, id-1, tag, &
                MPI_COMM_WORLD, status, error)
           v = v + vin
        end if

     end if
  end do


  ! Pass all values to master process
  call MPI_Gather(v(1),1,MPI_INTEGER,vscan,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)

  write(*,*) 'id,vsum=',id,v(2)

  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error) 
  
  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-------------------'
     write(*,*) 'scan array in 1D = ',vscan
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime0
     deallocate(vscan)
  end if
  
  ! Shut down MPI
  call MPI_Finalize(error)
  
end program scan1d_mpi

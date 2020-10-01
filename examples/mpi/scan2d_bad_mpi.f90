program scan2d_bad_mpi
  !----------------------------------------------------------------------------
  ! Test for scanning array of #N using MPI for a 2D mesh cores.
  ! Each core is connected only to its adjacent neighbors.
  ! See the details in EECS587, Assign1.
  !
  ! Sadly, your code is not what you think. You use nested for-loops here,
  ! which is totally unnecessary! Learn it in a hard way...
  !
  ! Hongyang Zhou, hyzhou@umich.edu  09/12/2017
  !----------------------------------------------------------------------------
  use mpi
  
  implicit none

  integer :: error,Nerror
  integer :: id,tag
  integer :: i,j,tStep,p,sqrtp
  integer :: v(2),vin(2)
  integer :: status(MPI_STATUS_SIZE)
  logical :: IsSquare
  real    :: wtime0
  integer, allocatable :: vscan(:)
  !----------------------------------------------------------------------------

  ! Initialize MPI
  call MPI_Init(error)

  ! Get the number of processes
  call MPI_Comm_size(MPI_COMM_WORLD, p, error)
  
  ! Get the individual process ID
  call MPI_Comm_rank(MPI_COMM_WORLD, id, error)

  ! Check and Print message from master worker
  if (id==0) then
     call PerfectSquare(p,IsSquare,sqrtp)
     if (.not.IsSquare) then
        write(*,*) 'Number of processors ',p,' is not a square! Exit...'
        call MPI_abort(MPI_COMM_WORLD, Nerror, error)
        stop
     end if
     allocate(vscan(0:p-1))
     wtime0 = MPI_Wtime()
     write(*,*) 'MPI test program: scan summation for 2D mesh'
     write(*,*) '# of processes  = ',p
  end if

  v = id+1
  call MPI_Bcast(sqrtp, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, error)
 
  ! Exchange info between neighbors
  
  ! First between columns 
  do j=1,sqrtp-1       
     if (j<sqrtp/2.) then
        do i=0,sqrtp-1

           ! Pass info from left to right
           if (id==i*sqrtp+j-1) then
              call MPI_Send(v,2,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD,error)
           end if
           if (id==i*sqrtp+j) then
              call MPI_Recv(vin,2,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD, &
                   status,error)
              v = v + vin
           end if

           ! Pass info from right to left
           if (id==p-j-i*sqrtp) then
              call MPI_Send(v(2),1,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD,error)
           end if
           if (id==p-j-i*sqrtp-1) then
              call MPI_Recv(vin(2),1,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD, &
                   status,error)
              v(2) = v(2) + vin(2)
           end if
        end do
     elseif (j>sqrtp/2.) then
        do i=0,sqrtp-1 
           ! Pass info from left to right
           if (id==j+i*sqrtp-1) then
              call MPI_Send(v,2,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD,error)
           end if
           if (id==j+i*sqrtp) then
              call MPI_Recv(vin,2,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD, &
                   status,error)
              v(1) = v(1) + vin(1)
              v(2) = vin(2)
           end if

           ! Pass info from right to left
           if (id==p-j-i*sqrtp) then
              call MPI_Send(v(2),1,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD,error)
           end if
           if (id==p-j-i*sqrtp-1) then
              call MPI_Recv(v(2),1,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD, &
                   status,error)
           end if
        end do
     else ! Exchange info between middle two
        do i=0,sqrtp-1 
           if (id==j+i*sqrtp-1) then
              call MPI_Sendrecv(v, 2, MPI_INTEGER, id+1,tag, &
                   vin, 2, MPI_INTEGER, id+1, tag, &
                   MPI_COMM_WORLD, status, error)
              v(2) = v(1) + vin(2)
           end if
           if (id==j+i*sqrtp) then
              call MPI_Sendrecv(v, 2, MPI_INTEGER, id-1,tag, &
                   vin, 2, MPI_INTEGER, id-1, tag, &
                   MPI_COMM_WORLD, status, error)
              v = v + vin
           end if
        end do
     end if
  end do


  ! Pass all values to master process
  call MPI_Gather(v,1,MPI_INTEGER,vscan,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)
  
  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-----------------------------------'
     write(*,*) 'scan array = '
     do i=0,sqrtp-1
        write(*,*) vscan(i*sqrtp:i*sqrtp+sqrtp-1)
     end do
  end if

  ! Then add from upper rows to lower rows
  do i=0,sqrtp-2       
     do j=0,sqrtp-1 
        ! Pass info from top to bottom
        if (id==i*sqrtp+j) then
           call MPI_Send(v,2,MPI_INTEGER,id+sqrtp,tag, &
                MPI_COMM_WORLD,error)
        end if
        if (id==(i+1)*sqrtp+j) then
           call MPI_Recv(vin,2,MPI_INTEGER,id-sqrtp,tag,MPI_COMM_WORLD, &
                status,error)
           v = v + vin(2)
        end if
     end do
  end do
     
  ! Pass all values to master process
  call MPI_Gather(v,1,MPI_INTEGER,vscan,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)

  ! Checkpoint
  call MPI_Barrier(MPI_COMM_WORLD, error) 
  
  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-----------------------------------'
     write(*,*) 'scan array = '
     do i=0,sqrtp-1
        write(*,*) vscan(i*sqrtp:i*sqrtp+sqrtp-1)
     end do
     write(*,*) 'Elapsed time    = ',MPI_Wtime() - wtime0
     deallocate(vscan)
  end if

  
  ! Shut down MPI
  call MPI_Finalize(error)
  
end program scan2d_bad_mpi
!==============================================================================
! Check if p is a perfect square
subroutine PerfectSquare(p,isSquare,sqrtp)
  implicit none
  integer,intent(in) :: p
  logical,intent(out) :: isSquare
  integer,intent(out) :: sqrtp
  integer :: h

  ! h is the last digit in hexadecimal format; for a square integer, it must be
  ! 0, 1, 4 or 9.
  h = mod(p,16)
  if (h>9) then
     isSquare = .false.
     return
  end if

  if ( h==0 .or. h==1 .or. h==4 .or. h==9 ) then
     ! Take square root if you must
     sqrtp = floor(sqrt(real(p) + 0.5))
     isSquare = (sqrtp*sqrtp == p)
  else
     isSquare = .false.
     return
  end if
  
end subroutine PerfectSquare



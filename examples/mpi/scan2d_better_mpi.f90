program scan2_better_mpi
  !----------------------------------------------------------------------------
  ! Test for scanning array of #N using MPI for a 2D mesh cores.
  ! Each core is connected only to its adjacent neighbors.
  ! See the details in EECS587, Assign1.
  ! This is a better way to write practical code, but may not perform better
  ! than my "ugly-writting" version.
  !
  ! Hongyang Zhou, hyzhou@umich.edu  09/16/2017
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
  
  ! 1.pass partial sum in each row from left to right
  if (mod(id,sqrtp)/=0) then 
     call MPI_Recv(vin,1,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD, &
          status,error)
     v = v + vin(1)
  end if

  if (mod(id,sqrtp)/=sqrtp-1) &
     call MPI_Send(v,1,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD,error)

  
  ! Pass all values to master process
  call MPI_Gather(v(1),1,MPI_INTEGER,vscan,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)
  
  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-----------------------------------'
     write(*,*) 'scan array = '
     do i=0,sqrtp-1
        write(*,*) vscan(i*sqrtp:i*sqrtp+sqrtp-1)
     end do
  end if


  ! 2.pass partial sum in the last column from top to bottom
  if (mod(id,sqrtp)==sqrtp-1 .and. id/=sqrtp-1) then
     call MPI_Recv(vin,1,MPI_INTEGER,id-sqrtp,tag,MPI_COMM_WORLD, &
          status,error)
     v(1) = v(1) + vin(1)
     v(2) = vin(1)
  end if

  if (mod(id,sqrtp)==sqrtp-1 .and. id/=p-1) &
     call MPI_Send(v,1,MPI_INTEGER,id+sqrtp,tag,MPI_COMM_WORLD,error)  

  ! Pass all values to master process
  call MPI_Gather(v(1),1,MPI_INTEGER,vscan,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)
  
  ! Print output summation from master worker
  if (id==0) then
     write(*,*) '-----------------------------------'
     write(*,*) 'scan array = '
     do i=0,sqrtp-1
        write(*,*) vscan(i*sqrtp:i*sqrtp+sqrtp-1)
     end do
  end if

  
  ! 3.pass partial sum in each row (except the 1st) from right to left
  if (mod(id,sqrtp)/=sqrtp-1 .and. id>=sqrtp) then 
     call MPI_Recv(vin(2),1,MPI_INTEGER,id+1,tag,MPI_COMM_WORLD, &
          status,error)
     v(1) = v(1) + vin(2)
     v(2) = vin(2)
  end if

  if (mod(id,sqrtp)/=0 .and. id>=sqrtp) &
     call MPI_Send(v(2),1,MPI_INTEGER,id-1,tag,MPI_COMM_WORLD,error)

  
  ! Pass all values to master process
  call MPI_Gather(v,1,MPI_INTEGER,vscan,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)

  
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
  
end program scan2_better_mpi
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



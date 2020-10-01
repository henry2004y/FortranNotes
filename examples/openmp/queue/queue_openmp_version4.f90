program main
  ! EECS587, OpenMP assignment. See the details in HW4.
  !
  ! This version is mainly for debugging. I intentionally excluded all the
  ! function calls except g. However, if I turn on recursion check in
  ! gfortran/nagfor, it will crash at runtime with the following message:
  ! 'Recursive call to non-recursive procedure GMODULE:G'
  ! God knows what`s going on here...
  !
  ! Hongyang Zhou, 10/26/2017

  use omp_lib
  use CircleQueue
  use estimate

  implicit none

  integer :: first, last
  real :: CurrentMax
  real, allocatable :: ThreadMax(:)
  integer :: ierror, timeExchange
  real :: left, right, range(2), wtime, gleft, gright
  logical :: status

  integer :: id, Nthread, NthreadMax, Qlength, subQlength, isub
  !----------------------------------------------------------------------------

  ! Initialize global queue
  first = 1
  last  = 1
  CurrentMax = -1e6
  ierror = 0
  isub = 1
  timeExchange = 1

  allocate(queue(2,1:MaxQueue))
  queue = 0
  
  ! Print out info
  write(*,*) 'EECS587 Assignment 4: OpenMP on finding max for 1D function'
  write(*,*) 'f(x),x in [a,b], |f''(x)| <= s'
  write(*,*) 'Breadth-first search is implemented with queue structure.'
  write(*,*) 'Version 4 by Hongyang Zhou'

  ! Add to global queue
  last = 1 + mod(last, MaxQueue)
  queue(:,last) = [a,b]

  NthreadMax = omp_get_max_threads()
  ! Allocate max var for each thread
  allocate(ThreadMax(1:NthreadMax))
  
  write ( *, '(a,i8)' ) &
       'The number of processors available = ', omp_get_num_procs()
  write ( *, '(a,i8)' ) &
       'The number of threads available    = ', NthreadMax
  
  ! Start measuring time
  wtime = omp_get_wtime()
  
  ! Extend the queue size to NthreadMax
  do while (ierror == 0 .and. isub < NthreadMax)
     
    ! Select the front of queue
     if ( first == MaxQueue ) then
        left = queue(1,1)
        right = queue(2,1)
     else                           
        left = queue(1,first + 1 )
        right = queue(2,first + 1)
     end if
     
     ! Estimation
     gleft  = g(left)
     gright = g(right)
     
     CurrentMax = max(CurrentMax ,gleft, gright)
     
     if( 0.5*(gleft+gright+s*(right-left)) <= CurrentMax + eps ) then
        status = .false.
     else
        status = .true.
     end if
            
     if(status) then
        ! Remove interval from the front of queue
        first = 1 + mod(first, MaxQueue)
        ! Add two sub-intervals to the queue
        last = 1 + mod(last, MaxQueue)
        queue(:,last) = [left, 0.5*(left+right)]
        last = 1 + mod(last, MaxQueue)
        queue(:,last) = [0.5*(left+right), right]
        
        isub = isub + 1
     else
        !write(*,*) 'Thread',id,', stop searching in [',left,',',right,']'
        
        first = 1 + mod(first, MaxQueue)
        
        isub = isub - 1
     end if

     if ( first == last ) then          ! Queue is empty
        ierror = -1                       ! Underflow occurs
     elseif ( first == last + 1 ) then  ! Queue is full
        ierror = -2                       ! Overflow *will* occur
     else                               ! All OK
        ierror = 0                        ! No over-/underflow
     end if

  end do

  ! Initialize the temporary max for each thread (which will be private).
  ThreadMax = CurrentMax
  
  ! Begin the parallel region.
  !$omp parallel private(range, status, left, right, id) &
  !$omp          shared (CurrentMax, ThreadMax, Nthread, Qlength, subQlength) &
  !$omp     firstprivate(queue, first, last, ierror, timeExchange)

  Nthread = omp_get_num_threads()
  id = omp_get_thread_num()

  !$omp single
  Qlength = last - first
  if ( Qlength < 0 ) Qlength = MaxQueue + Qlength
  ! equivalent to floor(real(Qlength)/Nthread)
  subQlength = Qlength/Nthread
  !$omp end single
  
  ! Assign queues for each thread
  first = first + subQlength*id
  if(id /= Nthread-1) last = first + subQlength
  
  do while (ierror == 0)

     ! Select the front of queue
     if ( first == Maxqueue ) then
        left = queue(1,1)
        right = queue(2,1)
     else                           
        left = queue(1,first + 1 )
        right = queue(2,first + 1)
     end if
     
     ! Check if we need to continue searching in this interval     
     gleft  = g(left)
     gright = g(right)
     
     ThreadMax(id+1) = max(ThreadMax(id+1) ,gleft, gright)
     
     if( 0.5*(gleft+gright+s*(right-left)) <= ThreadMax(id+1) + eps ) then
        status = .false.
     else
        status = .true.
     end if
     
     if(status) then
        ! Remove interval from the front of queue
        first = 1 + mod(first, MaxQueue)
        ! Add two sub-intervals to the queue
        last = 1 + mod(last, MaxQueue)
        queue(:,last) = [left, 0.5*(left+right)]
        last = 1 + mod(last, MaxQueue)
        queue(:,last) = [0.5*(left+right), right]
        
     else
        ! Remove interval from the front of queue
        first = 1 + mod(first, MaxQueue)
     end if
     
     if ( first == last ) then          ! Queue is empty
        ierror = -1                       ! Underflow occurs
     elseif ( first == last + 1 ) then  ! Queue is full
        ierror = -2                       ! Overflow *will* occur
     else                               ! All OK
        ierror = 0                        ! No over-/underflow
     end if
          
     timeExchange = mod(timeExchange+1,100)
     
     if(timeExchange==0) then
        !omp atomic
        ThreadMax(id+1) = maxval(ThreadMax)
        !omp end atomic
     end if
  end do
  
  !$omp end parallel

  ! Collect the max info
  CurrentMax = maxval(ThreadMax)
  
  ! End measuring time
  wtime = omp_get_wtime() - wtime

  ! Show the estimated max
  write(*,*) '------------------- '
  write(*,*) 'The estimated max =', CurrentMax
  write(*,*) 'Time elapsed      = ', wtime

  if(allocated(ThreadMax)) deallocate(ThreadMax)
  if(allocated(queue)) deallocate(queue)
  
end program main

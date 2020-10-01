program main
  ! EECS587, OpenMP assignment. See the details in HW4.
  !
  !
  ! Hongyang Zhou, 10/25/2017

  use omp_lib
  use CircleQueue
  use estimate

  implicit none

  integer :: first, last, localfirst
  real :: CurrentMax
  integer :: ierror
  real :: left, right, wtime
  logical :: status

  integer :: id, Nthread, NthreadMax, iloop=0
  real :: gleft, gright
  !----------------------------------------------------------------------------

  ! Initialize global queue
  first = 1
  last  = 1
  CurrentMax = -1e6
  ierror = 0

  allocate(queue(2,1:MaxQueue))
  queue = 0
  
  ! Print out info
  write(*,*) 'EECS587 Assignment 4: OpenMP on finding max for 1D function'
  write(*,*) 'f(x),x in [a,b], |f''(x)| <= s'
  write(*,*) 'Breadth-first search is implemented with queue structure.'
  write(*,*) 'Version 5 by Hongyang Zhou'

  ! Add to global queue
  call add_to_circqueue([a,b],queue,first,last,ierror)

  NthreadMax = omp_get_max_threads()
  
  write ( *, '(a,i8)' ) &
       'The number of processors available = ', omp_get_num_procs()
  write ( *, '(a,i8)' ) &
       'The number of threads available    = ', NthreadMax
  
  ! Start measuring time
  wtime = omp_get_wtime()

  
  ! Begin the parallel region.
  !$omp parallel private(left, right, id, gleft, gright, localfirst, status) &
  !$omp          shared (queue, first, last, CurrentMax, iloop, ierror)

  ! My feeling is that maybe there is false sharing!
  
  id = omp_get_thread_num()
  
  do while (ierror == 0)

     !$omp critical(Pop)
     if(first /= last) then
        first = 1 + mod(first, MaxQueue) ! pop out the front element
        localfirst = first
        status = .true.    ! allocate work to id
     end if  
     !$omp end critical(Pop)

     if(status) then
        left  = queue(1,localfirst)
        right = queue(2,localfirst)
     
        ! Check if we need to continue searching in this interval
        gleft  = g(left)
        gright = g(right)

        !$omp atomic
        CurrentMax = max(CurrentMax ,gleft, gright)
        !$omp end atomic
     end if

     if(status) then
        if( 0.5*(gleft+gright+s*(right-left)) > CurrentMax + eps ) then

           !$omp critical(Add)
           ! Add the first sub-interval
           last = 1 + mod(last, MaxQueue)
           
           if ( first /= last ) then       ! Queue is not full
              queue(:,last) = [left, 0.5*(left+right)]       ! Insert node
              ierror = 0                 ! Everything is OK       
           else                            ! Queue is full
              ierror = -2                ! Overflow occurs
           end if

           ! Add the second sub-interval
           last = 1 + mod(last, MaxQueue)
        
           if ( first /= last ) then       ! Queue is not full
              queue(:,last) = [0.5*(left+right),right]       ! Insert node
              ierror = 0                 ! Everything is OK       
           else                            ! Queue is full
              ierror = -2                ! Overflow occurs
           end if
           !$omp end critical(Add)
        end if
     end if

     status = .false.   ! threads are available now

     !$omp barrier
     !$omp single
     if ( first == last ) then          ! Queue is empty
        ierror = -1                       ! Underflow occurs
     elseif ( first == last + 1 ) then  ! Queue is full
        ierror = -2                       ! Overflow *will* occur
     else                               ! All OK
        ierror = 0                        ! No over-/underflow
     end if
     !$omp end single

  end do
  
  !$omp end parallel

  
  ! End measuring time
  wtime = omp_get_wtime() - wtime

  ! Show the estimated max
  write(*,*) '------------------- '
  write(*,*) 'The estimated max =', CurrentMax
  write(*,*) 'Time elapsed      = ', wtime

  if(allocated(queue)) deallocate(queue)
  
end program main

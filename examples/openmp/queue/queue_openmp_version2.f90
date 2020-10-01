program main
  ! EECS587, OpenMP assignment. See the details in HW4.
  !
  ! One problem I can see is that all threads only update the CurrentMax at
  ! the end, which will potentially waste some useful info. An idea is to
  ! exchange info more frequently. See version3.
  !
  ! Hongyang Zhou, 10/25/2017

  use omp_lib
  use CircleQueue
  use estimate

  implicit none

  integer :: first, last
  real :: CurrentMax, ThreadMax
  integer :: ierror
  real :: left, right, range(2), wtime
  logical :: status

  integer :: id, Nthread, NthreadMax, Qlength, subQlength, isub
  !----------------------------------------------------------------------------

  ! Initialize global queue
  first = 1
  last  = 1
  CurrentMax = -1e10
  ierror = 0
  isub = 1

  allocate(queue(2,1:MaxQueue))
  queue = 0
  
  ! Print out info
  write(*,*) 'EECS587 Assignment 4: OpenMP on finding max for 1D function'
  write(*,*) 'f(x),x in [a,b], |f''(x)| <= s'
  write(*,*) 'Breadth-first search is implemented with queue structure.'
  
  ! Add to global queue
  call add_to_circqueue([a,b],queue,first,last,ierror)

  ! Check queue
  !if(ierror==0) then
  !   write(*,*) 'Initial'
  !   call show_queue(queue,first,last)
  !end if

  NthreadMax = omp_get_max_threads()
  
  write ( *, '(a,i8)' ) &
       'The number of processors available = ', omp_get_num_procs()
  write ( *, '(a,i8)' ) &
       'The number of threads available    = ', NthreadMax
  
  ! Start measuring time
  wtime = omp_get_wtime()
  
  ! Extend the queue size to NthreadMax
  do while (ierror == 0 .and. isub < NthreadMax)
     
     call select_from_circqueue(range, queue, first, last, ierror)
     
     left = range(1); right = range(2)
     
     call estimate_interval(left,right,CurrentMax,status)
     
     ierror = queue_flag(first,last)
       
     if(status) then
        call delete_from_circqueue(queue,first,last,ierror)
        
        call add_to_circqueue([left, 0.5*(left+right)],queue,first,last,ierror)
        call add_to_circqueue([0.5*(left+right), right],queue,first,last,ierror)

        ierror = queue_flag(first,last)
        isub = isub + 1
     else
        !write(*,*) 'Thread',id,', stop searching in [',left,',',right,']'
        
        call delete_from_circqueue(queue,first,last,ierror)
        
        ierror = queue_flag(first,last)
        isub = isub - 1
     end if
     
  end do

  ! Initialize the temporary max for each thread (which will be private).
  ThreadMax = CurrentMax
  
  ! Begin the parallel region.
  !$omp parallel private(range, status, left, right, id) &
  !$omp          shared (CurrentMax, Nthread, Qlength, subQlength) &
  !$omp     firstprivate(queue, first, last, ierror, ThreadMax)

  Nthread = omp_get_num_threads()
  id = omp_get_thread_num()

  !$omp single
  write(*,*) 'Parallel region begins with ',Nthread,' threads.'
  write(*,*) ' '
  !$omp end single
  
  Qlength = cardinality_of_circqueue(first,last)
  ! equivalent to floor(real(Qlength)/Nthread)
  subQlength = Qlength/Nthread

  !$omp single
  write(*,*) 'Qlength,subQlength=',Qlength,subQlength
  !$omp end single
  
  ! Assign queues for each thread
  first = first + subQlength*id
  if(id /= Nthread-1) last = first + subQlength
  
  do while (ierror == 0)

     ! Select the front of queue
     call select_from_circqueue(range, queue, first, last, ierror)
     
     left = range(1); right = range(2)

     ! Check if we need to continue searching in this interval
     call estimate_interval(left,right,ThreadMax,status)
     
     ierror = queue_flag(first,last)
     
     if(status) then
        ! Remove interval from the front of queue
        call delete_from_circqueue(queue, first,last,ierror)
        ! Add two subintervals to the queue
        call add_to_circqueue([left, 0.5*(left+right)],queue,first,last,ierror)
        call add_to_circqueue([0.5*(left+right), right],queue,first,last,ierror)
     else        
        call delete_from_circqueue(queue,first,last,ierror)
     end if
     ierror = queue_flag(first,last)
  end do

  !$omp atomic
  CurrentMax = max(ThreadMax,CurrentMax)
  !$omp end atomic
  
  !$omp end parallel

  ! End measuring time
  wtime = omp_get_wtime() - wtime
  
  ! Show the estimated max
  write(*,*) '------------------- '
  write(*,*) 'The estimated max =', CurrentMax
  write(*,*) 'Time elapsed      = ', wtime

  deallocate(queue)
  
end program main

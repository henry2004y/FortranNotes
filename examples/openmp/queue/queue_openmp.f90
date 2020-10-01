program main
  ! Test of using queue structure in Fortran.
  !
  ! Hongyang Zhou, 10/23/2017

  use omp_lib
  use CircleQueue
  use estimate

  implicit none

  integer :: first, last
  real :: CurrentMax
  integer :: ierror
  real :: left, right, range(2), wtime
  logical :: status

  integer :: id, Nthread, NthreadMax, Qlength, subQlength, isub
  !----------------------------------------------------------------------------

  ! Initialize queue
  first = 1
  last  = 1
  CurrentMax = -1e10
  queue = 0
  ierror = 0
  isub = 1

  ! Print out info
  write(*,*) 'EECS587 Assignment 4: OpenMP on finding max for 1D function'
  write(*,*) 'Breadth-first search is implemented with queue structure.'
  
  ! Add to queue
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
  
  ! 
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

  !if(ierror==0) then
  !   write(*,*) 'Second:'
  !   write(*,*) 'first,last=',first,last
  !   call show_queue(first,last)
  !end if
  !write(*,*) 'isub=',isub

  ! There is some issue if queue is private! threadprivate? You cannot assume
  ! local Module scope variable !
  
  ! Begin the parallel region.
  !$omp parallel private(range, status, left, right, id) &
  !$omp          shared (CurrentMax, Nthread, Qlength, subQlength) &
  !$omp     firstprivate(queue, first, last, ierror)

  Nthread = omp_get_num_threads()
  id = omp_get_thread_num()
  
  !$omp single
  !write(*,*) 'thread ',id
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

  !if(ierror==0 .and. id==1) then
  !   write(*,*) 'Before entering while loop,thread=',id
  !   write(*,*) 'first,last=',first,last
  !   !write(*,*) 'queue=',queue(:,15:16)
  !   call show_queue(queue,first,last)
  !end if
  
  do while (ierror == 0)

     call select_from_circqueue(range, queue, first, last, ierror)
     
     left = range(1); right = range(2)

     !$omp critical
     call estimate_interval(left,right,CurrentMax,status)
     !$omp end critical
     
     ierror = queue_flag(first,last)
     
     !if(id==1)  write(*,*) 'status,ierror=',status,ierror
     
     if(status) then
        call delete_from_circqueue(queue, first,last,ierror)
        
        call add_to_circqueue([left, 0.5*(left+right)],queue,first,last,ierror)
        
        call add_to_circqueue([0.5*(left+right), right],queue,first,last,ierror)
        
     else        
        !write(*,*) 'Thread',id,', stop searching in [',left,',',right,']'
        
        call delete_from_circqueue(queue,first,last,ierror)
        
     end if
     ierror = queue_flag(first,last)
  end do

  !$omp end parallel

  ! End measuring time
  wtime = omp_get_wtime ( ) - wtime
  
  ! Show the estimated max
  write(*,*) '------------------- '
  write(*,*) 'The estimated max =', CurrentMax
  write(*,*) 'Time elapsed      = ', wtime

end program main

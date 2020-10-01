program main
  ! EECS587 Assignment 4. Serial version.
  ! My original version waste a lot of time on repeatly evaluating function
  ! values that should be stored together with x in the queue. This is a hard
  ! lesson: before implementing parallization, think about the algorithm more
  ! carefully!
  !
  ! Hongyang Zhou, 11/06/2017
  
  use CircleQueue
  use estimate

  implicit none

  integer :: first, last
  real :: CurrentMax
  integer :: ierror
  real :: left, right, range(2), start, finish
  logical :: status
  !----------------------------------------------------------------------------

  ! Initialize queue
  first = 1
  last  = 1
  CurrentMax = -1e6
  ierror = 0

  ! Each element in the queue represent:
  ! 1. x value left;
  ! 2. x value right;
  ! 3. f value left;
  ! 4. f value right;
  allocate(queue(4,1:MaxQueue))
  queue = 0
  
  ! Print out info
  write(*,*) 'EECS587 Assignment 4: serial version'
  write(*,*) 'Breadth-first branch and bound algorithm'
  
  call cpu_time(start)
  
  ! Add to queue
  call add_to_circqueue([a,b],queue, first,last,ierror)

  
  
  ! Check queue
  !if(ierror==0) then
  !   write(*,*) 'Initial'
  !   call show_queue(queue, first,last)
  !end if

  
  do while (ierror == 0)

     call select_from_circqueue(range, queue, first, last, ierror)

     left = range(1); right = range(2)
     
     call estimate_interval(left,right,CurrentMax,status)
     
     if(status  .and. ierror /= -2) then
        call delete_from_circqueue(queue,first,last,ierror)
        
        call add_to_circqueue([left, 0.5*(left+right)],queue,first,last,ierror)

        call add_to_circqueue([0.5*(left+right), right],queue,first,last,ierror)
        ierror = queue_flag(first,last)
     elseif (.not. status) then
        !write(*,*) 'stop searching in [',left,',',right,']'

        call delete_from_circqueue(queue,first,last,ierror)
        ierror = queue_flag(first,last)
     else
        write(*,*) 'The queue is full! Exit...'
        exit
     end if
  end do

  call cpu_time(finish)
  
  ! Show the estimated max
  write(*,*) 'The estimated max =', CurrentMax
  write(*,*) 'Time elapsed      = ', finish-start

  deallocate(queue)
  
end program main

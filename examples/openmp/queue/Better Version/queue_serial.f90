program main
  ! EECS587 Assignment 4. Serial version.
  ! My original version waste a lot of time on repeatly evaluating function
  ! values that should be stored together with x in the queue. This is a hard
  ! lesson: before implementing parallization, think about the algorithm more
  ! carefully!
  !
  ! Hongyang Zhou, 11/06/2017

  use omp_lib
  
  implicit none

  integer, parameter :: MaxQueue = 1e6
  ! Maximum slope
  real, parameter :: s = 12

  ! tolerate error
  real, parameter :: eps = 1e-6

  ! range of x
  real, parameter :: a=1, b=100
  
  integer :: first, last, localfirst
  real :: CurrentMax
  real :: left, right, middle, wtime
  logical :: status

  integer :: NthreadMax, Qlength
  real :: gleft, gright, gmiddle

  real, allocatable :: queue(:,:)
  !----------------------------------------------------------------------------

  ! Initialize global queue
  first = 1
  last  = 1
  CurrentMax = -1e6

  allocate(queue(4,1:MaxQueue))
  queue = 0
  
  ! Print out info
  write(*,*) 'EECS587 Assignment 4: OpenMP on finding max for 1D function'
  write(*,*) 'f(x),x in [a,b], |f''(x)| <= s'
  write(*,*) 'Breadth-first search is implemented with queue structure.'

  ! Add the first interval
  gleft  = g(a)
  gright = g(b)
  
  last = 1 + mod(last, MaxQueue)
  
  if ( first /= last ) then       ! Queue is not full
     ! Insert node
     queue(:,last) = [a, b, gleft, gright]   
  else                            ! Queue is full
     write(*,*) 'Error for the queue! Check your memory allocation!'
     stop
  end if

  NthreadMax = omp_get_max_threads()
  
  write ( *, '(a,i8)' ) &
       'The number of processors available = ', omp_get_num_procs()
  write ( *, '(a,i8)' ) &
       'The number of threads available    = ', NthreadMax
  
  ! Start measuring time
  wtime = omp_get_wtime()
  
  do while (.true.)     
     if(first /= last) then
        first = 1 + mod(first, MaxQueue) ! pop out the front element
        localfirst = first
        status = .true.    ! allocate work to id
     else
        exit
     end if  

     if(status) then
        left  = queue(1,localfirst)
        right = queue(2,localfirst)
        gleft = queue(3,localfirst)
        gright= queue(4,localfirst)
        
        ! Check if we need to continue searching in this interval
        CurrentMax = max(CurrentMax ,gleft, gright)

        if( 0.5*(gleft+gright+s*(right-left)) > CurrentMax + eps ) then

           ! Add the first sub-interval
           last = 1 + mod(last, MaxQueue)
           
           if ( first /= last ) then       ! Queue is not full
              middle =  0.5*(left+right)
              gmiddle = g(middle)
              ! Insert node
              queue(:,last) = [left, middle, gleft, gmiddle]
           else
              exit
           end if

           ! Add the second sub-interval
           last = 1 + mod(last, MaxQueue)
        
           if ( first /= last ) then       ! Queue is not full
              ! Insert node
              queue(:,last) = [middle, right, gmiddle, gright]
           else
              exit
           end if
        end if
     end if

     status = .false.   ! threads are available now     
  end do
    
  ! End measuring time
  wtime = omp_get_wtime() - wtime

  ! Show the estimated max
  write(*,*) '------------------- '
  write(*,*) 'The estimated max =', CurrentMax
  write(*,*) 'Time elapsed      = ', wtime

  if(allocated(queue)) deallocate(queue)

contains
  !===================================
  elemental function g(x) result(y)
    
    implicit none
    real, intent(in) :: x
    real ::  y, temp
    integer :: i, j
    !-----------------------------------
    y = 0
    do i = 100, 1, -1
       temp = 0.0
       do j = i, 1, -1
          temp = temp + (x + j)**(-3.1)
       end do
       y = y + sin(x + temp) / (1.2**i)
    end do
    
  end function g
  
end program main



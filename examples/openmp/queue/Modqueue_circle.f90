Module CircleQueue

  implicit none

  ! If I use 5e5, the size of queue would be 8e6 bytes, or 8MB.
  integer, parameter :: MaxQueue = 1e6

  !real :: queue(2,MaxQueue)
  real, allocatable :: queue(:,:)
  
  ! QUEUE must be implemented by client as Integer Array of
  !       shape (1:n), able to hold n-1 nodes. (Actually, it can hold n
  !       nodes the first time, but not after circulation has occured.
  !
  ! FIRST is a cursor that keeps track of the Front of the Queue.
  ! LAST is a cursor that keeps track of the Rear of the Queue.
  !      Both FIRST and LAST must be initialized to 1 by client to
  !      denote empty Queue. Client is responsible for keeping 
  !      them within bounds.

contains
  !============================================================================
  ! Enqueues node, i.e. adds it to the rear of the Queue. If the queue array
  ! size has been exceeded, it tries to insert the node, circularly, to the
  ! front of the array. However, if the queue array is really full, it returns
  ! without error (but sets the Overflow flag).  
  ! Client is responsible to check for Queue overflow.
  subroutine add_to_circqueue(node,queueIn,first,last,overflow)
    real, intent(in) ::  node(2)
    real, intent(inout) :: queueIn(2,MaxQueue)
    integer, intent(in) ::first
    integer, intent(inout) :: last, overflow
    !--------------------------------------------------------------------------

    ! Array size exceeded: start from beginning; else increase to next free
    last = 1 + mod(last, MaxQueue)
           
    if ( first /= last ) then       ! Queue is not full
       queueIn(:,last) = node       ! Insert node
       overflow = 0                 ! Everything is OK       
    else                            ! Queue is full
       overflow = -2                ! Overflow occurs
    end if

  end subroutine add_to_circqueue
  !============================================================================

  ! Dequeues front, i.e. deletes the front node from queue. If Queue is empty,
  ! it does nothing. Client is responsible to check underflow for violations
  ! of Queue size.
  subroutine delete_from_circqueue(queueIn,first,last,underflow)
    real, intent(inout)    :: queueIn(2,MaxQueue)
    integer, intent(inout) :: first
    integer, intent(out)   :: underflow
    integer, intent(in)    :: last
    !--------------------------------------------------------------------------
    
    if ( first == last ) then          ! Queue is empty
       underflow = -1                  ! Underflow occurs
    else                               ! Queue is not empty
       underflow = 0                   ! No underflow

       ! If size of queue array has been exceeded, start over; else, normal
       first = 1 + mod(first, MaxQueue)
    end if

  end subroutine delete_from_circqueue
  !============================================================================
  
  ! Selects the front element from queue without deleting it. If Queue is empty,
  ! it does nothing. Need to check for violations of Queue size.
  subroutine select_from_circqueue(node,queueIn,first,last,underflow)
    integer, intent(in)  :: last,first
    real, intent(in)  :: queueIn(2,MaxQueue)
    real, intent(out) :: node(2)
    integer, intent(out) ::underflow
    !--------------------------------------------------------------------------

    if ( first == last ) then          ! Queue is empty
       underflow = -1                  ! Underflow occurs
    else                               ! Queue is not empty
       underflow = 0                   ! No underflow
       
       if ( first == MaxQueue ) then   ! Size of queue array has 
          node = queueIn( :,1 )        ! been exceeded. Start over.
       else                            ! Has not been exceeded,
          node = queueIn( :,first + 1 )! proceed as usual.
       end if
    end if

  end subroutine select_from_circqueue
  !============================================================================

  ! Number of elements in Queue
  function cardinality_of_circqueue(first,last) result(card)
    integer, intent(in) :: last,first
    integer :: card
    !--------------------------------------------------------------------------
    card = last - first
    if ( card < 0 ) card = MaxQueue + card 

  end function cardinality_of_circqueue
  !============================================================================

  ! Assign Queue elements to Array in FIFO order
  !function queue2array(first,last) result(array)
  !  integer, intent(in) :: first, last
  !  real :: array(2,MaxQueue)
  !  integer :: k
  !  !--------------------------------------------------------------------------
  !  
  !  if ( last < first ) then
  !     k = MaxQueue - first
  !     array(:,1:k) = queue(:,first+1:first+k)
  !     array(:,k+1:k+last) = queue(:,1:last)
  !     array(:,k+last+1:MaxQueue) = 0
  !  else
  !     k = last - first 
  !     array(:,1:k) = queue(:,first+1:last)
  !     array(:,k+1:MaxQueue) = 0
  !  end if
  !end function queue2array
  !============================================================================

  ! Check queue for overflow or underflow
  function queue_flag(first,last) result(flag)
    integer, intent(in) :: first, last
    integer :: flag
    !--------------------------------------------------------------------------
    if ( first == last ) then          ! Queue is empty
       flag = -1                       ! Underflow occurs
    elseif ( first == last + 1 ) then  ! Queue is full
       flag = -2                       ! Overflow *will* occur
    else                               ! All OK
       flag = 0                        ! No over-/underflow
    end if

  end function queue_flag
  !=============================================================================

  ! Display the elements in the current queue
  subroutine show_queue(queueIn,first,last)
    real, intent(in) :: queueIn(2,MaxQueue)
    integer, intent(in) :: first,last
    integer :: i
    !--------------------------------------------------------------------------

    write(*,*) 'queue='

    if ( last < first ) then
       do i=first+1,MaxQueue
          write(*,*) queueIn(:,i)
       end do
       do i=1,last
          write(*,*) queueIn(:,i)
       end do
    else       
       do i=first+1,last
          write(*,*) queueIn(:,i)
       end do
    end if
    
  end subroutine show_queue
  
end Module CircleQueue

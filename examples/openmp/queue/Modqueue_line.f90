Module LineQueue

  implicit none
!
! QUEUE must be implemented by client as Integer Array.
!       Client is responsible for checking the array bounds.
! FIRST is a cursor that keeps track of the Front of the Queue.
! LAST is a cursor that keeps track of the Rear of the Queue.
!      Both FIRST and LAST must be initialized to 0 by client to
!      denote empty Queue. Client is responsible for keeping 
!      them within bounds.

contains
  !============================================================================
  ! Enqueues node, i.e. adds it to the rear of the Queue.
  ! If Queue size has been exceeded, it does nothing and 
  ! returns without error.  Client is responsible to
  ! check for Queue overflow.
  subroutine add_to_queue(node,queue,last)
    integer, intent(in)    :: node
    integer, intent(inout) :: last
    integer, intent(inout), dimension(:) :: queue
    !--------------------------------------------------------------------------

    if ( last < size(queue) ) then
       last = last + 1
       queue(last) = node
    end if

  end subroutine add_to_queue
  !============================================================================
  
  ! Dequeues front, i.e. deletes the front node from queue.
  ! If Queue is empty, it does nothing. Client is responsible
  ! to check for violations of Queue size.
  subroutine delete_from_queue(first,last)
    integer, intent(inout) :: first
    integer, intent(in) :: last
    !--------------------------------------------------------------------------
    
    if ( first < last ) then
       first = first + 1
    end if

  end subroutine delete_from_queuedelete_from_queue
  !============================================================================
  
  ! Selects the front element from queue without deleting it.
  ! If Queue is empty, it does nothing. Client is responsible
  ! to check  for violations of Queue size.
  subroutine select_from_queue(node,queue,first,last)
    integer, intent(in)  :: last,first
    integer, intent(out) :: node
    integer, intent(in), dimension(:) :: queue
    !--------------------------------------------------------------------------
    if ( first < last ) then
       node = queue(first+1)
    end if

  end subroutine select_from_queueselect_from_queue

end Module LineQueue

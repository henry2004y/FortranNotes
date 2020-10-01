MODULE QueueObject
  IMPLICIT NONE

  !----------! Interface provided to clients !-------------------!

  TYPE QueueT ! Queue able to hold Size-1 nodes, one less than expected.
     PRIVATE
     INTEGER, ALLOCATABLE, DIMENSION(:) :: key ! OBS: To 0:Size-1
     INTEGER :: First, Last, ErrorNr, Size
     LOGICAL :: Error
  END TYPE QueueT

  ! Queue Operators that are available to clients under different names


  INTERFACE enqueue
     MODULE PROCEDURE add_to_circqueue
  END INTERFACE enqueue

  INTERFACE dequeue
     MODULE PROCEDURE delete_from_circqueue
  END INTERFACE dequeue

  INTERFACE peek
     MODULE PROCEDURE select_from_circqueue
  END INTERFACE peek

  ! These operators are not available to clients

  PRIVATE :: add_to_circqueue, delete_from_circqueue, select_from_circqueue, SetErrorFlags

  ! All other Operators are public and thus, available to clients


CONTAINS !-----------! Operators are implemented below !---------!

  ! (1) Construct and initialize a new Queque

  FUNCTION NewQueue(QueueSize) Result(Queue)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: QueueSize
    INTEGER :: ierror
    TYPE(QueueT) :: Queue

    IF ( .NOT. ALLOCATED(Queue%Key) ) THEN
       ALLOCATE( Queue%Key(0:QueueSize-1), STAT=ierror )
       IF ( ierror /= 0 ) THEN
          CALL SetErrorFlags( Queue, .TRUE., -1 )
       ELSE
          Queue%Key = 0
          Queue%Size = QueueSize
          Queue%First = 0; Queue%Last = 0
          CALL SetErrorFlags( Queue, .FALSE., 0)
       ENDIF
    ENDIF

  END FUNCTION NewQueue

  ! (2) Destroy a Queue and free the memory it occupies

  SUBROUTINE DisposeQueue( Queue )
    IMPLICIT NONE
    INTEGER :: ierror
    TYPE(QueueT) :: Queue

    CALL SetErrorFlags(Queue,.FALSE.,0)

    IF ( ALLOCATED(Queue%Key) ) THEN
       DEALLOCATE( Queue%Key, STAT=ierror )
       IF ( ierror /= 0 ) THEN
          CALL SetErrorFlags( Queue, .TRUE., -2)
       ENDIF
    ENDIF

  END SUBROUTINE DisposeQueue

  !----! Routines, public under different names !-----------!

  ! (3) Add a Node to the rare of the  Queue (Enqueue)

  SUBROUTINE add_to_circqueue(Node,Queue)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: Node
    TYPE(QueueT) :: Queue

    Queue%Last = MOD(Queue%Last+1,Queue%Size)   ! Circular Queue

    IF ( Queue%First /= Queue%Last ) THEN       ! Queue is not ful
       Queue%Key(Queue%Last) = Node           ! Insert node
       CALL SetErrorFlags( Queue, .FALSE., 0 )
    ELSE                            ! Queue is ful
       CALL SetErrorFlags( Queue, .TRUE., -3 )
    ENDIF

  END SUBROUTINE add_to_circqueue

  ! (4) Remove a Node from the front of the Queue (Dequeue)

  SUBROUTINE delete_from_circqueue(Queue,Node)
    IMPLICIT NONE
    INTEGER, OPTIONAL :: Node
    TYPE(QueueT) :: Queue

    IF ( Queue%First /= Queue%Last ) THEN       ! Queue is not empty
       CALL SetErrorFlags( Queue, .FALSE., 0)
       Queue%First = MOD( Queue%First+1, Queue%Size )
       IF ( PRESENT(Node) ) Node = Queue%Key(Queue%First)
    ELSE                            ! Queue is empty
       CALL SetErrorFlags( Queue, .TRUE., -4)
       IF ( PRESENT(Node) ) Node = HUGE(Node)
    ENDIF

  END SUBROUTINE delete_from_circqueue

  ! (5) Select the Front Node of the Queue for examination (Peek)

  FUNCTION select_from_circqueue(Queue) RESULT(Node)
    IMPLICIT NONE
    INTEGER :: Node
    TYPE(QueueT) :: Queue
    INTEGER :: k

    IF ( Queue%First /= Queue%Last ) THEN ! Queue is not empty
       k = MOD( Queue%First+1, Queue%Size )
       Node = Queue%Key(k)
       CALL SetErrorFlags( Queue, .FALSE., 0)
    ELSE                      ! Queue is empty
       CALL SetErrorFlags( Queue, .TRUE., -5)
       Node = HUGE(Node)
    ENDIF

  END FUNCTION select_from_circqueue

  !----! Public Routines necessary due to Implementation Hiding !----!

  ! (6) Convert Queue to an Array

  SUBROUTINE Queue2Array(Queue,Array)
    IMPLICIT NONE
    INTEGER, DIMENSION(1:) :: Array
    TYPE(QueueT) :: Queue

    IF ( SIZE(Array) >= Queue%Size ) THEN
       Array(1:Queue%Size) = Queue%Key(0:Queue%Size-1)
       CALL SetErrorFlags(Queue, .FALSE., 0)
    ELSE
       Array = Queue%Key(0:SIZE(Array)-1)
       CALL SetErrorFlags(Queue, .TRUE., -6)
    ENDIF

  END SUBROUTINE Queue2Array

  ! (7) Check whether Queue has error flag on

  FUNCTION HasQueueError(Queue) RESULT(Answer)
    IMPLICIT NONE
    LOGICAL :: Answer
    TYPE(QueueT) :: Queue

    Answer = Queue%Error

  END FUNCTION HasQueueError

  ! (8) Check the error number of Queue

  FUNCTION IsQueueError(Queue) RESULT(n)
    IMPLICIT NONE
    INTEGER :: n
    TYPE(QueueT) :: Queue

    n = Queue%ErrorNr

  END FUNCTION IsQueueError


  ! (9) Check whether Queue is empty

  FUNCTION IsQueueEmpty(Queue) RESULT(Answer)
    IMPLICIT NONE
    LOGICAL :: Answer
    TYPE(QueueT) :: Queue

    Answer = ( Queue%Last == Queue%First )

  END FUNCTION IsQueueEmpty


  ! (10) Check whether Queue is ful

  FUNCTION IsQueueFul(Queue) RESULT(Answer)
    IMPLICIT NONE
    LOGICAL :: Answer
    TYPE(QueueT) :: Queue
    INTEGER :: k

    k = MOD(Queue%Last+1,Queue%Size)

    Answer = ( Queue%First == k )

  END FUNCTION IsQueueFul

  ! (11) How many Nodes populate the Queue?

  FUNCTION QueueCardinality(Queue) RESULT(n)
    IMPLICIT NONE
    INTEGER :: n
    TYPE(QueueT) :: Queue

    n = Queue%Last - Queue%First
    IF ( n < 0 ) n = Queue%Size+n ! Circulation has occured

  END FUNCTION QueueCardinality

  ! (12) Clear Queue and prepare it for new use

  SUBROUTINE ClearQueue(Queue)
    IMPLICIT NONE
    TYPE(QueueT) :: Queue

    Queue%Last = 0; Queue%First = 0

  END SUBROUTINE ClearQueue

  !--------------! Private !--------------------!
  ! Set error flags and error numbers of Queue

  SUBROUTINE SetErrorFlags(Queue, Happened, Value)
    IMPLICIT NONE
    LOGICAL :: Happened
    INTEGER :: Value
    TYPE(QueueT) :: Queue

    Queue%Error = Happened
    Queue%ErrorNr = Value

  END SUBROUTINE SetErrorFlags

  END MODULE QueueObject

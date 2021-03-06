program main

  !*****************************************************************************80
  !
  !! MAIN is the main program for HELLO_MPI.
  !
  !  Discussion:
  !
  !    HELLO is a simple MPI test program.
  !
  !    Each process prints out a "Hello, world!" message.
  !
  !    The master process also prints out a short message.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license.
  !
  !  Modified:
  !
  !    30 October 2008
  !
  !  Author:
  !
  !    John Burkardt
  !
  !  Reference:
  !
  !    William Gropp, Ewing Lusk, Anthony Skjellum,
  !    Using MPI: Portable Parallel Programming with the
  !    Message-Passing Interface,
  !    Second Edition,
  !    MIT Press, 1999,
  !    ISBN: 0262571323,
  !    LC: QA76.642.G76.
  !
  use mpi

  integer :: error
  integer :: id
  integer :: p
  real :: wtime
  !
  !  Initialize MPI.
  !
  call MPI_Init ( error )
  !
  !  Get the number of processes.
  !
  call MPI_Comm_size ( MPI_COMM_WORLD, p, error )
  !
  !  Get the individual process ID.
  !
  call MPI_Comm_rank ( MPI_COMM_WORLD, id, error )
  !
  !  Print a message.
  !
  if ( id == 0 ) then

     wtime = MPI_Wtime ( )

     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) 'HELLO_MPI - Master process:'
     write ( *, '(a)' ) '  FORTRAN90/MPI version'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  An MPI test program.'
     write ( *, '(a)' ) ' '
     write ( *, '(a,i8)' ) '  The number of processes is ', p
     write ( *, '(a)' ) ' '

  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,a)' ) '  Process ', id, ' says "Hello, world!"'

  if ( id == 0 ) then

     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) 'HELLO_MPI - Master process:'
     write ( *, '(a)' ) '  Normal end of execution: "Goodbye, world!".'

     wtime = MPI_Wtime ( ) - wtime
     write ( *, '(a)' ) ' '
     write ( *, '(a,g14.6,a)' ) &
          '  Elapsed wall clock time = ', wtime, ' seconds.'

  end if
  !
  !  Shut down MPI.
  !
  call MPI_Finalize ( error )

  stop
end program main

subroutine timestamp ( )

  !*****************************************************************************80
  !
  !! TIMESTAMP prints the current YMDHMS date as a time stamp.
  !
  !  Example:
  !
  !    31 May 2001   9:45:54.872 AM
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license.
  !
  !  Modified:
  !
  !    06 August 2005
  !
  !  Author:
  !
  !    John Burkardt
  !
  !  Parameters:
  !
  !    None
  !
  implicit none

  character ( len = 8 ) ampm
  integer :: d
  integer :: h
  integer :: m
  integer :: mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
       'January  ', 'February ', 'March    ', 'April    ', &
       'May      ', 'June     ', 'July     ', 'August   ', &
       'September', 'October  ', 'November ', 'December ' /)
  integer :: n
  integer :: s
  integer :: values(8)
  integer :: y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
     ampm = 'AM'
  else if ( h == 12 ) then
     if ( n == 0 .and. s == 0 ) then
        ampm = 'Noon'
     else
        ampm = 'PM'
     end if
  else
     h = h - 12
     if ( h < 12 ) then
        ampm = 'PM'
     else if ( h == 12 ) then
        if ( n == 0 .and. s == 0 ) then
           ampm = 'Midnight'
        else
           ampm = 'AM'
        end if
     end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
       d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
  end

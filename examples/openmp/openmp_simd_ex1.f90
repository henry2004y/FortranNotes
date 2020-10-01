program example

  implicit none

  integer, parameter :: N = 100
  integer :: i
  real :: a(N), b(N), c(N), d(N)

  a = 1.0
  b = 2.0
  c = 3.0
  d = 0.0

  do i=1,N
     d(i) = min_var(distsq(a(i),b(i)),c(i))
  end do

  contains
    function min_var(a,b)
      real, intent(in):: a,b
      real :: min_var

      if(a < b)then
         min_var = a
      else
         min_var = b
      end if
    end function min_var

    pure function distsq(x,y)
      real, intent(in):: x,y
      real :: distsq

      distsq = (x-y)*(x-y)

    end function distsq

end program example

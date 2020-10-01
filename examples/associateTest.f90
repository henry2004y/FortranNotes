program associateTest

  implicit none
  integer :: a=1,b=1
  associate( x => a*b )
    print *, x                ! yields: 1
    a=10
    print *, x                ! yields: 1
  end associate

  associate( x => a )
    print *, x                ! yields: 10
    a=100
    print *, x                ! yields: 100
  end associate

end program associateTest

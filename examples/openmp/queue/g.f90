module GModule
  implicit none
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
      
end module GModule

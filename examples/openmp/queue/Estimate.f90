module estimate

  use GModule
  
  implicit none

  ! Maximum slope
  real, parameter :: s = 12

  ! tolerate error
  real, parameter :: eps = 1e-6

  ! range of x
  real, parameter :: a=1, b=100
  
contains
  !=============================================================================

  subroutine estimate_interval(left,right,TempMax,status)

    real, intent(in) :: left, right
    real, intent(inout) :: TempMax
    logical, intent(out):: status

    real :: gleft, gright
    !real, parameter :: s = 12
    !---------------------------------------------------------------------------

    gleft  = gTest(left)
    gright = gTest(right)
    
    TempMax = max(TempMax ,gleft, gright)
    
    if( 0.5*(gleft+gright+s*(right-left)) <= TempMax + eps ) then
       status = .false.
    else
       status = .true.
    end if
    
  end subroutine estimate_interval
  !=============================================================================

  ! Test function for finding max
  function gTest(x)

    real, intent(in)  :: x
    real  :: gTest
    !---------------------------------------------------------------------------

    !gTest = -(x-50)**2
    !gTest = (x-30)*(x-110)*(x+10)
    !gTest = x**3
    gTest  = sin(x/30.)
    
  end function gTest
  !=============================================================================

  ! This is an example of horrible bug: y is not initialized!
  elemental function g1(x) result(y)
    real, intent(in) :: x
    real ::  y
    !---------------------------------------------------------------------------
    !y = 0
    y = y + sin(x)
	
  end function g1
  !=============================================================================
  
  function g2(x)
    implicit none
    real, intent(in) :: x
    real ::  g2, temp, y
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

    g2 = 1
    
  end function g2
  
  
end module estimate

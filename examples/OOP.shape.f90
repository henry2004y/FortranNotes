module shape_mod
  !! Module for shape class.

  type shape
     integer :: color
     logical :: filled
     integer :: x
     integer :: y
   contains
     procedure :: initialize
  end type shape

  type, extends(shape) :: rectangle
     integer :: length
     integer :: width
  end type rectangle

  type, extends(rectangle) :: square
  end type square

contains

subroutine initialize(sh, color, filled, x, y, length, width)
    ! initialize shape objects
    class(shape) :: sh
    integer :: color
    logical :: filled
    integer :: x
    integer :: y
    integer, optional :: length
    integer, optional :: width

    sh%color = color
    sh%filled = filled
    sh%x = x
    sh%y = y

    select type (sh)
    type is (shape)
          ! no further initialization required
    class is (rectangle)
        ! rectangle or square specific initializations
        if (present(length))  then
           sh%length = length
        else
           sh%length = 0
        endif
        if (present(width)) then
            sh%width = width
        else
            sh%width = 0
        endif
    class default
      ! give error for unexpected/unsupported type
         stop 'initialize: unexpected type for sh object!'
    end select

end subroutine initialize

end module

program main
  !! This is the test program for OOP in Fortran.

  use shape_mod

  type(shape) :: shp                       ! declare an instance of shape
  type(rectangle) :: rect                  ! declare an instance of rectangle
  type(square) :: sq                       ! declare an instance of square
  !----------------------------------------------------------------------------

  call shp%initialize(1, .true., 10, 20)   ! initialize shape
  call rect%initialize(2, .false., 100, 200, 50, 25)  ! initialize rectangle
  call sq%initialize(3, .false., 400, 500, 30, 20)    ! initialize rectangle

end program main

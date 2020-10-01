program test_loc

  implicit none

  integer :: i
  real :: r
  real, target :: var1
  real, pointer:: ptr1

  i = loc(r)
  print *, i

  var1 = 1.0
  
  !call association_test(ptr1,var1)

  ptr1 => var1

  !call association_test(ptr1,var1)

contains

  subroutine association_test(a,b)
    use iso_c_binding, only: c_associated, c_loc, c_ptr
    implicit none
    real, pointer :: a
    type(c_ptr) :: b
    if(c_associated(b, c_loc(a))) &
         stop 'b and a do not point to same target'
  end subroutine association_test

end program test_loc

program test

  use iso_c_binding
  
  implicit none
  
  interface
     subroutine cfunc(p,r) bind(c)
       import :: c_ptr, c_int
       type(c_ptr), value :: p
       integer(c_int), value :: r
     end subroutine cfunc
  end interface

  integer,parameter ::n=3
  real (c_double), allocatable, target :: xyz(:,:)
  real (c_double), target :: abc(3,3)
  type(c_ptr) :: cptr
  real (c_double), pointer :: fptr(:,:)
  allocate(xyz(n,n))
  cptr = c_loc(xyz(1,1))

  !
  xyz(1,1) = 1
  xyz(1,2) = 2
  xyz(1,3) = 3
  xyz(2,1) = 4
  xyz(2,2) = 5
  xyz(2,3) = 6
  xyz(3,1) = 7
  xyz(3,2) = 8
  xyz(3,3) = 9

  call cfunc(cptr, n)

  write(*,*) 'In Fortran,'
  call c_f_pointer(cptr, fptr, [3,3])
  write(*,*) 'fptr=',fptr(1,1), fptr(1,2), fptr(1,3)
  write(*,*) 'fptr=',fptr(2,1), fptr(2,2), fptr(2,3)
  write(*,*) 'fptr=',fptr(3,1), fptr(3,2), fptr(3,3)
  
  deallocate(xyz)
  
end program test

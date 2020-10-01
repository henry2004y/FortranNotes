program test

  use iso_c_binding
  
  implicit none
  
  interface
     subroutine cfunc(p,r) bind(c)
       import :: c_double, c_int
       real(c_double) :: p(*) ! seg fault if p(:,:)
       integer(c_int), value :: r
     end subroutine cfunc
  end interface

  integer, parameter ::n=3
  real (c_double), allocatable :: xyz(:,:)

  allocate(xyz(n,n))

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

  call cfunc(xyz, n)

  write(*,*) 'In Fortran,'
  write(*,*) 'xyz=',xyz(1,1), xyz(1,2), xyz(1,3)
  write(*,*) 'xyz=',xyz(2,1), xyz(2,2), xyz(2,3)
  write(*,*) 'xyz=',xyz(3,1), xyz(3,2), xyz(3,3)
  
  deallocate(xyz)
  
end program test

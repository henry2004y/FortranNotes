Module ModA
  use omp_lib
  implicit none

  integer :: iSide = 1
  !$omp threadprivate(iSide)
contains
  subroutine funcA
    !--------------
    write(*,*) 'ModA, iSide = ', iSide
  end subroutine funcA

end Module ModA

Module ModB
  implicit none

  integer :: iSide = 2
  !$omp threadprivate(iSide)
contains
  subroutine funcB
    !------------------
    write(*,*) 'ModB,iSide=',iSide
  end subroutine funcB

end Module ModB

program main

  use omp_lib
  use ModA, only: funcA
  use ModB, only: funcB

  implicit none

  integer :: i
  !----------------------------------------------------------------------------

  !$omp parallel do
  do i=1,10
     write(*,*) 'hello, i = ', i
     call funcA
     call funcB
  end do
  !$omp end parallel do

end program main

module ModTest
  use omp_lib

  implicit none

contains
  subroutine func_in_mod

  integer :: B0_C(4)
  integer :: i,j,k
  !-------------------------

  B0_C = 10

  !$omp parallel do private(B0_C)
  do i = 1, 4
     B0_C = i
     write(*,*) 'in loop, threadID=',omp_get_thread_num(), 'B0_C=', B0_C
     call func_contained
  end do
  !$omp end parallel do

  contains
    subroutine func_contained
      
      write(*,*) 'threadID=',omp_get_thread_num(), 'B0_C=', B0_C

    end subroutine func_contained

end subroutine func_in_mod


end module ModTest

program main

  use omp_lib
  use ModTest

  implicit none

  !-----------------------------

  call func_in_mod


end program main

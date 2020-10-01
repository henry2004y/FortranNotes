program test

  use omp_lib
  implicit none

  integer :: i,j,k
  integer :: a
  !------------------------------

  !$omp parallel num_threads(2)
  !$omp do ordered private(j,k) collapse(2) schedule(static,3)
  do k = 1,4
     do j = 1,3
        !$omp ordered
        print *, omp_get_thread_num(), k, j
        !$omp end ordered
        call work(a,j,k)
     end do
  end do
  !$omp end do
  
  !$omp end parallel

end program test
!========================
subroutine work(a,j,k)
  implicit none
  integer :: a,j,k
  !--------------
  
end subroutine work

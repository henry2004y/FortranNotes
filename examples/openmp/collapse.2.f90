program test

  use omp_lib
  implicit none

  integer :: i,j,k
  integer :: jlast, klast
  !------------------------------

  !$omp parallel
  !$omp do private(j,k) collapse(2) lastprivate(jlast, klast)
  do k = 1,2
     do j = 1,3
        jlast = j
        klast = k
     end do
  end do
  !$omp end do

  !$omp single
  print *, klast, jlast
  !$omp end single
  
  !$omp end parallel

end program test

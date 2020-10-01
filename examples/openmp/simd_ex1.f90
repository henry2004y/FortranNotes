program test

  use omp_lib
  implicit none

  integer :: N
  real, allocatable :: x(:), y(:)
  integer :: i
  real*8 :: tStart, tEnd 
  !-------------------------------

  N = 10000000

  allocate(x(N),y(N))

  x = 1.0

  tStart = omp_get_wtime()

  !$omp simd
  do i = 1, n
     y(i) = 2.0 * x(i)
     y(i) = sqrt(y(i)**2 + 1.0)
  enddo

  tEnd = omp_get_wtime()

  write(*,*) 'Elapsed time = ',tEnd-tStart

  deallocate(x,y)

end program test

program test

  use omp_lib
  
  implicit none

  real :: A(10000), B(10000)
  integer :: i
  real*8 :: start, end
  !----------------------
  A = 0.0

  !$omp parallel

  !$omp single
  write ( *, '(a,i8)' ) &
       'The number of processors available = ', omp_get_num_procs()
  write ( *, '(a,i8)' ) &
              'The number of threads available    = ', omp_get_max_threads()
  !$omp end single

  start = omp_get_wtime()
  
  !$omp do
  do i = 1,10000
     B(i) = 10 * i
     A(i) = A(i) + B(i)
  end do
  !$omp end do

  end = omp_get_wtime()
  
  !$omp end parallel

  write(*,*) 'A(1:10) = ',A(1:10)
  write(*,*) 'elapsed time = ',end-start  

end program test

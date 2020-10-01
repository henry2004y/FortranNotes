program test

  use omp_lib

  implicit none

  integer*8 :: i
  integer*8 :: a,b,c
  real*8 :: start, end, mid1, mid2
  !---------------------

  a = 0
  b = 0
  c = 0

  !$omp parallel

  !$omp single
  write ( *, '(a,i8)' ) &
       'The number of processors available = ', omp_get_num_procs()
  write ( *, '(a,i8)' ) &
       'The number of threads available    = ', omp_get_max_threads()
  !$omp end single 

  start = omp_get_wtime()
  
  ! Here is the wrong way of doing add: sometimes it may give wrong values!
  !$OMP DO
  do i = 1, 1000000
     a = a + i
  enddo
  !$OMP END DO

  mid1 = omp_get_wtime()
  
  !$omp do
  do i=1, 10000000
     !$omp atomic
     b = b + i
     !$omp end atomic
  end do
  !$omp end do

  mid2 = omp_get_wtime()
  
  !$OMP DO REDUCTION (+:c)
  do i=1,100000000
     c = c + i
  enddo
  !$OMP END DO

  end = omp_get_wtime()
  
  !$omp end parallel

  write(*,*) 'a,b,c=',a,b,c
  write(*,*) 'elapsed time =',mid1-start, mid2-mid1,end-mid2
  
end program test

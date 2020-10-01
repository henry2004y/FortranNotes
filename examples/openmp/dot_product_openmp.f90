program test

  use omp_lib

  implicit none

  real*8, allocatable:: a_I(:), b_I(:)
  real*8 :: C, tStart, tEnd
  integer :: N,i
  !---------------------------------------------------------------------------

  !$omp master
  write ( *, '(a,i8)' ) &
       'The number of processors available = ', omp_get_num_procs()
  write ( *, '(a,i8)' ) &
       'The number of threads available    = ', omp_get_max_threads()
  !$omp end master

  N = 100000000

  allocate(a_I(N),b_I(N))

  do i=1,N
     a_I(i) = i
     b_I(i) = i
  end do

  !$ tStart = omp_get_wtime()

  !$omp parallel                                                              
  !$omp workshare
  C = dot_product(a_I,b_I)
  !$omp end workshare
  !$omp end parallel

  !$ tEnd = omp_get_wtime()

  write(*,*) '---------------------'
  write(*,*) 'instrinsic dot_product with OpenMP:'
  write(*,*) 'result=',C
  write(*,*) 'elapsed time=',tEnd-tStart

  !$ tStart = omp_get_wtime()
  C = dot_product(a_I,b_I)
  !$ tEnd = omp_get_wtime()

  write(*,*) '---------------------'
  write(*,*) 'instrinsic dot_product without OpenMP:'
  write(*,*) 'result=',C
  write(*,*) 'elapsed time=',tEnd-tStart


  !$ tStart = omp_get_wtime()
  C = 0.0
  !$omp parallel do reduction(+:C)
  do i=1,N
     C = C + a_I(i)*b_I(i)
  end do
  !$omp end parallel do
  !$ tEnd = omp_get_wtime()

  write(*,*) '---------------------'
  write(*,*) 'explicit dot_product with OpenMP:'
  write(*,*) 'result=',C
  write(*,*) 'elapsed time=',tEnd-tStart


  !$ tStart = omp_get_wtime()
  C = 0.0
  do i=1,N
     C = C + a_I(i)*b_I(i)
  end do
  !$ tEnd = omp_get_wtime()

  write(*,*) '---------------------'
  write(*,*) 'explicit dot_product without OpenMP:'
  write(*,*) 'result=',C
  write(*,*) 'elapsed time=',tEnd-tStart

  deallocate(a_I,b_I)

end program test

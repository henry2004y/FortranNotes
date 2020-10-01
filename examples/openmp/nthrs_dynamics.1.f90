program example

  use omp_lib
  implicit none
  integer :: id
  !----------------
  
  call omp_set_dynamic(.false.)
  !$omp parallel num_threads(4)
  id = omp_get_thread_num()
  write(*,*) 'id=',id
  !$omp end parallel
  
  
end program example

program example
  ! Again, this example is not working on my laptop.
  use omp_lib

  implicit none
  integer :: id
  !------------

  call omp_set_dynamic(.true.)
  !$omp parallel num_threads(4)
  id = omp_get_thread_num()
  write(*,*) 'id=',id
  !$omp end parallel
  
end program example

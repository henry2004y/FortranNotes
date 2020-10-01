program test

  use omp_lib
  implicit none

  !----------------------------------------------------------------------------
  ! Explicitly disable dynamic teams
  !call omp_set_dynamic(.false.)
  ! Use 4 threads for all consecutive parallel regions
  call omp_set_num_threads(4)

  !$omp parallel

  write(*,*) 'hello from thread ', omp_get_thread_num()

  !$omp end parallel

end program test

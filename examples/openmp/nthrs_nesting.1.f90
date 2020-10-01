program icv
  ! I don`t quite understand this example.
  use omp_lib
  implicit none

  call omp_set_nested(.true.)
  call omp_set_dynamic(.false.)

  !$omp parallel

  !$omp parallel
  !$omp single
  print *, "Inner:num_thds=", omp_get_num_threads()
  !$omp end single
  !$omp end parallel

  !$omp barrier
  call omp_set_nested(.false.)

  !$omp parallel
  !$omp single
  print *, "Inner: num_thds=", omp_get_num_threads()
  !$omp end single
  !$omp end parallel

  !$omp barrier

  !$omp single
  print *, "Outer: num_thds=", omp_get_num_threads()
  !$omp end single
  
  !$omp end parallel
  

end program icv

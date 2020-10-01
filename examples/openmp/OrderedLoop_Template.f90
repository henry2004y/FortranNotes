program main

  use omp_lib

  implicit none

  integer :: i
  !-------------

  !$omp parallel
  !$omp do ordered
  do i = 1, 100
     !block1

     ! no thread can enter the ordered section until it is guaranteed that all
     ! previous iterations have been completed.
     !$omp ordered
     !block2
     !$omp end ordered

     ! note that only one ordered section is allowed to be executed by each
     ! iteration inside a parallelized do-loop. In this case, we cannot have
     ! another ordered section of code!
     
     !block3
  end do
  !$omp end do
  !$omp end parallel

end program main

program test

  use omp_lib
  implicit none

  !$OMP SECTIONS
  !$OMP SECTION
  write(*,*) "Hello"
  !$OMP SECTION
  write(*,*) "Hi"
  !$OMP SECTION
  write(*,*) "Bye"
  !$OMP END SECTIONS
  
end program test

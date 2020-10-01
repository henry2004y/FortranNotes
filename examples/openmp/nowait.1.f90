subroutine nowait_example(N,M,A,B,Y,Z)

  implicit none
  real :: A(*), B(*), Y(*), Z(*)
  integer :: i
  !-----------------------------

  !$omp parallel

  !$omp do
  do i=2,N
     B(i) = (A(i) + A(i-1)) / 2.0
  end do
  !$omp end do nowait

  !$omp do
  do i=1,M
     Y(i) = sqrt(Z(i))
  end do
  !$omp end do nowait

  !$omp end parallel
  
  
end subroutine nowait_example

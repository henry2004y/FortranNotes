program main
  use omp_lib
  implicit none
  real :: A(1000), B(1000), C(1000), Y(1000), Z(1000)
  !-------------

  A = 1.
  B = 3.
  C = 0.
  Y = 0.
  Z = 9.
  
  call nowait_example2(999,999,A,B,C,Y,Z)

  write(*,*) 'C=',C(1)
  write(*,*) 'Y=',Y(1)
  write(*,*) 'Z=',Z(1)
  
end program main

subroutine nowait_example2(N,M,A,B,C,Y,Z)

  implicit none
  integer :: N,M
  real, intent(inout) :: A(*), B(*), C(*), Y(*), Z(*)
  integer :: i
  !-----------------------------

  !$omp parallel

  !$omp do schedule(static)
  do i=1,N
     C(i) = (A(i) + B(i)) / 2.0
  end do
  !!$omp end do nowait

  !$omp do schedule(static)
  do i=1,M
     Y(i) = sqrt(Z(i))
  end do
  !$omp end do nowait

  write(*,*) 'sub1, B,C,Z=',B(1),C(1),Z(1)
  
  !$omp do schedule(static)
  do i=2,N+1
     Y(i) = Z(i-1) + A(i)
  end do
  !$omp end do nowait

  write(*,*) 'sub2, B,C,Z=',B(1),C(1),Z(1)
  
  !$omp end parallel
  
end subroutine nowait_example2

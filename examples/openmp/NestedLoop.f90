program test

  use omp_lib
  
  implicit none

  integer :: i,j,k
  integer :: A(100,100,100)
  real*8  :: Start, End, Middle1, Middle2, Middle3
  !----------------

  !$omp parallel

  !$omp single
  write ( *, '(a,i8)' ) &
       'The number of processors available = ', omp_get_num_procs()
  write ( *, '(a,i8)' ) &
       'The number of threads available    = ', omp_get_max_threads()
    !$omp end single

  ! Start measuring time
  Start = omp_get_wtime()

  do i = 1, 100
     do j = 1, 100
        !$OMP DO
        do k = 1, 100
           A(i,j,k) = i * j * k
        enddo
        !$OMP END DO
     enddo
  enddo
  
  ! Measure time1
  Middle1 = omp_get_wtime()
  
  !$OMP DO
  do i = 1, 100
     do j = 1, 100
        do k = 1, 100
           A(i,j,k) = i * j * k
        enddo
     enddo
  enddo
  !$OMP END DO

  ! measure time2
  Middle2 = omp_get_wtime()
  
  !$OMP DO
  do k = 1, 100
     do j = 1, 100
        do i = 1, 100
           A(i,j,k) = i * j * k
        enddo
     enddo
  enddo
  !$OMP END DO

  ! measure time3
  Middle3 = omp_get_wtime()

  !$omp do collapse(2) private(i,j,k)
  do k = 1, 100
     do j = 1, 100
        do i = 1, 100
           A(i,j,k) = i * j * k
        enddo
     enddo
  enddo
  !$omp end do
  
  !$omp end parallel

  ! End measuring time
  End = omp_get_wtime()

  !
  write(*,*) 'A(1,1,1) = ',A(1,1,1)
  
  ! Show the estimated max
  write(*,*) 'Performance for different loops:'
  write(*,*) '1: ', Middle1 - Start
  write(*,*) '2: ', Middle2 - Middle1
  write(*,*) '3: ', Middle3 - Middle2
  write(*,*) '4: ', End - Middle3
  
end program test

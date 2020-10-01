program Main

  use omp_lib

  implicit none

  integer(kind = OMP_lock_kind) :: lck
  !integer(kind = OMP_integer_kind) :: ID
  integer :: ID
  !-------------------------------------

  call OMP_init_lock(lck)

  !$OMP PARALLEL SHARED(LCK) PRIVATE(ID)
  ID = OMP_get_thread_num()
  do while(.not.OMP_test_lock(lck))
      !work to do while the thread is waiting to get owner of the lock
  enddo
      !work to do as the owner of the lock
  !$OMP END PARALLEL

  call OMP_destroy_lock(lck)

end program Main

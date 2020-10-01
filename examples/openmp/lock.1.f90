program Main

  use omp_lib

  implicit none

  integer(kind = OMP_lock_kind) :: lck
  !integer(kind = OMP_integer_kind) :: ID
  integer :: ID
  !--------------------------------------
  
  call OMP_init_lock(lck)

  ! Here we demonstrate the usage of locks. Same results can be achieved using
  ! critical directives.
  
  !$OMP PARALLEL SHARED(LCK) PRIVATE(ID)
  ID = OMP_get_thread_num()
  call OMP_set_lock(lck)
  write(*,*) "My thread is ", ID
  call OMP_unset_lock(lck)
  !$OMP END PARALLEL

  call OMP_destroy_lock(lck)
  
end program Main

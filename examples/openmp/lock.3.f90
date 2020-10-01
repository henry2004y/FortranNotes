module data_types
  use omp_lib, only: OMP_nest_lock_kind
  implicit none
  type number
     integer :: n
     integer(OMP_nest_lock_kind) :: lck
  end type number
end module data_types
!---------------------------------------------------------!
! This demo shous the usage of nested locks.
program Main
  
  use omp_lib
  use data_types

  implicit none

  type(number) :: x
  !---------------
  x%n = 0

  call OMP_init_nest_lock(x%lck)

  !$OMP PARALLEL SECTIONS SHARED(x)
  !$OMP SECTION
  call add(x,20)
  !$OMP SECTION
  call substract(x,10)
  !$OMP END PARALLEL SECTIONS

  call OMP_destroy_nest_lock(x%lck)

  write(*,*) 'n=',x%n
  
end program Main
!---------------------------------------------------------!
subroutine add(x,d)

  use omp_lib
  use data_types
  
  implicit none
  type(number) :: x
  integer :: d
  !-----------------
  call OMP_set_nest_lock(x%lck)
  x%n = x%n + d
  call OMP_unset_nest_lock(x%lck)
  
end subroutine add
!---------------------------------------------------------!
subroutine substract(x,d)

  use omp_lib
  use data_types

  implicit none
  type(number) :: x
  integer :: d
  !---------------------
  call OMP_set_nest_lock(x%lck)
  call add(x,-d)
  call OMP_unset_nest_lock(x%lck)

end subroutine substract

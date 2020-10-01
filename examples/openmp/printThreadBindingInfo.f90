program main

  use omp_lib
  implicit none

  integer :: i
  
  !$omp parallel

  call print_binding_info()

  !$omp end parallel

contains 

  subroutine print_binding_info
    integer :: my_place
    integer :: place_num_procs
    integer, allocatable :: place_processors(:)
    integer :: i
    
    my_place = omp_get_place_num()
    place_num_procs = omp_get_place_num_procs(my_place)

    write(*,*) "Place consists of %d processors: ", place_num_procs

    allocate(place_processors(place_num_procs))

    call omp_get_place_proc_ids(my_place, place_processors)

    do i = 1, place_num_procs
       write(*,*) place_processors(i)
    end do

    deallocate(place_processors)
end subroutine print_binding_info

end program main

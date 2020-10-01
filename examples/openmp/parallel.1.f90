program parexample
  ! The purpose of this example is to show how to parallelize the initialization
  ! of an array.
  implicit none

  real :: array(10000)
  !-----------------------------------------------------------------------------

  call sub(array,10000)

  write(*,*) 'Initialization finished!'
  
end program parexample

subroutine subdomain(x, iStart, iPoints)
  implicit none
  real :: x(*)
  integer :: iStart, iPoints
  integer i
  !-----------------------------------------------------------------------------
  do i=1,iPoints
     x(iStart+i) = 123.456
  end do
  
end subroutine subdomain
!=======================

subroutine sub(x,nPoints)
  use omp_lib
  implicit none
  real :: x(*)
  integer :: nPoints
  integer :: id, nT, iPoints, iStart
  !----------------------

  !$omp parallel default(private) shared(x,nPoints)
  
  id = omp_get_thread_num()
  nT  = omp_get_num_threads()
  iPoints = nPoints/nT
  iStart  = id * iPoints

  !$omp single
  write(*,*) 'Number of threads used =',nT
  write(*,*) 'Start dividing points...'
  !$omp end single

  ! Special treat for the last thread
  if (id == nT-1) then
     iPoints = nPoints - iStart
  end if
  
  call subdomain(x,iStart,iPoints)
  !$omp end parallel
  
end subroutine sub

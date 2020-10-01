program main

  use, intrinsic :: iso_fortran_env

  implicit none

  integer, parameter :: sp = REAL32
  integer, parameter :: dp = REAL64
  integer, parameter :: qp = REAL128

  real(dp), target, allocatable :: var(:,:,:,:)
  real(dp), pointer :: tmp(:)
  real(dp), allocatable :: tmp2(:)
  real(dp) :: tmp3(8)
  integer :: nI, nJ, nK, nVar, nStep
  integer :: i,j,k, iStep
  real :: start, finish
  
  !----------------------------------------

  nVar = 8
  nI = 20
  nJ = 20
  nK = 20
  nStep = 1000

  allocate(var(nVar,nI,nJ,nK))
  allocate(tmp2(nVar))

  var = 1.0

  ! using stack var
  call cpu_time(start)

  do iStep = 1,nStep
     do k = 1,nK
        do j = 1,nJ
           do i = 1,nI
              tmp3 = var(:,i,j,k)
              var(:,i,j,k) = sqrt(sin(tmp3)*cos(tmp3))
           end do
        end do
     end do
  end do

  call cpu_time(finish)
  print '("Time = ",f6.3," seconds.")',finish-start

  var = 1.0

  ! using pointer
  call cpu_time(start)

  do iStep = 1,nStep
     do k = 1,nK
        do j = 1,nJ
           do i = 1,nI
              tmp => var(:,i,j,k)
              var(:,i,j,k) = sqrt(sin(tmp)*cos(tmp))
           end do
        end do
     end do
  end do

  call cpu_time(finish)
  print '("Time = ",f6.3," seconds.")',finish-start

  var = 1.0

  ! using allocatable array
  call cpu_time(start)

  do iStep = 1,nStep
     do k = 1,nK
        do j = 1,nJ
           do i = 1,nI
              tmp2 = var(:,i,j,k)
              var(:,i,j,k) = sqrt(sin(tmp2)*cos(tmp2))
           end do
        end do
     end do
  end do

  call cpu_time(finish)
  print '("Time = ",f6.3," seconds.")',finish-start


  var = 1.0
  ! no intermediate array
  call cpu_time(start)

  do iStep = 1,nStep
     do k = 1,nK
        do j = 1,nJ
           do i = 1,nI
              var(:,i,j,k) = sqrt(sin(var(:,i,j,k))*cos(var(:,i,j,k)))
           end do
        end do
     end do
  end do

  call cpu_time(finish)
  print '("Time = ",f6.3," seconds.")',finish-start

  deallocate(var, tmp2)

end program main

program main

  use omp_lib
  
  implicit none

  integer, parameter :: NLength=10
  integer, parameter :: nI=10, nJ=10, nK=10, nBlock = 1000
  integer :: i,j,k,n, iBlock, iVar, nVar
  integer :: iVarMin=1, iVarMax=3
  
  real, allocatable :: State_VGB(:,:,:,:,:), d_C(:,:,:,:)
  real, allocatable :: x_I(:)
  real :: c
  real*8 :: wtime
  
  logical :: CondTest = .true.
  real :: start, finish

  !----------------------------------------------------------------------------

  nVar = iVarMax - iVarMin + 1
  
  allocate(State_VGB(nVar,nI,nJ,nK,nBlock))
  allocate(x_I(nI*nJ*nK*nBlock*nVar))
  allocate(d_C(nVar,nI,nJ,nK))
  
  State_VGB = 0.0
  x_I = 1.0
  d_C = 1.0
  
  call cpu_time(start)

  n = 0
  do iBlock = 1,nBlock
     do k=1,nK; do j=1,nJ; do i=1,nI
        do iVar=iVarMin,iVarMax
           n = n + 1
           State_VGB(iVar,i,j,k,iBlock) = x_I(n)
           !if (CondTest) then
           !   c = sin(1.0)
           !end if
        enddo
     enddo; enddo; enddo
  enddo
  
  call cpu_time(finish)

  print '("Time = ",f6.3," seconds.")',finish-start

  ! second loop
  call cpu_time(start)

  do iBlock=1,nBlock
     do k=1,nK; do j=1,nJ; do i=1,nI
        do iVar = iVarMin,iVarMax
           ! This index is just an example, not exactly true
           State_VGB(iVar,i,j,k,iBlock) = &
                x_I( (i-1)*nI+(j-1)*nJ+(k-1)*nK+iVar )
           !if (CondTest) then
           !   c = sin(1.0)
           !end if
        end do
     enddo; enddo; enddo
  enddo
     
  call cpu_time(finish)

  write(*,*) 'serial loop,'
  print '("Time = ",f6.3," seconds.")',finish-start

  call cpu_time(start)
  !wtime = omp_get_wtime ( )

  write(*,*) 'Nthread=',omp_get_num_threads ( )
  
  n = 0
  !$omp parallel do
  do iBlock = 1,nBlock
     n = (iBlock-1)*nI*nJ*nK*nVar + 1
     !write(*,*) 'n=',n
     do k=1,nK; do j=1,nJ; do i=1,nI
        do iVar=iVarMin,iVarMax
           n = n + 1
           State_VGB(iVar,i,j,k,iBlock) = x_I(n)
           !if (CondTest) then
           !   c = sin(1.0)
           !end if
        enddo
     enddo; enddo; enddo
  enddo
  !$omp end parallel do

  !wtime = omp_get_wtime ( ) - wtime
  
  call cpu_time(finish)

  print '("Time = ",f6.3," seconds.")',finish-start

  !write ( *, '(a,g14.6)' ) '  Elapsed wall clock time = ', wtime


  call cpu_time(start)
  
  do iBlock=1,nBlock
     call update_test(State_VGB(:,:,:,:,iBlock),d_C(:,:,:,:))     
  end do

  call cpu_time(finish)

  write(*,*) 'serial block loop:'
  print '("Time = ",f6.3," seconds.")',finish-start

  write(*,*) 'begin parallel loops:'
  
  call cpu_time(start)

  !$omp parallel do private(d_C)
  do iBlock=1,nBlock
!     write(*,*) 'iBlock=',iBlock
     call update_test(State_VGB(:,:,:,:,iBlock),d_C(:,:,:,:))
  end do
  !$omp end parallel do

  call cpu_time(finish)

  write(*,*) 'parallel block loop:'
  print '("Time = ",f6.3," seconds.")',finish-start

  
  deallocate(State_VGB)
  deallocate(x_I)
  !deallocate(c)
  deallocate(d_C)
  
contains
  subroutine update_test(State_VC,d)

    real, intent(out):: State_VC(nVar,nI,nJ,nK)
    real, intent(in) ::  d(nVar,nI,nJ,nK)
    !----------------------------------------

    write(*,*) 'hello?'
    
    do k=1,nK; do j=1,nJ; do i=1,nI
       State_VC(:,i,j,k) = d(:,i,j,k)
    enddo; enddo; enddo
    
  end subroutine update_test
  
end program main


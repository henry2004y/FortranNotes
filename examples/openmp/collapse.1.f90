
! k and j loops are associated with the loop construct. Therefore, the
! iterations of the k and j loops are collapsed into one loop with larger
! iteration space, and that loop is then divided among the threads in the
! current team.
subroutine sub(a)

  implicit none

  real :: a(*)
  integer :: kStart, kEnd, kSteps, jStart, jEnd, jSteps, iStart, iEnd, iSteps
  integer :: i,j,k

  !$omp do collapse(2) private(i,j,k)
  do k = kStart, kEnd, kSteps
     do j = jStart, jEnd, jSteps
        do i = iStart, iEnd, iSteps
           call bar(a,i,j,k)
        end do
     end do
  end do
  !$omp end do
  
end subroutine sub

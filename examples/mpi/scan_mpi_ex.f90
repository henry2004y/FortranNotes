program main
  !----------------------------------------------------------------------------
  ! Test for using MPI_Scan.
  ! 
  ! Hongyang Zhou, hyzhou@umich.edu  09/29/2017
  !----------------------------------------------------------------------------
  use mpi
  
  implicit none

  integer, parameter :: NUMPTS = 10
  integer :: error
  integer :: id,tag
  integer :: i,p
  integer :: status(MPI_STATUS_SIZE)
  real    :: wtime0
  real    :: l = 0.5
  real    :: exp_sum = 0
  real    :: exp_pdf_i = 0.0
  real    :: exp_cdf_i = 0.0
  real    :: DIV_CONST = 2.0
  !----------------------------------------------------------------------------

  ! Initialize MPI
  call MPI_Init(error)

  ! Get the number of processes
  call MPI_Comm_size(MPI_COMM_WORLD, p, error)

  ! Get the individual process ID
  call MPI_Comm_rank(MPI_COMM_WORLD, id, error)

  do i = 0, NUMPTS
     if (id == i) then
        exp_pdf_i = l*exp(-l * ( real(i) / DIV_CONST))
     end if
  end do

  call MPI_Scan(exp_pdf_i, exp_cdf_i, 1, MPI_REAL, MPI_SUM, MPI_COMM_WORLD, &
       error);

  do i = 0,NUMPTS
     if (id == i) then
       write(*,*)"process ",id," : cumulative sum = ", exp_cdf_i
    end if
 end do
 
 ! Shut down MPI
 call MPI_Finalize(error)
  
end program main

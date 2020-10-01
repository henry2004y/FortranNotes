program main
  ! Test for new feature after Fortran 2003.
  ! 1. array constructor with square brackets
  ! 2. arrays as indices
  ! 3. elemental functions (bitwise operation)
  ! Hongyang Zhou, hyzhou@umich.edu  10/02/2017
  
  implicit none

  integer :: i
  real, dimension(4) :: x = [ 1., 2., 3., 4. ]
  real, dimension(4) :: y,z
  integer, dimension(2) :: j = [ 2, 4 ]
  logical, dimension(4) :: ilogical = [.true.,.false.,.true.,.false.]
  real :: Jacobian(3,3), mat1(2,2), mat2(2,2)
  real, allocatable:: array(:)
  !----------------------------------------------------------------------------
  
  ! Array constructor
  y = [ -1., 0., 1., 2. ]
  ! with implicit loop
  z(1:4) = [ (sqrt(real(i)), i=1, 4) ]


  write(*,*) x
  write(*,*) y
  write(*,*) z
  write(*,*) "x==(1,2,3,4)=",x==[1.,2.,3.,4.]
  write(*,*) all(x==[1.,2.,3.,4.])

  write(*,*) x(j)

  ! This is not supported as in Matlab
  !write(*,*) x(ilogical)

  ! point-wise operations, same for sin,cos,sign,etc.
  write(*,*) 'sqr(x)=',sqr(x)

  where(sqr(x)>1.0)
     y = x**2
  end where

  if(any(sqr(x)>1.0)) then
     write(*,*) 'good'
  end if

  !Jacobian = [[1.,2.,3.],[4.,5.,6.],[7.,8.,9.]]
  Jacobian(1,1) = 1
  Jacobian(1,2) = 0
  Jacobian(1,3) = 0
  Jacobian(2,1) = 0
  Jacobian(2,2) = 2
  Jacobian(2,3) = 0
  Jacobian(3,1) = 0
  Jacobian(3,2) = 0
  Jacobian(3,3) = 3
  !write(*,*) 'Jacobian=',Jacobian

  write(*,*) 'determinant of matrix=',det_rosetta(Jacobian,3)

  mat1(1,1) = 1
  mat1(1,2) = 2
  mat1(2,1) = 3
  mat1(2,2) = 4

  mat2 = mat1
  
  write(*,*) 'matrix multiplication:',matmul(mat1,mat2)

  ! Always prefer KEYWORD ARGUMENT style for subroutines
  ! once a keyword is used, all subsequent arguments must be keyword arguments

  ! use protected keyword for module vars

  ! interface for generic names

  ! operator overloading, define new operations yourself

  !
  
  !write(*,*) 'sum=',sum(1)

  write(*,*) 'hello!'
  write(*,*) 'norm2=',norm2(y)
  ! I found a bug here: you can't use write inside the above function call!

  allocate(array(0:0))


  deallocate(array)
  

  !----------------------------------------------------------------------------
contains
  function norm2(x) result(norm)
    real, dimension(1:),intent(in) :: x
    real :: norm

    norm = sqrt(sum(x**2))
  end function norm2

  elemental function sqr(x) result(sqr_result)
    real, intent(in) :: x
    real :: sqr_result
    
    sqr_result = x * x
  end function sqr

  recursive function det_rosetta( mat, n ) result( accum )
    integer :: n
    real    :: mat(n, n)
    real    :: submat(n-1, n-1), accum
    integer :: i, sgn
    
    if ( n == 1 ) then
       accum = mat(1,1)
    else
       accum = 0.0
       sgn = 1
       do i = 1, n
          submat( 1:n-1, 1:i-1 ) = mat( 2:n, 1:i-1 )
          submat( 1:n-1, i:n-1 ) = mat( 2:n, i+1:n )

          accum = accum + sgn * mat(1, i) * det_rosetta( submat, n-1 )
          sgn = - sgn
       enddo
    endif
    end function
  
end program main


!***********************************************************!
!                                                           !
! This subroutine computes next values within global domain !
!                                                           !
!***********************************************************!

subroutine computeNext(x0, x, size_x, size_y, dt, hx, hy, diff, k0)

   ! Input parameters
   integer size_x, size_y
   double precision x(0:size_x+1,0:size_y+1)
   double precision dt, hx, hy, k0

   ! Output array
   double precision x0(0:size_x+1,0:size_y+1)

   ! Output difference
   double precision diff

   ! Index variables
   integer i, j

   ! Factors for the stencil
   double precision diagx, diagy, weightx, weighty

   ! Local variable for computing difference
   double precision ldiff

   ! The stencil of the explicit operator for the heat equation
   ! on a regular rectangular grid using a five point finite difference
   ! scheme in space is :
   !
   ! |                                    weightx * x[i-1][j]                                    |
   ! |                                                                                           |
   ! | weighty * x[i][j-1]   (diagx * weightx + diagy * weighty) * x[i][j]   weighty * x[i][j+1] |
   ! |                                                                                           |
   ! |                                    weightx * x[i+1][j]                                    |

   diagx = -2.0 + hx*hx/(2*k0*dt)
   diagy = -2.0 + hy*hy/(2*k0*dt)
   weightx = k0*dt/(hx*hx)
   weighty = k0*dt/(hy*hy)

   ! Perform an explicit update on the points within the domain.
   ! Optimization : inner loop on rows index (first index) since
   ! F90 is column major
   do j=1,size_y
     do i=1,size_x
       x(i,j) = weightx*(x0(i-1,j) + x0(i+1,j) + x0(i,j)*diagx) + &
                weighty*(x0(i,j-1) + x0(i,j+1) + x0(i,j)*diagy)
     end do
   end do

   ! Compute the difference into domain for convergence.
   ! Update the value x0(i,j).
   ! Optimization : inner loop on rows index (first index) since
   ! F90 is column major
   diff = 0.0
   do j=1,size_y
     do i=1,size_x
       ldiff = x0(i,j) - x(i,j)
       diff = diff + ldiff*ldiff
       x0(i,j) = x(i,j)
     end do
   end do

   return
end

!***********************************************************************!
!                                                                       !
! This subroutine sets up the initial temperature on borders and inside !
!                                                                       !
!***********************************************************************!

subroutine initvalues(x0, x_dim, y_dim, temp1_init, temp2_init)

   ! Input parameters
   integer x_dim, y_dim
   double precision temp1_init, temp2_init

   ! Output array
   double precision x0(0:x_dim+1,0:y_dim+1)

   ! Index variables
   integer i, j

   ! Setup temp1_init on borders
   do i=0,x_dim+1
     x0(i,0) = temp1_init
     x0(i,y_dim+1) = temp1_init
   end do

   do j=0,y_dim+1
     x0(0,j) = temp1_init
     x0(x_dim+1,j) = temp1_init
   end do

   ! Setup temp2_init inside
   do i=1,x_dim
     do j=1,y_dim
       x0(i,j) = temp2_init
     end do
   end do

   return
end

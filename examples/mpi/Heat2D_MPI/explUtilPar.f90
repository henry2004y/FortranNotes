
!*************************************************************************!
!                                                                         !
! This subroutine computes next values in subdomain of current process me !
!                                                                         !
!*************************************************************************!

subroutine computeNext(x0, x, size_total_x, size_total_y, dt, hx, hy, diff, &
                       me, xs, ys, xe, ye, nproc, k0)

   ! Input parameters
   integer size_total_x, size_total_y, me, nproc
   integer xs(0:nproc-1), ys(0:nproc-1), xe(0:nproc-1), ye(0:nproc-1)
   double precision x(0:size_total_x-1,0:size_total_y-1)
   double precision dt, hx, hy, k0

   ! Output array
   double precision x0(0:size_total_x-1,0:size_total_y-1)

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
   diff = 0.0
   do j=ys(me),ye(me)
     do i=xs(me),xe(me)
       x(i,j) = weightx*(x0(i-1,j) + x0(i+1,j) + x0(i,j)*diagx) &
              + weighty*(x0(i,j-1) + x0(i,j+1) + x0(i,j)*diagy)
     end do
   end do

   ! Compute the difference into domain for convergence.
   ! Update the value x0(i,j).
   ! Optimization : inner loop on rows index (first index) since
   ! F90 is column major
   diff = 0.0
   do j=ys(me),ye(me)
     do i=xs(me),xe(me)
       ldiff = x0(i,j) - x(i,j)
       diff = diff + ldiff*ldiff
       x0(i,j) = x(i,j)
     end do
   end do

   return
end

!************************************************************************!
!                                                                        !
! This subroutine sets up the initial temperatures on borders and inside !
!                                                                        !
!************************************************************************!

subroutine initValues(x0, size_total_x, size_total_y, temp1_init, temp2_init)

   ! Input parameters
   integer size_total_x, size_total_y
   ! Init Temperatures
   double precision temp1_init, temp2_init

   ! Output array
   double precision x0(0:size_total_x-1,0:size_total_y-1)

   ! Index variables
   integer i, j

   ! Setup temp1_init on borders
   do i=0,size_total_x-1
     x0(i,0) = temp1_init
     x0(i,size_total_y-1) = temp1_init
   end do

   do j=0,size_total_y-1
     x0(0,j) = temp1_init
     x0(size_total_x-1,j) = temp1_init
   end do

   do i=1,size_total_x-2
     x0(i,1) = temp1_init
     x0(i,size_total_y-2) = temp1_init
   end do

   do j=1,size_total_y-2
     x0(1,j) = temp1_init
     x0(size_total_x-2,j) = temp1_init
   end do

   ! Setup temp2_init inside
   do i=2,size_total_x-3
     do j=2,size_total_y-3
       x0(i,j) = temp2_init
     end do
   end do

   return
end

!**********************************************************!
!                                                          !
! This subroutine computes the coordinates xs, xe, ys, ye, !
! for each cell on the grid, respecting processes topology !
!                                                          !
!**********************************************************!

subroutine processToMap(xs, ys, xe, ye, xcell, ycell, x_domains, y_domains, nproc)

   ! Input parameters
   integer nproc
   integer xcell, ycell, x_domains, y_domains

   ! Output arrays
   integer xs(0:nproc-1), xe(0:nproc-1), ys(0:nproc-1), ye(0:nproc-1)

   ! Index variable
   integer i, j

   ! Computation of starting ys,ye on (Ox) standard axis
   ! for the first column of global domain,
   ! Convention x(i,j) with i row and j column
   ys(0:(x_domains-1)) = 2
   ! Here, ye(0:(x_domains-1)) = 2+ycell-1
   ye(0:(x_domains-1)) = ys(0:(x_domains-1))+ycell-1

   ! Computation of ys,ye on (Ox) standard axis
   ! for all other cells of global domain
   do i=1,(y_domains-1)
     ys(i*x_domains:(i+1)*x_domains-1) = ys((i-1)*x_domains:i*x_domains-1)+ycell+2
     ye(i*x_domains:(i+1)*x_domains-1) = ys(i*x_domains:(i+1)*x_domains-1)+ycell-1
   end do

   ! Computation of starting xs,xe on (Oy) standard axis
   ! for the first row of global domain,
   ! Convention x(i,j) with i row and j column
   do i=0,y_domains-1
     xs(i*x_domains) = 2
     ! Here, xe(i*x_domains) = 2+xcell-1
     xe(i*x_domains) = xs(i*x_domains)+xcell-1
   end do

   ! Computation of xs,xe on (Oy) standard axis
   ! for all other cells of global domain
   do i=1,y_domains
     do j=1,x_domains-1
       xs((i-1)*x_domains+j) = xs((i-1)*x_domains+(j-1))+xcell+2
       xe((i-1)*x_domains+j) = xs((i-1)*x_domains+j)+xcell-1
     end do
   end do

   return
end

program heat

   implicit none

   ! Index variables
   integer i, j

   ! Dimensions parameters
   integer size_x, size_y, size_total_x, size_total_y

   ! Arrays
   double precision, allocatable :: x(:,:), x0(:,:)

   ! Space and time steps
   double precision dt1, dt2, dt, hx, hy

   ! Current global difference and limit convergence
   double precision result, epsilon

   ! Time and step variables
   double precision t
   integer step

   ! Max step
   integer maxStep

   ! Variables for clock
   integer count_0, count_1
   integer count_rate, count_max
   double precision time_init, time_final, elapsed_time

   ! Physical parameters
   double precision temp1_init, temp2_init, k0

   ! temp1_init: temperature init on borders
   temp1_init = 10.0

   ! temp2_init: temperature init inside
   temp2_init = -10.0

   ! Diffusivity coefficient
   k0 = 1

   ! Get input parameters
   print *,'Size x of the square'
   read(*,*) size_x
   print *,'Size y of the square'
   read(*,*) size_y
   print *,'Max. number of steps'
   read(*,*) maxStep
   print *,'Time step'
   read(*,*) dt1
   print *,'Convergence'
   read(*,*) epsilon

   ! Define total sizes
   size_total_x = size_x+2
   size_total_y = size_y+2

   ! Compute space and time steps
   hx = 1.0d0/dble(size_total_x)
   hy = 1.0d0/dble(size_total_y)
   dt2 = 0.25*(min(hx,hy)**2)/k0

   ! Take a right time step for convergence
   if (dt1.ge.dt2) then
     write(*,*)
     write(*,*) ' Time step too large in ''param'' file -', &
                ' Taking convergence criterion'
     dt = dt2
   else
     dt = dt1
   endif

   ! Allocate 2D arrays x and x0 :
   ! size_total_x rows and size_total_y columns
   allocate(x(0:size_total_x-1,0:size_total_y-1))
   allocate(x0(0:size_total_x-1,0:size_total_y-1))

   ! Initialize values
   call initvalues(x0, size_x, size_y, temp1_init, temp2_init)

   ! Initialize step and time
   step = 0
   t = 0.0

   ! Starting time
   call system_clock(count_0, count_rate, count_max)
   time_init = count_0*1.0/count_rate

   ! Main loop : until convergence
   do
     ! Increment step and time
     step = step+1
     t = t+dt
     ! Perform one step of the explicit scheme
     call computeNext(x0, x, size_x, size_y, dt, hx, hy, result, k0)
     ! Current global difference
     result = sqrt(result)
     ! Break if convergence reached or step greater than maxStep
     if ((result.lt.epsilon).or.(step.ge.maxStep)) exit
   end do

   ! Ending time
   call system_clock(count_1, count_rate, count_max)
   time_final = count_1*1.0/count_rate
   ! Elapsed time
   elapsed_time = time_final - time_init

   ! Print results
   write(*,*)
   write(*,1000) dt
   write(*,*)
   write(*,1001) int(epsilon),epsilon-int(epsilon),step
   write(*,*)
   write(*,1002) size_x*size_y
   write(*,*)
   write(*,1003) int(elapsed_time),elapsed_time-int(elapsed_time)
   write(*,*)
   write(*,*) ' Computed solution in outputSeq.dat file '
   write(*,*)

   ! Store solution into output file :
   ! size_total_x = width
   ! size_total_y = height
   open(1,file='outputSeq.dat',action='write',status='replace')
   do j=0,size_total_y-1
     write(1,999) (x0(i,j),i=0,size_total_x-1)
   end do
   close(1)

   ! Free all arrays
   deallocate(x)
   deallocate(x0)

   ! Formats available to display the results and parameters
999 format(*(f15.11,1x))
1000 format('  Time step = ',1p,e15.9)
1001 format('  Convergence = ',i0,f0.9,' after ',i0,' steps ')
1002 format('  Problem size = ',i0)
1003 format('  Wall Clock = ',i0,f0.9)

   stop
end

program heat

   implicit none
   include 'mpif.h'

   ! Index variables
   integer i, j, k, l

   ! Various parameters of dimensions
   integer size_x, size_y, x_domains, y_domains
   integer size_global_x, size_global_y
   integer size_total_x, size_total_y

   ! Current process
   integer me

   ! Arrays
   double precision, allocatable :: x(:,:), x0(:,:)
   double precision, allocatable :: xtemp(:), xfinal(:)

   ! For reading parameters
   integer iconf(5)
   double precision conf(2)

   ! Space and time steps
   double precision dt, dt1, dt2, hx, hy

   ! Current local square difference
   double precision localDiff

   ! Current global difference and limit convergence
   double precision result, epsilon

   ! Time and step variables
   double precision t
   integer step

   ! Max step
   integer maxStep

   ! Variables for clock
   double precision time_init, time_final, elapsed_time

   ! Various variables for MPI implementation
   integer nproc, ndims, infompi, comm, comm2d
   integer, dimension(2) :: dims
   logical, dimension(2) :: periods
   logical, parameter :: reorganisation = .false.
   integer row_type
   integer, parameter :: S = 1, E = 2, N = 3, W = 4
   integer, dimension(4) :: neighBor
   integer xcell, ycell
   integer, allocatable :: xs(:), ys(:), xe(:), ye(:)

   ! Physical parameters
   double precision temp1_init, temp2_init, k0

   ! temp1_init: temperature init on borders
   temp1_init = 10.0

   ! temp2_init: temperature init inside
   temp2_init = -10.0

   ! Diffusivity coefficient
   k0 = 1

   ! MPI Initialization
   call MPI_Init(infompi)
   comm = MPI_COMM_WORLD
   call MPI_Comm_size(comm, nproc, infompi)
   call MPI_Comm_rank(comm, me, infompi)

   ! Get input parameters
   if (me.eq.0) then
     call readParam(iconf, conf)
   endif

   ! Broadcast input parameters
   call MPI_Bcast(iconf, 5, MPI_INTEGER, 0, comm, infompi)
   call MPI_Bcast(conf, 2, MPI_DOUBLE_PRECISION, 0, comm, infompi)

   ! Assign input parameters to variables
   size_x    = iconf(1)
   size_y    = iconf(2)
   x_domains = iconf(3)
   y_domains = iconf(4)
   maxStep   = iconf(5)
   dt1       = conf(1)
   epsilon   = conf(2)

   ! Warning message if dimensions and number of processes don't match
   if ((me.eq.0).and.(nproc.ne.(x_domains*y_domains))) then
     write(*,*) 'Number of processes not equal to Number of subdomains'
   endif

   ! Various other variables
   size_global_x = size_x+2
   size_global_y = size_y+2
   hx = 1.0d0/dble(size_global_x)
   hy = 1.0d0/dble(size_global_y)
   dt2 = 0.25*(min(hx,hy)**2)/k0
   size_total_x = size_x+2*x_domains+2
   size_total_y = size_y+2*y_domains+2

   ! Take a right time step for convergence
   if (dt1.ge.dt2) then
     if (me.eq.0) then
       write(*,*)
       write(*,*) ' Time step too large in ''param'' file -', &
                  ' Taking convergence criterion'
     endif
     dt = dt2
   else
     dt = dt1
   endif

   ! Allocate final 1D array
   allocate(xfinal(1:size_x*size_y))

   ! Allocate 2D contiguous arrays x and x0
   allocate(x(0:size_total_x-1,0:size_total_y-1))
   allocate(x0(0:size_total_x-1,0:size_total_y-1))

   ! Allocate coordinates of processes
   allocate(xs(0:nproc-1))
   allocate(xe(0:nproc-1))
   allocate(ys(0:nproc-1))
   allocate(ye(0:nproc-1))

   ! Create 2D cartesian grid
   periods(:) = .false.
   ! Number of dimensions
   ndims = 2
   ! Invert (Ox,Oy) classic convention
   dims(1) = y_domains
   dims(2) = x_domains
   call MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, &
                        reorganisation, comm2d, infompi)

   ! Identify neighBors
   neighBor(:) = MPI_Proc_null

   ! Left/West and Right/East neighbors
   call MPI_Cart_shift(comm2d, 0, 1, neighBor(W), neighBor(E), infompi)

   ! Bottom/South and Upper/North neighbors
   call MPI_Cart_shift(comm2d, 1, 1, neighBor(S), neighBor(N), infompi)

   ! Size of each cell
   xcell = (size_x/x_domains)
   ycell = (size_y/y_domains)

   ! Allocate subdomain : caution, index starting from 1 to xcell*ycell
   allocate(xtemp(1:xcell*ycell))

   ! Compute xs, xe, ys, ye for each cell on the grid
   call processToMap(xs, ys, xe, ye, xcell, ycell, x_domains, y_domains, nproc)

   ! Create row data type to communicate with South and North neighBors
   call MPI_Type_vector(ycell, 1, size_total_x, MPI_DOUBLE_PRECISION, &
                        row_type, infompi)
   call MPI_Type_commit(row_type, infompi)

   ! Initialize values
   call initValues(x0, size_total_x, size_total_y, temp1_init, temp2_init)

   ! Update the boundaries
   call updateBound(x0, size_total_x, size_total_y, neighBor, comm2d, &
                    row_type, me, xs, ys, xe, ye, xcell, nproc)

   ! Initialize step and time
   step = 0
   t = 0.0

   ! Starting time
   time_init = MPI_Wtime()

   ! Main loop : until convergence
   do
     ! Increment step and time
     step = step+1
     t = t+dt
     ! Perform one step of the explicit scheme
     call computeNext(x0, x, size_total_x, size_total_y, dt, hx, hy, localDiff, &
                      me, xs, ys, xe, ye, nproc, k0)
     ! Update the partial solution along the interface
     call updateBound(x0, size_total_x, size_total_y, neighBor, comm2d, &
                      row_type, me, xs, ys, xe, ye, xcell, nproc)
     ! Sum reduction to get global difference
     call MPI_Allreduce(localDiff, result, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
                        comm, infompi)
     ! Current global difference with convergence
     result= sqrt(result)
     ! Break if convergence reached or step greater than maxStep
     if ((result.lt.epsilon).or.(step.ge.maxStep)) exit
   end do

   ! Ending time
   time_final = MPI_Wtime()
   ! Elapsed time
   elapsed_time = time_final - time_init

   ! Gather all subdomains :
   ! inner loop on rows index (first index)
   ! to optimize since F90 is column major
   i = 1
   do j=ys(me),ye(me)
     xtemp((i-1)*xcell+1:i*xcell) = x0(xs(me):xe(me),j)
     i = i+1
   end do

   ! Perform gathering
   call MPI_Gather(xtemp, xcell*ycell, MPI_DOUBLE_PRECISION, xfinal, xcell*ycell, &
                   MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, infompi)

   ! Print results
   if (me.eq.0) then
     write(*,*)
     write(*,1000) dt
     write(*,*)
     write(*,1001) int(epsilon),epsilon-int(epsilon),step
     write(*,*)
     write(*,1002) size_x*size_y
     write(*,*)
     write(*,1003) int(elapsed_time),elapsed_time-int(elapsed_time)
     write(*,*)
     write(*,*) ' Computed solution in outputPar.dat file '
     write(*,*)

     ! Store solution into output file :
     ! x_domains = width
     ! y_domains = height */
     open(1,file='outputPar.dat',action='write',status='replace')
     write(1,999) (temp1_init,i=1,size_x+2)
     do i=1,y_domains
       do j=1,ycell
         write(1,999,advance='no') temp1_init
         do k=1,x_domains
           write(1,999,advance='no') (xfinal((i-1)*x_domains*xcell*ycell+ &
                                     (k-1)*xcell*ycell+(j-1)*xcell+l),l=1,xcell)
         end do
         write(1,998,advance='no') temp1_init
         write(1,*)
       end do
     end do
     write(1,999) (temp1_init,i=1,size_x+2)
     close(1)
   endif

   ! Free all arrays
   deallocate(x)
   deallocate(x0)
   deallocate(xtemp)
   deallocate(xfinal)
   deallocate(xs)
   deallocate(ys)
   deallocate(xe)
   deallocate(ye)

   ! Free row type
   call MPI_Type_free(row_type, infompi)

   ! Finish MPI
   call MPI_Finalize(infompi)

   ! Formats available to display the results and parameters
998 format(*(f15.11))
999 format(*(f15.11,1x))
1000 format('  Time step = ',1p,e15.9)
1001 format('  Convergence = ',i0,f0.9,' after ',i0,' steps ')
1002 format('  Problem size = ',i0)
1003 format('  Wall Clock = ',i0,f0.9)

   stop
end

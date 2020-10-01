
!*******************************************************************!
!             Update Bounds of subdomain with me process            !
!*******************************************************************!

subroutine updateBound(x, size_total_x, size_total_y, neighBor, comm2d, &
                       row_type, me, xs, ys, xe, ye, xcell, nproc)

   include 'mpif.h'

   ! Input parameters
   integer size_total_x, size_total_y, xcell, nproc, me
   double precision, dimension(0:size_total_x-1,0:size_total_y-1) :: x
   ! Type row_type
   integer row_type
   ! Various parameters
   integer, parameter :: S = 1, E = 2, N = 3, W = 4
   integer, dimension(4) :: neighBor
   integer infompi, comm2d
   integer flag
   integer, dimension(mpi_status_size) :: status
   integer xs(0:nproc-1), ys(0:nproc-1), xe(0:nproc-1), ye(0:nproc-1)

   !****************** North/South communication ******************!
   flag = 1
   ! Send my boundary to North and receive from South
   call MPI_Sendrecv(x(xe(me), ys(me)), 1, row_type, neighBor(N), flag, &
                     x(xs(me)-1, ys(me)), 1, row_type, neighBor(S), flag, &
                     comm2d, status, infompi)
   ! Send my boundary to South and receive from North
   call MPI_Sendrecv(x(xs(me), ys(me)), 1, row_type, neighBor(S), flag, &
                     x(xe(me)+1, ys(me)), 1, row_type, neighBor(N), flag, &
                     comm2d, status, infompi)

   !****************** East/West communication ********************!
   flag = 2
   ! Send my boundary to East and receive from West
   call MPI_Sendrecv(x(xs(me), ye(me)), xcell, MPI_DOUBLE_PRECISION, neighBor(E), flag, &
                     x(xs(me), ys(me)-1), xcell, MPI_DOUBLE_PRECISION, neighBor(W), flag, &
                     comm2d, status, infompi)
   ! Send my boundary to West and receive from East
   call MPI_Sendrecv(x(xs(me), ys(me)), xcell, MPI_DOUBLE_PRECISION, neighBor(W), flag, &
                     x(xs(me), ye(me)+1), xcell, MPI_DOUBLE_PRECISION, neighBor(E), flag, &
                     comm2d, status, infompi)
   return
end

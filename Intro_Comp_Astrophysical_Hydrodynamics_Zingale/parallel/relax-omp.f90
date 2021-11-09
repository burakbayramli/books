! Do brute force relaxation on a 2-d Poisson equation.  Here we 
! experiment with OpenMP
! 
! We solve
!    u'' = g(x), 
!    u(x=0,:  ) = 0, u(x=1,:  ) = 1
!    u(:  ,y=0) = 0, u(:  ,y=1) = 1
!
!with 
!    g(x) = cos(x)*sin(y)
!
!
! compile with:
!
!   gfortran -fopenmp -O -o relax relax.f90
! 
! run with:
!
!   export OMP_NUM_THREADS=2; time ./relax 
!
! M. Zingale (2010-03-07)

program relax

  USE omp_lib

  implicit none

  integer, parameter :: nx = 1024         ! number of interior zones in x
  integer, parameter :: ny = nx          ! number of interior zones in y
  integer, parameter :: ng = 1           ! number of guardcells

  integer, parameter :: nsmooth = 2500   ! number of smoothing blocks

  ! set the lo/hi boundary conditions in each coordinate direction
  double precision, parameter :: bc_lo_x = 0.d0
  double precision, parameter :: bc_hi_x = 0.d0
  double precision, parameter :: bc_lo_y = 0.d0
  double precision, parameter :: bc_hi_y = 0.d0

  ! imin/imax and jmin/jmax will always point to the starting and
  ! ending index of the interior zones
  integer :: imin, imax, jmin, jmax                   

  double precision :: xmin, xmax, ymin, ymax, dx, dy
  double precision :: source_norm

  double precision :: xx, yy

  integer :: i, j, n
  
  double precision, dimension(:,:), allocatable :: v, w, f

  double precision :: temp

  !real :: start, finish
  double precision :: startOMP, finishOMP


  ! measure time to run
  
  ! cpu_time() is a F95 intrinsic, but note that it adds up the time
  ! for all threads -- it is not wallclock time
  !call cpu_time(start)
  startOMP = omp_get_wtime()

  ! initialize the solution and rhs arrays
  allocate(f(-ng:nx+ng-1,-ng:ny+ng-1))
  allocate(v(-ng:nx+ng-1,-ng:ny+ng-1))
  allocate(w(-ng:nx+ng-1,-ng:ny+ng-1))


  ! integers indicating the range of valid data
  imin = 0           ! index 0 is the first valid cell
  imax = nx-1 

  jmin = 0           ! index 0 is the first valid cell
  jmax = ny-1 

  !$OMP PARALLEL DO PRIVATE (i,j)
  do j =jmin-ng, jmax+ng
     do i = imin-ng, imax+ng
        f(i,j) = 0.d0
        v(i,j) = 0.d0
     enddo
  enddo
  !$OMP END PARALLEL DO



  ! set the boundary conditions
  v(-ng:-1    ,:) = bc_lo_x
  v(nx:nx+ng-1,:) = bc_hi_x

  v(:,-ng:-1)     = bc_lo_y
  v(:,ny:ny+ng-1) = bc_hi_y

  
  ! setup the grid
  xmin = 0.d0
  xmax = 1.d0
  dx = (xmax - xmin)/dble(nx)

  ymin = 0.d0
  ymax = 1.0d0
  dy = (ymax - ymin)/dble(ny)

  if (dx /= dy) stop "ERROR: dx /= dy"




  ! fill the RHS of with the true RHS
  !$OMP PARALLEL DO PRIVATE (i,j,xx,yy)
  do j = jmin, jmax
     yy = (dble(j) + 0.5d0)*dy + ymin

     do i = imin, imax
        xx = (dble(i) + 0.5d0)*dx + xmin

        f(i,j) = g(xx,yy)
     enddo
  enddo
  !$OMP END PARALLEL DO


  ! compute the source norm -- we will use this for error estimating
  source_norm = error(nx, ny, ng, dx, dy, f)

  ! relax
  call smooth(nx, ny, ng, dx, dy, &
              bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y, &
              v, f, nsmooth)

  ! compare to the true solution

  !$OMP PARALLEL DO PRIVATE (i,j,xx,yy)
  do j = jmin, jmax
     yy = (dble(j) + 0.5d0)*dy + ymin

     do i = imin, imax
        xx = (dble(i) + 0.5d0)*dx + xmin
        w(i,j) = true(xx,yy) - v(i,j)
     enddo
  enddo
  !$OMP END PARALLEL DO

  close (unit=10)

100 format(1x, 1g13.6, 1x, 1g13.6, 1x, 1g13.6, 1x, 1g13.6)

  temp = error(nx, ny, ng, dx, dy, w)

  finishOMP = omp_get_wtime()
  !call cpu_time(finish)


  99 format(1x, "nx: ", i4, 3x, "threads: ", i3, 3x, "wallclock: ", g9.4, 3x, &
          "error: ", g10.5)

  write(*,99) nx, omp_get_max_threads(), finishOMP-startOMP, temp

contains

!=============================================================================
! g
!=============================================================================

function g(x,y)

  ! the RHS of the Poisson equation we are solving

  implicit none

  double precision :: g, x, y
  double precision, parameter :: pi = 3.14159265358979323846d0

  g = -2.d0*(2.0*pi)**2 * sin(2.0*pi*x) * sin(2.0*pi*y)

  return
end function g



!=============================================================================
! true
!=============================================================================

function true(x,y)

  ! the analytic solution to our equation

  implicit none

  double precision true, x, y
  double precision, parameter :: pi = 3.14159265358979323846d0
  
  true = sin(2.0*pi*x) * sin(2.0*pi*y)
  
  return
end function true



!=============================================================================
! error
!=============================================================================

function error(nx, ny, ng, dx, dy, v)

  ! compute the L2 norm 
  
  implicit none

  integer :: nx, ny, ng
  double precision :: dx, dy
  double precision :: v(-ng:,-ng:)

  integer :: i, j, imin, imax, jmin, jmax
  double precision :: error

  imin = 0
  imax = nx-1

  jmin = 0
  jmax = ny-1

  error = 0.d0

  !$OMP PARALLEL DO REDUCTION(+:error) PRIVATE (i,j)
  do j = jmin, jmax
     do i = imin, imax
        error = error + v(i,j)**2
     enddo
  enddo
  !$OMP END PARALLEL DO

  error = dx*dy*error    ! make it grid invariant
  error = sqrt(error)

  return
end function error



!=============================================================================
! smooth
!=============================================================================

subroutine smooth(nx, ny, ng, dx, dy, &
                  bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y, &
                  v, f, nsmooth)

  ! given a solution vector, v, and a RHS vector, f,
  ! smooth v to better satisfy the equation.  This is
  ! done in place, using Red-Black Gauss-Seidel

  ! hi and lo Dirichlet boundary conditions are also passed.
  ! Because we are finite-volume, and therefore, cell-centered, we 
  ! need to extrapolate to match the desired Dirichlet BC.

  implicit none

  integer :: nx, ny, ng, nsmooth
  double precision :: dx, dy
  double precision :: bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y
  double precision, dimension(-ng:,-ng:) :: v, f

  integer :: i, j, m, ioff, color
  integer :: imin, imax, jmin, jmax

  imin = 0
  imax = nx -1

  jmin = 0
  jmax = ny -1


  ! do some smoothing -- Red-Black Gauss-Seidel
  do m = 1, nsmooth

     do color = 0, 1

        ! set the guardcells to give the proper boundary condition, using
        ! extrapolation
        v(imin-1,:) = 2*bc_lo_x - v(imin,:)
        v(imax+1,:) = 2*bc_hi_x - v(imax,:)

        v(:,jmin-1) = 2*bc_lo_y - v(:,jmin)
        v(:,jmax+1) = 2*bc_hi_y - v(:,jmax)


        !$OMP PARALLEL DO PRIVATE (i,j,ioff)
        do j = jmin, jmax


           if (color == 0) then
              ioff = mod(j,2)
           else
              ioff = 1 - mod(j,2)
           endif

           do i = imin+ioff, imax, 2
              v(i,j) = 0.25d0*(v(i-1,j) + v(i+1,j) + &
                               v(i,j-1) + v(i,j+1) - dx*dx*f(i,j))
           enddo

        enddo
        !$OMP END PARALLEL DO

     enddo


  enddo

  return
end subroutine smooth

end program relax



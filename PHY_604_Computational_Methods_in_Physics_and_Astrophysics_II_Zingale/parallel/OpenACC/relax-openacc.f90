! Do brute force relaxation on a 2-d Poisson equation.  Here we
! experiment with OpenACC
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
!   pgf95 -fast -acc -ta=tesla:cuda9.0 -lcudart -mcmodel=medium -Mnoopenmp -Minfo -c relax-openacc.f90
!   pgf95 -acc -ta=tesla:cuda9.0 -o relax.pgf95.acc.ex relax-openacc.o
!
! M. Zingale

program relax

  implicit none

  integer, parameter :: nx = 4096         ! number of interior zones in x
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

  real :: start, finish

  ! measure time to run

  ! cpu_time() is a F95 intrinsic
  call cpu_time(start)

  ! initialize the solution and rhs arrays
  allocate(f(-ng:nx+ng-1,-ng:ny+ng-1))
  allocate(v(-ng:nx+ng-1,-ng:ny+ng-1))
  allocate(w(-ng:nx+ng-1,-ng:ny+ng-1))


  ! integers indicating the range of valid data
  imin = 0           ! index 0 is the first valid cell
  imax = nx-1

  jmin = 0           ! index 0 is the first valid cell
  jmax = ny-1

  do j =jmin-ng, jmax+ng
     do i = imin-ng, imax+ng
        f(i,j) = 0.d0
        v(i,j) = 0.d0
     enddo
  enddo


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
  do j = jmin, jmax
     yy = (dble(j) + 0.5d0)*dy + ymin

     do i = imin, imax
        xx = (dble(i) + 0.5d0)*dx + xmin

        f(i,j) = g(xx,yy)
     enddo
  enddo


  ! compute the source norm -- we will use this for error estimating
  source_norm = error(nx, ny, ng, dx, dy, f)

  ! relax
  call smooth(nx, ny, ng, dx, dy, &
              bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y, &
              v, f, nsmooth)

  ! compare to the true solution

  do j = jmin, jmax
     yy = (dble(j) + 0.5d0)*dy + ymin

     do i = imin, imax
        xx = (dble(i) + 0.5d0)*dx + xmin
        w(i,j) = true(xx,yy) - v(i,j)
     enddo
  enddo

100 format(1x, 1g13.6, 1x, 1g13.6, 1x, 1g13.6, 1x, 1g13.6)

  temp = error(nx, ny, ng, dx, dy, w)

  call cpu_time(finish)


  99 format(1x, "nx: ", i4, 3x, "wallclock: ", g9.4, 3x, "error: ", g10.5)

  write(*,99) nx, finish-start, temp

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

  !$acc data copyin(v, jmin, jmax, imin, imax) copy(error)
  !$acc parallel loop reduction(+:error)
  do j = jmin, jmax
     do i = imin, imax
        error = error + v(i,j)**2
     enddo
  enddo
  !$acc end data

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

  integer :: i, j, m
  integer :: imin, imax, jmin, jmax

  imin = 0
  imax = nx -1

  jmin = 0
  jmax = ny -1



  ! do some smoothing -- Red-Black Gauss-Seidel
  !$acc data copyin(f, dx, imin, imax, jmin, jmax, bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y) copy(v)
  do m = 1, nsmooth

     ! set the guardcells to give the proper boundary condition, using
     ! extrapolation
     !$acc parallel
     !$acc loop
     do j = jmin, jmax
        v(imin-1,j) = 2*bc_lo_x - v(imin,j)
        v(imax+1,j) = 2*bc_hi_x - v(imax,j)
     enddo

     !$acc loop
     do i = imin, imax
        v(i,jmin-1) = 2*bc_lo_y - v(i,jmin)
        v(i,jmax+1) = 2*bc_hi_y - v(i,jmax)
     enddo

     !$acc wait

     ! we'll do the updates in 4 groups
     ! the red are:
     !
     ! +--+--+
     ! |  | *|
     ! +--+--+
     ! |* |  |
     ! +--+--+
     !
     ! and the black are the other 2

     !$acc loop collapse(2)
     do j = jmin, jmax, 2
        do i = imin, imax, 2
           v(i,j) = 0.25d0*(v(i-1,j) + v(i+1,j) + &
                            v(i,j-1) + v(i,j+1) - dx*dx*f(i,j))
        enddo
     enddo

     !$acc loop collapse(2)
     do j = jmin+1, jmax, 2
        do i = imin+1, imax, 2
           v(i,j) = 0.25d0*(v(i-1,j) + v(i+1,j) + &
                            v(i,j-1) + v(i,j+1) - dx*dx*f(i,j))
        enddo
     enddo

     !$acc wait

     !$acc loop
     do j = jmin, jmax
        v(imin-1,j) = 2*bc_lo_x - v(imin,j)
        v(imax+1,j) = 2*bc_hi_x - v(imax,j)
     enddo

     !$acc loop
     do i = imin, imax
        v(i,jmin-1) = 2*bc_lo_y - v(i,jmin)
        v(i,jmax+1) = 2*bc_hi_y - v(i,jmax)
     enddo

     ! black

     !$acc wait

     !$acc loop collapse(2)
     do j = jmin, jmax, 2
        do i = imin+1, imax, 2
           v(i,j) = 0.25d0*(v(i-1,j) + v(i+1,j) + &
                            v(i,j-1) + v(i,j+1) - dx*dx*f(i,j))
        enddo
     enddo

     !$acc loop collapse(2)
     do j = jmin+1, jmax, 2
        do i = imin, imax, 2
           v(i,j) = 0.25d0*(v(i-1,j) + v(i+1,j) + &
                            v(i,j-1) + v(i,j+1) - dx*dx*f(i,j))
        enddo
     enddo
     !$acc end parallel

  enddo
  !$acc end data

  return
end subroutine smooth

end program relax

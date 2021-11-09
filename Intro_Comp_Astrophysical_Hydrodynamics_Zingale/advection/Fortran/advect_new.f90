! solve the linear advection equation on a finite-volume grid using
! using Godunov's method (piecewise constant), piecewise linear, or
! piecewise parabolc reconstruction.
!
! u_t + a u_x = 0
!
! M. Zingale (2012-02-29)
!
! version 1.01 (2013-03-24):
!
!      bug fix in PLM slope computation -- now gets second-order for u
!      positive or negative, and a bug in the computation of the PPM
!      uminus -- we were missing the leftmost value
!
! To really test this for errors, compile as:
!
!   gfortran -g -O0 -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -Wuninitialized -o advect advect.f90
!

module grid_module
  implicit none

  type grid_t
     integer :: nx = -1
     integer :: ng = -1
     integer :: ilo = -1
     integer :: ihi = -1

     double precision :: xmin
     double precision :: xmax
     double precision :: dx

     double precision, pointer :: x(:) => Null()
  end type grid_t

  type gridvar_t
     integer :: nvar
     double precision, allocatable :: d(:)
     type(grid_t) :: gd
  end type gridvar_t

contains

  subroutine build_grid(grid, nx, ng, xmin, xmax)

    type(grid_t), intent(inout) :: grid
    integer, intent(in) :: nx
    integer, intent(in) :: ng
    double precision, intent(in) :: xmin
    double precision, intent(in) :: xmax

    integer :: i

    grid % ilo = 0
    grid % ihi = nx - 1

    grid % nx = nx
    grid % ng = ng

    grid % xmin = xmin
    grid % xmax = xmax

    grid % dx = (xmax - xmin)/nx

    allocate(grid % x(-ng:nx+ng-1))

    do i = grid % ilo - ng, grid % ihi + ng
       grid % x(i) = xmin + dble(i + 0.5d0) * grid % dx
    enddo

  end subroutine build_grid

  subroutine build_gridvar(gridvar, grid)

    type(gridvar_t), intent(inout) :: gridvar
    type(grid_t),    intent(in   ) :: grid

    if (grid%nx == -1) then
       print *, "ERROR: grid not initialized"
    endif

    ! gridvar's grid type is simply a copy of the input grid
    gridvar % gd = grid

    ! now initialize the storage for the grid data
    allocate(gridvar % d(-grid % ng:grid % nx + grid % ng-1))
    gridvar % d(:) = 0.0d0

  end subroutine build_gridvar

  subroutine destroy_grid(grid)
    type(grid_t), intent(inout) :: grid
    deallocate(grid % x)
  end subroutine destroy_grid

  subroutine destroy_gridvar(gridvar)
    type(gridvar_t), intent(inout) :: gridvar
    deallocate(gridvar % d)
  end subroutine destroy_gridvar

end module grid_module

program advect

  use grid_module

  implicit none

  ! the number of zones (nx) and number of guardcells (ng)
  integer, parameter :: nx = 64
  integer, parameter :: ng = 3

  ! the domain size
  double precision, parameter :: xmin = 0.d0
  double precision, parameter :: xmax = 1.d0

  ! the advection velocity
  double precision, parameter :: u = 1.d0

  ! the CFL number
  double precision, parameter :: C = 0.8d0

  ! initial condition type
  integer, parameter :: inittype = 2

  ! slope type (1=godunov, 2=plm, 3=ppm)
  integer, parameter :: islopetype = 2

  integer, parameter :: plmlimiter = 1

  ! maximum simulation time
  double precision, parameter :: tmax = 5.d0

  type (grid_t) :: g
  type (gridvar_t) :: a, al, ar, f, ainit

  double precision :: time, dt

  ! setup the grid and set the initial conditions
  call build_grid(g, nx, ng, xmin, xmax)

  call build_gridvar(a, g)
  call build_gridvar(ainit, g)
  call build_gridvar(al, g)
  call build_gridvar(ar, g)
  call build_gridvar(f, g)

  call init(inittype, a)
  ainit % d(:) = a % d(:)

  time = 0.d0

  call output(inittype, islopetype, plmlimiter, a, time, u)


  ! evolution loop -- construct the interface states, solve the Riemann
  ! problem, and update the solution to the new time level
  do while (time < tmax)

     call fillBC(a)

     call timestep(a, C, u, dt)
     if (time + dt > tmax) then
        dt = tmax - time
     endif

     call states(dt, u, islopetype, plmlimiter, a, al, ar)

     call riemann(u, al, ar, f)

     call update(dt, a, f)

     time = time + dt

  enddo

  call output(inittype, islopetype, plmlimiter, a, time, u)

  print *, 'N, error: ', g % nx, &
       sqrt(g % dx*sum((a % d(a % gd %ilo:a % gd % ihi) - &
                        ainit % d(a % gd % ilo:a % gd % ihi))**2))

end program advect


!============================================================================
! init: set the initial conditions
!============================================================================
subroutine init(inittype, a)

  use grid_module

  implicit none

  integer, intent(in) :: inittype
  type (gridvar_t), intent(inout) :: a

  integer :: i
  double precision, parameter :: pi = 3.14159d0

  ! loop over all the zones and set the initial conditions.  To be
  ! consistent with the finite-volume discretization, we should store
  ! the zone averages here, but, to second-order accuracy, it is
  ! sufficient to evaluate the initial conditions at the zone center.

  do i = a % gd % ilo, a % gd % ihi

     if (inittype == 1) then
        ! sin wave
        a % d(i) = sin(2.d0*pi*a % gd % x(i))

     else if (inittype == 2) then
        ! square wave
        if (a % gd % x(i) > 0.333d0 .and. a % gd % x(i) < 0.666d0) then
           a % d(i) = 1.d0
        else
           a % d(i) = 0.d0
        endif

     else if (inittype == 3) then
        ! wave packet
        a % d(i) = sin(16.d0*pi*a % gd % x(i)) * &
             exp(-36.d0*(a % gd % x(i)-0.5d0)**2)

     else if (inittype == 4) then
        ! gaussian
        a % d(i) = exp(-(a % gd % x(i) - 0.5d0)**2/0.1d0**2)

     endif

  enddo

  return
end subroutine init


!============================================================================
! output: write out the solution
!============================================================================
subroutine output(inittype, islopetype, plmlimiter, a, time, u)

  use grid_module

  implicit none

  integer, intent(in) :: inittype, islopetype, plmlimiter
  type (gridvar_t), intent(in) :: a
  double precision, intent(in) :: time, u

  character (len=4) :: time_string
  character (len=16) :: slope, init, res

  integer :: i

  if (islopetype == 1) then
     slope = "godunov"
  else if (islopetype == 2) then
     if (plmlimiter == 1) then
        slope = "plm+MC"
     else if (plmlimiter == 2) then
        slope = "plm+SBee"
     endif
  else if (islopetype == 3) then
     slope = "ppm"
  endif

  if (inittype == 1) then
     init = "sine"
  else if (inittype == 2) then
     init = "tophat"
  else if (inittype == 3) then
     init = "packet"
  else if (inittype == 4) then
     init = "gaussian"
  endif


  ! open the output file
  write (time_string, '(f4.2)') time
  write (res, '(i8)') a % gd % nx

  open(unit=10, file="advect-"//trim(slope)//"-"//trim(init)//"-nx="//trim(adjustl(res))//"-t="//time_string, status="unknown")

  write (10,*) "# advection problem: a_t + u a_x = 0"
  write (10,*) "# u = ", u
  write (10,*) "# init = ", inittype
  write (10,*) "# slope type = ", islopetype
  if (islopetype == 2) then
     write (10,*) "# plm limiter = ", plmlimiter
  endif
  write (10,*) "# time = ", time


  do i = a % gd % ilo, a % gd % ihi
     write (10,*) a % gd % x(i), a % d(i)
  enddo

  return
end subroutine output


!============================================================================
! fillBC: fill the boundary conditions
!============================================================================
subroutine fillBC(a)

  use grid_module

  implicit none

  type (gridvar_t), intent(inout) :: a

  integer :: i, ilo, ihi, ng

  ilo = a % gd % ilo
  ihi = a % gd % ihi
  ng = a % gd % ng
  
  ! left boundary
  do i = 1, ng
     a % d(ilo - i) = a % d(ihi + 1 - i)
  enddo

  ! right boundary
  do i = 1, ng
     a % d(ihi + i) = a % d(ilo - 1 + i)
  enddo

  return
end subroutine fillBC


!============================================================================
! timestep: compute the new timestep
!============================================================================
subroutine timestep(a, cfl, u, dt)

  use grid_module

  implicit none

  type (gridvar_t), intent(in) :: a
  double precision, intent(in) :: cfl, u
  double precision, intent(inout) :: dt

  integer :: i

  ! in the linear advection equation, the timestep is trivial
  dt = cfl * a % gd % dx/abs(u)

  return
end subroutine timestep


!============================================================================
! states: compute the interface states used in solving the Riemann problem
!============================================================================
subroutine states(dt, u, islopetype, plmlimiter, a, al, ar)

  use grid_module

  implicit none

  integer, intent(in) :: islopetype, plmlimiter
  type (gridvar_t), intent(in) :: a
  type (gridvar_t), intent(inout) :: al, ar
  double precision, intent(in) :: dt, u
  
  type (gridvar_t) :: slope, aminus, aplus
  double precision :: du0, dup
  double precision :: slope1, slope2

  double precision :: minmod, maxmod

  integer :: i, ilo, ihi
  double precision :: dx

  call build_gridvar(slope, a % gd)
  call build_gridvar(aminus, a % gd)
  call build_gridvar(aplus, a % gd)

  ilo = a % gd % ilo
  ihi = a % gd % ihi
  dx = a % gd % dx

  ! compute the centered difference for linear slopes
  if (islopetype == 1) then

     ! Godunov's method (piecewise constant)

     ! for each interface, we want to construct the left and right
     ! states.  Here, interface i refers to the left edge of zone i

     ! interfaces imin to imax+1 affect the data in zones [imin,imax]
     do i = ilo, ihi+1

        ! the left state on the current interface comes from zone i-1.
        al % d(i) = a % d(i-1)

        ! the right state on the current interface comes from zone i
        ar % d(i) = a % d(i)
     enddo

  else if (islopetype == 2) then

     if (plmlimiter == 1) then
        ! PLM with MC limiter

        ! interface states are found by Taylor expansion in time
        ! (through dt/2) and space (dx/2 toward the interface)

        ! for each interface, we want to construct the left and right
        ! states.  Here, interface i refers to the left edge of zone i
        do i = ilo-1, ihi+1
           slope % d(i) = minmod(minmod(2.d0*(a % d(i) - a % d(i-1))/dx, &
                                        2.d0*(a % d(i+1) - a % d(i))/dx), &
                                 0.5d0*(a % d(i+1) - a % d(i-1))/dx)
        enddo

     else if (plmlimiter == 2) then
        ! PLM with SuperBee limiter

        do i = ilo-1, ihi+1
           slope1 = minmod((a % d(i+1) - a % d(i))/dx, &
                            2.d0*(a % d(i) - a % d(i-1))/dx)

           slope2 = minmod(2.d0*(a % d(i+1) - a % d(i))/dx, &
                          (a % d(i) - a % d(i-1))/dx)

           slope % d(i) = maxmod(slope1, slope2)
        enddo

     endif

     ! interfaces ilo to ihi+1 affect the data in zones [ilo, ihi]
     do i = ilo, ihi+1

        ! the left state on the current interface comes from zone i-1.
        al % d(i) = a % d(i-1) + 0.5*dx*(1.d0 - u*(dt/dx))*slope % d(i-1)

        ! the right state on the current interface comes from zone i
        ar % d(i) = a % d(i) - 0.5*dx*(1.d0 + u*(dt/dx))*slope % d(i)

     enddo

  else if (islopetype == 3) then

     ! PPM

     ! refer to the PPM paper for equation references

     ! use a cubic interpolation polynomial to find the edge states in
     ! zone i -- aminus(i) and aplus(i).  Here we loop over
     ! interfaces.
     do i = ilo-2, ihi+1

        ! du (C&W Eq. 1.7)
        du0 = 0.5*(a % d(i+1) - a % d(i-1))
        dup = 0.5*(a % d(i+2) - a % d(i))

        ! limiting (C&W Eq. 1.8)
        if ((a % d(i+1) - a % d(i))*(a % d(i) - a % d(i-1)) > 0) then
           du0 = sign(1.0d0, du0)*min(abs(du0), &
                                      2.0*abs(a % d(i) - a % d(i-1)), &
                                      2.0*abs(a % d(i+1) - a % d(i)))
        else
           du0 = 0.0
        endif

        if ((a % d(i+2) - a % d(i+1))*(a % d(i+1) - a % d(i)) > 0) then
           dup = sign(1.0d0, dup)*min(abs(dup), &
                                      2.0*abs(a % d(i+1) - a % d(i)), &
                                      2.0*abs(a % d(i+2) - a % d(i+1)))
        else
           dup = 0.0
        endif

        ! cubic (C&W Eq. 1.6)
        aplus % d(i) = 0.5*(a % d(i) + a % d(i+1)) - (1.0/6.0)*(dup - du0)
        aminus % d(i+1) = aplus % d(i)

     enddo

     ! now limit (C&W 1.10).  Here the loop is over cells, and
     ! considers the values on either side of the center of the cell
     ! (uminus and uplus)
     do i = ilo-1, ihi+1
        if ( (aplus % d(i) - a % d(i)) * &
             (a % d(i) - aminus % d(i)) <= 0.0) then
           aminus % d(i) = a % d(i)
           aplus % d(i) = a % d(i)

        else if ( (aplus % d(i) - aminus % d(i)) * &
                  (a % d(i) - 0.5*(aminus % d(i) + aplus % d(i))) > &
                  (aplus % d(i) - aminus % d(i))**2/6.0 ) then
           aminus % d(i) = 3.0*a % d(i) - 2.0*aplus % d(i)

        else if ( -(aplus % d(i) - aminus % d(i))**2/6.0 > &
                   (aplus % d(i) - aminus % d(i)) * &
                   (a % d(i) - 0.5*(aminus % d(i) + aplus % d(i))) ) then
           aplus % d(i) = 3.0*a % d(i) - 2.0*aminus % d(i)

        endif

     enddo

     ! finally integrate under the parabola (C&W Eq. 1.12), away from
     ! the interface.  Here the loop is over interfaces.  al and ar are
     ! the left and right states at the interface.
     do i = ilo, ihi+1

        al % d(i) = aplus % d(i-1) - 0.5*u*(dt/dx)*( (aplus % d(i-1) - aminus % d(i-1)) - &
             (1.0 - (2.0/3.0)*u*(dt/dx))*6.0*(a % d(i-1) - 0.5*(aminus % d(i-1) + aplus % d(i-1))) )

        ar % d(i) = aminus % d(i) + 0.5*u*(dt/dx)*( (aplus % d(i) - aminus % d(i)) + &
             (1.0 - (2.0/3.0)*u*(dt/dx))*6.0*(a % d(i) - 0.5*(aminus % d(i) + aplus % d(i))) )

     enddo

  endif

  return
end subroutine states



!============================================================================
! riemann: solve the Riemann problem
!============================================================================
subroutine riemann(u, al, ar, f)

  use grid_module

  implicit none

  double precision, intent(in) :: u
  type (gridvar_t), intent(in) :: al, ar
  type (gridvar_t), intent(inout) :: f

  integer :: i

  ! loop over all the interfaces and solve the Riemann problem.  Here,
  ! since we are doing the linear advection eq, we just use the advection
  ! velocity to tell us which direction is upwind, and use that state

  if (u >= 0.d0) then
     do i = f % gd % ilo, f % gd % ihi+1
        f % d(i) = u * al % d(i)
     enddo

  else
     do i = f % gd % ilo, f % gd % ihi+1
        f % d(i) = u * ar % d(i)
     enddo

  endif

  return
end subroutine riemann



!============================================================================
! update: conservatively update the solution to the new time level
!============================================================================
subroutine update(dt, a, f)

  use grid_module

  implicit none

  double precision, intent(in) :: dt
  type (gridvar_t), intent(in) :: f
  type (gridvar_t), intent(inout) :: a

  integer :: i

  do i = a % gd % ilo, a % gd % ihi
     a % d(i) = a % d(i) + (dt/a % gd % dx)*(f % d(i) - f % d(i+1))
  enddo

  return
end subroutine update



!============================================================================
! various limiter functions
!============================================================================
function minmod(a,b)

  implicit none

  double precision :: a, b
  double precision :: minmod

  if (abs(a) < abs(b) .and. a*b > 0.d0) then
     minmod = a
  else if (abs(b) < abs(a) .and. a*b > 0) then
     minmod = b
  else
     minmod = 0.d0
  endif

  return
end function minmod



function maxmod(a,b)

  implicit none

  double precision :: a, b
  double precision :: maxmod

  if (abs(a) > abs(b) .and. a*b > 0.d0) then
     maxmod = a
  else if (abs(b) > abs(a) .and. a*b > 0) then
     maxmod = b
  else
     maxmod = 0.d0
  endif

  return
end function maxmod

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

program advect

  implicit none

  ! the number of zones (nx) and number of guardcells (ng)
  integer, parameter :: nx = 64
  integer, parameter :: ng = 3

  ! the domain size
  double precision, parameter :: xmin = 0.d0
  double precision, parameter :: xmax = 1.d0

  ! the advection velocity
  double precision, parameter :: a = 1.d0

  ! the CFL number
  double precision, parameter :: cfl = 0.8d0

  ! initial condition type
  integer, parameter :: inittype = 2
  
  ! slope type (1=godunov, 2=plm, 3=ppm)
  integer, parameter :: islopetype = 3

  integer, parameter :: plmlimiter = 1

  ! maximum simulation time
  double precision, parameter :: tmax = 5.d0


  double precision, dimension(2*ng + nx) :: x
  double precision, dimension(2*ng + nx) :: u, ul, ur, f
  double precision, dimension(2*ng + nx) :: uinit

  double precision :: dx
  double precision :: time, dt

  integer :: imin, imax

  ! setup the grid and set the initial conditions
  call grid(nx, ng, xmin, xmax, dx, x)

  imin = ng+1
  imax = ng+nx

  call init(nx, ng, inittype, x, u)
  uinit = u

  time = 0.d0

  call output(nx, ng, inittype, islopetype, plmlimiter, a, time, x, u)


  ! evolution loop -- construct the interface states, solve the Riemann 
  ! problem, and update the solution to the new time level
  do while (time < tmax)

     call fillBC(nx, ng, u)

     call timestep(nx, ng, dx, a, cfl, u, dt)
     if (time + dt > tmax) then
        dt = tmax - time
     endif

     call states(nx, ng, dx, dt, a, islopetype, plmlimiter, u, ul, ur)

     call riemann(nx, ng, a, ul, ur, f)

     call update(nx, ng, dx, dt, u, f)

     time = time + dt

  enddo
     
  call output(nx, ng, inittype, islopetype, plmlimiter, a, time, x, u)

  print *, 'N, error: ', nx, sqrt(dx*sum((u(imin:imax)-uinit(imin:imax))**2))

end program advect
  


!============================================================================
! grid: create the grid
!============================================================================
subroutine grid(nx, ng, xmin, xmax, dx, x)

  implicit none

  integer :: nx, ng
  double precision :: xmin, xmax

  double precision :: dx
  double precision, dimension(2*ng+nx) :: x

  integer :: i

  ! create the grid
  dx = (xmax - xmin)/dble(nx)

  do i = 1, 2*ng+nx
     x(i) = (i-ng-0.5d0)*dx + xmin
  enddo

  return
end subroutine grid



!============================================================================
! init: set the initial conditions
!============================================================================
subroutine init(nx, ng, inittype, x, u)

  implicit none

  integer :: nx, ng
  integer :: inittype

  double precision, dimension(2*ng+nx) :: x, u

  integer :: i, imin, imax

  double precision, parameter :: pi = 3.14159d0

  imin = ng+1
  imax = ng+nx

  ! loop over all the zones and set the initial conditions.  To be
  ! consistent with the finite-volume discretization, we should store
  ! the zone averages here, but, to second-order accuracy, it is
  ! sufficient to evaluate the initial conditions at the zone center.

  do i = imin, imax
     
     if (inittype == 1) then
        ! sin wave
        u(i) = sin(2.d0*pi*x(i))

     else if (inittype == 2) then
        ! square wave
        if (x(i) > 0.333d0 .and. x(i) < 0.666d0) then
           u(i) = 1.d0
        else
           u(i) = 0.d0
        endif

     else if (inittype == 3) then
        ! wave packet
        u(i) = sin(16.d0*pi*x(i))*exp(-36.d0*(x(i)-0.5d0)**2)

     else if (inittype == 4) then
        ! gaussian
        u(i) = exp(-(x(i)-0.5d0)**2/0.1d0**2) 

     endif

  enddo

  return
end subroutine init



!============================================================================
! output: write out the solution
!============================================================================
subroutine output(nx, ng, inittype, islopetype, plmlimiter, a, time, x, u)

  implicit none

  integer :: nx, ng

  integer :: inittype, islopetype, plmlimiter

  double precision :: a

  double precision :: time

  double precision, dimension(2*ng+nx) :: x, u

  character (len=4) :: time_string
  character (len=16) :: slope, init, res

  integer :: i, imin, imax

  imin = ng+1
  imax = ng+nx


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
  write (res, '(i8)') nx

  open(unit=10, file="advect-"//trim(slope)//"-"//trim(init)//"-nx="//trim(adjustl(res))//"-t="//time_string, status="unknown")

  write (10,*) "# advection problem: u_t + a u_x = 0"
  write (10,*) "# a = ", a
  write (10,*) "# init = ", inittype
  write (10,*) "# slope type = ", islopetype
  if (islopetype == 2) then
     write (10,*) "# plm limiter = ", plmlimiter
  endif
  write (10,*) "# time = ", time


  do i = imin, imax
     write (10,*) x(i), u(i)
  enddo

  return
end subroutine output



!============================================================================
! fillBC: fill the boundary conditions
!============================================================================
subroutine fillBC(nx, ng, u)

  implicit none

  integer :: nx, ng
  double precision, dimension(2*ng+nx) :: u

  integer :: i, imin, imax

  imin = ng+1
  imax = ng+nx

  ! left boundary
  do i = 1, imin-1
     u(i) = u(imax-ng+i)
  enddo

  
  ! right boundary
  do i = imax+1, 2*ng+nx
     u(i) = u(i-imax+ng)
  enddo

  return
end subroutine fillBC
  
  

!============================================================================
! timestep: compute the new timestep
!============================================================================
subroutine timestep(nx, ng, dx, a, cfl, u, dt)
  
  implicit none

  integer :: nx, ng
  double precision :: dx
  double precision :: a, cfl
  double precision, dimension(2*ng+nx) :: u
  double precision :: dt
  
  integer :: i, imin, imax

  imin = ng+1
  imax = ng+nx
  
  
  ! in the linear advection equation, the timestep is trivial
  dt = cfl*dx/abs(a)
    
  return
end subroutine timestep
  


!============================================================================
! states: compute the interface states used in solving the Riemann problem
!============================================================================
subroutine states(nx, ng, dx, dt, a, islopetype, plmlimiter, u, ul, ur)

  implicit none

  integer :: nx, ng
  integer :: islopetype, plmlimiter

  double precision, dimension(2*ng+nx) :: u, ul, ur
  double precision, dimension(2*ng+nx) :: slope

  double precision, dimension(2*ng+nx) :: uminus, uplus
  double precision :: du0, dup
  double precision :: slope1, slope2

  double precision :: a
  double precision :: dx, dt

  double precision :: minmod, maxmod

  integer :: i, imin, imax

  imin = ng+1
  imax = ng+nx

  ! compute the centered difference for linear slopes
  if (islopetype == 1) then

     ! Godunov's method (piecewise constant)
     
     ! for each interface, we want to construct the left and right
     ! states.  Here, interface i refers to the left edge of zone i
     
     ! interfaces imin to imax+1 affect the data in zones [imin,imax]
     do i = imin, imax+1
        
        ! the left state on the current interface comes from zone i-1.  
        ul(i) = u(i-1) 
        
        ! the right state on the current interface comes from zone i
        ur(i) = u(i)          
        
     enddo
     
        
  else if (islopetype == 2) then
     
     if (plmlimiter == 1) then
        ! PLM with MC limiter
     
        ! interface states are found by Taylor expansion in time
        ! (through dt/2) and space (dx/2 toward the interface)
     

        ! for each interface, we want to construct the left and right
        ! states.  Here, interface i refers to the left edge of zone i
     
        do i = imin-1, imax+1
           
           slope(i) = minmod(minmod(2.d0*(u(i) - u(i-1))/dx, &
                                    2.d0*(u(i+1) - u(i))/dx), &
                             0.5d0*(u(i+1) - u(i-1))/dx)

        enddo
        
     else if (plmlimiter == 2) then
        ! PLM with SuperBee limiter

        do i = imin-1, imax+1
           slope1 = minmod((u(i+1) - u(i))/dx, &
                            2.d0*(u(i) - u(i-1))/dx)

           slope2 = minmod(2.d0*(u(i+1) - u(i))/dx, &
                          (u(i) - u(i-1))/dx)

           slope(i) = maxmod(slope1, slope2)
        enddo

     endif

     ! interfaces imin to imax+1 affect the data in zones [imin,imax]
     do i = imin, imax+1

        ! the left state on the current interface comes from zone i-1.  
        ul(i) = u(i-1) + 0.5*dx*(1.d0 - a*(dt/dx))*slope(i-1) 
        
        ! the right state on the current interface comes from zone i
        ur(i) = u(i) - 0.5*dx*(1.d0 + a*(dt/dx))*slope(i)
        
     enddo

  else if (islopetype == 3) then

     ! PPM 

     ! refer to the PPM paper for equation references
     
     ! use a cubic interpolation polynomial to find the edge states in
     ! zone i -- uminus(i) and uplus(i).  Here we loop over
     ! interfaces.
     do i = imin-2, imax+1

        ! du (C&W Eq. 1.7)
        du0 = 0.5*(u(i+1) - u(i-1))
        dup = 0.5*(u(i+2) - u(i))

        ! limiting (C&W Eq. 1.8)
        if ((u(i+1) - u(i))*(u(i) - u(i-1)) > 0) then
           du0 = sign(1.0d0,du0)*min(abs(du0), 2.0*abs(u(i) - u(i-1)), & 
                                               2.0*abs(u(i+1) - u(i)))
        else
           du0 = 0.0
        endif

        if ((u(i+2) - u(i+1))*(u(i+1) - u(i)) > 0) then
           dup = sign(1.0d0,dup)*min(abs(dup), 2.0*abs(u(i+1) - u(i)), &
                                               2.0*abs(u(i+2) - u(i+1)))
        else
           dup = 0.0
        endif

        ! cubic (C&W Eq. 1.6)
        uplus(i) = 0.5*(u(i) + u(i+1)) - (1.0/6.0)*(dup - du0)
        uminus(i+1) = uplus(i) 
        
     enddo
        
     ! now limit (C&W 1.10).  Here the loop is over cells, and
     ! considers the values on either side of the center of the cell
     ! (uminus and uplus)
     do i = imin-1, imax+1
        if ( (uplus(i) - u(i))*(u(i) - uminus(i)) <= 0.0) then
           uminus(i) = u(i)
           uplus(i) = u(i)

        else if ( (uplus(i) - uminus(i))*(u(i) - 0.5*(uminus(i) + uplus(i))) > &
                  (uplus(i) - uminus(i))**2/6.0 ) then
           uminus(i) = 3.0*u(i) - 2.0*uplus(i)

        else if ( -(uplus(i) - uminus(i))**2/6.0 > &
                   (uplus(i) - uminus(i))*(u(i) - 0.5*(uminus(i) + uplus(i))) ) then
           uplus(i) = 3.0*u(i) - 2.0*uminus(i)

        endif

     enddo

     ! finally integrate under the parabola (C&W Eq. 1.12), away from
     ! the interface.  Here the loop is over interfaces.  ul and ur are
     ! the left and right states at the interface.
     do i = imin, imax+1

        ul(i) = uplus(i-1) - 0.5*a*(dt/dx)*( (uplus(i-1) - uminus(i-1)) - &
             (1.0 - (2.0/3.0)*a*(dt/dx))*6.0*(u(i-1) - 0.5*(uminus(i-1) + uplus(i-1))) )
        
        ur(i) = uminus(i) + 0.5*a*(dt/dx)*( (uplus(i) - uminus(i)) + &
             (1.0 - (2.0/3.0)*a*(dt/dx))*6.0*(u(i) - 0.5*(uminus(i) + uplus(i))) )

     enddo

        

  endif

  return
end subroutine states
  


!============================================================================
! riemann: solve the Riemann problem
!============================================================================
subroutine riemann(nx, ng, a, ul, ur, f)

  implicit none

  integer :: nx, ng
  double precision, dimension(2*ng+nx) :: ul, ur, f

  double precision :: a

  integer :: i, imin, imax

  imin = ng+1
  imax = ng+nx


  ! loop over all the interfaces and solve the Riemann problem.  Here,
  ! since we are doing the linear advection eq, we just use the advection
  ! velocity to tell us which direction is upwind, and use that state

  if (a >= 0.d0) then
     do i = imin, imax+1
        f(i) = a*ul(i)
     enddo

  else
     do i = imin, imax+1
        f(i) = a*ur(i)
     enddo
     
  endif

  return
end subroutine riemann



!============================================================================
! update: conservatively update the solution to the new time level
!============================================================================
subroutine update(nx, ng, dx, dt, u, f)

  implicit none

  integer :: nx, ng

  double precision :: dx, dt

  double precision, dimension(2*ng+nx) :: u, f

  integer :: i, imin, imax

  imin = ng+1
  imax = ng+nx

  do i = imin, imax
     u(i) = u(i) + (dt/dx)*(f(i) - f(i+1))
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

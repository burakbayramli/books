! Relaxation with MPI.  Here we solve a Poisson problem in 2-d.  We do
! domain decomposition in the x-direction.
!
! M. Zingale (2013-04-15)

program relax

  use mpi

  implicit none

  integer, parameter :: nx = 512
  integer, parameter :: ny = 512
  integer, parameter :: ng = 1

  ! set the lo/hi boundary conditions in each coordinate direction
  double precision, parameter :: bc_lo_x = 0.d0
  double precision, parameter :: bc_hi_x = 0.d0
  double precision, parameter :: bc_lo_y = 0.d0
  double precision, parameter :: bc_hi_y = 0.d0

  ! domain info
  double precision, parameter :: xmin = 0.0d0
  double precision, parameter :: xmax = 1.0d0
  double precision, parameter :: ymin = 0.0d0
  double precision, parameter :: ymax = 1.0d0

  double precision :: dx, dy, xx, yy

  double precision :: err

  ! smoothing info
  integer, parameter :: nsmooth = 2500

  double precision, allocatable :: f(:,:), v(:,:), w(:,:)

  integer :: mype, nprocs, ierr
  integer :: ilo_global, ihi_global
  integer :: ilo, ihi, jlo, jhi, mywidth
  integer :: iwidth, iextra

  integer :: i, j

  double precision :: start, finish

  ! initialize MPI
  call MPI_Init(ierr)
  
  start = MPI_Wtime()

  ! do the domain decomposition
  call MPI_Comm_Size(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_Comm_Rank(MPI_COMM_WORLD, mype, ierr)

  ! we could use the MPI_Cart_Create() stuff, but let's do it
  ! ourselves.  We will do a 1-d decomposition
  
  ! each processor will hold a slab with the j coordinates spanning
  ! the entire y-domain, and the i coordinates spanning their own
  ! respective subset.  We will refer to a global index space of nx x ny.
  iwidth = floor(real(nx/nprocs))
  iextra = mod(nx,nprocs)

  ! the first iwidth processors have a width of iwidth+1
  mywidth = iwidth
  if (mype < iextra) mywidth = mywidth + 1

  ! current processor's index space
  if (mype < iextra) then
     ilo = (iwidth+1)*mype
     ihi = ilo + mywidth - 1
  else
     ilo = (iwidth+1)*iextra + iwidth*(mype-iextra)
     ihi = ilo + mywidth - 1
  endif

  jlo = 0
  jhi = ny-1

  print *, mype, ilo, ihi, mywidth

  ! allocate our part of the domain (including ghost cells).  Note
  ! that the indices refer to the full global nx x ny index space
  allocate(v(ilo-ng:ihi+ng,jlo-ng:jhi+ng))
  allocate(f(ilo-ng:ihi+ng,jlo-ng:jhi+ng))
  allocate(w(ilo-ng:ihi+ng,jlo-ng:jhi+ng))

  v(:,:) = 0.0d0
  f(:,:) = 0.0d0

  dx = (xmax-xmin)/nx
  dy = (ymax-ymin)/ny


  ! fill the RHS
  do j = jlo, jhi
     yy = (dble(j) + 0.5d0)*dy + ymin

     do i = ilo, ihi
        xx = (dble(i) + 0.5d0)*dx + xmin

        f(i,j) = g(xx, yy)

     enddo
  enddo


  ! smooth
  call smooth(ilo, ihi, jlo, jhi, ng, &
              dx, dy, &
              bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y, &
              v, f, nsmooth)


  ! compute the error

  ! fill the true solution
  do j = jlo, jhi
     yy = (dble(j) + 0.5d0)*dy + ymin

     do i = ilo, ihi
        xx = (dble(i) + 0.5d0)*dx + xmin

        w(i,j) = true(xx, yy) - v(i,j)

     enddo
  enddo

  err = error(ilo, ihi, jlo, jhi, ng, dx, dy, w)

  if (mype == 0) print *, "ERROR = ", err

  finish = MPI_Wtime()

  if (mype == 0) print *, "wallclock time: ", finish-start

  ! end
  call MPI_Finalize(ierr)


contains

  !===========================================================================
  ! g
  !===========================================================================
  function g(x,y)

    ! the RHS of the Poisson equation we are solving

    implicit none

    double precision :: g, x, y
    double precision, parameter :: pi = 3.14159265358979323846d0

    g = -2.d0*(2.0*pi)**2 * sin(2.0*pi*x) * sin(2.0*pi*y)

    return
  end function g


  !===========================================================================
  ! true
  !===========================================================================
  function true(x,y)

    ! the analytic solution to our equation

    implicit none

    double precision true, x, y
    double precision, parameter :: pi = 3.14159265358979323846d0
  
    true = sin(2.0*pi*x) * sin(2.0*pi*y)
  
    return
  end function true


  !===========================================================================
  ! fillGC
  !===========================================================================
  subroutine fillGC(ilo, ihi, jlo, jhi, ng, &
                    bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y, &
                    v)

    implicit none

    integer :: ilo, ihi, jlo, jhi, ng
    double precision :: bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y
    double precision :: v(ilo-ng:ihi+ng,jlo-ng:jhi+ng)

    integer :: nprocs, mype, ierr
    integer :: status(MPI_STATUS_SIZE)

    integer :: sendto, recvfrom

    ! isr = 1 use blocking send/receive pairs, isr = 2 combine sendrecv
    integer, parameter :: isr = 2

    ! we are doing a 1-d domain decomposition

    call MPI_Comm_Size(MPI_COMM_WORLD, nprocs, ierr)
    call MPI_Comm_Rank(MPI_COMM_WORLD, mype, ierr)

    ! first fill any physical boundaries
    if (ilo == 0) then
       v(ilo-ng,:) = 2.0*bc_lo_x - v(ilo,:)
    endif

    if (jlo == 0) then
       v(:,jlo-ng) = 2.0*bc_lo_y - v(:,jlo)
    endif

    if (ihi == nx-1) then
       v(ihi+ng,:) = 2.0d0*bc_hi_x - v(ihi,:)
    endif

    if (jhi == ny-1) then
       v(:,jhi+ng) = 2.0d0*bc_hi_y - v(:,jhi)
    endif

    ! now fill the interior ghostcells

    if (isr == 1) then
       ! this is the slow method -- blocking sends and receives
    
       ! send first column of valid data to the left to fill left PE's
       ! right GCs
       if (mype > 0) then
          call MPI_Send(v(ilo,:), ny+2*ng, MPI_DOUBLE_PRECISION, &
                        mype-1, mype, MPI_COMM_WORLD, ierr)
       endif

       if (mype < nprocs-1) then
          call MPI_Recv(v(ihi+1,:), ny+2*ng, MPI_DOUBLE_PRECISION, &
                        mype+1, mype+1, MPI_COMM_WORLD, status, ierr)
       endif


       ! send last column of valid data to the right to fill right PE's
       ! left GCs
       if (mype < nprocs-1) then
          call MPI_Send(v(ihi,:), ny+2*ng, MPI_DOUBLE_PRECISION, &
                        mype+1, mype, MPI_COMM_WORLD, ierr)
       endif

       if (mype > 0) then
          call MPI_Recv(v(ilo-1,:), ny+2*ng, MPI_DOUBLE_PRECISION, &
                        mype-1, mype-1, MPI_COMM_WORLD, status, ierr)
       endif

    else if (isr == 2) then

       ! send first column of valid data to the left to fill left PE's
       ! right GCs and receive from the right PE
       if (mype == 0) then
          sendto = MPI_PROC_NULL
       else
          sendto = mype-1
       endif

       if (mype == nprocs-1) then
          recvfrom = MPI_PROC_NULL
       else
          recvfrom = mype+1
       endif

       call MPI_Sendrecv(v(ilo,:), ny+2*ng, MPI_DOUBLE_PRECISION, sendto, 0, &
                         v(ihi+1,:), ny+2*ng, MPI_DOUBLE_PRECISION, recvfrom, 0, &
                         MPI_COMM_WORLD, status, ierr)


       ! send last column of valid data to the right to fill right PE's
       ! left GCs and receive from the left PE
       if (mype == nprocs-1) then
          sendto = MPI_PROC_NULL
       else
          sendto = mype+1
       endif

       if (mype == 0) then
          recvfrom = MPI_PROC_NULL
       else
          recvfrom = mype-1
       endif

       call MPI_Sendrecv(v(ihi,:), ny+2*ng, MPI_DOUBLE_PRECISION, sendto, 0, &
                         v(ilo-1,:), ny+2*ng, MPI_DOUBLE_PRECISION, recvfrom, 0, &
                         MPI_COMM_WORLD, status, ierr)

    endif


    ! debugging
    !
    !do i = 0, nprocs-1
    !   if (i == mype) then
    !      print *, "mype = ", mype
    !      do j = jlo-1, jhi+1
    !         print *, real(v(:,j))
    !      enddo
    !      print *, " "
    !   endif
    !   call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !enddo
    !
    !stop

  end subroutine fillGC


  !=============================================================================  
  ! smooth                                                                        
  !=============================================================================  
  subroutine smooth(ilo, ihi, jlo, jhi, ng, &
                    dx, dy, &
                    bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y, &
                    v, f, nsmooth)

    ! given a solution vector, v, and a RHS vector, f, smooth v to
    ! better satisfy the equation.  This is done in place, using
    ! Red-Black Gauss-Seidel

    ! hi and lo Dirichlet boundary conditions are also passed.
    ! Because we are finite-volume, and therefore, cell-centered, we
    ! need to extrapolate to match the desired Dirichlet BC.

    implicit none

    integer :: ilo, ihi, jlo, jhi, ng
    integer :: nsmooth
    double precision :: dx, dy
    double precision :: bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y
    double precision, dimension(ilo-ng:ihi+ng,jlo-ng:jhi+ng) :: v, f

    integer :: i, j, m, ioff, color


    ! do some smoothing -- Red-Black Gauss-Seidel                                 
    do m = 1, nsmooth

       do color = 0, 1

          ! set the guardcells to give the proper boundary condition, using       
          ! extrapolation 
          call fillGC(ilo, ihi, jlo, jhi, ng, &
                      bc_lo_x, bc_hi_x, bc_lo_y, bc_hi_y, &
                      v)


          do j = jlo, jhi

             if (color == 0) then
                ioff = mod(j,2)
             else
                ioff = 1 - mod(j,2)
             endif

             do i = ilo+ioff, ihi, 2
                v(i,j) = 0.25d0*(v(i-1,j) + v(i+1,j) + &
                                 v(i,j-1) + v(i,j+1) - dx*dx*f(i,j))
             enddo

          enddo
       enddo

    enddo

  end subroutine smooth


  !===========================================================================
  ! error
  !===========================================================================
  function error(ilo, ihi, jlo, jhi, ng, dx, dy, v)

    ! compute the L2 norm 
  
    implicit none

    integer :: ilo, ihi, jlo, jhi, ng
    double precision :: dx, dy
    double precision :: v(ilo-ng:ihi+ng,jlo-ng:jhi+ng)
    
    integer :: i, j
    double precision :: error, errlocal
    integer :: ierr

    errlocal = 0.d0

    do j = jlo, jhi
       do i = ilo, ihi
          errlocal = errlocal + v(i,j)**2
       enddo
    enddo
    
    ! we now have the sum e**2 local on each processor.  Add this across processors
    call MPI_Allreduce(errlocal, error, 1, MPI_DOUBLE_PRECISION, &
                       MPI_SUM, MPI_COMM_WORLD, ierr)

    error = dx*dy*error    ! make it grid invariant
    error = sqrt(error)

    return
  end function error


end program relax

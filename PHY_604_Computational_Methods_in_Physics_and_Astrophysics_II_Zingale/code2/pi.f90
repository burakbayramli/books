! simple program to compute pi by dividing up [0,1] across processors and having
! each processor compute
!
! this is based on the example in Using MPI

program pi_mpi

  use mpi

  implicit none

  double precision, parameter :: pi = 3.1415926535897932384626433832795028841971d0

  integer :: ierr, mype, nprocs
  integer :: i, n
  double precision :: x, dx, sum, pi_int

  call MPI_Init(ierr)

  call MPI_Comm_Size(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_Comm_Rank(MPI_COMM_WORLD, mype, ierr)

  if (mype == 0) then
     print *, 'Enter the number of intervals: '
     read (*,*) n
  endif

  ! send the number of intervals to all processors
  call MPI_Bcast(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

  dx = 1.0d0/n

  ! each processor uses a simple midpoint rule to compute their
  ! partial sum.  We loop over all the points in our discretization of
  ! [0,1], and each processor sees only every nproc point
  sum = 0.0d0
  do i = mype, n-1, nprocs
     x = (dble(i) + 0.5d0)*dx
     sum = sum + f(x)
  enddo
  sum = sum*dx

  ! reduce the sum across all processors -- here only processor 0 will
  ! get the total sum
  call MPI_Reduce(sum, pi_int,1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  if (mype == 0) then
     print *, "pi = ", pi_int, "error = ", pi_int-pi
  endif

  call MPI_Finalize(ierr)

contains

  function f(x)
    ! the function we are integrating
    
    implicit none

    double precision :: f, x

    f = 4.0d0/(1.0d0 + x**2)
    return

  end function f

end program pi_mpi

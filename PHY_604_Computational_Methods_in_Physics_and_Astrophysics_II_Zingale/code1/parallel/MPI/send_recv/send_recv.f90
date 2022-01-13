! a simple example of sending and receiving.  Each processor has 3
! numbers (based on their rank) and then send the end numbers to the
! left and right processors, to give a 5 number sequence.  This mimics
! ghost cell filling
!
! M. Zingale (2013-04-15)
!
! run as 
!
!   mpiexec -n X ./sendrecv | sort


program sendreceive

  use mpi

  implicit none

  integer :: mype, nprocs, ierr, status(MPI_STATUS_SIZE)

  integer :: data(5), buffer

  call MPI_Init(ierr)

  call MPI_Comm_Rank(MPI_COMM_WORLD, mype, ierr)
  call MPI_Comm_Size(MPI_COMM_WORLD, nprocs, ierr)

  data(:) = 0
  data(2) = mype*3 
  data(3) = mype*3 + 1
  data(4) = mype*3 + 2

  ! send to the left
  if (mype > 0) then
     call MPI_Send(data(2), 1, MPI_INTEGER4, mype-1, mype, MPI_COMM_WORLD, ierr)
  endif

  if (mype <  nprocs-1) then
     call MPI_Recv(data(5), 1, MPI_INTEGER4, mype+1, mype+1, MPI_COMM_WORLD, status, ierr)
  endif

  ! send to the right
  if (mype < nprocs-1) then
     call MPI_Send(data(4), 1, MPI_INTEGER4, mype+1, mype, MPI_COMM_WORLD, ierr)
  endif

  if (mype > 0) then
     call MPI_Recv(data(1), 1, MPI_INTEGER4, mype-1, mype-1, MPI_COMM_WORLD, status, ierr)
  endif


  print *, mype, ":", data(1:5)

  call MPI_Finalize(ierr)
  
end program sendreceive

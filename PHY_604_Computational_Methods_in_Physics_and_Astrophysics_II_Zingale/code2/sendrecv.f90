! a simple example of sending and receiving.  Each processor has 3
! numbers (based on their rank) and then send the end numbers to the
! left and right processors, to give a 5 number sequence.  This mimics
! ghost cell filling
!
! This version uses the combined sendrecv() call
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
  integer :: sendto, recvfrom
  integer :: data(5), buffer

  call MPI_Init(ierr)

  call MPI_Comm_Rank(MPI_COMM_WORLD, mype, ierr)
  call MPI_Comm_Size(MPI_COMM_WORLD, nprocs, ierr)

  data(:) = 0
  data(2) = mype*3 
  data(3) = mype*3 + 1
  data(4) = mype*3 + 2

  ! send to the left and receive from the right
  !
  ! send: mype -> mype-1
  ! receive: mype <- mype+1

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

  call MPI_Sendrecv(data(2), 1, MPI_INTEGER4, sendto, 0, &
                    data(5), 1, MPI_INTEGER4, recvfrom, 0, &
                    MPI_COMM_WORLD, status, ierr)

  ! send to the right and receive from the left
  !
  ! send: mype -> mype+1
  ! receive: mype <- mype-1

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

  call MPI_Sendrecv(data(4), 1, MPI_INTEGER4, sendto, 1, &
                    data(1), 1, MPI_INTEGER4, recvfrom, 1, &
                    MPI_COMM_WORLD, status, ierr)


  print *, mype, ":", data(1:5)

  call MPI_Finalize(ierr)
  
end program sendreceive

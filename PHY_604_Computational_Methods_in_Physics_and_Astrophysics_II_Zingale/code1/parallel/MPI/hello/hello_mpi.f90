program hello

  use mpi

  implicit none

  integer :: ierr
  integer :: mype
  integer :: nprocs

  call MPI_Init(ierr)

  call MPI_Comm_Rank(MPI_COMM_WORLD, mype, ierr)
  call MPI_Comm_Size(MPI_COMM_WORLD, nprocs, ierr)

  if (mype == 0) then
     print *, "Running Hello, World on ", nprocs, " processors"
  endif

  print *, "Hello World", mype

  call MPI_Finalize(ierr)

end program hello

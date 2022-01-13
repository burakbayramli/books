program matmul

  ! matrix multiply with OpenMP
  !
  ! M. Zingale

  use omp_lib

  implicit none

  integer, parameter :: N = 50000
  
  double precision, allocatable :: a(:,:)
  double precision x(N)
  double precision b(N)
  double precision :: start_omp, finish_omp

  integer :: i, j

  start_omp = omp_get_wtime()

  allocate(a(N,N))

  ! initialize the matrix and vector
  !$omp parallel private(i, j)
  !$omp do
  do j = 1, N
     do i = 1, N
        a(i,j) = dble(i + j)
     enddo
     x(j) = j
     b(j) = 0.0
  enddo
  !$omp end do

  !multiply
  !$omp do
  do j = 1, N
     do i = 1, N
        b(i) = b(i) + a(i,j)*x(j)
     enddo
  enddo
  !$omp end do
  !$omp end parallel

  finish_omp = omp_get_wtime()

  print *, "execution time: ", finish_omp - start_omp

end program matmul


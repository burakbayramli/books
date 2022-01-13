program matmul

  ! matrix multiply with OpenMP
  !
  ! M. Zingale (2013-04-14)

  integer, parameter :: N = 1000
  
  double precision a(N,N)
  double precision x(N)
  double precision b(N)

  integer :: i, j

  ! initialize the matrix and vector
  !$omp parallel do private(i, j)
  do j = 1, N
     do i = 1, N
        a(i,j) = dble(i + j)
     enddo
  enddo
  !$omp end parallel do

  do i = 1, N
     x(i) = i
  enddo

  ! multiply
  !$omp parallel do private(i, j)
  do i = 1, N
     b(i) = 0.0
     do j = 1, N
        b(i) = b(i) + a(i,j)*x(j)
     enddo
  enddo  
  !$end parallel do

end program matmul


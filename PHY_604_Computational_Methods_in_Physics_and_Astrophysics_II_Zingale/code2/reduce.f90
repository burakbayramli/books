program reduce

  ! example of OpenMP reduction.  Here we pick some widely-varying
  ! numbers to sum to demonstrate that roundoff can come into play
  ! with threads
  !
  ! M. Zingale (2013-04-14)

  implicit none

  integer :: i

  double precision :: sum

  sum = 0.0d0

!$omp parallel do private (i) reduction(+:sum)
  do i = 1, 10000

     sum = sum + exp((mod(dble(i), 5.0d0) - 2*mod(dble(i),7.0d0)))

  end do
!$omp end parallel do

  print *, sum

end program reduce

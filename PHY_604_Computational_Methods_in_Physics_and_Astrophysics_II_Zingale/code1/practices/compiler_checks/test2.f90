! a simple example of how to use the compiler to trap floating point
! exception.

program test

  implicit none

  double precision a, b

  a = -1.0d0
  b = sqrt(a)

end program test

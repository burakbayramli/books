! a simple example of how to use the compiler to trap uninitialzed
! variables

program test

  implicit none

  double precision a, b, c

  !a = 0.0d0
  b = 1.0d0

  c = a + b

end program test

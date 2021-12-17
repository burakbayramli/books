! a simple example of how to use the compiler to trap floating invalid
! -- do 0/0

program test

  implicit none

  double precision a, b, c

  a = 0.0d0
  b = 0.0d0

  ! this is bad!
  c = a/b

end program test

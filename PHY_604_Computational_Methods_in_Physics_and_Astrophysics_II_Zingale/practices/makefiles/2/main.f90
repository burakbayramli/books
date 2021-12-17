program main

  implicit none
  
  double precision :: x, y, z
  double precision :: compute

  x = 1.0
  y = 2.0

  z = compute(x, y)

  call print_result(z)

end program main

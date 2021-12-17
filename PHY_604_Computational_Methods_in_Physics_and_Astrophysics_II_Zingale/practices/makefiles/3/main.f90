program main

  use compute_module

  implicit none
  
  double precision :: x, y, z

  x = 1.0
  y = 2.0

  z = compute(x, y)

  call print_result(z)

end program main

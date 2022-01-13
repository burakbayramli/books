module compute_module

  implicit none

  double precision, save :: min_value

contains
  function compute(x, y) result (z)

    double precision :: x, y, z

    min_value = min(x,y)
    z = x + y

    return
  end function compute
end module compute_module

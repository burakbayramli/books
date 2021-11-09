module datatypes_module

  implicit none

  integer, parameter :: dp_t = selected_real_kind(15,307)

  real (kind=dp_t) :: pi = 3.141592653589793238462643383279502884_dp_t

end module datatypes_module

program prof_example

  implicit none

  integer, parameter :: n = 100000000
  double precision, allocatable :: a(:), b(:), c(:), x(:)
  double precision, parameter :: pi = 3.1415926535897932384626433832795028d0
  integer :: i
  

  ! allocate
  allocate(a(n), b(n), c(n), x(n))

  ! initialize
  do i = 1, n
     x(i) = 2.d0*pi*dble(i)/n
  enddo

  ! do some heavy work
  call set_a(n, x, a)

  call set_b(n, x, b)

  call set_c(n, x, c)

end program prof_example


subroutine set_a(n, x, a)

  implicit none

  integer, intent(in) :: n
  double precision, intent(in) :: x(n)
  double precision, intent(out) :: a(n)

  integer :: i

  do i = 1, n
     a(i) = x(i)*2.d0
  enddo

end subroutine set_a

subroutine set_b(n, x, b)

  implicit none

  integer, intent(in) :: n
  double precision, intent(in) :: x(n)
  double precision, intent(out) :: b(n)

  integer :: i

  do i = 1, n
     b(i) = cos(x(i)) + x(i)*2.d0
  enddo

end subroutine set_b

subroutine set_c(n, x, c)

  implicit none

  integer, intent(in) :: n
  double precision, intent(in) :: x(n)
  double precision, intent(out) :: c(n)

  integer :: i

  do i = 1, n
     c(i) = exp(-x(i)**2)
     c(i) = c(i)**2
     c(i) = c(i) + cos(x(i))*sin(x(i))
  enddo

end subroutine set_c
     

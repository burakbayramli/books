! comparison of simple FFT vs DFT performance

module fft_module

  implicit none

  double complex, parameter :: I = cmplx(0.0d0, 1.0d0)
  double precision, parameter :: pi = 4.0*atan(1.0d0)

contains

  subroutine dft(fn, Fk, N)

    implicit none

    integer, intent(in) :: N
    double complex, intent(in) :: fn(0:N-1)
    double complex, intent(inout) :: Fk(0:N-1)

    integer :: k, m

    do k = 0, N-1
       Fk(k) = 0.0d0
       do m = 0, N-1
          Fk(k) = Fk(k) + fn(m)*exp(-2.0*pi*I*m*k/N)
       enddo
    enddo

  end subroutine dft

  recursive subroutine fft(fn, Fk, N)

    implicit none

    integer, intent(in) :: N
    double complex, intent(in) :: fn(0:N-1)
    double complex, intent(inout) :: Fk(0:N-1)

    double complex :: F_even(0:N/2-1)
    double complex :: F_odd(0:N/2-1)

    double complex :: omega

    integer :: k

    if (N == 1) then
       Fk(0) = fn(0)
       return

    else

       call fft(fn(0:N-1:2), F_even, N/2)
       call fft(fn(1:N-1:2), F_odd, N/2)

       omega = exp(-2*pi*I/N)

       do k = 0, N/2-1
          Fk(k) = F_even(k) + omega**k * F_odd(k)
          Fk(k + N/2) = F_even(k) - omega**k * F_odd(k)
       enddo

       return

    endif

  end subroutine fft

end module fft_module


program test

  use fft_module, only : fft, dft, pi

  implicit none

  double complex, allocatable :: fn(:)
  double complex, allocatable :: Fk(:)

  double precision, parameter :: xmin = 0.0
  double precision, parameter :: xmax = 50.0
  double precision, parameter :: f_0 = 0.2
  double precision :: h, x
  double precision :: ts, te, fft_time, dft_time

  integer :: i, N

  N = 2
  do while (N <= 16384)

     allocate(fn(0:N-1))
     allocate(Fk(0:N-1))

     h = (xmax - xmin)/N

     do i = 0, N-1
        x = i*h
        fn(i) = cmplx(sin(2*pi*f_0*x), 0.0d0)
     enddo

     call cpu_time(ts)
     call fft(fn, Fk, N)
     call cpu_time(te)

     fft_time = te - ts

     call cpu_time(ts)
     call dft(fn, Fk, N)
     call cpu_time(te)

     dft_time = te - ts

     print *, N, dft_time, fft_time

     deallocate(fn)
     deallocate(Fk)

     N = N*2

  enddo

  !do i = 0, N-1
  !   print *, i, real(Fk(i)), aimag(Fk(i))
  !enddo

end program test

      program test
      integer k
      end


      subroutine loop(rep, func)
      integer rep, i
      character*(*) func
      real*8 x, y, z, r, fwconsts
      external fwconsts
      x = 0.1
      y = -0.1
      z = 1.0

      write(*,*) 'F77 repetitions:',rep,' of ',func
      if (func .eq. 'fempty') then
      do i = 1, rep
         call fempty(x, y, z)
      end do
      else if (func .eq. 'fwconsts') then
      do i = 1, rep
         r = fwconsts(x, y, z)
      end do
      end if

      write(*,*) x
      return
      end

      subroutine fempty(x, y, z)
      real*8 x, y, z
      return
      end

      real*8 function fwconsts(x, y, z)
      real*8 x, y, z, a, b, c
      a = 0.3
      b = 1.2
      c = 1.22E+02
      fwconsts = a*x + b*y + c*z
      return
      end

      subroutine flops(n, a)
      integer n, i
      real*8 a, b, c
Cf2py intent(out) a
      a = 1.01
      b = 0.98
      c = 0.99
      do i = 1, n
         a = a*b*c*b*a*c*b*c
         b = b*c
      end do
      return
      end

      subroutine sinloop(x, n)
      integer n, i
      real*8 x(n)
Cf2py intent(in,out) x
      do i = 1, n
         x(i) = sin(x(i))
      end do
      return
      end

      subroutine sincosloop(x, n)
      integer n, i
      real*8 x(n), xi
Cf2py intent(in,out) x
      do i = 1, n
         xi = x(i)
         x(i) = sin(xi)*cos(xi) + xi**2
      end do
      return
      end

      subroutine manyarit(x, n)
C     lots of arithmetic operations
      integer n, i
      real*8 x(n), xi
Cf2py intent(in,out) x
      do i = 1, n
         xi = x(i)
         x(i) = sin(xi)*cos(xi) + sin(2*xi)*cos(2*xi) + 
     &          sin(3*xi)*cos(3*xi) + sin(4*xi)*cos(4*xi) + 
     &          sin(5*xi)*cos(5*xi)
      end do
      return
      end

      subroutine ip2loop(x, n)
      integer n, i
      real*8 x(n)
Cf2py intent(in,out) x
      do i = 1, n
         x(i) = i+2
      end do
      return
      end

      subroutine gridloop2D1(u, x, y, nx, ny)
      integer nx, ny
      real*8 x(0:nx-1), y(0:ny-1), u(0:nx-1, 0:ny-1)
Cf2py intent(in,out) u
      real*8 I1, xc, yc
      integer i, j
      do j = 0, ny-1
         yc = y(j)
         do i = 0, nx-1
            xc = x(i)
            u(i,j) = I1(x(i), y(j))
         end do
      end do
      return
      end

      real*8 function I1(x, y)
      real*8 x, y
      I1 = sin(x)*cos(x)
      return
      end

      subroutine gridloop2D2(u, x, y, nx, ny)
      integer nx, ny
      real*8 x(0:nx-1), y(0:ny-1), u(0:nx-1, 0:ny-1)
Cf2py intent(in,out) u
      real*8 xc, yc
      integer i, j
      do j = 0, ny-1
         yc = y(j)
         do i = 0, nx-1
            xc = x(i)
            u(i,j) = sin(xc)*cos(yc)
         end do
      end do
      return
      end

      subroutine gridloop2D3(u, x, y, nx, ny, w1, w2)
      integer nx, ny
      real*8 x(0:nx-1), y(0:ny-1), u(0:nx-1, 0:ny-1)
Cf2py intent(in,out) u
      real*8 w1(0:nx-1), w2(0:ny-1)
Cf2py intent(in,hide,cache) w1, w2
      real*8 w2j
      integer i, j
      do i = 0, nx-1
         w1(i) = sin(x(i))
      end do
      do i = 0, ny-1
         w2(i) = cos(y(i))
      end do

      do j = 0, ny-1
         w2j = w2(j)
         do i = 0, nx-1
            u(i,j) = w1(i)*w2j
         end do
      end do
      return
      end

      subroutine prod4(m, v, w, nrows, ncolumns)
      integer nrows, ncolumns
      real*8 m(nrows,ncolumns), v(ncolumns)
      real*8 w(nrows)
Cf2py intent(out) w
      
      integer i, j
      real*8 h

C     algorithm: straightforward, stride n in matrix access
      do i = 1, nrows
         w(i) = 0.0
         do j = 1, ncolumns
            w(i) = w(i) + m(i,j)*v(j)
         end do
      end do
      return
      end

      subroutine prod5(m, v, w, nrows, ncolumns)
      integer nrows, ncolumns
      real*8 m(nrows,ncolumns), v(ncolumns)
      real*8 w(nrows)
Cf2py intent(out,hide,cache) w
      
      integer i, j
      real*8 h

C     algorithm: cache friendly, stride 1 in A and w access

      do i = 1, nrows
         w(i) = 0.0
      end do

      do j = 1, ncolumns
         h = v(j)
         do i = 1, nrows
            w(i) = w(i) + m(i,j)*h
         end do
      end do
      return
      end

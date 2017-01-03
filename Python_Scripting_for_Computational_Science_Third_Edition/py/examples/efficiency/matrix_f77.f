C Module for a dense matrix. The matrix is stored in a common block
C with a fixed chunk size. Various subroutines define access to the
C matrix. (set_a gets the matrix transferred, for comparisons.)
C The programming style applies interface subroutines to be accessed
C from the calling code. Each interface subroutine calls a local
C routine with the common block data as arguments. The local routine
C declares the matrix (from the common block) with apprpriate size
C and performs the desired operations. As an example, the interface
C routine set(i, j, value) calls a local version, 
C lset(i, j, value, a, m, n), which defines a(m,n) with its current
C size and sets a(i,j)=value. For each interface routine XXX there
C is a corresponding local routine lXXX.

      subroutine makematrix(nrows, ncolumns)
      integer nrows, ncolumns

      integer m, n, alength
      parameter (alength=10000000)
      real*8 a(alength)
      common /matrixdata/ m, n, a

      m = nrows
      n = ncolumns
      if (m*n .gt. alength) then
         write(*,*) 'no space for matrix of size', m, 'x', n
      end if
      return
      end

      subroutine set(i, j, value)
      integer i, j
      real*8 value

      integer m, n
      parameter (alength=10000000)
      real*8 a(alength)
      common /matrixdata/ m, n, a
      call lset(i, j, value, a, m, n)
      return
      end

      subroutine lset(i, j, value, a, m, n)
      integer i, j, m, n
      real*8 value
      real*8 a(0:m-1, 0:n-1)
      a(i,j) = value
      return
      end

      subroutine set_a(a, m, n, i, j, value)
      integer m, n, i, j
      real*8 a(0:m-1,0:n-1), value
Cf2py intent(in, out) a
      a(i,j) = value
      return
      end

      subroutine get(i, j, value)
      integer i, j
      real*8 value
Cf2py intent(out) value

      integer m, n
      parameter (alength=10000000)
      real*8 a(alength)
      common /matrixdata/ m, n, a
      call lget(i, j, value, a, m, n)
      return
      end

      subroutine lget(i, j, value, a, m, n)
      integer i, j, m, n
      real*8 value
      real*8 a(0:m-1, 0:n-1)
Cf2py intent(out) value
      value = a(i,j)
      return
      end

      subroutine fill1()
      integer m, n
      parameter (alength=10000000)
      real*8 a(alength)
      common /matrixdata/ m, n, a
      call lfill1(a, m, n)
      return
      end

      subroutine lfill1(a, m, n)
      integer m, n
      real*8 a(m, n)
Cf2py intent(in,out) a
      integer i, j
      do j = 1,n
         do i = 1,m
            a(i,j) = i*j -2
         end do
      end do
      return
      end

      subroutine fill2()
      integer m, n
      parameter (alength=10000000)
      real*8 a(alength)
      common /matrixdata/ m, n, a
      call lfill2(a, m, n)
      return
      end

      subroutine lfill2(a, m, n)
      integer m, n
      real*8 a(m, n), x, y
Cf2py intent(in,out) a
      integer i, j
      do j = 1,n
         y = j*0.1
         do i = 1,m
            x = i*0.1
            a(i,j) = sin(x)*sin(y)*exp(-x*y)
         end do
      end do
      return
      end


      subroutine tonumpy(array, am, an)
      integer am, an
      real*8 array(0:am-1,0:an-1)
Cf2py intent(out) array
Cf2py depend(am, an) array

      integer m, n
      parameter (alength=10000000)
      real*8 a(alength)
      common /matrixdata/ m, n, a
      call ltonumpy(array, a, m, n)
      return
      end

      subroutine ltonumpy(array, a, m, n)
      integer m, n
      real*8 array(m, n), a(m, n)
      integer i, j
      do j = 1,n
         do i = 1,m
            array(i,j) = a(i,j)
         end do
      end do
      return
      end

      subroutine adump()
      integer m, n
      parameter (alength=10000000)
      real*8 a(alength)
      common /matrixdata/ m, n, a
      call ladump(a, m, n)
      return 
      end

      subroutine ladump(a, m, n)
      integer m, n
      real*8 a(m, n)
      integer i, j
      do j = 1,n
         do i = 1,m
            write(*,*) 'a(',i,',',j,')=',a(i,j)
         end do
      end do
      return
      end

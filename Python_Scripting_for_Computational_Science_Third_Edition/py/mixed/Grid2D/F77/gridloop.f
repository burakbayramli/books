      program test
      integer nmax
      parameter (nmax=1100)
C      parameter (nmax=1000)
      real*8 f(nmax*nmax), xcoor(nmax), ycoor(nmax), f2, myfunc
      external f2, myfunc
      integer i, n
      n = nmax
      do i = 1,n
         xcoor(i) = (i-1)/float(n-1)
         ycoor(i) = (i-1)/float(n-1)
      end do
C     for timing purposes, repeat 50 times:
      do i = 1,50
         call gridloop1(f, xcoor, ycoor, n, n, myfunc)
      end do
      write(*,*) 'call gridloop1 was run 50 times!'

      n = 3
      do i = 1,n
         xcoor(i) = (i-1)/float(n-1)
         ycoor(i) = (i-1)/float(n-1)
      end do
      call gridloop1(f, xcoor, ycoor, n, n, f2)
      call dump(f, xcoor, ycoor, n, n)
      end

C note: gridloop2 is the recommended gridloop* function to
C interface from Python. Do not use gridloop1* as these are
C just made for teaching purposes.

      subroutine gridloop1(a, xcoor, ycoor, nx, ny, func1)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1), func1
C     call this function with an array a that has
C     column major storage (use as_column_major_storage(array)
C     to make such an array)
Cf2py intent(inout) a
Cf2py intent(in) xcoor
Cf2py intent(in) ycoor
Cf2py depend(nx, ny) a

      external func1
      integer i,j
      real*8 x, y
      do j = 0, ny-1
         y = ycoor(j)
         do i = 0, nx-1
            x = xcoor(i)
            a(i,j) = func1(x, y) 
         end do
      end do
      return
      end

      subroutine gridloop2(a, xcoor, ycoor, nx, ny, func1)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1), func1
      external func1
Cf2py intent(out) a
Cf2py intent(in) xcoor
Cf2py intent(in) ycoor
Cf2py depend(nx,ny) a

      integer i,j
      real*8 x, y
      do j = 0, ny-1
         y = ycoor(j)
         do i = 0, nx-1
            x = xcoor(i)
            a(i,j) = func1(x, y) 
         end do
      end do
      return
      end

      subroutine gridloop3(a, xcoor, ycoor, nx, ny, func1)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1), func1
Cf2py intent(in,out) a
Cf2py intent(in) xcoor
Cf2py intent(in) ycoor

      external func1
      integer i,j
      real*8 x, y
      do j = 0, ny-1
         y = ycoor(j)
         do i = 0, nx-1
            x = xcoor(i)
            a(i,j) = a(i,j) + func1(x, y) 
         end do
      end do
      return
      end

      subroutine gridloop4(a, xcoor, ycoor, nx, ny, func1)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1), func1
Cf2py intent(in,out,overwrite) a
Cf2py intent(in) xcoor
Cf2py intent(in) ycoor

      external func1
      integer i,j
      real*8 x, y
      do j = 0, ny-1
         y = ycoor(j)
         do i = 0, nx-1
            x = xcoor(i)
            a(i,j) = a(i,j) + func1(x, y) 
         end do
      end do
      return
      end

C as gridloop2, but the callback sends the arrays back for
C vectorized computations in Python (func1)
      subroutine gridloop_vec1(a, xcoor, ycoor, nx, ny, func1)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1)
Cf2py intent(in,out) a
      external func1

      call func1(a, xcoor, ycoor, nx, ny)
      return
      end

      subroutine gridloop_vec2(a, nx, ny, func1)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1)
Cf2py intent(in,out) a
      external func1

      call func1(a, nx, ny)
      return
      end

      subroutine gridloop2_str(a, xcoor, ycoor, nx, ny, func_str)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1)
      character*(*) func_str
Cf2py intent(out) a
Cf2py intent(in) xcoor
Cf2py intent(in) ycoor
Cf2py depend(nx,ny) a
      real*8 myfunc, f2
      external myfunc, f2
C     write(*,*) 'func_str=',func_str

      if (func_str .eq. 'myfunc') then
         call gridloop2(a, xcoor, ycoor, nx, ny, myfunc)
      else if (func_str .eq. 'f2') then
         call gridloop2(a, xcoor, ycoor, nx, ny, f2)
      end if
      return
      end

      subroutine gridloop_noalloc(a, xcoor, ycoor, nx, ny, func_str)
C     As gridloop2_str, but a is intent(in,out) such that the
C     wrapper code can avoid allocating a (this is important
C     if the function is called a large number of times, as
C     in our efficiency tests, for instance).
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1)
      character*(*) func_str
Cf2py intent(in,out) a
Cf2py intent(in) xcoor
Cf2py intent(in) ycoor
      real*8 myfunc, f2
      external myfunc, f2

      if (func_str .eq. 'myfunc') then
         call gridloop2(a, xcoor, ycoor, nx, ny, myfunc)
      else if (func_str .eq. 'f2') then
         call gridloop2(a, xcoor, ycoor, nx, ny, f2)
      end if
      return
      end


C here follows several experimental versions of gridloop1
C to demonstrate how F2py deals with input/output arguments
C of NumPy array type:

C gridloop_v1: first try, a doesn't get out properly
C gridloop_v2: intent(inout) a, works if a is column major
C gridloop_v3: intent(inout,c) a, works if a is row major
C gridloop_v4: intent(inout,c) a + transpose of a

      subroutine gridloop1_v1(a, xcoor, ycoor, nx, ny, func1)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1), func1
      external func1
      integer i,j
      real*8 x, y
      do j = 0, ny-1
         y = ycoor(j)
         do i = 0, nx-1
            x = xcoor(i)
            a(i,j) = func1(x, y) 
         end do
      end do
      return
      end


      subroutine gridloop1_v2(a, xcoor, ycoor, nx, ny, func1)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1), func1
Cf2py intent(inout) a
      external func1
      integer i,j
      real*8 x, y
      do j = 0, ny-1
         y = ycoor(j)
         do i = 0, nx-1
            x = xcoor(i)
            a(i,j) = func1(x, y) 
         end do
      end do
      return
      end

      subroutine gridloop1_v3(a, xcoor, ycoor, nx, ny, func1)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1), func1
Cf2py intent(inout,c) a
      external func1
      integer i,j
      real*8 x, y
      do j = 0, ny-1
         y = ycoor(j)
         do i = 0, nx-1
            x = xcoor(i)
            a(i,j) = func1(x, y) 
         end do
      end do
      return
      end

      subroutine gridloop1_v4(a, xcoor, ycoor, nx, ny, func1, at)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1), func1, 
     &       at(0:nx-1,0:ny-1)
C     at is a scratch array for transposing a
      external func1
Cf2py intent(inout,c) a
Cf2py intent(in,hide) at

      integer i,j
      real*8 x, y
      do j = 0, ny-1
         y = ycoor(j)
         do i = 0, nx-1
            x = xcoor(i)
            a(i,j) = func1(x, y) 
         end do
      end do
      call transpose2dim(a, at, nx, ny)
C     copy:
      do i = 0, nx-1
         do j = 0, ny-1
            a(i,j) = at(i,j)
         end do
      end do
      return
      end

      subroutine transpose2dim(a, at, nx, ny)
      integer nx, ny, i, j
      real*8 a(0:nx-1,0:ny-1), at(0:ny-1,0:nx-1)
      do i = 0, nx-1
         do j = 0, ny-1
            at(j,i) = a(i,j)
         end do
      end do
      return
      end

C some utility functions:

      subroutine dump(a, xcoor, ycoor, nx, ny)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1)
      integer i,j
      real*8 x, y
      do j = 0,ny-1
         y = ycoor(j)
         do i = 0,nx-1
            x = xcoor(i)
            write(*,1000) 'value at (',x,',',y,') = a(',i,',',j,
     >      ') = ',a(i,j)
 1000       format(a,f6.3,a,f6.3,a,i3,a,i3,a,e12.5)
         end do
      end do
      return
      end

      subroutine change(a, xcoor, ycoor, nx, ny)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1)
      integer j
      do j = 0, ny-1
         a(1,j) = -999
      end do
      xcoor(1) = -999
      ycoor(1) = -999
      return 
      end

C some functions for testing:      

      subroutine gridloop2_fixedfunc1(a, xcoor, ycoor, nx, ny)
      integer nx, ny
      real*8 a(0:nx-1,0:ny-1), xcoor(0:nx-1), ycoor(0:ny-1)
Cf2py intent(out) a
Cf2py intent(in) xcoor
Cf2py intent(in) ycoor
Cf2py depend(nx,ny) a

      integer i,j
      real*8 x, y, myfunc
      external myfunc
      do j = 0, ny-1
         y = ycoor(j)
         do i = 0, nx-1
            x = xcoor(i)
            a(i,j) = myfunc(x, y) 
         end do
      end do
      return
      end

      real*8 function myfunc(x, y)
      real*8 x, y
      myfunc = sin(x*y) + 8*x
      return
      end

      real*8 function f2(x, y)
      real*8 x, y
      f2 = x + 2*y
      write(*,*) 'in f2', f2
      return
      end


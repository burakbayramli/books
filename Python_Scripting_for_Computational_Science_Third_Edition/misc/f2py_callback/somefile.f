      subroutine r1(x, y, n, f1)
      integer n
      real*8 x(n), y(n)
      external f1
      call f1(x, y, n)
      return
      end

      subroutine r2(x, y, n, f2)
      integer n
      real*8 x(n), y(n)
      external f2
      call r1(x, y, n, f2)
      return
      end


      

      subroutine f1(func)
      external func
      real*8   func, s
      s = func(1.2)
      write(*,*) 'Result of callback:', s
      return
      end

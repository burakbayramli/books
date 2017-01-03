      program hwtest
      real*8 r1, r2 ,s
      r1 = 1.0
      r2 = 0.0
      s = hw1(r1, r2)
      write(*,*) 'hw1, result:',s
      write(*,*) 'hw2, result:'
      call hw2(r1, r2)
      call hw3(r1, r2, s)
      write(*,*) 'hw3, result:', s
      end

      real*8 function hw1(r1, r2)
      real*8 r1, r2
      hw1 = sin(r1 + r2)
      return
      end

      subroutine hw2(r1, r2)
      real*8 r1, r2, s
      s = sin(r1 + r2)
      write(*,1000) 'Hello, World! sin(',r1+r2,')=',s
 1000 format(A,F6.3,A,F8.6)
      return
      end

C     special version of hw1 where the result is
C     returned as an argument:

      subroutine hw3_v1(r1, r2, s)
      real*8 r1, r2, s
      s = sin(r1 + r2)
      return
      end

C     F2py treats s as input arg. in hw3_v1; fix this:

      subroutine hw3(r1, r2, s)
      real*8 r1, r2, s
Cf2py intent(out) s
      s = sin(r1 + r2)
      return
      end

C     test case sensitivity:

      subroutine Hw4(R1, R2, s)
      real*8 R1, r2, S
Cf2py intent(out) s
      s = SIN(r1 + r2)
      ReTuRn
      end

C end of F77 file


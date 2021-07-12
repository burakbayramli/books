c$Id:$
      subroutine pload(id,f1,dr,prop,flg)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Form nodal load vector for current time

c      Inputs:
c         id(*)    - Equation numbers for degree of freedom
c         prop     - Total proportional load level
c         flg      - Flag: Form residual if true; else reactions

c      Outputs:
c         f1(*)    - Total nodal load for t_n+1
c         dr(*)    - Total reaction/residual
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'ddata.h'
      include  'fdata.h'
      include  'prld1.h'
      include  'sdata.h'

      include  'pointer.h'
      include  'comblk.h'

      logical   flg
      integer   j,n, ipro
      integer   fn1,fn2,f01,f02, fu1,fu2
      integer   id(*)
      real*8    prop,thn, f1(nneq,*),dr(*)

c     Set force vectors for t_n+1

      fl(11) = .false.
      do n = 1,nneq

c               F
        fn1 = np(27) + n - 1
        fn2 = fn1 + nneq
c               FU
        fu1 = np(28) + n - 1
        fu2 = fu1 + nneq
c               F0
        f01 = fu2 + nneq
        f02 = f01 + nneq

c                   FPRO
        ipro = mr(np(29)+n-1)
        if(ipro.eq.0) then     ! {
          if(id(n).gt.0) then
            f1(n,1) = hr(fn1)*prop  + hr(f01) + hr(fu1)
          else
            f1(n,1) = hr(fn2)*prop  + hr(f02) + hr(fu2)
          endif
          f1(n,3)   = hr(fn1)*prop + hr(f01) + hr(fu1)
        else
          if(id(n).gt.0) then
            f1(n,1) = hr(fn1)*prldv(ipro) + hr(f01) + hr(fu1)
          else
            f1(n,1) = hr(fn2)*prldv(ipro) + hr(f02) + hr(fu2)
          endif
          f1(n,3) = hr(fn1)*prldv(ipro) + hr(f01) + hr(fu1)
        endif                  ! if }
      end do ! n

c     Initialize residual/reaction

      if(flg) then
        do n = 1,nneq
          dr(n) = 0.0d0
        end  do ! n
      endif

c     Compute interpolated load vector

      thn = 1.0d0 - theta(3)

      do n = 1,nneq
        j = id(n)
        if(j.gt.0) then
          if(flg) then
            dr(j) = dr(j) + theta(3)*f1(n,1) + thn*f1(n,2)
          else
            dr(n) =         theta(3)*f1(n,1) + thn*f1(n,2)
          endif
        endif
      end do ! n

      end

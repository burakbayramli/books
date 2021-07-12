c$Id:$
      subroutine int1dl(l,sw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose:  Obtains abcissae/weights of Gauss-Lobatto quadrature

c      Inputs:
c         l       - Number of points of quadrature

c      Outputs
c         sw(1,l) - Abcissae of quadrature
c         sw(2,l) - Weights of quadrature
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer   l
      real*8    sw(2,*), t0,t1,t2

      save

c     Set end points

      sw(1,1) = -1.d0
      sw(1,l) =  1.d0

c     2 points

      if(l.eq.2) then

        sw(2,1) =  1.d0
        sw(2,2) =  1.d0

c     3 points

      elseif(l.eq.3) then

        sw(1,2) =  0.d0

        sw(2,1) =  1.d0/3.d0
        sw(2,2) =  4.d0*sw(2,1)
        sw(2,3) =  sw(2,1)

c     4 points

      elseif(l.eq.4) then

        sw(1,2) = -sqrt(0.2d0)
        sw(1,3) = -sw(1,2)

        sw(2,1) =  1.d0/6.d0
        sw(2,2) =  5.d0*sw(2,1)
        sw(2,3) =  sw(2,2)
        sw(2,4) =  sw(2,1)

c     5 points

      elseif(l.eq.5) then

        sw(1,2) = -sqrt(3.d0/7.d0)
        sw(1,3) =  0.d0
        sw(1,4) = -sw(1,2)

        sw(2,1) =   0.1d0
        sw(2,2) =  49.d0/90.d0
        sw(2,3) =  64.d0/90.d0
        sw(2,4) =  sw(2,2)
        sw(2,5) =  sw(2,1)

c     6 points

      elseif(l.eq.6) then
        t0   =  sqrt(7.d0)
        t1   = (7.d0 + 2.d0*t0)/21.d0
        t2   = (7.d0 - 2.d0*t0)/21.d0

        sw(1,2) = -sqrt(t1)
        sw(1,3) = -sqrt(t2)
        sw(1,4) = -sw(1,3)
        sw(1,5) = -sw(1,2)

        sw(2,1) =  1.d0/15.d0
        sw(2,2) =  0.6d0/(t1*(1.d0-t0)**2)
        sw(2,3) =  0.6d0/(t2*(1.d0+t0)**2)
        sw(2,4) =  sw(2,3)
        sw(2,5) =  sw(2,2)
        sw(2,6) =  sw(2,1)

c     Requested integration not allowed

      else

        write(iow,3000) l

      endif

c     Format

3000  format(/5x,'Error in INT1DL.',5x,'No',i3,2x,
     &          'Integration points allowed'/)

      end

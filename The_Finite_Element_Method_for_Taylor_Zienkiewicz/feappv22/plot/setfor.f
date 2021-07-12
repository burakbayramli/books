c$Id:$
      subroutine setfor(f,f0,prop,nn, dr)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Set current value of nodal forces for plots

c      Inputs:
c         f(*)    - Value of force controlled by prop
c         f0(*)   - Base value of force
c         prop    - Proportional load factor
c         nn      - Number of dof

c      Outputs:
c         dr(*)   - Values of nodal forces
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pointer.h'
      include  'prld1.h'
      include  'comblk.h'

      integer   n,nn
      integer   nty
      real*8    prop
      real*8    f(*),f0(*),dr(*)

      save

c     Check for proportional loading: f = current load,
c         f0(2*nn) = base load, f0 = user supplied loads

      do n = 1,nn
        nty = mr(np(29)+n-1)
        if(nty.le.0) then
          dr(n) = dr(n) + f(n)*prop       + f0(n) + f0(2*nn+n)
        else
          dr(n) = dr(n) + f(n)*prldv(nty) + f0(n) + f0(2*nn+n)
        endif
      end do

      end

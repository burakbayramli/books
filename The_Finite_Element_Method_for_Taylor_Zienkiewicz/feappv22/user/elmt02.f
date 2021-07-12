c$Id:$
      subroutine elmt02(d,ul,xl,ix,tl,s,p,ndf,ndm,nst,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

      implicit  none

      integer   ndf , ndm , nst , isw
      integer   ix(*)
      real*8    d(*), ul(*), xl(*), tl(*), s(*), p(*)

      if(isw.gt.0) write(*,2000)
2000  format('    Elmt 02: *WARNING* Dummy subroutine called')
      end

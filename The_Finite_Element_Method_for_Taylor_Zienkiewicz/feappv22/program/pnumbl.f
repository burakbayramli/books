c$Id:$
      subroutine pnumbl(ndm,nr,ns,nt,ntyp, nf,ng, flag)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

      implicit  none

      logical   flag
      integer   ndm,nr,ns,nt,ntyp,nf,ng

      save

c     Check the 2-D types

      if(ntyp.lt.10) then
        if(flag) then
          ng = nr + 1
          if(ns.eq.1) then
            nf = nr
          else
            nf = nr/2
          endif
          nr = nr + 1
        else
          if (ntyp.eq.0) then
            nf = nr*ns
          elseif (abs(ntyp).eq.7) then
            nf = (nr*ns)/2
          elseif (ntyp.ge.8) then
            nf = (nr*ns)/4
          elseif (ntyp.lt.0) then
            nf = 4*nr*ns
          else
            nf = 2*nr*ns
          endif

c         Determine last node number to be generated

          nr = nr + 1
          ns = ns + 1
          if(ndm.eq.1) ns = 1
          ng = nr*ns
          if(ntyp.eq. -7) then
            ng = ng + (nr-1)*(ns-1)/2
          elseif(ntyp .eq. -1) then
            ng = ng + (nr-1)*(ns-1)
          elseif(ntyp .eq.  8) then
            ng = ng - ((nr-1)*(ns-1))/4
          endif
        endif

c     3-d generations

      elseif(ntyp.lt.20) then
        if(ntyp.eq.11) then
          nf = nr*ns*nt*6
          ng = (nr+1)*(ns+1)*(nt+1)
        else
          nf = nr*ns*nt
          ng = (nr+1)*(ns+1)*(nt+1)
        endif

c     Shell:

      elseif(ntyp.lt.30) then
        if (ntyp.eq.20) then
          nf = nr*ns
        elseif (ntyp.eq.27) then
          nf = (nr*ns)/2
        elseif (ntyp.ge.28) then
          nf = (nr*ns)/4
        else
          nf = 2*nr*ns
        endif

c       Determine last node number to be generated

        ng = (nr+1)*(ns+1)

c     Line:

      elseif(ntyp.lt.40) then
        if(ntyp.eq.33) then
          nf = nr/ns
        else
          nf = nr
        endif

        ng = nr + 1

      endif

      end

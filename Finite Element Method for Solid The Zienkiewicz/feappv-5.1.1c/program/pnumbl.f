!$Id:$
      subroutine pnumbl(ndm,nr,ns,nt,ntyp, nf,ng, flag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

      implicit  none

      logical       :: flag
      integer       :: ndm,nr,ns,nt,ntyp,nf,ng

      save

!     Check the 2-D types

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
          if (ntyp.eq.0) then              !  4-node quadrilateral
            nf = nr*ns
          elseif (abs(ntyp).eq.7) then     !  6/7-node triangles
            nf = (nr*ns)/2
          elseif (ntyp.ge.8) then          !  8/9-node quadrilateral
            nf = (nr*ns)/4
          elseif (ntyp.lt.0) then          !  4-node crossed triangles
            nf = 4*nr*ns
          else                             !  3-node triangles
            nf = 2*nr*ns
          endif

!         Determine last node number to be generated

          nr = nr + 1
          ns = ns + 1
          if(ndm.eq.1) ns = 1
          ng = nr*ns
          if(ntyp.eq. -7) then            !  7-node triangles
            ng = ng + (nr-1)*(ns-1)/2
          elseif(ntyp .eq. -1) then       !  3-node crossed triangles
            ng = ng + (nr-1)*(ns-1)
          elseif(ntyp .eq.  8) then       !  8-node quadrilaterals
            ng = ng - ((nr-1)*(ns-1))/4
          endif
        endif

!     3-d generations

      elseif(ntyp.lt.20) then
        if(ntyp.eq.11) then               !  4-node tetrahedron
          nf = nr*ns*nt*6
          ng = (nr+1)*(ns+1)*(nt+1)
        else                              !  8-node hexahedron
          nf = nr*ns*nt
          ng = (nr+1)*(ns+1)*(nt+1)
        endif

!     Shell:

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

!       Determine last node number to be generated

        ng = (nr+1)*(ns+1)

!     Line:

      elseif(ntyp.lt.40) then
        if(ns.ge.1) then
          nf = nr/ns
        else
          nf = nr
        endif

        ng = nr + 1

      endif

      end subroutine pnumbl

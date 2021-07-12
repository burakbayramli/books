c$Id:$
      subroutine pblendm(isd,blend,ndm,nen1,prt,prth,eflag,nflag)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]

c     Purpose:  Construct interpolation using blending functions

c     Inputs:
c        isd       - Dimension for sides array
c        blend     - Dimension for blending array
c        ndm       - Spatial dimension of mesh
c        nen1      - Dimension of ix array
c        prt       - Print control
c        prth      - Print header control
c        eflag     - Element generation flag
c        nflag     - Nodal generation flag

c     Outputs stored by pointer for:
c        x(ndm,*)  - Nodal coordinates for blended patch
c        ix(nen1,*)- Element connections

c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cblend.h'
      include  'cdata.h'
      include  'iofile.h'
      include  'pointer.h'
      include  'region.h'
      include  'comblk.h'

      logical   prt,prth,eflag,nflag, setvar,palloc
      integer   n,n1,isd,blend,ndm,nen1, nn,n2
      integer   iside(4),tblend(20)

      save

      do n = 1,numbd

c       Pointer to transformtion and surface type

        nn = np(66) + blend*(n-1) - 1

c       Surface generations

        do n1 = 1,blend
          tblend(n1) = mr(nn+n1)
        end do

c       Surface generations

        if(tblend(19).eq.1) then

          if(np(64).eq.0) then
            setvar = palloc( 64,'BSIDE',2,1)
          endif
          call pblend2a(mr(np(64)),tblend,iside,isd)
          nn = np(68) + mxilr*(n-1)
          n2 = np(65) +    12*(n-1)
          call pblend2b(n,hr(np(63)),mr(np(64)),hr(n2),tblend,
     &                  mr(nn),hr(np(43)),mr(np(33)),
     &                  iside,isd,ndm,nen1,prt,prth,eflag,nflag)

c       Solid generations

        elseif(tblend(19).eq.2) then

          nn = np(68) + mxilr*(n-1)
          n2 = np(65) +    12*(n-1)
          call pblend3(n,hr(n2),tblend,mr(nn),isd,ndm,nen1,
     &                 prt,prth,eflag,nflag)

c       Line generations

        elseif(tblend(19).eq.3) then

          if(np(64).eq.0) then
            setvar = palloc(64,'BSIDE',2,1)
          endif
          call pblend1a(mr(np(64)),tblend,iside,isd)
          nn = np(68) + mxilr*(n-1)
          n2 = np(65) +    12*(n-1)
          call pblend1b(hr(np(63)),mr(np(64)),hr(n2),tblend,
     &                  mr(nn),hr(np(43)),mr(np(33)),
     &                  iside,isd,ndm,nen1,prt,prth,eflag,nflag)
        endif

      end do ! n

      end

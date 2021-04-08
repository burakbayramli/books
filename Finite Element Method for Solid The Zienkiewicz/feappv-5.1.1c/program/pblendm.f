!$Id:$
      subroutine pblendm(isd,blend,ndm,nen1,prt,prth,eflag,nflag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]

!     Purpose:  Construct interpolation using blending functions

!     Inputs:
!        isd       - Dimension for sides array
!        blend     - Dimension for blending array
!        ndm       - Spatial dimension of mesh
!        nen1      - Dimension of ix array
!        prt       - Print control
!        prth      - Print header control
!        eflag     - Element generation flag
!        nflag     - Nodal generation flag

!     Outputs stored by pointer for:
!        x(ndm,*)  - Nodal coordinates for blended patch
!        ix(nen1,*)- Element connections

!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cblend.h'
      include  'cdata.h'
      include  'iofile.h'
      include  'p_int.h'
      include  'pointer.h'
      include  'region.h'
      include  'comblk.h'

      logical       :: prt,prth,eflag,nflag, setvar,palloc
      integer       :: n,n1,isd,blend,ndm,nen1
      integer       :: iside(4),tblend(21)

      save

      do n = 1,numbd

!       Pointer to transformtion and surface type

        fp(1) = np(164) + blend*(n-1) - 1

!       Surface generations

        do n1 = 1,blend
          tblend(n1) = mr(fp(1)+n1)
        end do

        netyp = tblend(21)

!       Surface generations

        if(tblend(19).eq.1) then

          if(np(162).eq.0) then
            setvar = palloc(162,'BSIDE',2,1)
          endif
          call pblend2a(tblend,iside,isd)
          fp(1) = np(166) + mxilr*(n-1)
          fp(2) = np(163) +    12*(n-1)
          call pblend2b(n,hr(np(161)),mr(np(162)),hr(fp(2)),tblend,
     &                  mr(fp(1)),hr(np(43)),mr(np(33)),
     &                  iside,isd,ndm,nen1,prt,prth,eflag,nflag)

!       Solid generations

        elseif(tblend(19).eq.2) then

          fp(1) = np(166) + mxilr*(n-1)
          fp(2) = np(163) +    12*(n-1)
          call pblend3(n,hr(fp(2)),tblend,mr(fp(1)),isd,ndm,nen1,
     &                 prt,prth,eflag,nflag)

!       Line generations

        elseif(tblend(19).eq.3) then

          if(np(162).eq.0) then
            setvar = palloc(162,'BSIDE',2,1)
          endif
          call pblend1a(mr(np(162)),tblend,iside,isd)
          fp(1) = np(166) + mxilr*(n-1)
          fp(2) = np(163) +    12*(n-1)
          call pblend1b(hr(np(161)),mr(np(162)),hr(fp(2)),tblend,
     &                  mr(fp(1)),hr(np(43)),mr(np(33)),
     &                  iside(1),isd,ndm,nen1,prt,prth,eflag,nflag)
        endif

      end do ! n

      end subroutine pblendm

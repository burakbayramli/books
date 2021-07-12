c$Id:$
      subroutine pblendx(nn,nr,ns,ni,ntyp,ndm, fxim,fetp,fxip,fetm,
     &                   nty,x,nflag,prt,prth)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]

c     Purpose:  Construct two dimensional interpolation using blending

c     Inputs:
c        nr        - Number of divisions in  xi-direction
c        ns        - Number of divisions in eta-direction
c        ni        - Number of last node generated
c        ntyp      - Element types
c        ndm       - Spatial dimension of mesh
c        fxim(2,*) - Interpolations on  xi = -1 side
c        fxip(2,*) - Interpolations on  xi =  1 side
c        fetm(2,*) - Interpolations on eta = -1 side
c        fetp(2,*) - Interpolations on eta =  1 side
c        prt       - Output list of coordinates if true
c        prth      - Output header information  if true

c     Outputs:
c        nty(*)    - Node type
c        x(ndm,*)  - Nodal coordinates for blended patch

c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'iofile.h'
      include  'pointer.h'
      include  'comblk.h'

      logical   nflag,prt,prth
      integer   i,j,k, nr,ns,ni,ntyp, nn, ndm, ninc

      real*8    n1i,n2i, n1j,n2j, rnr,rns

      integer   nty(*)
      real*8    fxim(ndm,0:nr), fxip(ndm,0:nr)
      real*8    fetm(ndm,0:ns), fetp(ndm,0:ns)
      real*8    x(ndm,*)

      save

c     Blend interpolations to form nodal coordinates

      rnr = 1.d0/dble(nr)
      rns = 1.d0/dble(ns)

      nn  = ni - 1

      if(prt) then
        call prtitl(prth)
        write(iow,2000) (i,i=1,ndm)
      endif
      do j = 0,ns

        n2j = dble(j)*rns
        n1j = 1.d0 - n2j
        if(ntyp.eq.8 .and. mod(j,2).eq.1) then
          ninc = 2
        else
          ninc = 1
        endif

        do i = 0,nr,ninc

          nn = nn + 1

          if(nflag .or. nty(nn).ge.0) then
            n2i = dble(i)*rnr
            n1i = 1.d0 - n2i

            nty(nn) = 0
            do k = 1,ndm
              x(k,nn) = n1i*fetm(k,j)       + n2i*fetp(k,j)
     &                + n1j*fxim(k,i)       + n2j*fxip(k,i)
     &                - n1i*(n1j*fxim(k, 0) + n2j*fxip(k, 0))
     &                - n2i*(n1j*fxim(k,nr) + n2j*fxip(k,nr))
            end do ! k

            if(prt) then
              write(iow,2001) nn,(x(k,nn),k=1,ndm)
            endif

          endif

        end do

      end do

c     Formats

2000  format('   B l e n d e d   C o o r d i n a t e s'//
     &       '     Node',4(i5,'-Coordinate':))

2001  format(i8,1p,4e15.5)

      end

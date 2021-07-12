c$Id:$
      subroutine pblend1a(is,iblend,iside,isd)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]

c     Purpose:  Construct one dimensional interpolation using blending

c     Inputs:
c        is(isd,*) - Blending side supernode lists
c        iblend(*) - Blending functions parameters/sides
c        isd       - Dimension for sides array

c     Outputs:
c        iside     - Number of side to construct
c-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cblend.h'
      include   'cdata.h'
      include   'iofile.h'
      include   'pointer.h'
      include   'region.h'
      include   'trdata.h'
      include   'comblk.h'

      logical    setvar, palloc
      integer    isd,iside(*), is(isd,*),iblend(*)
      integer    i, j, i1,i2,i3,i4

      save

c     Set side number to use

      i1 = iblend(11)
      i2 = iblend(12)
      do j = 1,numsd
        i3 = is(1,j)
        if(i3.eq.2) then
          do i4 = 3,isd
            if(is(i4,j).ne.0) then
              i = i4
            endif
          end do ! i4
        else
          i = 3
        endif
        if((i1.eq.is(2,j) .and. i2.eq.is(i,j)) .or.
     &     (i1.eq.is(i,j) .and. i2.eq.is(2,j))) then
          iside(1) = j
          return
        endif
      end do ! j

c     Add new side

      numsd  = numsd + 1
      setvar = palloc( 64,'BSIDE',numsd*isd,1)
      i3     = 1
      call pblenda2(i3,i1,i2,mr(np(64)),iside,isd)

      end

      subroutine pblend1b(xs,is,trb,iblend,ilr,x,ix,
     &                    iside,isd,ndm,nen1,prt,prth,eflag,nflag)

c-----[--.----+----.----+----.-----------------------------------------]

c     Purpose:  Construct one dimensional interpolation using blending

c     Inputs:
c        xs(3,*)   - Blending supernode connections
c        is(isd,*) - Blending side supernode lists
c        trb       - Transformation for blending coordinates
c        iblend(*) - Blending functions parameters/sides
c        ilr(*)    - Material quantities for blends
c        isd       - Dimension for sides array
c        ndm       - Spatial dimension of mesh
c        nen1      - Dimension of ix array

c     Outputs:
c        x(ndm,*)  - Nodal coordinates for blended patch
c        ix(nen1,*)- Element connections
c-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cblend.h'
      include   'cdata.h'
      include   'iofile.h'
      include   'pointer.h'
      include   'region.h'
      include   'trdata.h'
      include   'comblk.h'

      logical    prt,prth,eflag,nflag, setvar, palloc
      character  ctype*4
      integer    isd,ndm,nen1,nrig, nsn, iside
      integer    i, j,jj, k, ma
      integer    ne,nf,ni,nm,nn,nr,ns,nodinc,ntyp, styp, dlayer
      integer    is(isd,*),iblend(*), ix(nen1,*), ilr(*)
      real*8     xs(3,*),trb(3,4),x(ndm,*), trdeto

      save

c     Get edge interpolations

      nr   = iblend(1)

      setvar = palloc ( 81, 'TEMP1',(nr+1)*ndm  ,2)
      setvar = palloc ( 82, 'TEMP2',(nr+1)      ,2)
      setvar = palloc ( 83, 'TEMP3',(nr+1)*3    ,2)

      nreg = iblend(10)
      nrig = iblend(20)
      jj   = abs(iside)
      styp = is(1,jj)
      do j = isd,2,-1
        if(is(j,jj).ne.0) go to 110
      end do ! j
      write(*,3000) j
      call plstop()
110   nsn = j - 1

      call pside1(nr, xs, trb, iside,is(2,jj), nsn,ndm,
     &            hr(np(82)),hr(np(83)), hr(np(81)),styp)

      ni  = iblend(3)

      call pblend1x(nn,nr,ni,ndm, hr(np(81)),mr(np(49)),x,
     &              nflag,prt,prth)

      setvar = palloc ( 83, 'TEMP3',0 ,2)
      setvar = palloc ( 82, 'TEMP2',0 ,2)
      setvar = palloc ( 81, 'TEMP1',0 ,2)

      if(eflag) then
        ne     = iblend(4)
        ma     = iblend(5)
        ntyp   = iblend(6)
        nm     = 4
        nodinc = 0
        ctype  = 'blen'
        dlayer = 0
        if(ntyp.eq.3) then
          ns   = 2
        else
          ns   = 1
        endif
        nm     = 2
        nr     = nr + 1

        trdeto = trdet
        trdet  = trb(1,1)*(trb(2,2)*trb(3,3) - trb(2,3)*trb(3,2))
     &         + trb(1,2)*(trb(2,3)*trb(3,1) - trb(2,1)*trb(3,3))
     &         + trb(1,3)*(trb(2,1)*trb(3,2) - trb(2,2)*trb(3,1))

        call sblke(nr,ns,x,ix,ni,ne,nn,ndm,nen1,nodinc,ntyp,nm,ma,
     &             dlayer,ilr,ctype)
        trdet  = trdeto
        nf     = nn
      endif

c     Set region numbers

      if(eflag) then
        do nn = ne,nf
          ix(nen1-1,nn) = nreg
        end do ! nn

c       Print lists if wanted

        if(prt.and.ne.gt.0) then
          do nn = ne,nf,50
            call prtitl(prth)
            write(iow,2000) (i,i=1,nen)
            if(ior.lt.0) then
              write(  *,2000) (i,i=1,nen)
            endif
            j = min(nf,nn+49)
            do i = nn,j
              ma = ix(nen1,i)
              write(iow,2001) i,ma,nreg,(ix(k,i),k=1,nen)
              if(ior.lt.0) then
                write(  *,2001) i,ma,nreg,(ix(k,i),k=1,nen)
              endif
            end do ! i
          end do ! nn
        endif
      endif

c     Formats

2000  format('   E l e m e n t   C o n n e c t i o n s'//
     &   '   Elmt Mat Reg',8(i3,' node'):/(15x,8(i3,' node')))

2001  format(i7,2i4,8i8:/(15x,8i8))

3000  format(' *ERROR* PBLEND1: No side nodes found for side',i4)

      end

      subroutine pblend1x(nn,nr,ni,ndm, fxim,nty,x,nflag,prt,prth)

      implicit   none

      include   'iofile.h'

      logical    nflag,prt,prth
      integer    i,k, nr,ni, nn,ndm, nty(*)
      real*8     n1i,n2i, rnr, fxim(ndm,0:nr), x(ndm,*)

      save

      nn  = ni - 1
      rnr = 1.d0/dble(nr)

      if(prt) then
        call prtitl(prth)
        write(iow,2000) (i,i=1,ndm)
      endif
      do i = 0,nr
        nn = nn + 1
        if(nflag .or. nty(nn).ge.0) then
          n2i     = dble(i)*rnr
          n1i     = 1.d0 - n2i
          nty(nn) = 0
          do k = 1,ndm
c           x(k,nn) = fxim(k,i) - n1i*fxim(k,0) - n2i*fxim(k,nr)
            x(k,nn) = fxim(k,i)
          end do ! k
          if(prt) then
            write(iow,2001) nn,(x(k,nn),k=1,ndm)
          endif
        end if
      end do ! i

c     Formats

2000  format('   B l e n d e d   C o o r d i n a t e s'//
     &       '     Node',4(i5,'-Coordinate':))

2001  format(i8,1p,4e15.5)

      end

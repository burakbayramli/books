!$Id:$
      subroutine pblend1a(is,iblend,iside,isd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]

!     Purpose:  Construct one dimensional interpolation using blending

!     Inputs:
!        is(isd,*) - Blending side supernode lists
!        iblend(*) - Blending functions parameters/sides
!        isd       - Dimension for sides array

!     Outputs:
!        iside     - Number of side to construct
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cblend.h'
      include   'cdata.h'
      include   'iofile.h'
      include   'pointer.h'
      include   'region.h'
      include   'trdata.h'
      include   'comblk.h'

      logical        :: setvar, palloc
      integer        :: isd,iside(*), is(isd,*),iblend(*)
      integer        :: i, j, i1,i2,i3,i4

      save

!     Set side number to use

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

!     Add new side

      numsd  = numsd + 1
      setvar = palloc(162,'BSIDE',numsd*isd,1)
      i3     = 1
      call pblenda2(i3,i1,i2,mr(np(162)),iside,isd)

      end subroutine pblend1a

      subroutine pblend1b(xs,is,trb,iblend,ilr,x,ix,
     &                    iside,isd,ndm,nen1,prt,prth,eflag,nflag)

!-----[--.----+----.----+----.-----------------------------------------]

!     Purpose:  Construct one dimensional interpolation using blending

!     Inputs:
!        xs(3,*)   - Blending supernode connections
!        is(isd,*) - Blending side supernode lists
!        trb       - Transformation for blending coordinates
!        iblend(*) - Blending functions parameters/sides
!        ilr(*)    - Material quantities for blends
!        isd       - Dimension for sides array
!        ndm       - Spatial dimension of mesh
!        nen1      - Dimension of ix array

!     Outputs:
!        x(ndm,*)  - Nodal coordinates for blended patch
!        ix(nen1,*)- Element connections
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cblend.h'
      include   'cdata.h'
      include   'iofile.h'
      include   'pointer.h'
      include   'region.h'
      include   'trdata.h'
      include   'comblk.h'

      logical    prt,prth,eflag,nflag, setvar, palloc
      character  ctype*15, etype*5, pelabl*5
      integer    isd,ndm,nen1,nrig, nsn, iside
      integer    i, j,jj, k, ma
      integer    ne,nf,ni,nm,nn,nr,ns,nodinc,ntyp, styp, dlayer
      integer    is(isd,*),iblend(*), ix(nen1,*), ilr(*)
      real (kind=8) ::  xs(3,*),trb(3,4),x(ndm,*), trdeto

      integer    nel

      save

!     Get edge interpolations

      nr   = iblend(1)

      setvar = palloc (111, 'TEMP1',(nr+1)*ndm  ,2)
      setvar = palloc (112, 'TEMP2',(nr+1)      ,2)
      setvar = palloc (113, 'TEMP3',(nr+1)*3    ,2)

      nreg = iblend(10)
      nrig = iblend(20)
      jj   = abs(iside)
      styp = is(1,jj)
      do j = isd,2,-1
        if(is(j,jj).ne.0) go to 110
      end do ! j
      write(*,3000) j
      call plstop(.true.)
110   nsn = j - 1

      call pside1(nr, xs, trb, iside,is(2,jj), nsn,ndm,
     &            hr(np(112)),hr(np(113)), hr(np(111)),styp)

      ni  = iblend(3)

      call pblend1x(nn,nr,ni,ndm, hr(np(111)),mr(np(190)),x,
     &              nflag,prt,prth)

      setvar = palloc (113, 'TEMP3',0 ,2)
      setvar = palloc (112, 'TEMP2',0 ,2)
      setvar = palloc (111, 'TEMP1',0 ,2)

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

!     Set region numbers

      if(eflag) then
        do nn = ne,nf
          ix(nen1-1,nn) = nreg
        end do ! nn

!       Print lists if wanted

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
              etype = pelabl(ix(nen+7,i))
              nel   = 0
              do k = 1,nen
                if(ix(k,i).gt.0) nel = k
              end do ! k
              write(iow,2001) i,ma,nreg,etype,(ix(k,i),k=1,nel)
              if(ior.lt.0) then
                write(*,2001) i,ma,nreg,etype,(ix(k,i),k=1,nel)
              endif
            end do ! i
          end do ! nn
        endif
      endif

!     Formats

2000  format('   E l e m e n t   C o n n e c t i o n s'//
     &   '   Elmt Mat Reg  Type',7(i3,'-node'):/(21x,7(i3,'-node')))

2001  format(i7,2i4,1x,a5,7i8:/(21x,7i8))

3000  format(' *ERROR* PBLEND1: No side nodes found for side',i4)

      end subroutine pblend1b

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
!           x(k,nn) = fxim(k,i) - n1i*fxim(k,0) - n2i*fxim(k,nr)
            x(k,nn) = fxim(k,i)
          end do ! k
          if(prt) then
            write(iow,2001) nn,(x(k,nn),k=1,ndm)
          endif
        end if
      end do ! i

!     Formats

2000  format('   B l e n d e d   C o o r d i n a t e s'//
     &       '     Node',4(i5,'-Coordinate':))

2001  format(i8,1p,4e15.5)

      end subroutine pblend1x

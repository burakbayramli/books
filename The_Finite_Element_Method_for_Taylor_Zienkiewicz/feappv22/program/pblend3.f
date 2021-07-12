c$Id:$
      subroutine pblend3(n,tb,iblend,ilr,isd,ndm,nen1,
     &                   prt,prth,eflag,nflag)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]

c     Purpose:  Construct two dimensional interpolation using blending

c     Inputs:
c        n         - Block number
c        tb(3,4)   - Transformation array
c        iblend(*) - Blending functions parameters/sides
c        ilr(*)    - Blending material numbers
c        isd       - Dimension for sides array
c        ndm       - Spatial dimension of mesh
c        nen1      - Dimension of ix array

c     Outputs through pointers to:
c        x(ndm,*)  - Nodal coordinates for blended patch
c        ix(nen1,*)- Element connections

c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'iofile.h'
      include  'pointer.h'
      include  'region.h'
      include  'trdata.h'
      include  'comblk.h'

      logical   prt,prth,eflag,nflag, setvar, palloc
      integer   i,ii,in, j, n,ne,ni,nf,ntyp
      integer   isd,ndm,nr,ns,nt,nen1,ma,dlayer
      real*8    trdeto

      integer   ic(6),ilr(*),nty
      integer   iblend(*)
      integer   lblend(20,6),mblend(20),iside(4)

      real*8    t3(3,4),tb(3,4)

      save

c     Save current transformation determinant

      trdeto  = trdet
      do i = 1,4
        do j = 1,3
          t3(j,i) = tb(j,i)
        end do ! j
      end do ! i

c     Save iblend in case pointer changes

      do in = 1,20
        mblend(in) = iblend(in)
        do i = 1,6
          lblend(in,i) = 0
        end do ! i
      end do ! in

      do in = 9,1,-1
        if(mblend(in+10).ne.0) go to 100
      end do ! in

      write(iow,*) ' ERROR - Incorrect number super nodes  specified'
      call plstop()

c     Determine the sides for the faces

100   call mkface(mblend,lblend)

c     Compute coordinates for each face

      ii = min(mblend(1),mblend(2),mblend(3))
      in = max(mblend(1),mblend(2),mblend(3))
      ii = mblend(1) + mblend(2) + mblend(3) - ii - in

      in = (ii+1)*(in+1)*3
      if(np(64).eq.0) then
        setvar = palloc(64,'BSIDE', 2, 1)
      endif
      setvar = palloc(87,'TEMP7', 6*in, 2)

      do i = 1,6
        call pblend2a(mr(np(64)),lblend(1,i),iside,isd)
        ic(i) = np(87) + in*(i-1)
        call pblend2b(n,hr(np(63)),mr(np(64)),t3,lblend(1,i),
     &                ilr,hr(ic(i)),mr(np(33)),
     &                iside,isd,3,nen1,.false.,.false.,.false.,nflag)
      end do ! i

c     Correct pointer if pblend2 added sides

      do i = 1,6
        ic(i) =np(87) + in*(i-1)
      end do

c     Form coordinates for the mesh

      call pblend3x(hr(ic(1)),lblend(1,1),lblend(2,1),
     &              hr(ic(2)),lblend(1,2),lblend(2,2),
     &              hr(ic(3)),lblend(1,3),lblend(2,3),
     &              hr(ic(4)),lblend(1,4),lblend(2,4),
     &              hr(ic(5)),lblend(1,5),lblend(2,5),
     &              hr(ic(6)),lblend(1,6),lblend(2,6),
     &              mr(np(49)),hr(np(43)),ndm,mblend,nf)
      setvar = palloc(87,'TEMP7',    0, 2)

c     Output coordinates in blended block

      if(prt) then
        call prtitl(prth)
        write(iow,2001) (i,i=1,ndm)
        do i = mblend(4),nf
          nty = np(43) + ndm*(i-1) -1
          write(iow,2002) i,(hr(nty+j),j=1,ndm)
        end do ! i
      endif

      if(eflag) then

c       Compute current transformation determinant

        trdet   = t3(1,1)*(t3(2,2)*t3(3,3) - t3(2,3)*t3(3,2))
     &          + t3(1,2)*(t3(2,3)*t3(3,1) - t3(2,1)*t3(3,3))
     &          + t3(1,3)*(t3(2,1)*t3(3,2) - t3(2,2)*t3(3,1))

        nr     = mblend(3) + 1
        ns     = mblend(1) + 1
        nt     = mblend(2) + 1
        ni     = mblend(4)
        ne     = mblend(5)
        ma     = mblend(6)
        ntyp   = mblend(7)
        nreg   = mblend(10)

        if(ma.lt.0) then
          dlayer = -ma
        else
          dlayer =  0
        end if
        call vblke(nr,ns,nt,mr(np(33)),ni,ne,nf,nen1,ma,ntyp,dlayer,ilr)

c       Set region number

        call psregn(mr(np(33)),nen,nen1,ne,nf,nreg,prt,prth)

      endif

c     Restore current transformation determinant

      trdet  = trdeto

c     Formats

2001  format('   B l e n d e d   C o o r d i n a t e s'//
     &       '     Node',4(i5,'-Coordinate':))

2002  format(i8,1p,4e15.5)

      end

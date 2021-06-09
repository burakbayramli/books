!$Id:$
      subroutine pblend3(n,tb,iblend,ilr,isd,ndm,nen1,
     &                   prt,prth,eflag,nflag)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]

!     Purpose:  Construct two dimensional interpolation using blending

!     Inputs:
!        n         - Block number
!        tb(3,4)   - Transformation array
!        iblend(*) - Blending functions parameters/sides
!        ilr(*)    - Blending material numbers
!        isd       - Dimension for sides array
!        ndm       - Spatial dimension of mesh
!        nen1      - Dimension of ix array

!     Outputs through pointers to:
!        x(ndm,*)  - Nodal coordinates for blended patch
!        ix(nen1,*)- Element connections

!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'cdata.h'
      include  'iofile.h'
      include  'p_int.h'
      include  'p_point.h'
      include  'pointer.h'
      include  'region.h'
      include  'trdata.h'
      include  'comblk.h'

      logical       :: prt,prth,eflag,nflag, setvar, palloc
      integer       :: i,ii,in, j, n,ne,ni,nf,ntyp
      integer       :: isd,ndm,nr,ns,nt,nen1,ma,dlayer
      real (kind=8) :: trdeto

      integer       :: ilr(*)
      integer       :: iblend(*)
      integer       :: lblend(20,6),mblend(20),iside(4)

      real (kind=8) :: t3(3,4),tb(3,4)

      save

!     Save current transformation determinant

      trdeto  = trdet
      do i = 1,4
        do j = 1,3
          t3(j,i) = tb(j,i)
        end do ! j
      end do ! i

!     Save iblend in case pointer changes

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
      call plstop(.true.)

!     Determine the sides for the faces

100   call mkface(mblend,lblend)

!     Compute coordinates for each face

      ii = min(mblend(1),mblend(2),mblend(3))
      in = max(mblend(1),mblend(2),mblend(3))
      ii = mblend(1) + mblend(2) + mblend(3) - ii - in

      in = (ii+1)*(in+1)*3
      if(np(162).eq.0) then
        setvar = palloc(162,'BSIDE', 2, 1)
      endif
      setvar = palloc(117,'TEMP7', 6*in, 2)

      do i = 1,6
        call pblend2a(lblend(1,i),iside,isd)
        fp(i) = np(117) + in*(i-1)
        call pblend2b(n,hr(np(161)),mr(np(162)),t3,lblend(1,i),
     &                ilr,hr(fp(i)),mr(np(33)),
     &                iside,isd,3,nen1,.false.,.false.,.false.,nflag)
      end do ! i

!     Correct pointer if pblend2 added sides

      do i = 1,6
        fp(i) =np(117) + in*(i-1)
      end do

!     Form coordinates for the mesh

      call pblend3x(hr(fp(1)),lblend(1,1),lblend(2,1),
     &              hr(fp(2)),lblend(1,2),lblend(2,2),
     &              hr(fp(3)),lblend(1,3),lblend(2,3),
     &              hr(fp(4)),lblend(1,4),lblend(2,4),
     &              hr(fp(5)),lblend(1,5),lblend(2,5),
     &              hr(fp(6)),lblend(1,6),lblend(2,6),
     &              mr(np(190)),hr(np(43)),ndm,mblend,nf)
      setvar = palloc(117,'TEMP7',    0, 2)

!     Output coordinates in blended block

      if(prt) then
        call prtitl(prth)
        write(iow,2001) (i,i=1,ndm)
        do i = mblend(4),nf
          point = np(43) + ndm*(i-1) -1
          write(iow,2002) i,(hr(point+j),j=1,ndm)
        end do ! i
      endif

      if(eflag) then

!       Compute current transformation determinant

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

!       Set region number

        call psregn(mr(np(33)),nen,nen1,ne,nf,nreg,prt,prth)

      endif

!     Restore current transformation determinant

      trdet  = trdeto

!     Formats

2001  format('   B l e n d e d   C o o r d i n a t e s'//
     &       '     Node',4(i5,'-Coordinate':))

2002  format(i8,1p,4e15.5)

      end subroutine pblend3

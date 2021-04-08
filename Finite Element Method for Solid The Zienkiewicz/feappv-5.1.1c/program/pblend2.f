!$Id:$
      subroutine pblend2a(iblend,iside,isd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]

!     Purpose:  Construct two dimensional interpolation using blending

!     Inputs:
!        iblend(*) - Blending functions parameters/sides
!        isd       - Dimension for sides array

!     Outputs:
!        iside(4)  - Side connection numbers for face
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cblend.h'
      include  'pointer.h'
      include  'comblk.h'

      logical       :: setvar,palloc, flsd
      integer       :: isd,i,i1,i2

      integer       :: iblend(*),iside(*), lblend(4)

      save

!     Save values of vertex nodes in case pointer to iblend changes

      do i = 1,4
        lblend(i) = iblend(10+i)
      end do

!     Check each side

      do i = 1,4
        i1 = lblend(i)
        i2 = lblend(mod(i,4)+1)
        call pblenda1(i,i1,i2,mr(np(162)),iside,isd, flsd)

        if(flsd) then
          numsd = numsd + 1
          setvar = palloc(162,'BSIDE',numsd*isd,1)
          call pblenda2(i,i1,i2,mr(np(162)),iside,isd)
        endif
      end do ! i

      end subroutine pblend2a

      subroutine pblenda1(i,i1,i2,is,iside,isd,flsd)

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose:  Find side number for 2-d blend.

!     Inputs:
!        is(isd,*) - Blending side supernode lists
!        iblend(*) - Blending functions parameters/sides
!        isd       - Dimension for sides array

!     Outputs:
!        iside(4)  - Side connection numbers for face
!        flsd      - false if no new side needed
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cblend.h'

      logical       :: flsd
      integer       :: i,i1,i2,i3,i4, j,k, isd
      integer       :: is(isd,*),iside(*)

      save

!     Check each side

      do j = 1,numsd
        i3 = is(1,j)
        if(i3.eq.0 .or. i3.eq.1 .or.i3.eq.3) then
          k = 3
        elseif(i3.eq.2) then
          do i4 = 3,isd
            if(is(i4,j).ne.0) then
              k = i4
            endif
          end do ! i4
        endif
        if((i1.eq.is(2,j) .and. i2.eq.is(k,j)) .or.
     &     (i1.eq.is(k,j) .and. i2.eq.is(2,j))) then
           iside(i) = j
           flsd     = .false.
           return
        endif
      end do ! j
      flsd = .true.

      end subroutine pblenda1

      subroutine pblenda2(i,i1,i2,is,iside,isd)

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose:  Set new side number for 2-d blend.

!     Inputs:
!        is(isd,*) - Blending side supernode lists
!        iblend(*) - Blending functions parameters/sides
!        isd       - Dimension for sides array

!     Outputs:
!        iside(4)  - Side connection numbers for face
!        flsd      - false if no new side needed
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cblend.h'

      integer       :: i,i1,i2,i3, isd
      integer       :: is(isd,*),iside(*)

      save

      do i3 = 1,isd
        is(i3,numsd) = 0
      end do ! i3
      is(2,numsd) = i1
      is(3,numsd) = i2
      iside(i)    = numsd

      end subroutine pblenda2

      subroutine pblend2b(n,xs,is,trb,iblend,ilr,x,ix,
     &                    iside,isd,ndm,nen1,prt,prth,eflag,nflag)

!-----[--.----+----.----+----.-----------------------------------------]

!     Purpose:  Construct two dimensional interpolation using blending

!     Inputs:
!        n         - Block number
!        xs(3,*)   - Blending supernode connections
!        is(isd,*) - Blending side supernode lists
!        trb       - Transformation for blending coordinates
!        iblend(*) - Blending functions parameters/sides
!        ilr(*)    - Material quantities for blends
!        iside(*)  - Side connections for face
!        isd       - Dimension for sides array
!        ndm       - Spatial dimension of mesh
!        nen1      - Dimension of ix array

!     Outputs:
!        x(ndm,*)  - Nodal coordinates for blended patch
!        ix(nen1,*)- Element connections

!-----[--.----+----.----+----.-----------------------------------------]

      implicit   none

      include   'cdata.h'
      include   'iofile.h'
      include   'pointer.h'
      include   'region.h'
      include   'trdata.h'
      include   'comblk.h'

      character (len=15) :: ctype
      character (len=5)  :: etype, pelabl

      logical       :: prt,prth,eflag,nflag, setvar, palloc
      integer       :: i,ii,in, j,jj, k, ma,m1,m2
      integer       :: n,ne,nf,ni,nm,nn,nr,ns,nodinc,ntyp, styp, dlayer
      real (kind=8) ::  trdeto

      integer       :: isd,ndm,nen1
      integer       :: is(isd,*),iblend(*), ix(nen1,*), ilr(*)

      real (kind=8) :: xs(3,*),trb(3,4),x(ndm,*)

      integer       :: nsn(4), iside(4), nel

      save

!     Set up face

      do j = 1,4
        iblend(j+10) = iside(j)
      end do

!     Set for 4 sides

      in = 4

!     Check signs on sides for blend

      call mkside(n,iblend(11),is,isd)

!     Check for matching end nodes for blending functions

      do i = 1,in
        ii = iblend(i+10)
        if(ii.gt.0) then
          if(is(1,abs(ii)).eq.2) then
            do k = 3,isd
              if(is(k,abs(ii)).ne.0) m1 = k
            end do
          else
            m1 = 3
          endif
        elseif(ii.lt.0) then
          m1 = 2
        else
          write(*,*) ' ERROR - Zero side number specified'
        endif

        j  = mod(i,in) + 1
        jj = iblend(j+10)
        if(jj.gt.0) then
          m2 = 2
        elseif(jj.lt.0) then
          if(is(1,abs(jj)).eq.2) then
            do k = 3,isd
              if(is(k,abs(jj)).ne.0) m2 = k
            end do
          else
            m2 = 3
          endif
        else
          write(*,*) ' ERROR - Zero side number specified'
        endif

        if(is(m1,abs(ii)).ne.is(m2,abs(jj))) then
          if(ior.lt.0) write(*,2000) n,i,j,ii,jj
          write(iow,2000) n,i,j,ii,jj
        endif

      end do ! i

!     Quadrilateral region blends

      if(in.eq.4) then

!       Determine the number of nodes for each side
        do i = 1,in
          ii = abs(iblend(i+10))
          do j = isd,2,-1
            if( is(j,ii).ne.0 ) go to 110
          end do ! j
          write(*,*) ' ERROR - No side nodes found for side',i
          call plstop(.true.)
110       nsn(i) = j-1
        end do ! i

!       Get edge interpolations

        nr   = iblend(1)
        ns   = iblend(2)
        ntyp = iblend(6)

        setvar = palloc (111, 'TEMP1',(nr+1)*ndm  ,2)
        setvar = palloc (112, 'TEMP2',(ns+1)*ndm  ,2)
        setvar = palloc (113, 'TEMP3',(nr+1)*ndm  ,2)
        setvar = palloc (114, 'TEMP4',(ns+1)*ndm  ,2)
        setvar = palloc (115, 'TEMP5',max(nr,ns)+1,2)
        setvar = palloc (116, 'TEMP6',(nr+1)*3    ,2)

        nreg = iblend(10)
        ii   = iblend(11)
        jj   = abs(ii)
        styp = is(1,jj)

        call pside1(nr, xs, trb, ii,is(2,jj), nsn(1),ndm,
     &              hr(np(115)),hr(np(116)), hr(np(111)),styp)

        ii   = iblend(12)
        jj   = abs(ii)
        styp = is(1,jj)

        call pside1(ns, xs, trb, ii,is(2,jj), nsn(2),ndm,
     &              hr(np(115)),hr(np(116)), hr(np(112)),styp)

        ii   =-iblend(13)
        jj   = abs(ii)
        styp = is(1,jj)

        call pside1(nr, xs, trb, ii,is(2,jj), nsn(3),ndm,
     &              hr(np(115)),hr(np(116)), hr(np(113)),styp)

        ii   =-iblend(14)
        jj   = abs(ii)
        styp = is(1,jj)
        call pside1(ns, xs, trb, ii,is(2,jj), nsn(4),ndm,
     &              hr(np(115)),hr(np(116)), hr(np(114)),styp)

        ni = iblend(3)
        call pblendx(nn,nr,ns,ni,ntyp,ndm, hr(np(111)),hr(np(112)),
     &               hr(np(113)),hr(np(114)),mr(np(190)),x,
     &               nflag,prt,prth)

        setvar = palloc (116, 'TEMP6',0 ,2)
        setvar = palloc (115, 'TEMP5',0 ,2)
        setvar = palloc (114, 'TEMP4',0 ,2)
        setvar = palloc (113, 'TEMP3',0 ,2)
        setvar = palloc (112, 'TEMP2',0 ,2)
        setvar = palloc (111, 'TEMP1',0 ,2)

        if(eflag) then
          ne     = iblend(4)
          ma     = iblend(5)
          nm     = 4
          nodinc = 0
          ctype  = 'blen'

          nr     = nr + 1
          ns     = ns + 1

          trdeto = trdet
          trdet  = trb(1,1)*(trb(2,2)*trb(3,3) - trb(2,3)*trb(3,2))
     &           + trb(1,2)*(trb(2,3)*trb(3,1) - trb(2,1)*trb(3,3))
     &           + trb(1,3)*(trb(2,1)*trb(3,2) - trb(2,2)*trb(3,1))

          if(ma.lt.0) then
            dlayer = -ma
          else
            dlayer =  0
          endif

          call sblke(nr,ns,x,ix,ni,ne,nn,ndm,nen1,nodinc,ntyp,nm,ma,
     &               dlayer,ilr,ctype)

          trdet  = trdeto

          nf     = nn
        endif

      endif

!     Set region numbers

      if(eflag) then
        do nn = ne,nf
          ix(nen1-1,nn) = nreg
        enddo

!       Print lists if wanted

        if(prt.and.ne.gt.0) then
          do nn = ne,nf,50
            call prtitl(prth)
            write(iow,2003) (i,i=1,nen)
            if(ior.lt.0) then
              write(  *,2003) (i,i=1,nen)
            endif
            j = min(nf,nn+49)
            do i = nn,j
              ma = ix(nen1,i)
              etype = pelabl(ix(nen+7,i))
              nel   = 0
              do k = 1,nen
                if(ix(k,i).gt.0) nel = k
              end do ! k
              write(iow,2004) i,ma,nreg,etype,(ix(k,i),k=1,nel)
              if(ior.lt.0) then
                write(*,2004) i,ma,nreg,etype,(ix(k,i),k=1,nel)
              endif
            end do ! i
          end do ! nn
        endif
      endif

!     Formats

2000  format(' ERROR - Blending function',i3/
     &       '         End node 2 for side',i2,' not same as'/
     &       '         End node 1 for side',i2/
     &       '         Node-2 =',i8,' Node-1 =',i8)

2003  format('   E l e m e n t   C o n n e c t i o n s'//
     &   '   Elmt Mat Reg  Type',7(i3,' node'):/(21x,7(i3,' node')))

2004  format(i7,2i4,1x,a5,7i8:/(21x,7i8))

      end subroutine pblend2b

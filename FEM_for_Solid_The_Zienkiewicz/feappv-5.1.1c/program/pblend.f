!$Id:$
      subroutine pblend(trb,iblend,ilr,numbl,numb,ndm,prt,prth)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Generate nodes and elements for 2 & 3-d problems

!      Inputs:

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cblend.h'
      include  'cdata.h'
      include  'cblktr.h'
      include  'iofile.h'
      include  'region.h'
      include  'trdata.h'

      character (len=15) :: ctype

      logical       :: flag, prt,prth, errck, tinput, pcomp, pblktyp
      logical       :: eltype

      integer       :: numbl,numb,ndm
      integer       :: i,j,nr,ns,nt,ni,ne,ma,mab,ntyp,nf,ng
      integer       :: btyp,dlayer,nlay
      integer       :: iblend(numb,*),ilr(mxilr,*)
      real (kind=8) :: tc(1),td(15),trb(3,4,*)

      save

!     Blend functions

      if(ior.lt.0) write(*,3000)

      errck = tinput(ctype,1,td,9)

      if(pcomp(ctype,'surf',4)) then
        btyp             = 1
      elseif(pcomp(ctype,'soli',4)) then
        btyp             = 2
      elseif(pcomp(ctype,'line',4)) then
        btyp             = 3
      else
        write(iow,4000) ctype
      endif

!     Save blend type for block

      iblend(19,numbl) = btyp
      iblend(21,numbl) = 0         ! Default undefined

      do i = 1,9
        iblend(i,numbl) = nint(td(i))
      end do ! i
      iblend(10,numbl) = nreg

!     Set block vertex nodes

      ntyp   = iblend(6,numbl)    ! Element type
      mab    = 0                  ! Material number
      errck  = tinput (ctype,1,td,7)
      eltype = .false.
      netyp  = 0
      do while (.not.eltype)
        eltype = pblktyp(ctype,td, ntyp,iblend(2,numbl),mab)
        if(btyp.eq.2 .and. ntyp.ne.iblend(7,numbl)) then
          iblend(7,numbl) = ntyp
        elseif(btyp.ne.2.and. ntyp.ne.iblend(6,numbl)) then
          iblend(6,numbl) = ntyp
        endif
        iblend(21,numbl) = netyp
        if(.not.eltype) then
          errck = tinput (ctype,1,td,7)
        else
          iblend(21,numbl) = netyp
        endif
      end do ! while

!     Record was vertex nodes of blend block

      call setval(ctype,15,tc(1))
      iblend(11,numbl) = nint(tc(1))
      do i = 2,8
        iblend(i+10,numbl) = nint(td(i-1))
      end do ! i

!     Determine last element number to be generated

      nr = iblend(1,numbl)
      ns = max(1,iblend(2,numbl))

      if(btyp.eq.1) then
        ni   = iblend(3,numbl)
        ne   = iblend(4,numbl)
        ma   = iblend(5,numbl)
        ntyp = iblend(6,numbl)
        flag = iblend(12,numbl).eq.0
        if(ma.lt.0) then
          dlayer = -ma
          if(dlayer.eq.1) then
            nlay = iblend(1,numbl)
          elseif(dlayer.eq.2) then
            nlay = iblend(2,numbl)
          endif
        else
          dlayer = 0
        endif
      elseif(btyp.eq.2) then
        nt   = max(1,iblend(3,numbl))
        ni   = iblend(4,numbl)
        ne   = iblend(5,numbl)
        ma   = iblend(6,numbl)
        ntyp = max(10,iblend(7,numbl))
        flag = .false.
        if(ma.lt.0) then
          dlayer = -ma
          if(dlayer.eq.1) then
            nlay = iblend(1,numbl)
          elseif(dlayer.eq.2) then
            nlay = iblend(2,numbl)
          elseif(dlayer.eq.3) then
            nlay = nt
          endif
        else
          dlayer = 0
        end if ! ma < 0
      elseif(btyp.eq.3) then
        ni   = iblend(3,numbl)
        ne   = iblend(4,numbl)
        ma   = iblend(5,numbl)
        ntyp = iblend(6,numbl) + 30
        flag = iblend(12,numbl).eq.0
        dlayer = 0
      endif

!     Set final material number

      if(mab.gt.0) then
        ma = mab
      endif

!     Reset to default values if necessary

      if(ni.eq.0) ni = nio + 1
      if(ne.eq.0) ne = neo + 1
      if(ma.eq.0) ma = mao

!     Input layer material properties

      if(dlayer.gt.0) then
110     errck = tinput(ctype,1,td,15)
        if(errck) go to 110

        do i = 1,min(15,nlay)
          ilr(i,numbl) = nint(td(i))
        end do ! i
        j = min(16,nlay+1)
        do while(j.le.nlay)
120       errck = tinput(ctype,0,td,15)
          if(errck) go to 120
          do i = j,min(j+15,nlay)
            ilr(i,numbl) = nint(td(i-j+1))
          end do ! i
          j = j + 16
        end do ! while
        if(prt) then
          write(iow,2005) (j,j=1,min(5,nlay))
          write(iow,2006) (j,ilr(j,numbl),j=1,nlay)
        end if ! prt
      else
        ma     = max(ma,1)
      endif

      nr     = max(nr,1)
      ns     = max(ns,1)
      nt     = max(nt,1)
      ni     = max(ni,1)

      call pnumbl(ndm,nr,ns,nt,ntyp, nf,ng, flag)

!     Set correct values into the iblend array

      if(btyp.eq.1 .or. btyp.eq.3) then
        iblend(3,numbl) = ni
        iblend(4,numbl) = ne
        iblend(5,numbl) = ma
        iblend(6,numbl) = ntyp
      elseif(btyp.eq.2) then
        iblend(4,numbl) = ni
        iblend(5,numbl) = ne
        iblend(6,numbl) = ma
        iblend(7,numbl) = ntyp
      endif

!     Save transformation data

      do j = 1,3
        do i = 1,3
          trb(i,j,numbl) = tr(i,j)
        end do ! i
        trb(j,4,numbl) = xr(j)
      end do ! j

!     Set old numbers

      if(ne.gt.0) then
        neo = neo + nf
      endif
      nio = nio + ng
      mao = ma

!     Output values

      if(prt) then
        call prtitl(prth)
        if(btyp.eq.1) then
          write(iow,2001) numbl,(iblend(i,numbl),i=1,6),
     &                    (iblend(i,numbl),i=11,14)
          if(ior.lt.0) then
            write(*,2001) numbl,(iblend(i,numbl),i=1,6),
     &                    (iblend(i,numbl),i=11,14)
          endif
        elseif(btyp.eq.2) then
          write(iow,2002) numbl,(iblend(i,numbl),i=1,7),
     &                    (iblend(i,numbl),i=11,18)
          if(ior.lt.0) then
            write(*,2002) numbl,(iblend(i,numbl),i=1,7),
     &                    (iblend(i,numbl),i=11,18)
          endif
        elseif(btyp.eq.3) then
          write(iow,2003) numbl,iblend(1,numbl),
     &                    (iblend(i,numbl),i=3,6),
     &                    (iblend(i,numbl),i=11,12)
          if(ior.lt.0) then
            write(*,2003) numbl,iblend(1,numbl),
     &                    (iblend(i,numbl),i=3,6),
     &                    (iblend(i,numbl),i=11,12)
          endif
        endif
      endif

2001  format('    B l e n d i n g   F u n c t i o n   N u m b e r',i3//
     &       10x,'Number of 1-increments =',i5/
     &       10x,'Number of 2-increments =',i5/
     &       10x,'First node number      =',i5/
     &       10x,'First element number   =',i5/
     &       10x,'Material set number    =',i5/
     &       10x,'Block type             =',i5//
     &       10x,'1-SNode 2-SNode 3-SNode 4-SNode'/8x,4i8)

2002  format('    B l e n d i n g   F u n c t i o n   N u m b e r',i3//
     &       10x,'Number of 1-increments =',i5/
     &       10x,'Number of 2-increments =',i5/
     &       10x,'Number of 3-increments =',i5/
     &       10x,'First node number      =',i5/
     &       10x,'First element number   =',i5/
     &       10x,'Material set number    =',i5/
     &       10x,'Block type             =',i5//
     &       10x,'1-SNode 2-SNode 3-SNode 4-SNode 5-SNode 6-SNode ',
     &           '7-SNode 8-SNode'/8x,8i8)

2003  format('    B l e n d i n g   F u n c t i o n   N u m b e r',i3//
     &       10x,'Number of 1-increments =',i5/
     &       10x,'First node number      =',i5/
     &       10x,'First element number   =',i5/
     &       10x,'Material set number    =',i5/
     &       10x,'Block type             =',i5//
     &       10x,'1-SNode 2-SNode'/8x,2i8)

2005  format(/5x,'Layered Material Properties'/
     &      /(7x,4(i2,'-Layer Matl')))

2006  format(7x,4(i8,i5))

3000  format('  Input: Blending function parameters')

4000  format(' *ERROR* PBLEND: Type: ',a,' not available')

      end subroutine pblend

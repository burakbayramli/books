!$Id:$
      subroutine poutblk1d(blk,uu,rr,
     &                     knots,nsides,lknot,lside,nblk,ktnum,nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Block outputs for knot insertion in 1 d blocks

!      Use:   INSErt block u_knot num_times

!      Data structure:
!         Knots:
!            lknot(1,k) - Length of knot vector 'k'
!            lknot(2,k) - Order  of knot vector 'k'
!            lknot(3,k) - Number control points for knot 'k'
!            knots(j,i) - Knot values for vector 'k'

!         Sides:
!            lside(1,s) - Length of side 'k'
!            lside(2,s) - Knot vector for side 'k'
!            nsides(j,s) - Control point numbers for side 'k'

!         Blocks:
!            nblk(1,b)   - Dimension of block (1 or 2)
!            nblk(4,b)   - Number of 1-direction sides
!            nblksd(j,b) - 1-direction side numbers
!            nblksd(e,b) - 2-direction side number (e = lblksd(b) + 1)

!      Inputs:
!         blk       - Block number
!         uu        - New knot value
!         rr        - Nuber of insertions of uu

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cnurb.h'
      include   'igdata.h'
      include   'iodata.h'
      include   'iofile.h'
      include   'nblend.h'
      include   'p_ptname.h'
      include   'sdata.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    setvar,palloc

      integer    blk, bdm, rr
      integer    sid1,kno1,len1,lek1,ord1
      integer    l, leq,leu, cnurnp
      real*8     uu
      real*8     knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*),nblk(14,*)
      integer    ktnum(6,*),nblksd(dblokig,*)

      save

!     Set block dimension

      bdm  = nblk(1,blk)          ! Block dimension

!     Set side 1

      sid1 = nblksd(1,blk)
      len1 = lside(1,sid1)        ! Length of side 1
      kno1 = lside(2,sid1)        ! Knot vector for side 1
      lek1 = lknot(1,kno1)        ! Length of knot vector for side 1
      ord1 = lknot(2,kno1)        ! Order  of knot vector for side 1

!     Save last control point value

      cnurnp = nnurnp

!     Place side-1 nodes in list

      do l = 1,len1
!       ns2(l) = nsides(l,nblksd(1,blk))
        ns2(l) = nsides(l,sid1)
      end do ! l
      call pcurvin(len1,ord1,lek1, uu,rr, ns2,knots(1,kno1),
     &             hr(np(43)),hr(np(263)), leq,leu)

!     Output knot vectors

      write(ios,'(/a)') 'KNOTS'
      write(ios,2002) kno1,leu,(UU2(l),l=1,leu)
      ktnum(1,blk) = kno1

!     Allocate storage to save new coordinates and weights

      setvar = palloc(111, 'TEMP1', ndm*leq, 2) ! Coords
      setvar = palloc(112, 'TEMP2',     leq, 2) ! Wts

      call psetnurb(hr(np(111)),hr(np(112)), QQ2, 0,leq,1)

!     Output NURB coordinates and weights

      l = 1
      call poutnurb(hr(np(111)),hr(np(112)), leq,l)

!     Increment side number and output side vector

      if(blockfl) then
        nnside = nnside + 1
        write(ios,'(/a)') 'NSIDEs'
        write(ios,2001) nnside,leq,kno1,(cnurnp+l,l=1,leq)
      endif

!     Output Region description

      if(nblk(3,blk).gt.0) then
        write(ios,'(/a,i5)') 'REGIon ',nblk(3,blk)
      endif

!     Output Block description

      if(blockfl) then

        write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
        write(ios,2003) bdm,nblk(2,blk),nnside

!     Output Patch description

      else

        write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
        write(ios,2004) nblk(2,blk),leq,kno1
        write(ios,2005) (cnurnp+l,l=1,leq)

      endif

      setvar = palloc(111, 'TEMP1', 0, 2)
      setvar = palloc(112, 'TEMP2', 0, 2)

      write(ios,'(a)') ' '

!     Formats

2001  format('  side',3i4,12i6:/(16i6:))
2002  format('  open',2i4,1p,13e16.8:/(1p,16e16.8:))
2003  format('  block',3i6)
2004  format('  line',3i6)
2005  format(16i6)

      end

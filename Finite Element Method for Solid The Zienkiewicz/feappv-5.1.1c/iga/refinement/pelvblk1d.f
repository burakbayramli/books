!$Id:$
      subroutine pelvblk1d(ctl,knots,nsides,lknot,lside,nblk,ktnum,
     &                     nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Curve: Elevation of order for 1-d block

!      Use:      ELEVate block num_blk dir inc_order

!      Data structure:
!         Knots:
!            lknot(1,k)  - Length of knot vector 'k'
!            lknot(2,k)  - Order  of knot vector 'k'
!            lknot(3,k)  - Number control points for knot 'k'
!            knots(j,i)  - Knot values for vector 'k'

!         Sides:
!            lside(1,s)  - Length of side 'k'
!            lside(2,s)  - Knot vector for side 'k'
!            nsides(j,s) - Control point numbers for side 'k'

!         Blocks:
!            nblkdm(b)   - Dimension of block (1 or 2)
!            lblksd(b)   - Number of 1-direction sides
!            nblksd(j,b) - 1-direction side numbers
!            nblksd(e,b) - 2-direction side number (e = lblksd(b) + 1)

!      Inputs:
!         lct       - Command character parameters
!         ctl(3)    - Command numerical parameters

!      Outputs:
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cnurb.h'
      include   'iodata.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'nblend.h'
      include   'p_ptname.h'
      include   'sdata.h'
      include   'umac1.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    setvar,palloc
      real*8     ctl(*),knots(dknotig,*)
      integer    nsides(dsideig,*), lknot(0:4,*), lside(2,*), nblk(14,*)
      integer    ktnum(6,*),nblksd(dblokig,*)

      integer    blk, bdim
      integer    el1,le1, l, sid1
      integer    les,lek,kno,ord, leq,leu, cnurnp

      save

      blk    = nint(ctl(1))        ! Block number to refine
      bdim   = nblk(1,blk)         ! Dimension of block - nblkdm(blk)

      cnurnp = nnurnp              ! Start of control point numbers

      if(bdim.eq.1) then
!       el1 = nint(ctl(2))         ! Elevation order
        el1 = nint(ctl(3))         ! Elevation order
        le1 = nblk(4,blk)          ! Length of block -lblksd(blk)
        write(iow,2000) blk,el1
        if(ior.lt.0) then
          write(*,2000) blk,el1
        endif
      endif

!     Elevate 1 direction knots
      sid1 = nblksd(1,blk)
      les  = lside(1,sid1)     ! Length of side 1
      kno  = lside(2,sid1)     ! Knot vector for side 1 - kside(sid1)
      lek  = lknot(1,kno)      ! Length of knot vector for side 1
      ord  = lknot(2,kno)      ! Order of knot vector for side 1

!     Place side-1 nodes in list in order inserted above
      do l = 1,le1
        ns2(l) = nsides(l,sid1)
      end do ! l

      call pcurvel(les,ord,lek, el1, ns2,knots(1,kno),
     &             hr(np(43)),hr(np(263)), leq,leu)

!     Write knot vectors
      write(ios,'(/a)') 'KNOTs'
      write(ios,2002) kno,leu,(UU2(l),l=1,leu)
      ktnum(1,blk) = kno

!     Allocate storage to save new coordinates and weights
      setvar = palloc(111, 'TEMP1', ndm*leq, 2) ! Coords
      setvar = palloc(112, 'TEMP2',     leq, 2) ! Wts

      call psetnurb(hr(np(111)),hr(np(112)), QQ2, 0,leq,1)

!     Output NURB coordinates and weights
      call poutnurb(hr(np(111)),hr(np(112)), leq,  1)

      setvar = palloc(111, 'TEMP1', 0, 2)
      setvar = palloc(112, 'TEMP2', 0, 2)

!     NBLOck output form
      if(blockfl) then

!       Output side control points
        write(ios,'(/a)') 'NSIDes'
        nnside = nnside + 1
        write(ios,2001) nnside,leq,kno,(l+cnurnp,l=1,leq)

      endif

!     Output Region description
      if(nblk(3,blk).gt.0) then   ! nuregn(blk)
        write(ios,'(/a,i5)') 'REGIon1',nblk(3,blk)
      endif

!     Output Block description
      if(blockfl) then

        write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
        write(ios,2003)     1,nblk(2,blk),nnside  ! nurmat(blk)

!     Output Patch description
      else

        write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
        write(ios,2004) nblk(2,blk),leq,kno
        write(ios,2005) (l+cnurnp,l=1,leq)

      endif

      write(ios,'(a)') ' '

!     Formats

2000  format('-> Elevate Block =',i3,': Increase Order =',i3)
2001  format('  side',3i4,12i6:/(16i6:))
2002  format('  open',2i4,1p,13e16.8:/(1p,16e16.8:))
2003  format('  block',3i6)
2004  format('  line',3i6)
2005  format(16i6:)

      end

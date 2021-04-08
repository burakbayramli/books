!$Id:$
      subroutine poutblk2d(blk,dir,uu,rr,
     &                     knots,nsides,lknot,lside,nblk,ktnum,nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Block outputs for knot insertion (1 and 2 d blocks)

!      Use:   INSErt block dir u_knot num_times

!      Data structure:
!         Knots:
!            lknot(1,k) - Length of knot vector 'k'
!            lknot(2,k) - Order  of knot vector 'k'
!            lknot(3,k) - Number control points for knot 'k'
!            knots(j,i) - Knot values for vector 'k'

!         Sides:
!            lside(1,s)  - Length of side 'k'
!            lside(2,s)  - Knot vector for side 'k'
!            nsides(j,s) - Control point numbers for side 'k'

!         Blocks:
!            nblk(1,b)   - Dimension of block (1 or 2)
!            nblk(4,b)   - Number of 1-direction sides
!            nblksd(j,b) - 1-direction side numbers
!            nblksd(e,b) - 2-direction side number (e = lblksd(b) + 1)

!      Inputs:
!         blk       - Block number
!         dir       - Block direction to insert
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

      integer    blk, dir, bdm, rr
      integer    sid1,kno1,len1,lek1,ord1
      integer    sid2,kno2,len2,lek2,ord2
      integer    l,n, leq,leu, cnurnp
      real*8     uu
      real*8     knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*),nblk(14,*)
      integer    ktnum(6,*),nblksd(dblokig,*)

      save

!     Set block dimension

      bdm  = nblk(1,blk)          ! Block dimension

!     Set side 1

      sid1 = nblksd(1,blk)
      len1 = lside(1,sid1)          ! Length of side 1
      kno1 = lside(2,sid1)          ! Knot vector for side 1
      lek1 = lknot(1,kno1)        ! Length of knot vector for side 1
      ord1 = lknot(2,kno1)        ! Order  of knot vector for side 1

!     Get side 2

      if(bdm.eq.1) then
        len2 = 1
      else
        len2 = nblk(4,blk)        ! Length of Side 2 control nodes
        sid2 = nblksd(len2+1,blk)
        kno2 = lside(2,sid2)
        lek2 = lknot(1,kno2)
        ord2 = lknot(2,kno2)
      endif

!     Save last control point value

      cnurnp = nnurnp

!     Refine the 1 direction knots

      if(dir.eq.1) then

!       Place side-1 nodes in list

        do l = 1,len1
          ns2(l) = nsides(l,nblksd(1,blk))
        end do ! l
        call pcurvin(len1,ord1,lek1, uu,rr, ns2,knots(1,kno1),
     &               hr(np(43)),hr(np(263)), leq,leu)

!       Output knot vectors

        write(ios,'(/a)') 'KNOTS'
        write(ios,2002) kno1,leu,(UU2(l),l=1,leu)
        ktnum(1,blk) = kno1
        if(bdm.gt.1) then
          write(ios,2002) kno2,lek2,(knots(l,kno2),l=1,lek2)
        endif

!       Allocate storage to save new coordinates and weights

        setvar = palloc(111, 'TEMP1', ndm*leq*len2, 2) ! Coords
        setvar = palloc(112, 'TEMP2',     leq*len2, 2) ! Wts

        call psetnurb(hr(np(111)),hr(np(112)), QQ2, 0,leq,1)

!       Generate remaining new control vector locations
!       Insert control node numbers from position 'l' in side 'n'

        do n  = 2,len2
          do l = 1,len1
            ns2(l) = nsides(l,nblksd(n,blk))
          end do ! l
          call pcurvin(len1,ord1,lek1, uu,rr, ns2,knots(1,kno1),
     &                 hr(np(43)),hr(np(263)), leq,leu)
          call psetnurb(hr(np(111)),hr(np(112)), QQ2, leq*(n-1),leq,1)
        end do ! n

!       Output NURB coordinates and weights

        call poutnurb(hr(np(111)),hr(np(112)), leq,len2)

!       Output SIDEs for NBLOcks

        if(blockfl) then

!         Store edge side numbers

          nblk( 8,blk) = nnside + 1          ! eside(1,blk)
          nblk( 9,blk) = nnside + len2       ! eside(2,blk)
          nblk(10,blk) = nnside + len2 + 1   ! eside(3,blk)
          nblk(11,blk) = nnside + len2 + 2   ! eside(4,blk)

!         Write side vectors

          write(ios,'(/a)') 'NSIDEs'
          do n = 1,len2
            write(ios,2001) nnside+n,leq,kno1,
     &                     (cnurnp+leq*(n-1)+l,l=1,leq)
          end do ! n
          if(bdm.gt.1) then

!           Write bottom vector

            nnside = nnside + len2 + 1
            write(ios,2001) nnside,len2,kno2,
     &                     (cnurnp+leq*(l-1)+1,l=1,len2)

!           Write top vector

            nnside = nnside + 1
            write(ios,2001) nnside,len2,kno2,(cnurnp+leq*l,l=1,len2)
          else
            nnside = nnside + len2
          endif
        endif

!       Output Region description

        if(nblk(3,blk).gt.0) then
          write(ios,'(/a,i5)') 'REGIon5',nblk(3,blk)
        endif

!       Output Block description

        if(blockfl) then

          write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
          write(ios,2003) bdm,nblk(2,blk),nnside-1
          write(ios,2004) (nblk(n,blk),n=8,11)

!       Output Patch description

        else

          write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
          write(ios,2005) nblk(2,blk),leq,len2,kno1,kno2
          do n = 1,len2
            write(ios,2006) (cnurnp+leq*(n-1)+l,l=1,leq)
          end do ! n

        endif

!     Refine 2 direction knots

      elseif(dir.eq.2) then

!       Side control points are first entry of ech side 1

        do l = 1,len2
          ns2(l) = nsides(1,nblksd(l,blk))
        end do ! l
        call pcurvin(len2,ord2,lek2, uu,rr, ns2,knots(1,kno2),
     &               hr(np(43)),hr(np(263)), leq,leu)
        ktnum(2,blk) = kno2

!       Allocate storage to save new coordinates and weights

        setvar = palloc(111, 'TEMP1', ndm*leq*len1, 2) ! Coords
        setvar = palloc(112, 'TEMP2',     leq*len1, 2) ! Wts

        call psetnurb(hr(np(111)),hr(np(112)), QQ2, 0,leq,1)

!       Generate remaining new control vector locations
!       List of nodes for side 2 are 'n' positions of each nblksd(l,blk)

        do n  = 2,len1
          do l = 1,len2
            ns2(l) = nsides(n,nblksd(l,blk))
          end do ! l
          call pcurvin(len2,ord2,lek2, uu,rr, ns2,knots(1,kno2),
     &                 hr(np(43)),hr(np(263)), leq,leu)
          call psetnurb(hr(np(111)),hr(np(112)), QQ2, leq*(n-1),leq,1)
        end do ! n

!       Output knot vector for both directions

        write(ios,'(/a)') 'KNOTS'
        write(ios,2002) kno1,lek1,(knots(l,kno1),l=1,lek1)
        write(ios,2002) kno2,leu,(UU2(l),l=1,leu)

!       Write NURB coordinates and weights

        call poutnurb(hr(np(111)),hr(np(112)), len1,leq)

!       Output SIDEs for NBLOcks

        if(blockfl) then

!         Store edge side numbers

          nblk( 8,blk) = nnside + 1         ! eside(1,blk)
          nblk( 9,blk) = nnside + leq       ! eside(2,blk)
          nblk(10,blk) = nnside + leq + 1   ! eside(3,blk)
          nblk(11,blk) = nnside + leq + 2   ! eside(4,blk)

!         Write side vectors

          write(ios,'(/a)') 'NSIDEs'
          do n = 1,leq
            write(ios,2001) nnside+n,len1,kno1,
     &                     (cnurnp+leq*(l-1)+n,l=1,len1)
          end do ! n

!         Write bottom vector

          nnside = nnside + leq + 1
          write(ios,2001) nnside,leq,kno2,(cnurnp+l,l=1,leq)

!         Write top vector

          nnside = nnside + 1
          write(ios,2001) nnside,leq,kno2,
     &                   (cnurnp+leq*(len1-1)+l,l=1,leq)
        endif

!       Output Region description

        if(nblk(3,blk).gt.0) then
          write(ios,'(/a,i5)') 'REGIon6',nblk(3,blk)
        endif

!       Output Block description

        if(blockfl) then

          write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
          write(ios,2003) 2,nblk(2,blk),nnside-1
          write(ios,2004) (nblk(n,blk),n=8,11)

!       Output Patch description

        else

          write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
          write(ios,2005) nblk(2,blk),len1,leq,kno1,kno2
          do n = 1,leq
            write(ios,2006) (cnurnp+leq*(l-1)+n,l=1,len1)
          end do ! n

        endif

      endif

      setvar = palloc(111, 'TEMP1', 0, 2)
      setvar = palloc(112, 'TEMP2', 0, 2)

      write(ios,'(a)') ' '

!     Formats

2001  format('  side',3i4,12i6:/(16i6:))
2002  format('  open',2i4,1p,13e16.8:/(1p,16e16.8:))
2003  format('  block',3i6)
2004  format('  eside',4i6)
2005  format('  surface',5i6)
2006  format(16i6:)

      end

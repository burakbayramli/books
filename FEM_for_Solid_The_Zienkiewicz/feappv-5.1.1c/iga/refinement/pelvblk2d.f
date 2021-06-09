!$Id:$
      subroutine pelvblk2d(ctl,knots,nsides,lknot,lside,nblk,ktnum,
     &                     nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Curve: Elevation of order for 2-d

!      Use:      ELEVate block num_blk dir inc_order

!      Data structure:
!         Knots:
!            lknot(1,k)  - Length of knot vector 'k'
!            lknot(2,k)  - Order  of knot vector 'k'
!            lknot(3,k)  - Number control points for knot 'k'
!            knots(j,i)  - Knot values for vector 'k'

!         Sides:
!            lside(s)    - Length of side 'k'
!            kside(s)    - Knot vector for side 'k'
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
      real*8     ctl(3),knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*),nblk(14,*)
      integer    ktnum(6,*),nblksd(dblokig,*)

      integer    blk, dir, elv
      integer    sid1,kno1,len1,lek1,ord1
      integer    sid2,kno2,len2,lek2,ord2
      integer    l,n, leq,leu, cnurnp

      save

!     Control information to elevate in block

      blk  = nint(ctl(1))          ! Block number to refine
      dir  = nint(ctl(2))          ! Direction to elevate
      elv  = nint(ctl(3))          ! Elevation order

      write(iow,2000) blk,dir,elv
      if(ior.lt.0) then
        write(*,2000) blk,dir,elv
      endif

      sid1 = nblksd(1,blk)       ! Side 1 number
      len1 = lside(1,sid1)       ! Length of Side 1 control nodes
      kno1 = lside(2,sid1)       ! Knot number of side 1 - kside(sid1)
      lek1 = lknot(1,kno1)       ! Length of knot vector
      ord1 = lknot(2,kno1)       ! Order  of knot vector

!     Matching side "side2"

      len2 = nblk(4,blk)         ! lblksd(blk)
      sid2 = nblksd(len2+1,blk)
      kno2 = lside(2,sid2)
      lek2 = lknot(1,kno2)
      ord2 = lknot(2,kno2)

      ktnum(1,blk) = kno1
      ktnum(2,blk) = kno2

      cnurnp       = nnurnp

!     Elevate 1 direction sides and knots

      if(dir.eq.1) then

!       Place side-1 nodes in list in order inserted above

        do l = 1,len1
          ns2(l) = nsides(l,nblksd(1,blk))
        end do ! l

        call pcurvel(len1,ord1,lek1, elv, ns2,knots(1,kno1),
     &               hr(np(43)),hr(np(263)), leq,leu)

!       Allocate storage to save new coordinates and weights

        setvar = palloc(111, 'TEMP1', ndm*leq*len2, 2) ! Coords
        setvar = palloc(112, 'TEMP2',     leq*len2, 2) ! Wts

        call psetnurb(hr(np(111)),hr(np(112)), QQ2, 0,leq,1)

!       Generate remaining new control vector locations
!       Obtain each side-1 number from order inserted into new arrays

        do n  = 2,len2
          do l = 1,len1
            ns2(l) = nsides(l,nblksd(n,blk))
          end do ! l
          call pcurvel(len1,ord1,lek1, elv, ns2,knots(1,kno1),
     &               hr(np(43)),hr(np(263)), leq,leu)
          call psetnurb(hr(np(111)),hr(np(112)), QQ2, leq*(n-1),leq,1)
        end do ! n

!       Write new knot vectors for next level mesh

        write(ios,'(/a)') 'KNOTS'
        write(ios,2002) kno1,leu, (UU2(l),l=1,leu)
        write(ios,2002) kno2,lek2,(knots(l,kno2),l=1,lek2)

!       Output NURB coordinates and weights

        call poutnurb(hr(np(111)),hr(np(112)), leq,len2)

!       Output side data for NBLOck

        if(blockfl) then

!         Store edge side numbers

          nblk( 8,blk) = nnside + 1         ! eside(1,blk
          nblk( 9,blk) = nnside + len2      ! eside(2,blk
          nblk(10,blk) = nnside + len2 + 1  ! eside(3,blk
          nblk(11,blk) = nnside + len2 + 2  ! eside(4,blk

!         Write side vectors

          write(ios,'(/a)') 'NSIDEs'
          do n = 1,len2
            write(ios,2001) nnside+n,leq,kno1,
     &                     (cnurnp+leq*(n-1)+l,l=1,leq)
          end do ! n

!         Write bottom vector

          nnside = nnside + len2 + 1
          write(ios,2001) nnside,len2,kno2,(cnurnp+leq*(l-1)+1,l=1,len2)

!         Write top vector

          nnside = nnside + 1
          write(ios,2001) nnside,len2,kno2,(cnurnp+leq*l,l=1,len2)
        endif

!       Output region description

        if(nblk(3,blk).gt.0) then
          write(ios,'(/a,i5)') 'REGIon2',nblk(3,blk) ! nuregn(blk)
        endif

!       Output block descriptor

        if(blockfl) then

          write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
          write(ios,2003) 2,nblk(2,blk),nnside-1
          write(ios,2004) (nblk(n,blk),n=8,11)

!       Output patch descriptor

        else

          write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
          write(ios,2005) nblk(2,blk),leq,len2,kno1,kno2
          do n = 1,len2
            write(ios,2006) (cnurnp+leq*(n-1)+l,l=1,leq)
          end do ! n

        endif

!     Elevate 2 direction sides and knots

      elseif(dir.eq.2) then

!       Side control points are first entry of each side 1

        do l = 1,len2
          ns2(l) = nsides(1,nblksd(l,blk))
        end do ! l

        call pcurvel(len2,ord2,lek2, elv, ns2,knots(1,kno2),
     &               hr(np(43)),hr(np(263)), leq,leu)

!       Allocate storage to save new coordinates and weights

        setvar = palloc(111, 'TEMP1', ndm*leq*len1, 2) ! Coords
        setvar = palloc(112, 'TEMP2',     leq*len1, 2) ! Wts

        call psetnurb(hr(np(111)),hr(np(112)), QQ2, 0,leq,1)

!       Generate remaining new control vector locations
!       List of nodes for side 2 are 'n' position of each nblksd(l,blk)

        do n  = 2,len1
          do l = 1,len2
            ns2(l) = nsides(n,nblksd(l,blk))
          end do ! l
          call pcurvel(len2,ord2,lek2, elv, ns2,knots(1,kno2),
     &                 hr(np(43)),hr(np(263)), leq,leu)
          call psetnurb(hr(np(111)),hr(np(112)), QQ2, leq*(n-1),leq,1)
        end do ! n

!       Write new knot vectors for next level mesh

        write(ios,'(/a)') 'KNOTS'
        write(ios,2002) kno1,lek1,(knots(l,kno1),l=1,lek1)
        write(ios,2002) kno2,leu, (UU2(l),l=1,leu)

!       Output NURB coordinates and weights

        call poutnurb(hr(np(111)),hr(np(112)), len1,leq)

!       Side output for NBLOck

        if(blockfl) then

!         Store edge side numbers

          nblk( 8,blk) = nnside + 1         ! eside(1,blk)
          nblk( 9,blk) = nnside + leq       ! eside(1,blk)
          nblk(10,blk) = nnside + leq + 1   ! eside(1,blk)
          nblk(11,blk) = nnside + leq + 2   ! eside(1,blk)

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

!       Output region description

        if(nblk(3,blk).gt.0) then
          write(ios,'(/a,i5)') 'REGIon3',nblk(3,blk)
        endif

!       Output block description

        if(blockfl) then

          write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
          write(ios,2003) 2,nblk(2,blk),nnside-1
          write(ios,2004) (nblk(n,blk),n=8,11)

!       Output patch description

        else

          write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
          write(ios,2005) nblk(2,blk),len1,leq,kno1,kno2
          do n = 1,leq
            write(ios,2006) (cnurnp+leq*(l-1)+n,l=1,len1)
          end do ! n

        endif

      endif ! dir

      setvar = palloc(111, 'TEMP1', 0, 2)
      setvar = palloc(112, 'TEMP2', 0, 2)

      write(ios,'(a)') ' '

!     Formats

2000  format(/'  Elevate: Block ',i4,' direction',i3,': Order =',i3)
2001  format('  side',3i4,12i6:/(16i6:))
2002  format('  open',2i4,1p,13e16.8:/(1p,16e16.8:))
2003  format('  block',3i6)
2004  format('  eside',4i6)
2005  format('  surface',5i6)
2006  format(16i6:)

      end

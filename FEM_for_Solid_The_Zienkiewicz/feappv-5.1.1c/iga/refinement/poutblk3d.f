!$Id:$
      subroutine poutblk3d(blk,dir,uu,rr,
     &                     knots,nsides,lknot,lside,nblk,ktnum,nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Block outputs for knot insertion (3 d blocks)

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
!            nblkdm(b)   - Dimension of block (1 or 2)
!            lblksd(b)   - Number of 1-direction sides
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

      integer    blk,  dir, rr, i,ii, j,jj, k, l,ll, side1, inside
      integer    le1,le2,le3,les,lek,ke3,kno,ord, leq,leu, cnurnp
      integer    nnblk3d(2), nnblkss(300)
      real*8     uu
      real*8     knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*),nblk(14,*)
      integer    ktnum(6,*),nblksd(dblokig,*)

      save

!     Write knot vectors

      do i = 1,2
        nnblk3d(i)       = nblk(i+5,blk)
!       nmcp(nnblk3d(i)) = lknot(3,nnblk3d(i))
        lknot(4,nnblk3d(i)) = lknot(3,nnblk3d(i))
      end do ! i

!     Save initial side number

      inside = nnside
      cnurnp = nnurnp

!     Refine the 1 direction knots

      if(dir.eq.1) then

        side1 = nblksd(1,blk)
        le3   = lside(1,side1)
        ke3   = lside(2,side1)
        kno   = nblk(6,blk)
        le1   = lknot(3,kno)
        lek   = lknot(1,kno)
        ord   = lknot(2,kno)
        le2   = lknot(3,nblk(7,blk))

!       Set number of control points in new mesh

        lknot(4,kno) = le1 + rr
        jj = (le1+rr)*le2
        do j = 1,jj
          nnblkss(j) = j + inside
        end do

        jj = jj*le3
        setvar = palloc(111, 'TEMP1', ndm*jj, 2) ! Coords
        setvar = palloc(112, 'TEMP2',     jj, 2) ! Wts

        ll = 0
        do k = 1,le3
          jj = 0
          do j = 1,le2
            do l = 1,le1
              ns2(l) = nsides(k,nblksd(jj+l,blk))
            end do ! l
            call pcurvin(le1,ord,lek, uu,rr, ns2,knots(1,kno),
     &                   hr(np(43)),hr(np(263)), leq,leu)
            call psetnurb(hr(np(111)),hr(np(112)), QQ2,ll,leq,1)
            ll = ll + leq
            jj = jj + le1
          end do ! j
        end do ! k

!       Output the side numbers

        if(blockfl) then
          write(ios,'(/a)') 'NSIDes'
          jj = 0
          do j = 1,le2
            do i = 1,leq
              jj = jj + 1
              ii = cnurnp
              do k = 1,le3
                ns2(k) = ii + jj
                ii = ii + leq*le2
              end do ! k
              write(ios,2001) jj+inside,le3,ke3,(ns2(l),l=1,le3)
            end do ! i
          end do ! j
          nnside = jj + inside
        else
          le1 = leq
        endif

!       Output knot vector for three directions

        write(ios,'(/a)') 'KNOTS'

        write(ios,2002) nblk(6,blk),leu,(UU2(l),l=1,leu)
        lek = nblk(7,blk)
        les = lknot(1,lek)
        write(ios,2002) lek,les,(knots(l,lek),l=1,les)
        lek = lside(2,side1)
        les = lknot(1,lek)
        write(ios,2002) lek,les,(knots(l,lek),l=1,les)

!       Output control points

        call poutnurb(hr(np(111)),hr(np(112)), ll,1)

!     Refine the 2 direction knots

      elseif(dir.eq.2) then

        side1 = nblksd(1,blk)
        le3   = lside(1,side1)
        ke3   = lside(2,side1)
        le1   = lknot(3,nblk(6,blk))
        kno   = nblk(7,blk)
        lek   = lknot(1,kno)
        ord   = lknot(2,kno)
        le2   = lknot(3,kno)

!       Set number of control points in new mesh

        lknot(4,kno) = le2 + rr
        jj = le1*(le2+rr)
        do j = 1,jj
          nnblkss(j) = j + inside
        end do

        jj = jj*le3
        setvar = palloc(111, 'TEMP1', ndm*jj, 2) ! Coords
        setvar = palloc(112, 'TEMP2',     jj, 2) ! Wts

        ll = 0
        do k = 1,le3
          do j = 1,le1
            ii = 0
            do l = 1,le2
              ns2(l) = nsides(k,nblksd(j+ii,blk))
              ii     = ii + le1
            end do ! l
            call pcurvin(le2,ord,lek, uu,rr, ns2,knots(1,kno),
     &                   hr(np(43)),hr(np(263)), leq,leu)

            call psetnurb(hr(np(111)),hr(np(112)), QQ2,ll,leq,1)

            ll = ll + leq
          end do ! j
        end do ! k

!       Output the side numbers

        if(blockfl) then
          write(ios,'(/a)') 'NSIDes'
          l = inside
          do j = 1,leq
            jj = j
            do i = 1,le1
              ii = cnurnp
              do k = 1,le3
                ns2(k) = ii + jj
                ii = ii + le1*leq
              end do ! k
              jj = jj + leq
              l  = l  + 1
              write(ios,2001) l,le3,ke3,(ns2(k),k=1,le3)
            end do ! i
          end do ! j
          nnside = l
        else
          le2 = leq
        endif
!       Output knot vector for three directions

        write(ios,'(/a)') 'KNOTS'

        lek = nblk(6,blk)
        les = lknot(1,lek)
        write(ios,2002) lek,les,(knots(l,lek),l=1,les)
        write(ios,2002) nblk(7,blk),leu,(UU2(l),l=1,leu)
        lek = lside(2,side1)
        les = lknot(1,ke3)
        write(ios,2002) lek,les,(knots(l,lek),l=1,les)

        call poutnurb(hr(np(111)),hr(np(112)), ll,1)

!     Refine the 3 direction knots

      elseif(dir.eq.3) then

        side1 = nblksd(1,blk)
        les   = lside(1,side1)
        kno   = lside(2,side1)
        lek   = lknot(1,kno)
        ord   = lknot(2,kno)

        jj = lknot(3,nblk(6,blk))*lknot(3,nblk(7,blk))
        do j = 1,jj
          nnblkss(j) = nblksd(j,blk)
        end do ! j

!       Allocate storage to save new coordinates and weights

        jj = jj*(les+rr)
        setvar = palloc(111, 'TEMP1', ndm*jj, 2) ! Coords
        setvar = palloc(112, 'TEMP2',     jj, 2) ! Wts

        ll = 0
        jj = lknot(3,nnblk3d(2))*lknot(3,nnblk3d(1))
        do j = 1,jj

!         Side control points are first entry of each side 1

          do l = 1,les
            ns2(l) = nsides(l,nblksd(j,blk))
          end do ! l
          call pcurvin(les,ord,lek, uu,rr, ns2,knots(1,kno),
     &                 hr(np(43)),hr(np(263)), leq,leu)

          call psetnurb(hr(np(111)),hr(np(112)), QQ2,ll,leq,1)

          ll = ll + leq
        end do ! j

        if(blockfl) then
          write(ios,'(/a)') 'NSIDes'
          ll = 0
          do j = 1,jj
            write(ios,2001) j+inside,leq,kno,(cnurnp+l,l=ll+1,ll+leq)
            ll = ll + leq
          end do ! j
          nnside = inside + jj
        else
          le1 = lknot(3,nblk(6,blk))
          le2 = lknot(3,nblk(7,blk))
          le3 = leq
        endif

!       Output knot vector for three directions

        write(ios,'(/a)') 'KNOTS'

        ktnum(kno,blk) = kno
        do i = 1,2
          lek = nblk(i+5,blk)   ! Knot number of side i
          les = lknot(1,lek)   ! Length of knot vector for side 1
          write(ios,2002) lek,les,(knots(l,lek),l=1,les)
        end do ! i
        write(ios,2002) kno,leu,(UU2(l),l=1,leu)

        call poutnurb(hr(np(111)),hr(np(112)), ll,1)

      endif ! dir 3

!     Update final nurbs and side values

      if(nblk(3,blk).gt.0) then
        write(ios,'(/a,i5)') 'REGIon7',nblk(3,blk)
      endif

!     Output block data

      if(blockfl) then

        write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
        write(ios,2003)   3,nblk(2,blk),nnblk3d(1),nnblk3d(2)
        jj = lknot(4,nnblk3d(1))*lknot(4,nnblk3d(2))
        write(ios,2004) (nnblkss(i),i=1,jj)

!     Output patch data

      else

        write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
        write(ios,2005) nblk(2,blk),le1,le2,le3,
     &                  nblk(6,blk),nblk(7,blk),lside(2,side1)

        if(dir.eq.1) then

          jj = 0
          do j = 1,le2
            do i = 1,le1
              jj = jj + 1
              ii = cnurnp
              do k = 1,le3
                ns2(k) = ii + jj
                ii = ii + le1*le2
              end do ! k
              write(ios,2004) (ns2(l),l=1,le3)
            end do ! i
          end do ! j

        elseif(dir.eq.2) then

          l = inside
          do j = 1,le2
            jj = j
            do i = 1,le1
              ii = cnurnp
              do k = 1,le3
                ns2(k) = ii + jj
                ii = ii + le1*le2
              end do ! k
              jj = jj + le2
              l  = l  + 1
              write(ios,2004) (ns2(k),k=1,le3)
            end do ! i
          end do ! j

        elseif(dir.eq.3) then

          ll = 0
          do j = 1,jj
            write(ios,2004) (cnurnp+l,l=ll+1,ll+leq)
            ll = ll + leq
          end do ! j

        endif
      endif

      write(iow,'(a)') ' '

!     Formats

2001  format('  side',3i4,12i6:/(16i6:))
2002  format('  open',2i4,1p,13e16.8:/(1p,16e16.8:))
2003  format('  block',4i6)
2004  format(16i6:)
2005  format('  solid',7i6)

      end

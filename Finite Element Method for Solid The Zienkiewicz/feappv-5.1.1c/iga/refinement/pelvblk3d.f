!$Id:$
      subroutine pelvblk3d(ctl,knots,nsides,lknot,lside,nblk,nblksd)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose:  Curve: Elevation of order for 3-d

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
!            nblkdm(b)   - Dimension of block (1, 2 or 3)
!            nurmat(b)   - Material number of block

!            1-d or 2-d blocks
!              lblksd(b)   - Number of 1-direction sides
!              nblksd(j,b) - 1-direction side numbers
!              nblksd(e,b) - 2-direction side number (e = lblksd(b)+1)

!            3-d blocks
!              nbk3d(j,b)  - 1 and 2 direction knot number
!              nblksd(j,b) - lknot(3,k1)*lknot(3,k2) side # in 3-dir

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
      include   'sdata.h'
      include   'umac1.h'

      include   'p_ptname.h'

      include   'pointer.h'
      include   'comblk.h'

      logical    setvar,palloc, memfl
      real*8     ctl(3),knots(dknotig,*)
      integer    nsides(dsideig,*),lknot(0:4,*),lside(2,*),nblk(14,*)
      integer    nblksd(dblokig,*)

      integer    blk, dir, elv, sid3, inside
      integer    ii,jj,ll, i,j,k,l,n, leq,leu, cnurnp
      integer    kno1,kno2,kno3
      integer    lek1,lek2,lek3
      integer    len1,len2,len3
      integer    ord1,ord2,ord3
      integer    nnblk3d(2), nnblkss(300)

      save

!     Control information to elevate in block

      blk  = nint(ctl(1))          ! Block number to refine
      dir  = nint(ctl(2))          ! Direction to elevate
      elv  = nint(ctl(3))          ! Elevation order

      write(iow,2000) blk,dir,elv
      if(ior.lt.0) then
        write(*,2000) blk,dir,elv
      endif

!     Block size

      kno1 = nblk(6,blk)         ! Knot vector for side 1 - nbk3d(1,blk)
      lek1 = lknot(1,kno1)       ! Length of knot for side 1
      ord1 = lknot(2,kno1)       ! Order of side 1
      len1 = lknot(3,kno1)       ! Number of control points on side 1

      kno2 = nblk(7,blk)         ! Knot vector for side 2
      lek2 = lknot(1,kno2)       ! Length of knot for side 2
      ord2 = lknot(2,kno2)       ! Order of side 2
      len2 = lknot(3,kno2)       ! Number of control points on side 2

      sid3 = nblksd(1,blk)       ! 3-direction side
      len3 = lside(1,sid3)       ! Number of control points on side 3
      kno3 = lside(2,sid3)       ! Knot vector for side 3 - kside(sid3)
      lek3 = lknot(1,kno3)       ! Length of knot for side 2
      ord3 = lknot(2,kno3)       ! Order of side 3

!     Set new blk numbers

      do n = 1,2
        nnblk3d(n)          = nblk(n+5,blk)  ! nbk3d(n,blk)
        lknot(4,nnblk3d(n)) = lknot(3,nnblk3d(n))
      end do ! n

!     Save nnurnp and nnside

      cnurnp = nnurnp
      inside = nnside

!     Elevate 1-direction

      if(dir.eq.1) then

        memfl = .true.
        jj    = len2*len3
        ll    = 0
        do k = 1,len3
          ii = 0
          do j = 1,len2
            do l = 1,len1
              ns2(l) = nsides(k,nblksd(ii+l,blk))
            end do ! l
            call pcurvel(len1,ord1,lek1, elv,ns2,knots(1,kno1),
     &                   hr(np(43)),hr(np(263)), leq,leu)
            if(memfl) then
              memfl  = .false.
              jj     = jj*leq
              setvar = palloc(111, 'TEMP1', ndm*jj, 2) ! Coords
              setvar = palloc(112, 'TEMP2',     jj, 2) ! Wts
              jj     = len2*leq
              do l = 1,jj
                nnblkss(l) = l + inside
              end do ! l
              lknot(4,kno1) = leq
            endif

            call psetnurb(hr(np(111)),hr(np(112)), QQ2, ll,leq,1)
            ll = ll + leq
            ii = ii + len1
          end do ! j
        end do ! k

!       Output side numbers

        if(blockfl) then
          write(ios,'(/a)') 'NSIDes'
          jj = 0
          do j = 1,len2
            do i = 1,leq
              jj = jj + 1
              ii = cnurnp
              do k = 1,len3
                ns2(k) = ii + jj
                ii     = ii + leq*len2
              end do ! k
              write(ios,2001) nnside+jj,len3,kno3,(ns2(l),l=1,len3)
            end do ! i
          end do ! j
          nnside = nnside + leq*len2
        else
          len1 = leq
        endif

!       Output knot vectors for next level mesh

        write(ios,'(/a)') 'KNOTS'
        write(ios,2002) kno1,leu, (UU2(l),l=1,leu)
        write(ios,2002) kno2,lek2,(knots(l,kno2),l=1,lek2)
        write(ios,2002) kno3,lek3,(knots(l,kno3),l=1,lek3)

!     Elevate 2-direction

      elseif(dir.eq.2) then

!       Set number of control points in new mesh

        memfl = .true.
        jj    = len1*len3
        ll    = 0
        do k = 1,len3
          do j = 1,len1
            ii = 0
            do l = 1,len2
              ns2(l) = nsides(k,nblksd(j+ii,blk))
              ii     = ii + len1
            end do ! l
            call pcurvel(len2,ord2,lek2, elv,ns2,knots(1,kno2),
     &                   hr(np(43)),hr(np(263)), leq,leu)
            if(memfl) then
              memfl  = .false.
              jj     = jj*leq
              setvar = palloc(111, 'TEMP1', ndm*jj, 2) ! Coords
              setvar = palloc(112, 'TEMP2',     jj, 2) ! Wts
              jj     = len1*leq
              do l = 1,jj
                nnblkss(l) = l + inside
              end do ! l
              lknot(4,kno2) = leq
            endif

            call psetnurb(hr(np(111)),hr(np(112)), QQ2,ll,leq,1)

            ll = ll + leq
          end do ! j
        end do ! k

!       Output side numbers

        if(blockfl) then
          write(ios,'(/a)') 'NSIDes'
          l = 0
          do j = 1,leq
            jj = j
            do i = 1,len1
              ii = cnurnp
              do k = 1,len3
                ns2(k) = ii + jj
                ii     = ii + len1*leq
              end do ! k
              jj = jj + leq
              l  = l  + 1
              write(ios,2001) nnside+l,len3,kno3,(ns2(k),k=1,len3)
            end do ! i
          end do ! j
          nnside = nnside + leq*len1
        else
          len2 = leq
        endif

!       Write new knot vectors for next level mesh

        write(ios,'(/a)') 'KNOTS'
        write(ios,2002) kno1,lek1,(knots(l,kno1),l=1,lek1)
        write(ios,2002) kno2,leu, (UU2(l),l=1,leu)
        write(ios,2002) kno3,lek3,(knots(l,kno3),l=1,lek3)

!     Elevate 3-direction

      elseif(dir.eq.3) then

!       Allocate storage to save new coordinates and weights

        memfl = .true.
        jj    = len1*len2
        ll    = 0
        do j = 1,len1*len2
          do l = 1,len3
            ns2(l) = nsides(l,nblksd(j,blk))
          end do ! l

          call pcurvel(len3,ord3,lek3, elv,ns2,knots(1,kno3),
     &                 hr(np(43)),hr(np(263)), leq,leu)
          if(memfl) then
            memfl = .false.
            do l = 1,jj
              nnblkss(l) = l + inside
            end do ! l
            lknot(4,kno3) = leq
            jj         = jj*leq
            setvar     = palloc(111, 'TEMP1', ndm*jj, 2) ! Coords
            setvar     = palloc(112, 'TEMP2',     jj, 2) ! Wts
          endif
          call psetnurb(hr(np(111)),hr(np(112)), QQ2, ll,leq,1)
          ll = ll + leq
        end do ! j

!       Output side control points

        if(blockfl) then
          write(ios,'(/a)') 'NSIDes'
          ll    = 0
          do j = 1,len1*len2
            write(ios,2001) nnside+j,leq,kno3,(l+cnurnp,l=ll+1,ll+leq)
            ll = ll + leq
          end do ! j
          nnside = nnside + len1*len2
        else
          len3 = leq
        endif

!       write new knot vector for next level mesh

        write(ios,'(/a)') 'KNOTS'
        write(ios,2002) kno1,lek1,(knots(l,kno1),l=1,lek1)
        write(ios,2002) kno2,lek2,(knots(l,kno2),l=1,lek2)
        write(ios,2002) kno3,leu ,(UU2(l),l=1,leu)

      endif

!     Output NURB coordinates and weights

      call poutnurb(hr(np(111)),hr(np(112)), ll,1)

!     Output Region description

      if(nblk(3,blk).gt.0) then
        write(ios,'(/a,i5)') 'REGIon4',nblk(3,blk) ! nuregn(blk)
      endif

!     Output Block description

      if(blockfl) then
        write(ios,'(/a,a)') 'NBLOck PART=',partname(nblk(5,blk))
        write(ios,2003) 3,nblk(2,blk),(nnblk3d(j),j=1,2)
        jj = lknot(4,nnblk3d(1))*lknot(4,nnblk3d(2))
        write(ios,2004) (nnblkss(n),n=1,jj)

!     Output Patch description

      else

        write(ios,'(/a,a)') 'NPATch PART=',partname(nblk(5,blk))
        write(ios,2005) nblk(2,blk),len1,len2,len3,kno1,kno2,kno3
        if(dir.eq.1) then
          jj = 0
          do j = 1,len2
            do i = 1,len1
              jj = jj + 1
              ii = cnurnp
              do k = 1,len3
                ns2(k) = ii + jj
                ii     = ii + len1*len2
              end do ! k
              write(ios,2004) (ns2(l),l=1,len3)
            end do ! i
          end do ! j

        elseif(dir.eq.2) then
          l = 0
          do j = 1,len2
            jj = j
            do i = 1,len1
              ii = cnurnp
              do k = 1,len3
                ns2(k) = ii + jj
                ii     = ii + len1*len2
              end do ! k
              jj = jj + len2
              l  = l  + 1
              write(ios,2004) (ns2(k),k=1,len3)
            end do ! i
          end do ! j

        elseif(dir.eq.3) then

          ll    = 0
          do j = 1,len1*len2
            write(ios,2004) (l+cnurnp,l=ll+1,ll+len3)
            ll = ll + len3
          end do ! j

        endif

      endif

      write(ios,'(a)') ' '

!     Delete temporary memory use

      setvar = palloc(111, 'TEMP1', 0, 2) ! Coords
      setvar = palloc(112, 'TEMP2', 0, 2) ! Wts

!     Formats

2000  format(/'  Elevate: Block ',i4,' direction',i3,': Order =',i3)
2001  format('  side',3i4,12i6:/(16i6:))
2002  format('  open',2i4,1p,13e16.8:/(1p,16e16.8:))
2003  format('  block',4i6)
2004  format(16i6:)
2005  format('  solid',7i6)

      end

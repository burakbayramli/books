!$Id:$
      subroutine pknots(lknot, knots, eknot, prt)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Compute nonzero basis functions and their derivatives.

!      Data:
!        open, knum, lknot(1,knum), knots(j,knum):j=1,lknot(1,knum)

!      Inputs:
!        prt    - Flag to control outputs

!      Outputs:

!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'bdata.h'
      include   'cnurb.h'
      include   'igdata.h'
      include   'iofile.h'

      integer (kind=4) :: lknot(0:4,*)
      integer (kind=4) :: eknot(lknotig,*)
      real    (kind=8) :: knots(dknotig,*)

      logical          :: pcomp,errck,tinput,pinput, prt, strtfl
      character        :: tx*15, ktype(2)*6
      integer (kind=4) :: j, ii,jj,jx, knum,knums,knume
      real    (kind=8) :: td(16),tmax, value

      save

      data       ktype / '  Open',' Close' /

!     Set initial parameters to read knot vectors

      ii     = 0
      jj     = 0
      jx     = 0
      knum   = 0
      knums  = 1
      knume  = 0
      tmax   = 0.0d0
      strtfl =.false.
      tx = 'start'
      do while(.not.pcomp(tx,'    ',4))
        errck = tinput(tx,1,td,15)
        if(pcomp(tx,'knot',4) .or.
     &     pcomp(tx,'open',4) .or.
     &     pcomp(tx,'side',4)) then

!         Start new knot vector description

          strtfl        = .true.
          knum          = nint(td(1))
          lknot(0,knum) = 1
          lknot(1,knum) = nint(td(2))
          knume         = max(knume,knum)

!         Set knot vector

          do ii = 1,min(13,lknot(1,knum))
            knots(ii,knum) = td(ii+2)
          end do ! ii
          do jj = 14,lknot(1,knum),16
            errck = pinput(td,16)
            do ii = jj,min(jj+15,lknot(1,knum))
              knots(ii,knum) = td(ii-jj+1)
            end do ! ii
          end do ! jj

!         Compute order of knot

          value = knots(1,knum)
          jj    = 1
          do while (value.eq.knots(jj,knum))
             jj = jj + 1
          end do ! while
          if(jj.eq.2) then ! error no repeated nodes
            write(iow,3000)
            call plstop(.true.)
          endif
          lknot(2,knum) = jj - 2
          lknot(3,knum) = lknot(1,knum) - lknot(2,knum) - 1

!         Check for repeated knots at end of knot vector

          if(lknot(1,knum).gt.0) then

            call cknotv(knots(1,knum),lknot(1,knum))

            jj = lknot(1,knum)
            value = knots(jj,knum)
            do while( value.eq.knots(jj,knum))
              jj = jj - 1
            end do ! while

            if(lknot(1,knum)-jj-1.ne.lknot(2,knum)) then
              call mprint(knots(1,knum),1,jj,1,'KNOT_ERROR')
              write(iow,3001)
!             call plstop(.true.)
            endif
          endif

!       Closed or Periodic knots

        elseif(pcomp(tx,'clos',4) .or.
     &         pcomp(tx,'peri',4)) then

!         Start new knot vector description

          strtfl        = .true.
          knum          = nint(td(1))
          lknot(0,knum) = 2
          lknot(1,knum) = nint(td(2))
          lknot(2,knum) = nint(td(3))
          lknot(3,knum) = lknot(1,knum) - lknot(2,knum) - 1
          knume         = max(knume,knum)

!         Set knot vector

          do ii = 1,min(12,lknot(1,knum))
            knots(ii,knum) = td(ii+3)
          end do ! ii
          do jj = 13,lknot(1,knum),16
            errck = pinput(td,16)
            do ii = jj,min(jj+15,lknot(1,knum))
              knots(ii,knum) = td(ii-jj+1)
            end do ! ii
          end do ! jj
        endif
        jx    = max(jx,lknot(1,knum))

!       Compute element array

        jj = 0
        do ii = 2,lknot(1,knum)
          if(knots(ii,knum).gt.knots(ii-1,knum)) then
            jj             = jj + 1
            eknot(jj,knum) = ii - 1
          endif
        end do ! ii
      end do ! while

!     Output knot results

      if(prt) then
        write(iow,2000) head,(j,j=1,jx)
        do knum = knums,knume
          write(iow,2001) knum,ktype(lknot(0,knum)),
     &                    lknot(2,knum),lknot(3,knum),
     &                    (knots(j,knum),j=1,lknot(1,knum))
        end do ! knum
      endif

!     Formats

2000  format(/1x,19a4,a3//5x,'K N O T   V e c t o r s'//
     &  ' Knum  Type Ord  CP ',5(i3,'-Knot',4x)/
     &                 (20x,5(i3,'-Knot',4x)))

2001  format(i5,a6,2i4,1p,5e12.4/(19x,1p,5e12.4))

3000  format(' *ERROR*  Knot has no repeated initial values'/
     &       '          Check for wrong INSERT on BLOCK')
3001  format(' *ERROR*  Knot has wrong number of repeated final values'/
     &       '          Check for wrong INSERT on BLOCK')

      end subroutine pknots

      subroutine cknotv(knots,lknot)

      implicit   none

      integer (kind=4) :: lknot(4)
      real    (kind=8) :: knots(*), kn
      integer (kind=4) :: order, length, l, ll

      order  = lknot(2)
      length = lknot(1)

!     Count number of repeated final values

      ll = 0
      kn = knots(length)
      do l = length-1,1,-1
        if(knots(l).eq.kn) then
          ll = ll + 1
        else
          exit
        endif
      end do ! l

!     Adjust length

      if(ll+1.eq.length .and. kn.lt.0.0d0) then
        lknot(1) = 2*length
        lknot(3) = lknot(1) - lknot(2) - 1
        do l = length+1,lknot(1)
          knots(l) = 0.0d0
        end do ! l
      elseif(ll.gt.order) then
        lknot(1) = lknot(1) - (ll - order)
        lknot(3) = lknot(1) - lknot(2) - 1
      elseif(ll.lt.order .and. kn.lt.0.0d0) then
        lknot(1) = lknot(1) + order + 1
        lknot(3) = lknot(1) - lknot(2) - 1
        do l = ll,lknot(1)
          knots(l) = 0.0d0
        end do ! l
      endif

      end subroutine cknotv

!$Id:$
      subroutine pnblkel(nuren,tnume, knots, lknot,lside,nsides,nblksd,
     &                    nblk,ktnum)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute total number of elements

!     Inputs:

!     Outputs:
!        e        - Number of nurb elements
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cnurb.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'nblend.h'

      integer    i,j,n, e, nuren,tnume, nume,nure
      integer    blkd,nbs1,nbs2, nd1,nd2, nkn, sd2
      integer    lknot(0:4,*),lside(2,*),nsides(dsideig,*)
      integer    nblk(14,*),ktnum(6,*),nblksd(dblokig,*)
      real*8     knots(dknotig,*)

      nuren = 0
      do n = 1,nurbk

        blkd = nblk(1,n)             ! nblkdm(n)
        if(blkd.le.2) then
          nbs1 = nblksd(1,n)
          nbs2 = nbs1
          nkn = lside(2,nbs1)        ! kside(nbs1)
          ktnum(1,n) = nkn           ! knotnum(*)
          if(blkd.eq.2) then
            do i = 2,nblk(4,n)      ! lblksd(n)
              nbs2 = nblksd(i,n)
              if(nkn.ne.lside(2,nbs2)) then
                write(  *,4000) n,nbs2,nkn
                write(iow,4000) n,nbs2,nkn
                call plstop(.true.)
              endif
            end do ! i
            ktnum(1,n) = nkn         ! knotnum(*)
          endif

!         Compute number of element spaces in direction 1

          e    = 0
          j    = lknot(2,nkn)
          nure = j + 1
          nume = 1
          do while (j.lt.lknot(1,nkn)-lknot(2,nkn))
            if(knots(j,nkn).ne.knots(j+1,nkn)) then
              e = e + 1
            else
              do while (knots(j+1,nkn).eq.knots(j+2,nkn) .and.
     &                  j.lt.lknot(1,nkn))
                j = j + 1
              end do ! while
            endif
            j = j + 1
          end do ! while

          nume = nume * e

!         Check other directions

          if(blkd.gt.1) then

!           Get knot vector in other direction

            nd1 = nsides(1,nbs1)
            nd2 = nsides(1,nbs2)

            sd2 = 0
            do i = 1,nursd
              if    (nd1.eq.nsides(1,i) .and.
     &               nd2.eq.nsides(lside(1,i),i)) then
                sd2 = i
                exit
              elseif(nd2.eq.nsides(1,i) .and.
     &               nd1.eq.nsides(lside(1,i),i)) then
                sd2 = i
                exit
              endif
            end do ! i
            if(sd2.eq.0) then
              write(iow,4001) n,nd1,nd2
            endif
            e    = 0
            nkn  = lside(2,sd2)        ! kside(sd2)
            j    = lknot(2,nkn)
            nure = nure * (j + 1)
            do while (j.lt.lknot(1,nkn)-lknot(2,nkn))
              if(knots(j,nkn).ne.knots(j+1,nkn)) then
                e = e + 1
              else
                do while (knots(j+1,nkn).eq.knots(j+2,nkn) .and.
     &                    j.lt.lknot(1,nkn))
                  j = j + 1
                end do ! while
              endif
              j = j + 1
            end do ! while
            ktnum(2,n) = nkn

            nume  = nume * e
          endif ! blkd.gt.1

!         Set total number of elements

          tnume = tnume + nume
          nuren = max(nuren,nure)

        endif ! 1- and 2-d blocks

      end do ! n

!     Formats

4000  format(' *ERROR* in NBLKEL: Block =',i5,' Side =',i5,' has ',
     &       'inconsistent knot vector'/'         Knot =',i5)

4001  format(' *ERROR* in NBLKEL: Bad Side vector in block =',i4/
     &       '         ND1 =',i5,' ND2 =',i5)

      end

!$Id:$
      subroutine pnblk3el(ndm,nuren,tnume, knots, lknot,lside,nblksd,
     &                    nblk,ktnum)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Compute total number of 3-d elements

!     Inputs:
!        ndm      - Number spatial dimensions

!     Outputs:
!        e        - Number of nurb elements
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cnurb.h'
      include   'igdata.h'
      include   'iofile.h'
      include   'nblend.h'

      integer    i,j,n, e, ndm, nuren,tnume, nume,nure
      integer    nk(3),lknot(0:4,*),lside(2,*),nblksd(dblokig,*)
      integer    nblk(14,*),ktnum(6,*)
      real*8     knots(dknotig,*)

      nuren = 0
      do n = 1,nurbk

        if(nblk(1,n).eq.3) then           ! nblkdm(n)
          nk(1) = nblk(6,n)               ! nbk3d(1,n)
          nk(2) = nblk(7,n)               ! nbk3d(2,n)
          e     = nblksd(1,n)
          nk(3) = lside(2,e)              ! kside(e)
          ktnum(1,n) = nk(1)
          ktnum(2,n) = nk(2)
          ktnum(3,n) = nk(3)

!         Compute number of element spaces in direction 1

          nume = 1
          nure = 1
          do i = 1,ndm
            e = 0
            j = lknot(2,nk(i))                   ! Knot order
            nure = nure * (j + 1)
            do while (j.lt.lknot(1,nk(i))-lknot(2,nk(i)))
              if(knots(j,nk(i)).ne.knots(j+1,nk(i))) then
                e = e + 1
              else
                do while (knots(j+1,nk(i)).eq.knots(j+2,nk(i)) .and.
     &                    j.lt.lknot(1,nk(i)))
                  j = j + 1
                end do ! while
              endif
              j = j + 1
            end do ! while

            nume = nume * e
          end do ! i

!         Set total number of elements

          tnume = tnume + nume
          nuren = max(nuren,nure)

        endif ! 3-d blocks

      end do ! n

      end

!$Id:$
      subroutine pknotel(knots,lknot,knotnum, knotlen,knotdiv,ndm)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Set element spacing in each direction of bloc

!     Inputs:
!         ndm             - Space dimension of block

!     Outputs:
!         knotdiv(4,*) - Knot divisions for block
!         knotlen(*)   - Length of each knot vector
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

!     include   'cnurb.h'
      include   'igdata.h'

      integer (kind=4) :: ndm
      integer (kind=4) :: knotnum(*),knotlen(*),knotdiv(4,*)
      integer (kind=4) :: lknot(0:4,*)
      real    (kind=8) :: knots(dknotig,*)


      logical          :: exflg
      integer (kind=4) :: i, ii,i1,j1,k1, ic

      k1 = 0
      do i = 1,ndm
        ii = knotnum(i)
        i1 = lknot(2,ii) + 1
        j1 = 1
        k1 = k1 + 1
        knotdiv(1,k1) = i1
        knotdiv(3,k1) = j1
        ic = lknot(2,ii)
        exflg = .false.
        do while(.not.exflg)
          if(knots(i1+1,ii).gt.knots(i1,ii)) then
            i1            = i1 + 1
            knotdiv(2,k1) = i1
            knotdiv(4,k1) = j1 + ic
            if(i1.lt.(lknot(1,ii) - lknot(2,ii) + 1)) then
              j1            = j1 + 1
              k1            = k1 + 1
              knotdiv(1,k1) = i1
              knotdiv(3,k1) = j1
            endif
          else
            do while(knots(i1+1,ii).eq.knots(i1,ii))
              i1            = i1 + 1
              j1            = j1 + 1
              knotdiv(1,k1) = i1
              knotdiv(3,k1) = knotdiv(4,k1-1)
              if(i1.ge.lknot(1,ii)) exit
            end do ! while
            if(i1.lt.(lknot(1,ii) - lknot(2,ii) + 1)) then
              knotdiv(2,k1) = i1
              knotdiv(4,k1) = j1 + ic
              knotdiv(3,k1) = knotdiv(4,k1) - ic
            else
              k1 = k1 - 1
            endif
          endif
          exflg = i1.ge.(lknot(1,ii) - lknot(2,ii) + 1)
        end do ! while
        knotlen(i) = k1
      end do ! i

      end subroutine pknotel

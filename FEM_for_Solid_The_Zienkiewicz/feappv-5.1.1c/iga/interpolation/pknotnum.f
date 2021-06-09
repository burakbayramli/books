!$Id:$
      subroutine pknotnum(lknot,ktnum,knots, ndm, kmax)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: Set element spacing in each direction of bloc "nb"

!     Inputs:
!         ndm             - Space dimension of block

!     Outputs:
!         knotlen(ndm,nb) - Length of each knot vector
!         kmax            - Maximum 'k1' dimension
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit   none

      include   'cnurb.h'
      include   'igdata.h'

      integer (kind=4) :: ndm, kmax
      integer (kind=4) :: lknot(0:4,*), ktnum(6,*)
      real    (kind=8) :: knots(dknotig,*)

      logical    exflg
      integer (kind=4) :: i, ii,i1,k1, ic, nb

      do nb = 1,nurbk
        k1 = 0
        do i = 1,ndm
          ii = ktnum(i,nb)
          if(ii.gt.0) then  ! For blocks dimensions smaller than ndm
            ic = lknot(2,ii)
            i1 = ic + 1
            k1 = k1 + 1
            exflg = .false.
            do while(.not.exflg)
              if(knots(i1+1,ii).gt.knots(i1,ii)) then
                i1 = i1 + 1
                if(i1.lt.(lknot(1,ii) - lknot(2,ii) + 1)) then
                  k1 = k1 + 1
                endif
              else
                do while(knots(i1+1,ii).eq.knots(i1,ii))
                  i1 = i1 + 1
                  if(i1.ge.lknot(1,ii)) exit
                end do ! while
                if(i1.ge.(lknot(1,ii) - lknot(2,ii) + 1)) then
                  k1    = k1 - 1
                  exflg = .true.
                endif
              endif
            end do ! while
          endif
          ktnum(i+3,nb) = k1
        end do ! i
      end do ! nb

!     Compute kmax (maximum for 'kdiv' dimension in ktdiv)

      kmax = 0
      do nb = 1,nurbk
        do i = 1,ndm
          kmax = max(kmax,ktnum(i+3,nb)+1)
        end do ! i
      end do ! nb

      end subroutine pknotnum

!$Id:$
      subroutine plbkpqr(np,iel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set 3-D Plot Sequence for 64-node brick elements

!      Inputs:
!         np        - Order of element (3 for 64 node brick)
!         iel       - Element number: > 0 for user    elements
!                                     < 0 for program elements

!      Outputs:
!         none      - Sequesnce returned in common /pdata6/
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'pdata5.h'
      include  'pdata6.h'

      integer      :: np, iel, i
      integer      :: ns(3),nt,nu, ns12

      save

!     Set control variables

      do i = 1,3
        ns(i) = np + 1
      end do ! i
      ns12 = ns(1)*ns(2)

!     Set number of points

      if(iel.gt.0) then

!       Trace around bottom

        do i = 1,ns(1)
          ipord(i,iel) = i
        end do ! i
        nt = ns(1)
        nu = ns(1)

        do i = 2,ns(2)
          nt            = nt + 1
          nu            = nu + ns(1)
          ipord(nt,iel) = nu
        end do ! i

        do i = ns(1)-1,1,-1
          nt            = nt + 1
          nu            = nu - 1
          ipord(nt,iel) = nu
        end do ! i

        do i = ns(2)-1,1,-1
          nt            = nt + 1
          nu            = nu - ns(1)
          ipord(nt,iel) = nu
        end do ! i

!       Up first 3-edge

        do i = 2,ns(3)
          nt            = nt + 1
          nu            = nu + ns12
          ipord(nt,iel) = nu
        end do ! i

!       Around top first edge

        do i = 2,ns(1)
          nt            = nt + 1
          nu            = nu + 1
          ipord(nt,iel) = nu
        end do ! i

!       Down-up second 3-edge

        do i = ns(3)-1,1,-1
          nt            = nt + 1
          nu            = nu - ns12
          ipord(nt,iel) = nu
        end do ! i

        do i = 2,ns(3)
          nt            = nt + 1
          nu            = nu + ns12
          ipord(nt,iel) = nu
        end do !

!       Second top edge

        do i = 2,ns(2)
          nt            = nt + 1
          nu            = nu + ns(1)
          ipord(nt,iel) = nu
        end do ! i

!       Down-up third 3-edge

        do i = ns(3)-1,1,-1
          nt            = nt + 1
          nu            = nu - ns12
          ipord(nt,iel) = nu
        end do ! i

        do i = 2,ns(3)
          nt            = nt + 1
          nu            = nu + ns12
          ipord(nt,iel) = nu
        end do !

!       Third top edge

        do i = ns(1)-1,1,-1
          nt            = nt + 1
          nu            = nu - 1
          ipord(nt,iel) = nu
        end do ! i

!       Down-up fourth 3-edge

        do i = ns(3)-1,1,-1
          nt            = nt + 1
          nu            = nu - ns12
          ipord(nt,iel) = nu
        end do ! i

        do i = 2,ns(3)
          nt            = nt + 1
          nu            = nu + ns12
          ipord(nt,iel) = nu
        end do !

!       Last top edge

        do i = ns(2)-1,1,-1
          nt            = nt + 1
          nu            = nu - ns(1)
          ipord(nt,iel) = nu
        end do ! i

        inord(iel) = nt

      elseif(iel.lt.0) then

!       Trace around bottom

        do i = 1,ns(1)
          epord(i,-iel) = i
        end do ! i
        nt = ns(1)
        nu = ns(1)

        do i = 2,ns(2)
          nt            = nt + 1
          nu            = nu + ns(1)
          epord(nt,-iel) = nu
        end do ! i

        do i = ns(1)-1,1,-1
          nt            = nt + 1
          nu            = nu - 1
          epord(nt,-iel) = nu
        end do ! i

        do i = ns(2)-1,1,-1
          nt            = nt + 1
          nu            = nu - ns(1)
          epord(nt,-iel) = nu
        end do ! i

!       Up first 3-edge

        do i = 2,ns(3)
          nt            = nt + 1
          nu            = nu + ns12
          epord(nt,-iel) = nu
        end do ! i

!       Around top first edge

        do i = 2,ns(1)
          nt            = nt + 1
          nu            = nu + 1
          epord(nt,-iel) = nu
        end do ! i

!       Down-up second 3-edge

        do i = ns(3)-1,1,-1
          nt            = nt + 1
          nu            = nu - ns12
          epord(nt,-iel) = nu
        end do ! i

        do i = 2,ns(3)
          nt            = nt + 1
          nu            = nu + ns12
          epord(nt,-iel) = nu
        end do !

!       Second top edge

        do i = 2,ns(2)
          nt            = nt + 1
          nu            = nu + ns(1)
          epord(nt,-iel) = nu
        end do ! i

!       Down-up third 3-edge

        do i = ns(3)-1,1,-1
          nt            = nt + 1
          nu            = nu - ns12
          epord(nt,-iel) = nu
        end do ! i

        do i = 2,ns(3)
          nt            = nt + 1
          nu            = nu + ns12
          epord(nt,-iel) = nu
        end do !

!       Third top edge

        do i = ns(1)-1,1,-1
          nt            = nt + 1
          nu            = nu - 1
          epord(nt,-iel) = nu
        end do ! i

!       Down-up fourth 3-edge

        do i = ns(3)-1,1,-1
          nt            = nt + 1
          nu            = nu - ns12
          epord(nt,-iel) = nu
        end do ! i

        do i = 2,ns(3)
          nt            = nt + 1
          nu            = nu + ns12
          epord(nt,-iel) = nu
        end do !

!       Last top edge

        do i = ns(2)-1,1,-1
          nt            = nt + 1
          nu            = nu - ns(1)
          epord(nt,-iel) = nu
        end do ! i

        exord(-iel) = nt

      endif

      end subroutine plbkpqr

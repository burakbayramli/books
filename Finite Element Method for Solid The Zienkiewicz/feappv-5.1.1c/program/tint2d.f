!$Id:$
      subroutine tint2d(l,lint,el)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set gauss points and weights for triangular elements

!      Inputs:
!         l       - Number of gauss points indicator

!      Outputs:
!         lint    - Total number of points
!         el(4,*) - Area coordinate points and weights for quadrature
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'iofile.h'

      integer       :: l, lint
      real (kind=8) :: el(4,*), r0,r1,r2, ww, eta

      save

      data ww, eta / 0.3333333333333333d0 , 0.1666666666666667d0 /

!     1-point gauss integration

      if(l.eq.1) then
        el(1,1) = ww
        el(2,1) = ww
        el(3,1) = ww
        el(4,1) = 1.d0
        lint    = 1

!     3-point integration: mid-edge points

      elseif(l.eq.3) then
        el(1,1) = 0.d0
        el(2,1) = 0.5d0
        el(3,1) = 0.5d0
        el(4,1) = ww

        el(1,2) = 0.5d0
        el(2,2) = 0.d0
        el(3,2) = 0.5d0
        el(4,2) = ww

        el(1,3) = 0.5d0
        el(2,3) = 0.5d0
        el(3,3) = 0.d0
        el(4,3) = ww

        lint    = 3

!     3-point integration: interior points

      elseif(l.eq.-3) then

        el(1,1) = 1.0d0 - ww
        el(2,1) = eta
        el(3,1) = eta
        el(4,1) = ww

        el(1,2) = eta
        el(2,2) = 1.0d0 - ww
        el(3,2) = eta
        el(4,2) = ww

        el(1,3) = eta
        el(2,3) = eta
        el(3,3) = 1.0d0 - ww
        el(4,3) = ww

        lint    = 3

!     4-point gauss integration

      elseif(l.eq.4) then
        el(1,1) =  ww
        el(2,1) =  ww
        el(3,1) =  ww
        el(4,1) = -27.d0/48.d0

        el(1,2) =  0.6d0
        el(2,2) =  0.2d0
        el(3,2) =  0.2d0
        el(4,2) =  25.d0/48.d0

        el(1,3) =  0.2d0
        el(2,3) =  0.6d0
        el(3,3) =  0.2d0
        el(4,3) =  el(4,2)

        el(1,4) =  0.2d0
        el(2,4) =  0.2d0
        el(3,4) =  0.6d0
        el(4,4) =  el(4,2)

        lint    =  4

!     6-point nodal integration

      elseif(l.eq.6) then

        el(1,1) =  1.0d0
        el(2,1) =  0.0d0
        el(3,1) =  0.0d0
        el(4,1) =  eta

        el(1,2) =  0.0d0
        el(2,2) =  1.0d0
        el(3,2) =  0.0d0
        el(4,2) =  eta

        el(1,3) =  0.0d0
        el(2,3) =  0.0d0
        el(3,3) =  1.0d0
        el(4,3) =  eta

        el(1,4) =  0.5d0
        el(2,4) =  0.5d0
        el(3,4) =  0.0d0
        el(4,4) =  eta

        el(1,5) =  0.0d0
        el(2,5) =  0.5d0
        el(3,5) =  0.5d0
        el(4,5) =  eta

        el(1,6) =  0.5d0
        el(2,6) =  0.0d0
        el(3,6) =  0.5d0
        el(4,6) =  eta

        lint    =  6

!     6-point order 4 formula

      elseif(l.eq.-6) then

        el(1,1) = 0.816847572980459d0
        el(2,1) = 0.091576213509771d0
        el(3,1) = 0.091576213509771d0
        el(4,1) = 0.109951743655322d0

        el(1,2) = 0.091576213509771d0
        el(2,2) = 0.816847572980459d0
        el(3,2) = 0.091576213509771d0
        el(4,2) = 0.109951743655322d0

        el(2,3) = 0.091576213509771d0
        el(1,3) = 0.091576213509771d0
        el(3,3) = 0.816847572980459d0
        el(4,3) = 0.109951743655322d0

        el(1,4) = 0.108103018168070d0
        el(2,4) = 0.445948490915965d0
        el(3,4) = 0.445948490915965d0
        el(4,4) = 0.223381589678011d0

        el(1,5) = 0.445948490915965d0
        el(2,5) = 0.108103018168070d0
        el(3,5) = 0.445948490915965d0
        el(4,5) = 0.223381589678011d0

        el(1,6) = 0.445948490915965d0
        el(2,6) = 0.445948490915965d0
        el(3,6) = 0.108103018168070d0
        el(4,6) = 0.223381589678011d0

        lint    = 6

!     7-point gauss integration

      elseif(l.eq.7) then
        r0      =  sqrt(15.0d0)
        r1      =  3.d0/7.d0
        r2      =  (r0 + r0)/21.d0

        el(1,1) =  ww
        el(2,1) =  el(1,1)
        el(3,1) =  el(1,1)
        el(4,1) =  0.225d0

        el(1,2) =  r1 + r2
        el(2,2) =  0.5d0 - 0.5d0*el(1,2)
        el(3,2) =  el(2,2)
        el(4,2) =  (155.d0 - r0)/1200.d0

        el(1,3) =  el(2,2)
        el(2,3) =  el(1,2)
        el(3,3) =  el(2,2)
        el(4,3) =  el(4,2)

        el(1,4) =  el(2,2)
        el(2,4) =  el(2,2)
        el(3,4) =  el(1,2)
        el(4,4) =  el(4,2)

        el(1,5) =  r1 - r2
        el(2,5) =  0.5d0 - 0.5d0*el(1,5)
        el(3,5) =  el(2,5)
        el(4,5) =  (155.d0 + r0)/1200.d0

        el(1,6) =  el(2,5)
        el(2,6) =  el(1,5)
        el(3,6) =  el(2,5)
        el(4,6) =  el(4,5)

        el(1,7) =  el(2,5)
        el(2,7) =  el(2,5)
        el(3,7) =  el(1,5)
        el(4,7) =  el(4,5)

        lint    =  7

!     12-point order 6 formula

      elseif(l.eq.12) then

        el(1, 1) = 0.873821971016996d0
        el(2, 1) = 0.063089014491502d0
        el(3, 1) = 0.063089014491502d0
        el(4, 1) = 0.050844906370207d0

        el(1, 2) = 0.063089014491502d0
        el(2, 2) = 0.873821971016996d0
        el(3, 2) = 0.063089014491502d0
        el(4, 2) = 0.050844906370207d0

        el(1, 3) = 0.063089014491502d0
        el(2, 3) = 0.063089014491502d0
        el(3, 3) = 0.873821971016996d0
        el(4, 3) = 0.050844906370207d0

        el(1, 4) = 0.501426509658179d0
        el(2, 4) = 0.249286745170910d0
        el(3, 4) = 0.249286745170910d0
        el(4, 4) = 0.116786275726379d0

        el(1, 5) = 0.249286745170910d0
        el(2, 5) = 0.501426509658179d0
        el(3, 5) = 0.249286745170910d0
        el(4, 5) = 0.116786275726379d0

        el(1, 6) = 0.249286745170910d0
        el(2, 6) = 0.249286745170910d0
        el(3, 6) = 0.501426509658179d0
        el(4, 6) = 0.116786275726379d0

        el(1, 7) = 0.636502499121399d0
        el(2, 7) = 0.310352451033785d0
        el(3, 7) = 0.053145049844816d0
        el(4, 7) = 0.082851075618374d0

        el(1, 8) = 0.636502499121399d0
        el(2, 8) = 0.053145049844816d0
        el(3, 8) = 0.310352451033785d0
        el(4, 8) = 0.082851075618374d0

        el(1, 9) = 0.310352451033785d0
        el(2, 9) = 0.636502499121399d0
        el(3, 9) = 0.053145049844816d0
        el(4, 9) = 0.082851075618374d0

        el(1,10) = 0.053145049844816d0
        el(2,10) = 0.636502499121399d0
        el(3,10) = 0.310352451033785d0
        el(4,10) = 0.082851075618374d0

        el(1,11) = 0.310352451033785d0
        el(2,11) = 0.053145049844816d0
        el(3,11) = 0.636502499121399d0
        el(4,11) = 0.082851075618374d0

        el(1,12) = 0.053145049844816d0
        el(2,12) = 0.310352451033785d0
        el(3,12) = 0.636502499121399d0
        el(4,12) = 0.082851075618374d0

        lint     = 12

!     Unspecified quadrature specified

      else
        write(  *,2000) l
        write(iow,2000) l
        lint    = -1
      endif

!     Format

2000  format(' *ERROR* TINT2D: Wrong quadrature, l =',i3)

      end subroutine tint2d

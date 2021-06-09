!$Id:$
      subroutine pfclip(os,is)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose:
!     Set coords for a clip fill

!         Window for plot defined by edges and corners

!          c4          e3          c3
!            o--------------------o
!            |                    |
!            |                    |
!            |                    |
!        e4  |                    |  e2
!            |                    |
!            |                    |
!            |                    |
!            o--------------------o
!          c1          e1          c2

!        Edges  : e_i;   i = 1,2,3,4
!        Corners: c_i;   i = 1,2,3,4

!      Inputs:
!         os        - Exit  edge side number
!         is        - Enter edge side number

!      Outputs:
!         none      - Outputs through common
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'pdatap.h'
      include  'pdataq.h'
      include  'plclip.h'

      integer       :: os,is,cs

      save

!     Add corner points to list

      cs = os
100   if(cs.ne.is) then
        cs   = mod(cs,4) + 1
        ipan     = ipan + 1
        xp(ipan) = real(xc(cs)*0.78125d0)
        yp(ipan) = real(yc(cs)*0.78125d0)
        go to 100
      end if

      end subroutine pfclip

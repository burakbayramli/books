c$Id: pfclip.f,v 1.1 2000/08/24 20:49:58 rlt Exp $
      subroutine pfclip(os,is)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--+---------+---------+---------+---------+---------+---------+-]
c      Purpose:
c     Set coords for a clip fill

c         Window for plot defined by edges and corners

c          c4          e3          c3
c            o--------------------o
c            |                    |
c            |                    |
c            |                    |
c        e4  |                    |  e2
c            |                    |
c            |                    |
c            |                    |
c            o--------------------o
c          c1          e1          c2

c        Edges  : e_i;   i = 1,2,3,4
c        Corners: c_i;   i = 1,2,3,4

c      Inputs:
c         os        - Exit  edge side number
c         is        - Enter edge side number

c      Outputs:
c         none      - Outputs through common
c-----[--+---------+---------+---------+---------+---------+---------+-]

      implicit  none

      include  'pdatap.h'
      include  'pdataq.h'
      include  'plclip.h'

      integer   os,is,cs

      save

c     Add corner points to list

      cs = os
100   if(cs.ne.is) then
        cs   = mod(cs,4) + 1
        ipan     = ipan + 1
        xp(ipan) = xc(cs)*0.78125d0
        yp(ipan) = yc(cs)*0.78125d0
        go to 100
      end if

      end

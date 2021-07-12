c$Id:$
      subroutine setpcd(yyy,v,vv)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c      Purpose: Put string into string in widths of 15

c      Inputs:
c         yyy(*)    - String of input data
c         v         - Character string for compare
c         vv        - String to insert for character string

c      Outputs:
c         yyy(*)    - String after substitution
c-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character yyy*80,v*15,vv*15
      logical   pcomp

      save

      if(pcomp(yyy(31:44),v,14)) yyy(31:45) = vv
      if(pcomp(yyy(46:59),v,14)) yyy(46:60) = vv
      if(pcomp(yyy(61:74),v,14)) yyy(61:75) = vv

      end

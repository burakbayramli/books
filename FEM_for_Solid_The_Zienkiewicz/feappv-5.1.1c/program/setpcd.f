!$Id:$
      subroutine setpcd(yyy,v,vv)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Put string into string in widths of 15

!      Inputs:
!         yyy(*)    - String of input data
!         v         - Character string for compare
!         vv        - String to insert for character string

!      Outputs:
!         yyy(*)    - String after substitution
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      character (len=80) :: yyy
      character (len=15) :: v,vv

      logical       :: pcomp

      save

      if(pcomp(yyy(31:44),v,14)) yyy(31:45) = vv
      if(pcomp(yyy(46:59),v,14)) yyy(46:60) = vv
      if(pcomp(yyy(61:74),v,14)) yyy(61:75) = vv

      end subroutine setpcd

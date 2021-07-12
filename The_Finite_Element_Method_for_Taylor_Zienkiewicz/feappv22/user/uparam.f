c$Id:$
      subroutine uparam(ct,nrk,nrc,nrm,nrt,isw)

c      * * F E A P * * A Finite Element Analysis Program

c....  Copyright (c) 1984-2009: Robert L. Taylor
c                               All rights reserved

c-----[--.----+----.----+----.-----------------------------------------]
c     Purpose: User timeintegration routine.  Specify parameters
c              Command:  tran,user,(ct(i),i=1,3)

c     Inputs :
c       ct(3)   - Command line input parameters
c       isw     - Switch: 0 - set  value of nrt and return
c                         1 - input/set parameters

c     Outputs:
c       nrk     - Vector storing stiffness vector
c       nrc     - Vector storing damping vector
c       nrm     - Vector storing mass vector
c       nrt     - Total vectors required by algorithm
c-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer   nrk,nrc,nrm,nrt,isw
      real*8    ct(3)

      if(isw.eq.0) then
        nrt = 0
      else
      endif

      end

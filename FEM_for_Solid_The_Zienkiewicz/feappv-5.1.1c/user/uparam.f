!$Id:$
      subroutine uparam(ct,nrk,nrc,nrm,nrt,isw)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Purpose: User timeintegration routine.  Specify parameters
!              Command:  tran,user,(ct(i),i=1,3)

!     Inputs :
!       ct(3)   - Command line input parameters
!       isw     - Switch: 0 - set  value of nrt and return
!                         1 - input/set parameters

!     Outputs:
!       nrk     - Vector storing stiffness vector
!       nrc     - Vector storing damping vector
!       nrm     - Vector storing mass vector
!       nrt     - Total vectors required by algorithm
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      integer       :: nrk,nrc,nrm,nrt,isw
      real (kind=8) :: ct(3)

      if(isw.eq.0) then
        nrt = 0
      else
      endif

      end subroutine uparam

!$Id:$
      subroutine ufacelib(pstyp,nel,iu,ufac)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set plot date for user elements

!      Inputs:
!         pstyp    - Element topology
!         nel      - Number element nodes

!      Output:
!         iu(4,*)  - Quadrilateral face node lists
!         ufac     - Number of quad faces
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'
      include   'ublk1.h'

      integer        :: pstyp,nel,iu(4,*),ufac, uptyp

!     Reverse sign on typ

      uptyp   = -pstyp
      ublknum = max(1,uptyp/100)
      uptyp   = mod(uptyp,100)

!     User type 1

      if(uptyp.eq.1) then

        call uface01(uptyp,nel,iu,ufac)

!     User type 2

      elseif(uptyp.eq.2) then

        call uface02(uptyp,nel,iu,ufac)

!     User type 3

      elseif(uptyp.eq.3) then

        call uface03(uptyp,nel,iu,ufac)

!     User type 4

      elseif(uptyp.eq.4) then

        call uface04(uptyp,nel,iu,ufac)

!     User type 5

      elseif(uptyp.eq.5) then

        call uface05(uptyp,nel,iu,ufac)

      else

        write(  *,*) ' **ERROR** User plot number not between -1 and -5'
        write(iow,*) ' **ERROR** User plot number not between -1 and -5'
        call plstop(.true.)

      endif

      end subroutine ufacelib

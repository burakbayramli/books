!$Id:$
      subroutine uftyplib(pstyp,nel,iel)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Set plot date for user elements

!      Inputs:
!         pstyp   - Element topology
!         nel     - Number of element nodes
!         iel     - Element number

!      Output:
!         exord   - Number of plot
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'iofile.h'
      include   'ublk1.h'

      integer        :: pstyp,nel,iel, uptyp

!     Reverse sign on pstyp

      uptyp   = -pstyp
      ublknum = max(1,uptyp/100)
      uptyp   = mod(uptyp,100)

!     User type 1

      if(uptyp.eq.1) then

        call uftyp01(uptyp,nel,iel)

!     User type 2

      elseif(uptyp.eq.2) then

        call uftyp02(uptyp,nel,iel)

!     User type 3

      elseif(uptyp.eq.3) then

        call uftyp03(uptyp,nel,iel)

!     User type 4

      elseif(uptyp.eq.4) then

        call uftyp04(uptyp,nel,iel)

!     User type 5

      elseif(uptyp.eq.5) then

        call uftyp05(uptyp,nel,iel)

      else

        write(  *,*) ' **ERROR** User plot number not between -1 and -5'
        write(iow,*) ' **ERROR** User plot number not between -1 and -5'
        call plstop(.true.)

      endif

      end subroutine uftyplib

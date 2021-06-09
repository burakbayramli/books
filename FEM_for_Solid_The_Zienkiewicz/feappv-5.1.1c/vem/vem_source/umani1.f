!$Id:$
      subroutine umani1

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Make radius constant

!      Inputs:

!      Outputs:
!         none   - Users are responsible for generating outputs
!                  through common blocks, etc.  See programmer
!                  manual for example.
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include 'umac1.h'
      include 'pointer.h'
      include 'comblk.h'

      logical  pcomp

      save

      if (pcomp(uct,'man1',4)) then
          uct = 'brad'
      else

        call u_brad(hr(np(43)))

      end if

      end subroutine umani1

      subroutine u_brad(x)

      implicit   none

      include   'cdata.h'
      include   'sdata.h'
      include   'iofile.h'

      logical          :: setval, tinput, pinput, pcomp
      character        :: text(1)*15
      integer          :: n, nod
      real    (kind=8) :: x(ndm,numnp)
      real    (kind=8) :: td(3), xc(2)
      real    (kind=8) :: rad, rr

      setval = tinput(text,1,td,3)

      if(pcomp(text(1),'radi',4)) then
        rad   = td(1)
        xc(:) = td(2:3)
      else
      endif

      write(iow,'(/a,1p,1e12.4)') ' Bradius = ',rad

      do n = 1,numnp
        setval = pinput(td,3)
        nod = nint(td(3))
        if(nod.gt.0) then
          write(iow,'(i8,1p,2e12.4)') nod, x(1:2,nod)
          rr = sqrt((x(1,nod)-xc(1))**2 + (x(2,nod)-xc(2))**2)
          x(1,nod) = (x(1,nod)-xc(1))*rad/rr + xc(1)
          x(2,nod) = (x(2,nod)-xc(2))*rad/rr + xc(2)
          x(1:2,nod) = x(1:2,nod)*rad/rr
          write(iow,'(i8,1p,2e12.4)') nod, x(1:2,nod)
        else
          exit
        endif
      end do ! n

      end subroutine u_brad

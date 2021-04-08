!$Id:$
      subroutine uplot3(ctl)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plot location of linear Bezier plot nodes

!      Inputs:
!         ctl(3) - Parameters for plots

!      Outputs:
!         Graphics display of nodes
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include 'cnurb.h'
      include 'comblk.h'
      include 'sdata.h'
      include 'pointer.h'
      include 'umac1.h'  ! uct

      logical  pcomp, setval,palloc
      integer  i,ii, icol
      real*8   ctl(3)

!     Provide user plot name

      if (pcomp(uct,'plt3',4)) then
          uct = 'x_li'

!     Perform user plot function

      else

        if(np(277).ne.0) then

!         Set color

          if(nint(ctl(1)).ne.0) then
            icol = max(1,min(16,nint(ctl(1))))
          else
            icol = 7
          endif

!         Plot location of Bezier nodes for plots

          call pppcol(icol,1)
          setval = palloc(115,'TEMP5', nd_lin,  1)
          do i = 1,nd_lin
            mr(np(115)+i-1) = i
          end do

          ii = nint(ctl(1))
!         call pltnod(hr(np(277)),mr(np(115)), ndm,nd_lin, ii,1,nd_lin)
          call pltnod(hr(np(277)),mr(np(115)), ndm,1,1,nd_lin)
          setval = palloc(115,'TEMP5',      0,  1)
        else
          write(*,*) ' *WARNING* Not available for this mesh'
        endif

      end if

      end

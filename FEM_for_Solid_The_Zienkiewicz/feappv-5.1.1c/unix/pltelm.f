!$Id:$
      subroutine pltelm(x,ie,ix,pscale,nie,ndm,nen1,n1,n2)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!     Modification log                                Date (dd/mm/year)
!       Original version                                    01/11/2006
!       1. Remove 'pdata2.h' (unused)                       27/04/2018
!-----[--+---------+---------+---------+---------+---------+---------+-]
!      Purpose: Place element numbers on plots of mesh

!      Inputs:
!         x(ndm,*)   - Nodal coordinates of mesh
!         ie(nie,*)  - Assembly data for material sets
!         ix(nen1,*) - Element nodal connections
!         pscale     - Plot scale factor
!         nie        - Dimension of ie array
!         ndm        - Dimension of x array
!         nen1       - Dimension of ix array
!         n1         - First element number to display
!         n2         - Last element number to display

!      Outputs:
!         none       - Plot outputs to screen/file
!-----[--+---------+---------+---------+---------+---------+---------+-]
      implicit  none

      include  'pbody.h'
      include  'pdata4.h'
      include  'plflag.h'

      logical       :: zoom
      integer       :: nie,ndm,nen1, i,j,n,ii,jj,nn,ma,nd, n1,n2
      integer       :: iplt(50)
      real (kind=8) :: pscale,dx1

      integer       :: ie(nie,*),ix(nen1,*)
      real (kind=8) :: x(ndm,*),xx(3)

      save

!     Write element labels

      dx1 = .005d0/pscale
      nd = 5
      do n = n1,n2
        ma = ix(nen1,n)
        if(ix(nen1-1,n).ge.0 .and. ma.gt.0 .and.
     &            (maplt.eq.0 .or. ma.eq.maplt)) then
          xx(1) = 0.0d0
          xx(2) = 0.0d0
          xx(3) = 0.0d0
          jj = 0
          call pltord(ix(1,n),ie(nie-1,ma), nn,iplt)
          nn = max(1,nn-1)
          do i = 1,nn
            j  = iplt(i)
            ii = ix(j,n)
            if(ii.gt.0) then
              jj = jj + 1
              xx(1) = xx(1) + x(1,ii)
              if(ndm.ge.2) xx(2) = xx(2) + x(2,ii)
              if(ndm.ge.3) xx(3) = xx(3) + x(3,ii)
            endif
          end do ! i
          if(jj.gt.0) then
            xx(1) = xx(1)/jj
            xx(2) = xx(2)/jj
            xx(3) = xx(3)/jj
            if(zoom(xx(1),ndm)) then
              call plotl(xx(1)-dx1*nd,xx(2)-dx1,xx(3),3)
              if(clip) call plabl(n)
            endif
          endif
        endif
      end do ! n

      end subroutine pltelm

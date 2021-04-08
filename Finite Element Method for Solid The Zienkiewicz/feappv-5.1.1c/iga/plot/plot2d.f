!$Id:$
      subroutine plot2d(ie,ix,ip,x,xl,nie,ndm,nen,nen1,
     &                  numel,n1,n2)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Two dimensional mesh plot routine

!      Inputs:
!         ie(nie,*) - Assembly data for material sets
!         ix(nen1,*)- Element nodal connections
!         ip(*)     - Sorted element order
!         x(ndm,*)  - Nodal coordinates
!         ndm       - Dimension of x  array
!         nen       - Number of nodes on element
!         nen1      - Dimension of ix array
!         numel     - Number of elements
!         n1        - Color for plots
!         n2        - Outline indicator

!      Scratch:
!         xl(ndm,*) - Element nodal coordinates

!      Outputs:
!         none      - Plot output to screen/file
!-----[--.----+----.----+----.-----------------------------------------]
      implicit  none

      include  'cnurb.h'
      include  'eldata.h'
      include  'pbody.h'
      include  'pdata2.h'
      include  'qudshp.h'

      integer   nie, ndm, nen, nen1, numel, n1,n2, isp
      integer   i, j, ii, n, nn, nume
      integer   ie(nie,*),ix(nen1,*), ip(*)
      real*8    x(ndm,*),xl(ndm,*)

      save

!     Loop over elements to draw mesh

      if(nurbfl) then
        nume = min(numel,ne_lin)
      else
        nume = numel
      endif
      do nn = 1,nume

        n  = ip(nn)

        if(n.gt.0) then
          if(ix(nen1-1,n).ge.0) then

            ma = ix(nen1,n)

!           Plot correct material number: ma > 0 active material

            if( ma.gt.0 .and. (maplt.eq.0 .or. ma.eq.maplt)) then
              if(n1.eq.0) then
                icolr = ma + 1
              elseif(n1.lt.0) then
                icolr = mod(ix(nen1-1,n),7) + 1
              else
                icolr = mod(n1-1,8) + 1
              endif
              call pppcol(icolr,8)

              do i = 1,nen
                ii = abs(ix(i,n))
                if(ii.gt.0) then
                  nel = i
                  do j = 1,ndm
                    xl(j,i) = x(j,ii)
                  end do ! j
                else
                  do j = 1,ndm
                    xl(j,i) = 0.0d0
                  end do ! j
                endif
              end do ! i

!             Check for a line element

              if(ix(1,n).eq.ix(4,n) .and. ix(2,n).eq.ix(3,n)) nel = 2

              if(n2.ge.0) then
                isp =  1
              else
                isp = -1
              endif
              call plot9(ie(1,ma),ix(1,n),xl,ndm,nel,isp)

            endif
          endif
        endif

      end do ! nn

      end

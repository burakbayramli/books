!$Id:$
      subroutine plenode(ct,ie,ix,x,xbez,wbez,ix_bez,ip_bez,ip)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plot element and associated nodes

!      Inputs:
!         ct(3)        - Data
!         ie(nie,*)    - Element properties
!         ix(nen1,*)   - Element connection list
!         x(ndm,*)     - Deformed coordinates
!         xbez(ndm,*)  - Deformed bezier coordinates
!         wbez(*)      - Bezier weights
!         ix_bez(n1,*) - Element connection list
!         ip_bez(3,*)  - Bezier orders
!         ip(*)        - Active element list

!      Outputs:
!         Screen plot of element
!-----[--.----+----.----+----.-----------------------------------------]
      implicit   none

      include   'cdat1.h'
      include   'cdata.h'
      include   'cnurb.h'
      include   'sdata.h'
      include   'qudshp.h'
      include   'pointer.h'
      include   'comblk.h'

      real*8     ct(3)
      integer    ie(nie,*), ix(nen1,*) , ix_bez(nnpl+1,*), ip(*)
      integer    ip_bez(3,*), ixne(8), jplt(1)
      real*8     x(ndm,*) , xbez(ndm,*), wbez(*), xl(3,8), sg(3,8)
      real*8     wb(64)
      integer    ne,pl,ma,nel, pstyp, iel, i,j,k, pp,qq,rr, iu,iplt

      save

      data       ixne / 1,2,3,4,5,6,7,8 /
      data       sg   / -1.0d0, -1.0d0, -1.0d0,
     &                   1.0d0, -1.0d0, -1.0d0,
     &                   1.0d0,  1.0d0, -1.0d0,
     &                  -1.0d0,  1.0d0, -1.0d0,
     &                  -1.0d0, -1.0d0,  1.0d0,
     &                   1.0d0, -1.0d0,  1.0d0,
     &                   1.0d0,  1.0d0,  1.0d0,
     &                  -1.0d0,  1.0d0,  1.0d0 /

!     Set element number to plot

      ne = max(1,min(numel,nint(ct(1))))  ! Element nodes to plot
      pl = nint(ct(2))                    ! Put number if positive

!     Determine material number of element and plot type

      ma    = ix(nen1,ne)
      pstyp = ie(1    ,ma)
      iel   = ie(nie-1,ma)

!     Compute number of nodes on Bezier element

      nel = 0
      do i = 1,nnpl
        if(ix_bez(i,ne).gt.0) then
          wb(i) = wbez(ix_bez(i,ne))
          nel = i
        else
          wb(i) = 0.0d0
        endif
      end do ! i

!     Set plot coordinates for pstype

      if(    pstyp.eq.1) then    ! Plot line
        call plftyp(1,2,1)
        jplt(1) = iplt
        call pltord(ixne(1),1, iu,jplt)
        do i = 1,2
          call bezier1d(sg(1,i), 1, shp2)
          do k = 1,ndm
            xl(k,i) = 0.0d0
            do j = 1,nel
              xl(k,i) = xl(k,i) + shp2(1,j,1)*xbez(k,ix_bez(j,ne))
            end do ! k
          end do ! j
        end do ! i

      elseif(pstyp.eq.2) then    ! Plot quadrilateral
        call plftyp(2,4,1)
        jplt(1) = iplt
        call pltord(ixne(1),1, iu,jplt(1))
        pp = ip_bez(1,ne)
        qq = ip_bez(2,ne)
        do i = 1,4
          call shp2d_bez(sg(1,i),wb, pp,qq, nel, shp2)
          do k = 1,ndm
            xl(k,i) = 0.0d0
            do j = 1,nel
              xl(k,i) = xl(k,i) + shp2(3,j,1)*xbez(k,ix_bez(j,ne))
            end do ! k
          end do ! j
        end do ! i
      elseif(pstyp.eq.3) then    ! Plot brick
        call plftyp(3,8,1)
        jplt(1) = iplt
        call pltord(ixne(1),1, iu,jplt)
        pp = ip_bez(1,ne)
        qq = ip_bez(2,ne)
        rr = ip_bez(3,ne)
        do i = 1,8
          call shp3d_bez(sg(1,i),wb, pp,qq,rr, nel, shp3)
          do k = 1,ndm
            xl(k,i) = 0.0d0
            do j = 1,nel
              xl(k,i) = xl(k,i) + shp3(4,j,1)*xbez(k,ix_bez(j,ne))
            end do ! k
          end do ! j
        end do ! i
      endif

      write(*,*) ' PLOT ELEMENT',ne,' NODES',pl,nen
!     call iprint(ip,1,numnp,1,'IP')
!     call mprint(x,ndm,numnp,ndm,'X')
!     call iprint(ix,nen1,numel,nen1,'IX')

      call plopen
      call pppcol(8,0)
      do i = 1,nen
        if(ix(i,ne).gt.0) then
          call pltnod(x,ip,ndm,pl,ix(i,ne),ix(i,ne))
        endif
      end do ! i

      end

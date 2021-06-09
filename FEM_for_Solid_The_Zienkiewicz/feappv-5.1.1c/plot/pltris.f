!$Id:$
      subroutine pltris(ic,nc,n,ns,iutot,ndm,ndf,nen,nen1,nlabi,
     +                  icolor,ix,x,xl,v,vc,dx1,vl,vu,tvc,cont,vflg)

!      * * F E A P * * A Finite Element Analysis Program

!....  Copyright (c) 1984-2020: Regents of the University of California
!                               All rights reserved

!-----[--.----+----.----+----.-----------------------------------------]
!      Purpose: Plot contours/fills for 2-d surface elements.
!               N.B. All facets are divided into triangle for plots

!      Inputs:
!         ic        - Contour to plot
!         nc        - Total number of contours
!         n         - Number of element/face for plot
!         ns        - Number of segments to describe element plot
!         iutot     - Element type indicator
!         ndm       - Dimension of x array
!         ndf       - Number dof/node
!         nen       - Number of nodes/element
!         nen1      - Dimension of ix array
!         nlabi     - ??
!         icolor    - Color indicator for lines
!         ix(nen1,*)- Element nodal connections
!         x(ndm,*)  - Nodal coordinates for mesh
!         v(*)      - Values of plot value at nodes of element
!         vc(nc)    - Contour values list
!         dx1       - Offset for placing numbers
!         vl        - Minimum value of contour
!         vu        - Maximum value of contour
!         tvc(9,9)  - Array to indicate where to place numbers near
!                     contour lines
!         cont      - Flag, draw lines if true; otherwise fill
!         vflg      - Flag, add labels to lines if true

!      Scratch:
!         xl(3,*)   - Element nodal coordinate array

!      Outputs:
!         none      - Plot outputs to screen/file
!-----[--.----+----.----+----.-----------------------------------------]

      implicit  none

      include  'pdata2.h'
      include  'pdatri.h'
      include  'plflag.h'

      logical       :: tvc(9,9),cont,vflg,center
      integer       :: ic,nc,n,ns,ndm,ndf,nen,nen1,nlabi,icolor,iutot
      integer       :: nsi,i,j,ii,ivc,jvc,nn,nnn, ilc(4)
      integer       :: ix(nen1,*)
      real (kind=8) :: s,dx1,x1,y1,z1,vv,vl,vu
      real (kind=8) :: x(ndm,*),xl(3,29),v(29),vc(12),vt(4),xt(3,4)

      save

!     Get center values - assign to node 1 and 4 of triangle

      if(ns.gt.4) then
        nsi = 1
        center = .false.
        if(nen.ge.9) then
          ii = 0
          if(iutot.eq.9) ii  = ix(9,n)
          if(iutot.eq.7) ii  = ix(7,n)
          if(ii.gt.0) center = .true.
        endif
        if(center) then
          xt(1,1) = x(1,ii)
          xt(2,1) = x(2,ii)
          if(ndm.ge.3) xt(3,1) = x(3,ii)
          j       = ndf*(ii-1) + ic
          vt(1)   = v(29)
        elseif(ns.eq.9) then
          xt(1,1) = - 0.25d0*(xl(1,1) + xl(1,3) + xl(1,5) + xl(1,7))
     &              + 0.50d0*(xl(1,2) + xl(1,4) + xl(1,6) + xl(1,8))
          xt(2,1) = - 0.25d0*(xl(2,1) + xl(2,3) + xl(2,5) + xl(2,7))
     &              + 0.50d0*(xl(2,2) + xl(2,4) + xl(2,6) + xl(2,8))
          if(ndm.eq.3) then
            xt(3,1) = - 0.25d0*(xl(3,1) + xl(3,3) + xl(3,5) + xl(3,7))
     &                + 0.50d0*(xl(3,2) + xl(3,4) + xl(3,6) + xl(3,8))
          endif
          vt(1) = - 0.25d0*(v(1) + v(3) + v(5) + v(7))
     &            + 0.50d0*(v(2) + v(4) + v(6) + v(8))
        else
          xt(1,1) = 0.d0
          xt(2,1) = 0.d0
          xt(3,1) = 0.d0
          vt(1)   = 0.d0
          ii = ns-1
          do i = 1,ii
            xt(1,1) = xt(1,1) + xl(1,i)
            xt(2,1) = xt(2,1) + xl(2,i)
            if(ndm.ge.3) xt(3,1) = xt(3,1) + xl(3,i)
            vt(1)   = vt(1) + v(i)
          end do
          xt(1,1) = xt(1,1)/ii
          xt(2,1) = xt(2,1)/ii
          xt(3,1) = xt(3,1)/ii
          vt(1)   = vt(1)/ii
        endif
      else
        nsi     = 2
        xt(1,1) = xl(1,1)
        xt(2,1) = xl(2,1)
        xt(3,1) = xl(3,1)
        vt(1) = v(1)
      endif
      xt(1,4) = xt(1,1)
      xt(2,4) = xt(2,1)
      xt(3,4) = xt(3,1)
      vt(4) = vt(1)

!     Loop over subtriangles

      do ii = nsi,ns-nsi

!       Set other points on triangle

        xt(1,2) = xl(1,ii)
        xt(2,2) = xl(2,ii)
        xt(3,2) = xl(3,ii)
        vt(2) = v(ii)
        xt(1,3) = xl(1,ii+1)
        xt(2,3) = xl(2,ii+1)
        xt(3,3) = xl(3,ii+1)
        vt(3) = v(ii+1)
        if(cont) then

!         Plot all contours which intersect element

          do nn = 1,nc
            vv = vc(nn)
            if(vv.ge.vl.and.vv.le.vu) then
              call pppcol(icolor+nn,1)

!             Loop over sides of triangle to find plot points

              j = 3
              do i = 1,3
                if(vv.eq.vt(i)) then
                  x1 = xt(1,i)
                  y1 = xt(2,i)
                  z1 = xt(3,i)
                  call plotl(x1,y1,z1,j)
                  j = 2
                elseif((vt(i)-vv)*(vt(i+1)-vv).lt.0.0d0) then
                  s = (vv - vt(i))/(vt(i+1)-vt(i))
                  x1 = xt(1,i) + s*(xt(1,i+1) - xt(1,i))
                  y1 = xt(2,i) + s*(xt(2,i+1) - xt(2,i))
                  z1 = xt(3,i) + s*(xt(3,i+1) - xt(3,i))
                  call plotl(x1,y1,z1,j)
                  j = 2
                endif

!               Add labels

                if(vflg.and.j.eq.2) then
                  ivc = nint((x1-xmn)*xmx) + 1
                  ivc = max(1,min(9,ivc))
                  jvc = nint((y1-ymn)*ymx) + 1
                  jvc = max(1,min(9,jvc))
                  if(tvc(ivc,jvc)) then
                    tvc(ivc,jvc) = .false.
                    call plotl(x1-dx1,y1,z1,3)
                    nnn = nlabi + nn
                    if(clip) call plabl(nnn)
                    call plotl(x1,y1,z1,3)
                  endif
                endif
              end do
            endif
          end do
        else
          call pltcor(3,ilc,vt,vc,nc)
          call pltefl(3,ilc,xt,vt,vc,nc)
        endif
      end do

!     Draw border around element

      call pppcol(0,1)
      if(.not.cont .and. ipb.eq.0 ) then
        z1 = 0.0d0
        if(ndm.eq.3) z1 = xl(3,1)
        call plotl(xl(1,1),xl(2,1),z1,3)
        do i = 1,ns
          if(ndm.eq.3) z1 = xl(3,i)
          call plotl(xl(1,i),xl(2,i),z1,2)
        end do
        if(ndm.eq.3) z1 = xl(3,1)
        call plotl(xl(1,1),xl(2,1),z1,2)
      endif

      end subroutine pltris
